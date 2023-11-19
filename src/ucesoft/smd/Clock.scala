package ucesoft.smd

import ucesoft.smd.cpu.m68k.M68000
import ucesoft.smd.cpu.z80.Z80

import java.util.concurrent.CountDownLatch
import scala.collection.mutable

object Clock:
  trait EventID:
    def cancel(): Unit
    def isCanceled: Boolean

  trait Clockable:
    def clock(cycles:Long): Unit

class Clock (val name: String,private var clocksPerSecond: Int) extends Runnable:
  import Clock.*

  private class ClockEvent(val when:Long,val action:Clockable,val period:Int = 0):
    var canceled : Boolean = false
  private class EventList(val e:ClockEvent,var next:EventList = null)

  private val thread : Thread = {
    val t = new Thread(this, s"MasterClock-$name")
    t.setPriority(Thread.MAX_PRIORITY)
    t
  }
  private var running = false
  private var clockCycles = 0L

  private var suspended = false
  private val suspendedLock = new Object
  private var suspendedNotifier = new CountDownLatch(1)
  private val shutdownNotifier = new CountDownLatch(1)

  private var warpMode = false
  private var lastCorrectionTime = 0L
  private var lastCorrectionCycles = 0L
  private var nextPerformanceMeasurementTime = 0L
  private var lastPerformance = 0
  private var throttleStartedAt = 0L
  private var skipThrottle = false
  private var throttleCycleCount = 0
  private inline val THROTTLE_CYCLE_TARGET = 10_000
  private inline val PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS = 1 * 1000
  private var freqDiv1000, freqInvBy1000 = 0.0

  private var events : EventList = _

  private var errorHandler : Throwable => Unit = _

  private var m68k : M68000 = _
  private var z80 : Z80 = _
  private var vdp : VDP = _

  private var m68KRemainingCycles, z80RemainingCycles = 0

  private var m68kClockDivider = 1
  private var z80ClockDivider = 1
  private var vdpClockDivider = 1

  private var m68kClockSteps = 0
  private var z80ClockSteps = 0
  private var vdpClockSteps = 0

  private var m68kClockCycles = 0L
  private var z80ClockCycles = 0L
  private var vdpClockCycles = 0L

  private var m68kClockCount = 0
  private var z80ClockCount = 0
  private var vdpClockCount = 0

  setFrequency(clocksPerSecond)

  final def setM68KClockDivider(div:Int): Unit =
    m68kClockDivider = div
    m68kClockSteps = div
    m68kClockCount = 0

  final def setZ80ClockDivider(div: Int): Unit =
    z80ClockDivider = div
    z80ClockSteps = div
    z80ClockCount = 0

  final def setVDPClockDivider(div: Int): Unit =
    vdpClockDivider = div
    vdpClockSteps = div
    vdpClockCount = 0

  final def setComponents(m68k:M68000,z80:Z80,vdp:VDP): Unit =
    this.m68k = m68k
    this.z80 = z80
    this.vdp = vdp

  final def setErrorHandler(eh:Throwable => Unit): Unit =
    this.errorHandler = errorHandler

  final def setFrequency(clocksPerSecond:Int): Unit =
    this.clocksPerSecond = clocksPerSecond
    freqDiv1000 = clocksPerSecond / 1000.0
    freqInvBy1000 = 1000.0 / clocksPerSecond

  final def setWarpMode(enabled:Boolean): Unit = warpMode = enabled


  final def cycles: Long = clockCycles

  final def shutdown(waitForShutdown:Boolean = false): Unit =
    running = false
    if waitForShutdown then
      shutdownNotifier.await()

  final def start(): Unit =
    if !running then
      thread.start()

  final def cyclesForMillis(millis:Float): Int =
    math.round(clocksPerSecond * millis / 1000f)

  final def scheduleMillis(millis:Float,action:Clockable,isPeriodic: Boolean = false): EventID =
    schedule(cyclesForMillis(millis),action,isPeriodic)
  final def schedule(cyclesFromNow:Int,action:Clockable,isPeriodic: Boolean = false): EventID =
    val event = new ClockEvent(clockCycles + cyclesFromNow,action,if isPeriodic then cyclesFromNow else 0)
    val id = new EventID:
      override def cancel(): Unit = event.canceled = true
      override def isCanceled: Boolean = event.canceled

    if events == null then
      events = new EventList(event)
    else if event.when <= events.e.when then
      events = new EventList(event, events)
    else
      var ptr = events
      var ptrNext = events.next
      val when = event.when
      while ptrNext != null && when > ptrNext.e.when do
        ptr = ptrNext
        ptrNext = ptrNext.next

      ptr.next = new EventList(event, ptrNext)

    id

  override final def run(): Unit =
    running = true
    while running do
        if suspended then
          while suspended do
            suspendedLock.synchronized {
              suspendedNotifier.countDown()
              suspendedLock.wait()
            }
        try
          checkEvents()
          doAction()
          throttleCycleCount += 1
          if throttleCycleCount == THROTTLE_CYCLE_TARGET then
            throttleCycleCount = 0
            throttle()
        catch
          case t: Throwable =>
            if errorHandler != null then
              errorHandler(t)
            else
              t.printStackTrace()
    end while

    shutdownNotifier.countDown()

  private inline def doAction(): Unit =
    clockCycles += 1
    // m68k
    m68kClockCount += 1
    if m68kClockCount == m68kClockSteps then
      m68kClockCount = 0
      m68kClockCycles += 1
      emulateM68KCycle()
    // z80
    z80ClockCount += 1
    if z80ClockCount == z80ClockSteps then
      z80ClockCount = 0
      z80ClockCycles += 1
      emulateZ80Cycle()
    // VDP
    vdpClockCount += 1
    if vdpClockCount >= vdpClockSteps then
      vdpClockCount = 0
      vdpClockCycles += 1
      vdp.clock(vdpClockCycles)

  private inline def emulateM68KCycle(): Unit =
    if m68KRemainingCycles == 0 then
      m68KRemainingCycles = m68k.execute() - 1
    else
      m68KRemainingCycles -= 1

  private inline def emulateZ80Cycle(): Unit =
    if z80RemainingCycles == 0 then
      //println(z80.disassemble(z80.ctx.PC)._1)
      z80RemainingCycles = z80.clock() - 1
    else
      z80RemainingCycles -= 1

  private inline def checkEvents(): Unit =
    while events != null && clockCycles >= events.e.when do
      if !events.e.canceled then
        events.e.action.clock(clockCycles)
        if events.e.period > 0 then
          schedule(events.e.period,events.e.action,isPeriodic = true)
      val next = events.next
      events.next = null // cut from list
      events = next

  private inline def setupNextMeasurement(): Unit =
    lastCorrectionTime = System.currentTimeMillis
    lastCorrectionCycles = clockCycles
    throttleStartedAt = clockCycles
    nextPerformanceMeasurementTime = System.currentTimeMillis + PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS

  private inline def throttle(): Unit =
    if !warpMode && !skipThrottle then
      val timeDiff = System.currentTimeMillis - lastCorrectionTime
      val cyclesDiff = cycles - lastCorrectionCycles
      val expectedCycles = timeDiff * freqDiv1000
      if cyclesDiff > expectedCycles then
        val waitTime = freqInvBy1000 * (cyclesDiff - expectedCycles)
        val millis = math.floor(waitTime).asInstanceOf[Int]
        val nanos = ((waitTime - millis) * 1000).toInt
        Thread.sleep(millis,nanos)

    if skipThrottle || System.currentTimeMillis > nextPerformanceMeasurementTime then
      skipThrottle = false
      val executed = cycles - throttleStartedAt
      lastPerformance = math.round(100.0 * executed / clocksPerSecond / (PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS / 1000)).toInt
      setupNextMeasurement()

  final def getLastPerformance: Int =
    lastPerformance

  final def isPaused: Boolean = suspended

  final def pause(): Unit =
    if Thread.currentThread() != thread then
      if !suspended then
        suspended = true
        suspendedNotifier.await()
        suspendedNotifier = new CountDownLatch(1)

  final def play(): Unit =
    if suspended then
      suspendedLock.synchronized {
        suspended = false
        suspendedLock.notify()
      }
