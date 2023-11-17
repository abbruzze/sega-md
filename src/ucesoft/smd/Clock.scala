package ucesoft.smd

import java.util.concurrent.CountDownLatch
import scala.collection.mutable

object Clock:
  trait EventID:
    def cancel(): Unit
    def isCanceled: Boolean

  trait Clockable:
    def clock(cycles:Long,skipCycles:Int => Unit): Unit
    def onClockError(t:Throwable): Unit = throw t

  private val masterClockMap = new mutable.HashMap[String,Clock]()
  
  def getClock(name:String): Option[Clock] = masterClockMap.get(name)
  
  def makeMasterClock(name:String,clocksPerSecond:Int): Clock =
    val masterClock = new Clock(name,clocksPerSecond)
    masterClockMap += name -> masterClock
    masterClock

class Clock private (val name:String,val clocksPerSecond:Int,private var masterClock : Clock = null,private var action : Clock.Clockable = null) extends Runnable:
  import Clock.*
  private class DerivedClockInfo(val cyclesPerSecond:Int, val parentClocks:Int, val clock:Clock) {
    private var clockCount = 0

    def incClockAndCheck(): Unit =
      clockCount += 1
      if clockCount >= parentClocks then
        clockCount = 0
        clock.run()
  }
  private class ClockEvent(val when:Long,val action:Clockable,val period:Int = 0):
    var canceled : Boolean = false
  private class EventList(val e:ClockEvent,var next:EventList = null)

  final val isMasterClock : Boolean = masterClock == null
  private val thread = if isMasterClock then 
    val t = new Thread(this,s"MasterClock-$name")
    t.setPriority(Thread.MAX_PRIORITY)
    t
  else null
  private var running = false
  private var clockCycles = 0L
  private val subClocks = new collection.mutable.HashMap[String,Clock]
  private var clocks = Array[DerivedClockInfo]()

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
  private inline val PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS = 1 * 1000
  private var freqDiv1000, freqInvBy1000 = 0.0

  private var skipCycles = 0

  private var events : EventList = _

  initFreq()

  private def initFreq(): Unit =
    if isMasterClock then
      freqDiv1000 = clocksPerSecond / 1000.0
      freqInvBy1000 = 1000.0 / clocksPerSecond

  final def setWarpMode(enabled:Boolean): Unit = warpMode = enabled

  final def deriveDivClock(name:String,divFactor:Int,action:Clock.Clockable = null): Clock = deriveClock(name,this.clocksPerSecond / divFactor,action)
  final def deriveClock(name:String,cyclesPerSecond:Int,action:Clock.Clockable = null): Clock =
    if subClocks.contains(name) then
      throw new IllegalArgumentException(s"Invalid name '$name': already exists")

    val clockRatio = math.round(this.clocksPerSecond.toDouble / cyclesPerSecond).toInt
    if clockRatio == 0 then
      throw new IllegalArgumentException(s"Invalid clock frequency: $cyclesPerSecond: must be less than ${this.clocksPerSecond}")

    val subClock = new Clock(name,clocksPerSecond,if isMasterClock then this else masterClock,action)
    subClock.running = true
    subClocks += name -> subClock
    val clockInfo = new DerivedClockInfo(cyclesPerSecond,clockRatio,subClock)
    clocks = clocks :+ clockInfo
    subClock

  final def changeDivClock(name:String,divFactor:Int): Option[Clock] = changeClock(name,this.clocksPerSecond / divFactor)
  final def changeClock(name:String,cyclesPerSecond:Int): Option[Clock] =
    var i = 0
    while i < clocks.length do
      if clocks(i).clock.name == name then
        val clockRatio = math.round(this.clocksPerSecond.toDouble / cyclesPerSecond).toInt
        if clockRatio == 0 then
          throw new IllegalArgumentException(s"Invalid clock frequency: $cyclesPerSecond: must be less than ${this.clocksPerSecond}")

        val subClock = new Clock(name, clocksPerSecond, if isMasterClock then this else masterClock, clocks(i).clock.action)
        subClock.running = true
        subClocks.update(name,subClock)
        val clockInfo = new DerivedClockInfo(cyclesPerSecond,clockRatio,subClock)
        clocks(i) = clockInfo

        return Some(subClock)
      else
        i += 1

    None

  final def setAction(action:Clockable): Unit = this.action = action

  final def getClock(name:String): Option[Clock] = subClocks.get(name)

  final def cycles: Long = clockCycles

  final def shutdown(waitForShutdown:Boolean = false): Unit =
    if !isMasterClock then
      masterClock.shutdown(waitForShutdown)
    else
      running = false
      if waitForShutdown then
        shutdownNotifier.await()

  final def start(): Unit =
    if !running then
      if isMasterClock then
        thread.start()
      else
        running = true

  final def scheduleMillis(millis:Float,action:Clockable,isPeriodic: Boolean = false): EventID =
    schedule(math.round(clocksPerSecond * millis / 1000f),action,isPeriodic)
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
    try
      if isMasterClock then
        running = true

        while running do
            if suspended then
              while suspended do
                suspendedLock.synchronized {
                  suspendedNotifier.countDown()
                  suspendedLock.wait()
                }
            checkEvents()
            doAction()
            throttle()
        end while

        shutdownNotifier.countDown()
      else
        checkEvents()
        doAction()
    catch
      case t: Throwable =>
        if action != null then
          action.onClockError(t)
        else
          t.printStackTrace()

  private inline def checkEvents(): Unit =
    while events != null && clockCycles >= events.e.when do
      if !events.e.canceled then
        events.e.action.clock(clockCycles,cycles => skipCycles = cycles)
        if events.e.period > 0 then
          schedule(events.e.period,events.e.action,isPeriodic = true)
      val next = events.next
      events.next = null // cut from list
      events = next

  private inline def doAction(): Unit =
    if action != null then
      if skipCycles == 0 then
        action.clock(clockCycles,cycles => skipCycles = cycles)
      else
        skipCycles -= 1
    clockCycles += 1
    // subclocks
    var c = 0
    val clocksCount = clocks.length
    while c < clocksCount do
      val clockInfo = clocks(c)
      clockInfo.incClockAndCheck()
      c += 1

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
        //Thread.sleep(if (waitTime == 0) 1 else waitTime)
        Thread.sleep(millis,nanos)

    if skipThrottle || System.currentTimeMillis > nextPerformanceMeasurementTime then
      skipThrottle = false
      val executed = cycles - throttleStartedAt
      lastPerformance = math.round(100.0 * executed / clocksPerSecond / (PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS / 1000)).toInt
      setupNextMeasurement()

  final def getLastPerformance: Int =
    if isMasterClock then
      lastPerformance
    else
      masterClock.getLastPerformance

  final def isPaused: Boolean = suspended

  final def pause(): Unit =
    if !isMasterClock then
      masterClock.pause()
    else
      if Thread.currentThread() != thread then
        if !suspended then
          suspended = true
          suspendedNotifier.await()
          suspendedNotifier = new CountDownLatch(1)

  final def play(): Unit =
    if !isMasterClock then
      masterClock.play()
    else
      if suspended then
        suspendedLock.synchronized {
          suspended = false
          suspendedLock.notify()
        }
