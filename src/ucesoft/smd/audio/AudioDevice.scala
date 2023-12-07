package ucesoft.smd.audio

import ucesoft.smd.SMDComponent

import java.util.concurrent.LinkedBlockingDeque
import javax.sound.sampled.*

abstract class AudioDevice(val sampleRate:Int,val name:String) extends SMDComponent with Runnable:

  private var CPU_CYCLE_PER_SECOND = 0
  private var cyclePerSample = 0
  private val queue = new LinkedBlockingDeque[Array[Byte]]
  private var bufferSize = 0

  private var buffer: Array[Byte] = Array()
  private var cycle = 0L

  private var bufferId = 0
  private var bufferPos = 0
  private var bufferInMillis = 10
  private val thread = new Thread(this,s"AudioDevice-$name")
  private var muted = false
  private val muteLock = new Object
  private var sourceLine : SourceDataLine = _
  private var volumeLine : FloatControl = _
  private var volume = 0
  private var stopped = false

  private val isStereoInternal = isStereo

  setBufferInMillis(bufferInMillis)
  thread.setPriority(Thread.MAX_PRIORITY)
  
  def setCPUFrequency(f:Int): Unit =
    CPU_CYCLE_PER_SECOND = f
    cyclePerSample = CPU_CYCLE_PER_SECOND / sampleRate
    reset()

  def isStereo: Boolean = false

  protected def getBytesPerSample: Int = 1

  def setBufferInMillis(bim:Int) : Unit =
    bufferInMillis = bim
    val scale = if isStereo then 2 else 1
    bufferSize = scale * (sampleRate * bim / 1000.0).toInt
    buffer = Array.ofDim[Byte](bufferSize)

  def clock(): Unit =
    if !muted then
      if cycle == cyclePerSample then
        cycle = 0
        val level = getLevel()
        if isStereoInternal then
          buffer(bufferId) = (level >> 8).asInstanceOf[Byte]
          buffer(bufferId + 1) = (level & 0xFF).asInstanceOf[Byte]
          bufferId += 2
        else
          buffer(bufferId) = level.asInstanceOf[Byte]
          bufferId += 1
        if bufferId == bufferSize then
          queue.put(buffer)
          buffer = Array.ofDim[Byte](bufferSize)
          bufferId = 0

      cycle += 1
      
  protected def getLevel(): Int = 0

  override protected def reset(): Unit =
    queue.clear()

  inline private def dequeue() : Array[Byte] =
    muteLock.synchronized {
      while muted do
        muteLock.wait()
    }
    queue.take()

  def start(): Unit =
    if !thread.isAlive then thread.start()

  def isMuted : Boolean = muted

  def mute(muted:Boolean) : Unit =
    muteLock.synchronized {
      this.muted = muted
      muteLock.notify()
    }

  def stop(): Unit =
    stopped = true

  inline private def getSourceLine: Option[SourceDataLine] =
    try
      val format = new AudioFormat(sampleRate.toFloat, 8 * getBytesPerSample, if isStereo then 2 else 1, true, true)

      val info = new DataLine.Info(classOf[SourceDataLine], format)
      val sourceLine = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
      sourceLine.open(format)

      volumeLine = sourceLine.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl]
      setMasterVolume(50)

      sourceLine.start()
      Some(sourceLine)
    catch
      case t: Throwable =>
        None

  def setMasterVolume(v: Int): Unit =
    if volumeLine != null then
      val max = volumeLine.getMaximum
      val min = volumeLine.getMinimum / 2f
      volumeLine.setValue((v / 100.0f) * (max - min) + min)
      volume = v

  def getVolume: Int = volume

  override def run(): Unit =
    getSourceLine match
      case Some(sl) =>
        sourceLine = sl
        log.info("Audio System %s started",name)
        while !stopped do
          val samples = queue.take()
          val available = sourceLine.available()
          if available >= samples.length then
            sourceLine.write(samples, 0, samples.length)
          else
            sourceLine.write(samples, 0, available)

        sourceLine.drain()
        sourceLine.close()
      case None =>
        log.error("Cannot initialize audio system")