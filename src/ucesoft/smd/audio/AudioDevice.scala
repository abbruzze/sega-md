package ucesoft.smd.audio

import ucesoft.smd.SMDComponent

import java.util.concurrent.LinkedBlockingDeque
import javax.sound.sampled.*

abstract class AudioDevice(_sampleRate:Int,val name:String) extends SMDComponent with Runnable:

  private var sampleRate = _sampleRate
  private var CPU_CYCLE_PER_SECOND = 0
  private var cyclePerSample = 1
  private val queue = new LinkedBlockingDeque[Array[Byte]]
  private var bufferSize = 0

  private var buffer: Array[Byte] = Array()

  private var bufferId = 0
  private var bufferPos = 0
  private var bufferInMillis = 10
  private val thread = new Thread(this,s"AudioDevice-$name")
  private var muted = false
  private var sourceLine : SourceDataLine = _
  private var volumeLine : FloatControl = _
  private var volume = 0
  private var stopped = false
  private val stereoLR = Array(0,0)
  private var pendingNewSampleRate = 0

  private val isStereoInternal = isStereo

  setBufferInMillis(bufferInMillis)
  thread.setPriority(Thread.MAX_PRIORITY)
  
  def setCPUFrequency(f:Int): Unit =
    CPU_CYCLE_PER_SECOND = f
    cyclePerSample = CPU_CYCLE_PER_SECOND / sampleRate
    reset()
    
  def getCyclesPerSample: Int = cyclePerSample

  def setSampleRate(sr:Int): Unit =
    pendingNewSampleRate = sr

  def isStereo: Boolean = false

  def setBufferInMillis(bim:Int) : Unit =
    bufferInMillis = bim
    val scale = if isStereo then 4 else 1 // stereo has 2 channels 16 bits each
    bufferSize = scale * (sampleRate * bim / 1000.0).toInt
    buffer = Array.ofDim[Byte](bufferSize)

  def clock(): Unit =
    if isStereoInternal then
      getLevelStereo16Bit(stereoLR)
      if muted then
        stereoLR(0) = 0
        stereoLR(1) = 0
      val leftLevel = stereoLR(0)
      val rightLevel = stereoLR(1)
      buffer(bufferId) = (leftLevel >> 8).asInstanceOf[Byte] ; bufferId += 1
      buffer(bufferId) = (leftLevel & 0xFF).asInstanceOf[Byte] ; bufferId += 1
      buffer(bufferId) = (rightLevel >> 8).asInstanceOf[Byte] ; bufferId += 1
      buffer(bufferId) = (rightLevel & 0xFF).asInstanceOf[Byte] ; bufferId += 1
    else
      var level = getLevelMono8Bit()
      if muted then
        level = 0
      buffer(bufferId) = level.asInstanceOf[Byte]
      bufferId += 1
    if bufferId == bufferSize then
      queue.put(buffer)
      buffer = Array.ofDim[Byte](bufferSize)
      bufferId = 0

  protected def getLevelMono8Bit(): Int = 0
  protected def getLevelStereo16Bit(LR:Array[Int]): Unit = {}

  override protected def reset(): Unit =
    queue.clear()

  inline private def dequeue() : Array[Byte] = queue.take()

  def start(): Unit =
    if !thread.isAlive then thread.start()

  def isMuted : Boolean = muted

  def mute(muted:Boolean) : Unit =
    this.muted = muted

  def stop(): Unit =
    stopped = true

  inline private def getSourceLine: Option[SourceDataLine] =
    try
      val format = new AudioFormat(sampleRate.toFloat, 8 * (if isStereo then 2 else 1), if isStereo then 2 else 1, true, true)

      val info = new DataLine.Info(classOf[SourceDataLine], format)
      val sourceLine = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
      sourceLine.open(format)

      volumeLine = sourceLine.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl]
      setMasterVolume(100)

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

  def available(): Int = if sourceLine == null then 0 else sourceLine.available()

  override def run(): Unit =
    getSourceLine match
      case Some(sl) =>
        sourceLine = sl
        log.info("Audio System %s started",name)
        while !stopped do
          if pendingNewSampleRate > 0 then
            sampleRate = pendingNewSampleRate
            pendingNewSampleRate = 0
            sourceLine.drain()
            sourceLine.close()
            sourceLine = getSourceLine.get
            log.info("Audio system %s updated with new sample rate %d",name,sampleRate)
          end if
          val samples = queue.take()
          val available = sourceLine.available()
          if available >= samples.length then
            sourceLine.write(samples, 0, samples.length)
          else
            sourceLine.write(samples, 0, available)

        sourceLine.drain()
        sourceLine.close()
        sourceLine = null
        log.info("Audio System %s stopped",name)
      case None =>
        log.error("Cannot initialize audio system")