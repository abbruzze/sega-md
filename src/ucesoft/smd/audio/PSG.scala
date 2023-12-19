package ucesoft.smd.audio

/**
 * @author Alessandro Abbruzzetti
 *         Created on 06/12/2023 18:49  
 */
class PSG(sampleRate:Int, override val name:String) extends AudioDevice(sampleRate,name):
  final val sn76489 = new SN76489

  override final def isStereo: Boolean = true

  override def reset(): Unit =
    super.reset()
    sn76489.reset()

  override def setCPUFrequency(f: Int): Unit =
    super.setCPUFrequency(f)
    sn76489.init(f,sampleRate)

  //override final protected def getLevelMono8Bit(): Int = sn76489.clock()
  override final protected def getLevelStereo16Bit(LR:Array[Int]): Unit =
    val value16 = sn76489.clock() << 7
    LR(0) = value16
    LR(1) = value16