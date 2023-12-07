package ucesoft.smd.audio

/**
 * @author Alessandro Abbruzzetti
 *         Created on 06/12/2023 18:49  
 */
class PSG(override val sampleRate:Int, override val name:String) extends AudioDevice(sampleRate,name):
  final val sn76489 = new SN76489

  override def setCPUFrequency(f: Int): Unit =
    super.setCPUFrequency(f)
    sn76489.init(f,sampleRate)

  override protected def getLevel(): Int = sn76489.clock()


