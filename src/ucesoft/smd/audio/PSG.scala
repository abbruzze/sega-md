package ucesoft.smd.audio
import ucesoft.smd.StateBuilder

/**
 * @author Alessandro Abbruzzetti
 *         Created on 06/12/2023 18:49  
 */
class PSG(sampleRate:Int, override val name:String) extends AudioDevice(sampleRate,name):
  private var chip = new SN76489
  
  final def write(value:Int): Unit = chip.write(value)

  override final def isStereo: Boolean = true

  override def reset(): Unit =
    super.reset()
    chip.reset()

  override def setCPUFrequency(f: Int): Unit =
    super.setCPUFrequency(f)
    chip.init(f,sampleRate)

  //override final protected def getLevelMono8Bit(): Int = sn76489.clock()
  override final protected def getLevelStereo16Bit(LR:Array[Int]): Unit =
    val value16 = chip.clock() << 7
    LR(0) = value16
    LR(1) = value16

  // ================== State ===========================
  override protected def createState(sb: StateBuilder): Unit =
    sb.serialize("chip",chip,zip = true)

  override protected def restoreState(sb: StateBuilder): Unit =
    chip = sb.deserialize[SN76489]("chip",zip = true)