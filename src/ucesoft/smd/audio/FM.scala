package ucesoft.smd.audio
import ucesoft.smd.StateBuilder

/**
 * @author Alessandro Abbruzzetti
 *         Created on 09/12/2023 19:54  
 */
class FM(sampleRate: Int, override val name: String) extends AudioDevice(sampleRate, name):
  override protected val smdComponentName = "FM"
  private val ym3438 = new Ym3438
  private var chip = new IYm3438.IYm3438_Type
  private final val LR = Array(0,0)
  private var outputCycles = 0
  private var L, R = 0

  inline private val AUDIO_SCALE_SHIFT = 3

  override final def isStereo: Boolean = true

  override final def clock(): Unit =
    ym3438.OPN2_Clock(chip, LR)
    L += LR(0)
    R += LR(1)
    outputCycles += 1
    if outputCycles == 24 then
      outputCycles = 0
      super.clock()

  override def reset(): Unit =
    super.reset()
    ym3438.OPN2_Reset(chip)
    L = 0
    R = 0

  final def write(address:Int,value:Int): Unit =
    ym3438.OPN2_Write(chip, address, value)

  final def read(address:Int): Int =
    ym3438.OPN2_Read(chip,address)

  override final protected def getLevelStereo16Bit(LR:Array[Int]): Unit =
    /*L = (L + pL) >> 1
    R = (R + pL) >> 1
    pL = L
    pR = R*/
    LR(0) = L << AUDIO_SCALE_SHIFT
    LR(1) = R << AUDIO_SCALE_SHIFT
    L = 0
    R = 0

  // ======================= State ========================
  override protected def createState(sb: StateBuilder): Unit =
    sb.
      w("LR",LR).
      w("L",L).
      w("R",R).
      w("outputCycles",outputCycles).
      serialize("chip",chip,zip = true)

  override protected def restoreState(sb: StateBuilder): Unit =
    import sb.*
    r("LR",LR)
    L = r[Int]("L")
    R = r[Int]("R")
    outputCycles = r[Int]("outputCycles")
    chip = deserialize[IYm3438.IYm3438_Type]("chip",zip = true)



