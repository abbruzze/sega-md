package ucesoft.smd.audio

/**
 * @author Alessandro Abbruzzetti
 *         Created on 09/12/2023 19:54  
 */
class FM(override val sampleRate: Int, override val name: String) extends AudioDevice(sampleRate, name):
  private val ym3438 = new Ym3438
  private val chip = new IYm3438.IYm3438_Type
  private final val LR = Array(0,0)
  private var outputCycles = 0
  private var L, R = 0

  //inline val AUDIO_SCALE_BITS = 3

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
    chip.reset()

  final def write(address:Int,value:Int): Unit =
    ym3438.OPN2_Write(chip, address, value)

  final def read(address:Int): Int =
    ym3438.OPN2_Read(chip,address)

  override final protected def getLevelStereo16Bit(LR:Array[Int]): Unit =
    LR(0) = L * 11
    LR(1) = R * 11
    L = 0
    R = 0



