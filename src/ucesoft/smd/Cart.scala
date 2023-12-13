package ucesoft.smd

object Cart:
  enum ExtraMemoryType:
    case RAM_NO_SAVE, RAM_SAVE, ROM, UNKNOWN
  case class ExtraMemory(memType:ExtraMemoryType,startAddress:Int,endAddress:Int)

/**
 * @author Alessandro Abbruzzetti
 *         Created on 28/08/2023 19:10  
 */
class Cart(val file:String):
  import Cart.*
  private inline val CHECKSUM_ADDR = 0x18E
  private inline val EXTRA_MEMORY_ADDR = 0x1B0
  private inline val NAME_DOMESTIC_ADDR = 0x120
  private inline val NAME_OVERSEA_ADDR = 0x150

  private var rom : Array[Int] = _
  private var extraMemory : ExtraMemory = _
  private var cartNameDomestic, cartNameOversea : String = _

  loadROM()

  private def loadROM(): Unit =
    import java.nio.file.Files
    import java.io.File
    val f = new File(file)
    if !f.exists() then
      throw new IllegalArgumentException(s"Cartridge $file does not exist")
    rom = Files.readAllBytes(f.toPath).map(_.toInt & 0xFF)
    Logger.getLogger.info(s"Loaded ${rom.length} from cartridge $file")

    val fileChecksum = rom(CHECKSUM_ADDR) << 8 | rom(CHECKSUM_ADDR + 1)
    val calculatedChecksum = checksum()
    if fileChecksum != calculatedChecksum then
      Logger.getLogger.warning("ROM checksum %X is different from calculated one %X",fileChecksum,calculatedChecksum)
    if checkExtraMemory() then
      Logger.getLogger.info("Found extra memory: %s [%X,%X]",extraMemory.memType,extraMemory.startAddress,extraMemory.endAddress)

    cartNameDomestic = getCartName(NAME_DOMESTIC_ADDR)
    cartNameOversea = getCartName(NAME_OVERSEA_ADDR)
  private def checksum(): Int =
    var cs = 0
    for a <- 0x200 until rom.length by 2 do
      cs += rom(a) << 8 | (if a + 1 < rom.length then rom(a + 1) else 0)
    cs & 0xFFFF

  private def getCartName(offset:Int): String =
    val sb = new StringBuilder()
    for c <- 0 until 48 do
      sb.append(rom(offset + c).toChar)
    sb.toString.trim

  private def checkExtraMemory(): Boolean =
    if rom(EXTRA_MEMORY_ADDR) == 'R' && rom(EXTRA_MEMORY_ADDR + 1) == 'A' then
      val memType = rom(EXTRA_MEMORY_ADDR + 2)
      val startAddress = rom(EXTRA_MEMORY_ADDR + 4) << 24 | rom(EXTRA_MEMORY_ADDR + 5) << 16 | rom(EXTRA_MEMORY_ADDR + 6) << 8 | rom(EXTRA_MEMORY_ADDR + 7)
      val endAddress = rom(EXTRA_MEMORY_ADDR + 8) << 24 | rom(EXTRA_MEMORY_ADDR + 9) << 16 | rom(EXTRA_MEMORY_ADDR + 10) << 8 | rom(EXTRA_MEMORY_ADDR + 11)
      val mt = memType match
        case 0xA0|0xB0|0xB8 => ExtraMemoryType.RAM_NO_SAVE
        case 0xE0|0xF0|0xF8 => ExtraMemoryType.RAM_SAVE
        case 0xE8 => ExtraMemoryType.ROM
        case _ => ExtraMemoryType.UNKNOWN
      extraMemory = ExtraMemory(mt,startAddress, endAddress)
    extraMemory != null

  def getROM: Array[Int] = rom
  def getExtraMemoryInfo: Option[ExtraMemory] = Option(extraMemory)

  def getDomesticName: String = cartNameDomestic
  def getOveseaName: String = cartNameOversea
