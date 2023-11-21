package ucesoft.smd

/**
 * @author Alessandro Abbruzzetti
 *         Created on 28/08/2023 19:10  
 */
class Cart(val file:String):
  private inline val CHECKSUM_ADDR = 0x18E

  private var rom : Array[Int] = _

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
      Logger.getLogger.warning(s"ROM checksum ${fileChecksum.toHexString} is different from calculated one ${calculatedChecksum.toHexString}")

  private def checksum(): Int =
    var cs = 0
    for a <- 0x200 until rom.length by 2 do
      cs += rom(a) << 8 | (if a + 1 < rom.length then rom(a + 1) else 0)
    cs & 0xFFFF

  def getROM: Array[Int] = rom
