package ucesoft.smd

object Cart:
  enum SYSTEM_TYPE:
    case MEGA_DRIVE, MEGA_DRIVE_32X, MEGA_DRIVE_EVERDRIVE_EXT, MEGA_DRIVE_SSF_EXT, MEGA_DRIVE_WIFI_EXT, PICO, TERA_DRIVE68K, TERA_DRIVEX86, UNKNOWN
  enum ExtraMemoryType:
    case RAM_NO_SAVE, RAM_SAVE, ROM, UNKNOWN
  enum Region:
    case Japan, Americas, Europe
  enum Device:
    case Controller_3, Controller_6, MasterSystemController, AnalogJoystick,
         Multitap, Lightgun, Activator, Mouse,
         Trackball, Tablet, Paddle, Keyboard,
         RS232, Printer, CDROM, FloppyDrive,
         Download, UNKNOWN
  case class ExtraMemory(memType:ExtraMemoryType,startAddress:Int,endAddress:Int)

/**
 * @author Alessandro Abbruzzetti
 *         Created on 28/08/2023 19:10  
 */
class Cart(val file:String):
  import Cart.*
  private inline val SYSTEM_TYPE_ADDR = 0x100
  private inline val CHECKSUM_ADDR = 0x18E
  private inline val EXTRA_MEMORY_ADDR = 0x1B0
  private inline val NAME_DOMESTIC_ADDR = 0x120
  private inline val NAME_OVERSEA_ADDR = 0x150
  private inline val REGION_ADDR = 0x1F0
  private inline val DEVICE_ADDR = 0x190

  private var rom : Array[Int] = _
  private var extraMemory : ExtraMemory = _
  private var cartNameDomestic, cartNameOversea : String = _
  private var systemType = SYSTEM_TYPE.MEGA_DRIVE
  private var regions : List[Region] = Nil
  private var devices : List[Device] = Nil

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
    systemType = _getSystemType
    regions = getRegions
    devices = getDeviceSupport

  private def checksum(): Int =
    var cs = 0
    for a <- 0x200 until rom.length by 2 do
      cs += rom(a) << 8 | (if a + 1 < rom.length then rom(a + 1) else 0)
    if (rom.length & 1) == 1 then
      cs += rom(rom.length - 1)
    cs & 0xFFFF

  private def getDeviceSupport: List[Device] =
    import Device.*
    val devices = (DEVICE_ADDR until DEVICE_ADDR + 16).map(rom).filterNot(_ == 32).map(_.toChar).toList

    val unfiltered = devices.map {
      case 'J' => Controller_3
      case '6' => Controller_6
      case '0' => MasterSystemController
      case 'A' => AnalogJoystick
      case '4' => Multitap
      case 'G' => Lightgun
      case 'L' => Activator
      case 'M' => Mouse
      case 'B' => Trackball
      case 'T' => Tablet
      case 'V' => Paddle
      case 'K' => Keyboard
      case 'R' => RS232
      case 'P' => Printer
      case 'C' => CDROM
      case 'F' => FloppyDrive
      case 'D' => Download
      case _ => UNKNOWN
    }
    
    unfiltered.filterNot(_ == UNKNOWN)

  private def getRegions: List[Region] =
    val regs = (REGION_ADDR until REGION_ADDR + 3).map(rom).filterNot(_ == 32)
    if regs.nonEmpty then
      getRegionOldStyle(regs(0)) match
        case Some(_) =>
          regs.flatMap(getRegionOldStyle).toList
        case _ =>
          getRegionNewStyle(regs(0))
    else
      Nil

  private def getRegionOldStyle(c:Int): Option[Region] =
    c.toChar.toUpper match
      case 'J' => Some(Region.Japan)
      case 'E' => Some(Region.Europe)
      case 'U' => Some(Region.Americas)
      case _ => None

  private def getRegionNewStyle(c:Int): List[Region] =
    val digit =
      try
        java.lang.Integer.parseInt(c.toChar.toUpper.toString,16)
      catch
        case _:NumberFormatException =>
          0
    List(0,2,3).map(b => digit & (1 << b)).filter(_ > 0).map {
      case 1 => Region.Japan
      case 4 => Region.Americas
      case 8 => Region.Europe
    }


  private def getCartName(offset:Int): String =
    val sb = new StringBuilder()
    for c <- 0 until 48 do
      sb.append(rom(offset + c).toChar)
    sb.toString.trim.split("""\s+""").mkString(" ")

  private def _getSystemType: SYSTEM_TYPE =
    import SYSTEM_TYPE.*
    val sb = new StringBuilder()
    for c <- SYSTEM_TYPE_ADDR until SYSTEM_TYPE_ADDR + 16 do
      sb.append(rom(c).toChar)
    sb.toString().trim match
      case "SEGA MEGA DRIVE" | "SEGA GENESIS" => MEGA_DRIVE
      case "SEGA 32X" => MEGA_DRIVE_32X
      case "SEGA EVERDRIVE" => MEGA_DRIVE_EVERDRIVE_EXT
      case "SEGA SSF" => MEGA_DRIVE_SSF_EXT
      case "SEGA MEGAWIFI" => MEGA_DRIVE_WIFI_EXT
      case "SEGA PICO" => PICO
      case "SEGA TERA68K" => TERA_DRIVE68K
      case "SEGA TERA286" => TERA_DRIVEX86
      case _ => UNKNOWN

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
  def getOverseaName: String = cartNameOversea
  def getSystemType: SYSTEM_TYPE = systemType
  def getRegionList: List[Region] = regions
  def getDeviceList: List[Device] = devices

  override def toString: String =
    s"""Cart[file="${new java.io.File(file).getName}" system type=$systemType regions=${regions.mkString("[",",","]")} devices=${devices.mkString("[",",","]")} oversea name="$cartNameOversea" extra memory=${if extraMemory == null then "N/A" else extraMemory}]"""
