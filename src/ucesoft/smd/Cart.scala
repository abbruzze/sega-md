package ucesoft.smd

import java.util.zip.CRC32

/*
Address	Size	  Description
================================================
$100	16 bytes	System type
$110	16 bytes	Copyright and release date
$120	48 bytes	Game title (domestic)
$150	48 bytes	Game title (overseas)
$180	14 bytes	Serial number
$18E	2 bytes	  ROM checksum
$190	16 bytes	Device support
$1A0	8 bytes	  ROM address range
$1A8	8 bytes	  RAM address range
$1B0	12 bytes	Extra memory
$1BC	12 bytes	Modem support
$1C8	40 bytes	(reserved, fill with spaces)
$1F0	3 bytes	  Region support
$1F3	13 bytes	(reserved, fill with spaces)
 */
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
  case class ExtraMemory(memType:ExtraMemoryType,startAddress:Int,endAddress:Int,extraRAM:Array[Int])

/**
 * @author Alessandro Abbruzzetti
 *         Created on 28/08/2023 19:10  
 */
class Cart(val file:String,fixChecksum: Boolean = false):
  import Cart.*
  private inline val SYSTEM_TYPE_ADDR = 0x100
  private inline val CHECKSUM_ADDR = 0x18E
  private inline val EXTRA_MEMORY_ADDR = 0x1B0
  private inline val NAME_DOMESTIC_ADDR = 0x120
  private inline val NAME_OVERSEA_ADDR = 0x150
  private inline val SERIAL_NUMBER_ADDR = 0x180
  private inline val REGION_ADDR = 0x1F0
  private inline val DEVICE_ADDR = 0x190

  private val log = Logger.getLogger

  private var rom : Array[Int] = _
  private var extraMemory : ExtraMemory = _
  private var cartNameDomestic, cartNameOversea : String = _
  private var systemType = SYSTEM_TYPE.MEGA_DRIVE
  private var regions : List[Region] = Nil
  private var devices : List[Device] = Nil
  private var crc32 = ""
  private var checksumOK = false
  private var serial = ""

  loadROM()

  private def isSMDFormat: Boolean =
    rom.length > 512 && rom(8) == 0xAA && rom(9) == 0xBB
  private def convertSMD2Bin(): Unit =
    val declaredBlocks = rom(0)
    val realBlocks = (rom.length - 0x200) / 0x4000
    if declaredBlocks != realBlocks then
      log.warning("SMD to BIN conversion: declaredBlocks %d is different from file blocks %d",declaredBlocks,realBlocks)

    val binRom = Array.ofDim[Int](realBlocks * 0x4000)
    for block <- 0 until realBlocks do
      val blockOffset = block * 0x4000
      for p <- 0 until 0x2000 do
        binRom(blockOffset + p * 2 + 1) = rom(0x200 + blockOffset + p)
        binRom(blockOffset + p * 2)     = rom(0x200 + blockOffset + 0x2000 + p)

    rom = binRom

  private def loadROM(): Unit =
    import java.nio.file.Files
    import java.io.File
    val f = new File(file)
    if !f.exists() then
      throw new IllegalArgumentException(s"Cartridge $file does not exist")
    rom = Files.readAllBytes(f.toPath).map(_.toInt & 0xFF)
    log.info(s"Loaded ${rom.length} bytes from cartridge $file")

    if isSMDFormat then
      log.info("Found SMD format, converting to BIN ...")
      convertSMD2Bin()

    val fileChecksum = rom(CHECKSUM_ADDR) << 8 | rom(CHECKSUM_ADDR + 1)
    val calculatedChecksum = checksum()
    checksumOK = fileChecksum == calculatedChecksum
    if !checksumOK then
      log.warning("ROM checksum %X is different from calculated one %X",fileChecksum,calculatedChecksum)
      if fixChecksum then
        rom(CHECKSUM_ADDR) = calculatedChecksum >> 8
        rom(CHECKSUM_ADDR + 1) = calculatedChecksum & 0xFF
        log.info(s"Checksum fixed to ${calculatedChecksum.toHexString}")
    if checkExtraMemory() then
      log.info("Found extra memory: %s [%X,%X]",extraMemory.memType,extraMemory.startAddress,extraMemory.endAddress)

    cartNameDomestic = getCartName(NAME_DOMESTIC_ADDR)
    cartNameOversea = getCartName(NAME_OVERSEA_ADDR)
    systemType = _getSystemType
    regions = getRegions
    devices = getDeviceSupport
    serial = getSerial
    
    val crc = new CRC32
    for i <- rom.indices do
      crc.update(rom(i))
    crc32 = crc.getValue.toHexString
  end loadROM

  private def getSerial: String =
    val sb = new StringBuilder()
    for c <- 0 until 14 do
      sb.append(rom(SERIAL_NUMBER_ADDR + c).toChar)
    sb.toString

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
      extraMemory = ExtraMemory(mt,startAddress, endAddress,Array.ofDim[Int](endAddress - startAddress + 1))
    extraMemory != null

  def getROM: Array[Int] = rom
  def getExtraMemoryInfo: Option[ExtraMemory] = Option(extraMemory)

  def getDomesticName: String = cartNameDomestic
  def getOverseaName: String = cartNameOversea
  def getSystemType: SYSTEM_TYPE = systemType
  def getRegionList: List[Region] = regions
  def getDeviceList: List[Device] = devices
  def getCRC32: String = crc32
  def isChecksumOK: Boolean = checksumOK
  def getSerialNumber: String = serial

  override def toString: String =
    s"""Cart[file="${new java.io.File(file).getName}" serial="$serial" system type="$systemType" CRC32="$crc32" regions=${regions.mkString("[",",","]")} devices=${devices.mkString("[",",","]")} oversea name="$cartNameOversea" extra memory=${if extraMemory == null then "N/A" else extraMemory}]"""
