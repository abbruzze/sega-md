package ucesoft.smd.cpu.svp

import ucesoft.smd.cpu.m68k.Size
import ucesoft.smd.cpu.m68k.Size.{Byte, Long, Word}
import ucesoft.smd.{Cart, Clock, MMU}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 28/05/2024 13:50
 *
 *         68k space      SVP space     word address    name
 *         0-1fffff       0-1fffff       0- fffff       game ROM
 *         200000-2fffff              ?              ?  unused (1)
 *         300000-31ffff  300000-31ffff  180000-18ffff  DRAM
 *         320000-37ffff              ?              ?  3 mirrors od DRAM
 *         380000-38ffff              ?              ?  unused (1)
*                      ?  390000-3907ff  1c8000-1c83ff  IRAM
 *         390000-39ffff              ?              ?  "cell arrange" 1
 *         3a0000-3affff              ?              ?  "cell arrange" 2
 *         3b0000-3fffff              ?              ?  unused (2)
 *         a15000-a1500f            n/a            n/a  Status/control registers
 *
 *         unused (1) - reads seem to return data from internal bus (last word read by
 *         SSP160x). Writes probably have no effect.
 *         unused (2) - reads return 0xffff, writes have no effect.
 */
class SVPMapper(cart:Cart) extends MMU.M68KMapper with SVPMemory:
  override protected val smdComponentName : String = "SVPMapper"

  override final val iramRomWord = Array.ofDim[Int](0x10000)
  private final val svpRom = Array.ofDim[Int](0x400)
  private final val gameRom = cart.getROM
  private final val dram = Array.ofDim[Int](0x10000)
  private var lastWord = 0
  private val svp = new SVP(this)

  override def reset(): Unit =
    java.util.Arrays.fill(dram,0)
    java.util.Arrays.fill(iramRomWord,0,0x3FF,0)

  override protected def init(): Unit =
    val bios = getClass.getResource("/resources/rom/svp_rom.bin")
    if bios == null then
      log.error("svp_rom.bin not found in resources")
      return

    val rom = bios.openStream().readAllBytes().map(_.toInt & 0xFF).sliding(2,2).map(w => w(0) << 8 | w(1)).toArray
    log.info("SVP ROM loaded")
    System.arraycopy(rom,0,iramRomWord,0xFC00,0x400)
    for a <- 0x400 until 0xFC00 do
      val ga = a << 1
      iramRomWord(a) = gameRom(ga) << 8 | gameRom(ga + 1)
    log.info("IRAM/ROM ready")

    add(svp)

  def getSVP: SVP = svp
  def getDRAM: Array[Int] = dram

  override def getClockable: Clock.Clockable = svp

  override def getClockPeriod: Int = 8
  override def getCycles: Int = 8

  override final def isAddressMapped(address: Int): Boolean =
    (address >= 0x20_0000 && address < 0x40_0000) || (address >= 0xA1_5000 && address < 0xA1_5010)

  override final def svpExternalRead(wordAddress: Int): Int =
    lastWord = if wordAddress < 0x100000 then
      val byteAddress = wordAddress << 1
      gameRom(byteAddress) << 8 | gameRom(byteAddress + 1)
    else if wordAddress < 0x18_0000 then
      lastWord
    else if wordAddress < 0x1C_0000 then
      dram(wordAddress & 0xFFFF)
    else if wordAddress < 0x1C_8000 then
      lastWord
    else if wordAddress < 0x1C_8400 then
      iramRomWord(wordAddress & 0x3FF)
    else
      println(s"SVP external read from ${wordAddress.toHexString}")
      0xFFFF

    lastWord

  override final def svpExternalWrite(wordAddress: Int, value: Int): Unit =
    if wordAddress >= 0x18_0000 && wordAddress < 0x1C_0000 then
      dram(wordAddress & 0xFFFF) = value & 0xFFFF
    else if wordAddress >= 0x1C_8000 && wordAddress < 0x1C_8400 then
      iramRomWord(wordAddress & 0x3FF) = value

  override final def read(address: Int, size: Size, readOptions: Int): Int =
    if address < 0x30_0000 then // unused (1)
      println("Reading unused(1) SVP space")
      size match
        case Byte => lastWord & 0xFF
        case Word => lastWord
        case Long => lastWord << 16 | lastWord
    else if address < 0x38_0000 then // DRAM
      val adr = (address >> 1) & 0xFFFF
      size match
        case Byte =>
          dram(adr) & 0xFF // ?
        case Word =>
          dram(adr)
        case Long =>
          dram(adr) << 16 | dram((adr + 1) & 0xFFFF)
    else if address < 0x39_0000 then // unused (1)
      println("Reading unused(1) SVP space")
      size match
        case Byte => lastWord & 0xFF
        case Word => lastWord
        case Long => lastWord << 16 | lastWord
    else if address < 0x3A_0000 then // cell arrange 1
      // ignore size
      var adr = address >> 1
      adr = (adr & 0x7001) | ((adr & 0x3e) << 6) | ((adr & 0xfc0) >> 5)
      dram(adr & 0xFFFF)
    else if address < 0x3B_0000 then // cell arrange 2
      // ignore size
      var adr = address >> 1
      adr = (adr & 0x7801) | ((adr & 0x1e) << 6) | ((adr & 0x7e0) >> 4)
      dram(adr & 0xFFFF)
    else if address >= 0xA1_5000 && address < 0xA1_5010 then
      size match
        case Byte =>
          val read = readReg(address & ~1)
          if (address & 1) != 0 then read & 0xFF else read >> 8
        case Word =>
          readReg(address)
        case Long =>
          readReg(address) << 16 | readReg(address + 2)
    else // unused (2)
      size match
        case Byte => 0xFF
        case Word => 0xFFFF
        case Long => 0xFFFFFFFF
  end read

  private def readReg(address:Int): Int =
    address & 0xF match
      case 0 | 2 =>
        svp.m68kReadXST()
      case 4 =>
        svp.m68kReadPM0()
      case _ =>
        println(s"M68K reads from ${address.toHexString}")
        0

  override final def write(address: Int, value: Int, size: Size, writeOptions: Int): Unit =
    if address >= 0x30_0000 && address < 0x38_0000 then // DRAM
      val adr = (address >> 1) & 0xFFFF
      size match
        case Byte =>
          dram(adr) = value // ??
          println("M68k writes byte to DRAM!!")
        case Word =>
          dram(adr) = value
        case Long =>
          dram(adr) = value >>> 16
          dram((adr + 1) & 0xFFFF) = value & 0xFFFF
    else if address >= 0xA1_5000 && address < 0xA1_5010 then
      size match
        case Byte =>
          println(s"M68k writes register with BYTE size ${address.toHexString} = ${value.toHexString}")
        case Word =>
          writeReg(address,value)
        case Long =>
          writeReg(address,value >>> 16)
          writeReg(address + 2,value & 0xFFFF)
  end write

  private def writeReg(address:Int,value:Int): Unit =
    address & 0xF match
      case 0 | 2 =>
        svp.m68kWriteXST(value)
      case 6 =>
        /*
         possibly halts the SVP. Before doing DMA from DRAM, 68k code
         writes 0xa, and after it's finished, writes 0. This is probably
         done to prevent SVP accessing DRAM and avoid bus clashes.
         */
        svp.halt(value == 0xA)
      case 8 =>
        /*
         possibly causes an interrupt. There is (unused?) code which
         writes 0, 1, and again 0 in sequence.
         */
        println(s"SVP Interrupt? $value")
      case _ =>
        println(s"M68K writes to ${address.toHexString}")