package ucesoft.smd

import ucesoft.smd.audio.{FM, PSG}
import ucesoft.smd.cheat.Cheat.CheatCode
import ucesoft.smd.controller.Controller
import ucesoft.smd.cpu.m68k.{M68000, Memory, Size}
import ucesoft.smd.cpu.z80.Z80

import scala.annotation.tailrec
import scala.compiletime.uninitialized

object MMU:
  inline val Z80_CPU_MEM_OPTION = 1 << 2 // Z80 cpu must use this as read/write option
  inline val VDP_MEM_OPTION     = 2 << 2 // VDP must use this as read option

/**
 * @author Alessandro Abbruzzetti
 * Created on 28/08/2023 18:33
 *
 * Memory map:
 * 0x000000 +----------+
 *          |    ROM   |
 *          |   CART   |
 *          |          |
 * 0x3FFFFF +----------+
 *          |   SEGA   | 0x400000
 *          | RESERVED |
 *          |    (1)   | (1) = Reads return the MSB of the next instruction to be fetched, with the LSB set to zero. Writes do nothing.
 * 0x7FFFFF +----------+
 *          |   SEGA   | 0x800000
 *          | RESERVED |
 *          |    (2)   | (2) = Reading or writing any address will lock up the machine.
 * 0x9FFFFF +----------+
 *          | SYSTEM IO| 0xA00000
 * 0xAFFFFF +----------+
 *          |   SEGA   | 0xB00000
 *          | RESERVED |
 * 0xBFFFFF +----------+
 *          |    VDP   | 0xC00000
 *          |          |
 *          |    (5)   | (5) = 110n n000 nnnn nnnn 000m mmmm , 'm' - VDP addresses (00-1Fh)
 * 0xDFFFFF +----------+
 *          |    RAM   | 0xF00000
 *          |    (6)   | (6) = The RAM is 64K in size and is repeatedly mirrored throughout the entire
 * 0xFFFFFF +----------+
 *
 * 68000 Memory Map
 * Start address	End address	Description
  $000000	$3FFFFF	Cartridge ROM/RAM
  $400000	$7FFFFF	Reserved (used by the Sega CD and 32x)
  $800000	$9FFFFF	Reserved (used by the 32x?)
  $A00000	$A0FFFF	Z80 addressing space
  $A10000	$A10001	Version register (read-only word-long)
  $A10002	$A10003	Controller 1 data
  $A10004	$A10005	Controller 2 data
  $A10006	$A10007	Expansion port data
  $A10008	$A10009	Controller 1 control
  $A1000A	$A1000B	Controller 2 control
  $A1000C	$A1000D	Expansion port control
  $A1000E	$A1000F	Controller 1 serial transmit
  $A10010	$A10011	Controller 1 serial receive
  $A10012	$A10013	Controller 1 serial control
  $A10014	$A10015	Controller 2 serial transmit
  $A10016	$A10017	Controller 2 serial receive
  $A10018	$A10019	Controller 2 serial control
  $A1001A	$A1001B	Expansion port serial transmit
  $A1001C	$A1001D	Expansion port serial receive
  $A1001E	$A1001F	Expansion port serial control
  $A10020	$A10FFF	Reserved
  $A11000			    Memory mode register
  $A11002	$A110FF	Reserved
  $A11100	$A11101	Z80 bus request
  $A11102	$A111FF	Reserved
  $A11200	$A11201	Z80 reset
  $A11202	$A13FFF	Reserved
  $A14000	$A14003	TMSS register
  $A14004	$BFFFFF	Reserved
  $C00000			    VDP Data Port (W)
  $C00002			    VDP Data Port (Mirror)
  $C00004			    VDP Control Port(W)
  $C00006			    VDP Control Port (Mirror)
  $C00008			    H/V Counter
  $C0000A			    H/V Counter (Mirror)
  $C0000C			    H/V Counter (Mirror)
  $C0000E			    H/V Counter (Mirror)
  $C00011			    SN76489 PSG
  $C00013			    SN76489 PSG (Mirror)
  $C00015			    SN76489 PSG (Mirror)
  $C00017			    SN76489 PSG (Mirror)
  $C0001C			    Disable/Debug register
  $C0001E			    Disable/Debug register (Mirror)
  $C0001E	$FEFFFF	Reserved
  $FF0000	$FFFFFF	68000 RAM

  Z80 Memory Map
    From	To	Meaning
  $0000	$1FFF	Sound Ram
  $2000	$3FFF	Reserved
  $4000       YM2612 A0
  $4001       YM2612 D0
  $4002       YM2612 A1
  $4003       YM2612 D1
  $4000	$5FFF	Sound Chip
  $6000       Bank Register
  $6000	$7F10	Misc
  $7F11       PSG 76489
  $7F12	$7FFF	Misc
  $8000	$FFFF	68000 Bank
 */
class MMU(busArbiter:BusArbiter) extends SMDComponent with Memory with Z80.Memory with Z80.IOMemory:
  override protected val smdComponentName: String = "MMU"
  import MMU.*
  import Size.*

  private inline val M68K_WAIT_CYCLES_Z80_ACCESS = 3
  private inline val M68K_WAIT_CYCLES_Z80_ACCESS_BANK = 11
  private inline val Z80_WAIT_CYCLES_ACCESS_BANK = 4

  private final val m68kram = Array.ofDim[Int](0x10000)
  private final val z80ram = Array.ofDim[Int](0x2000)
  private var bankRegisterShifter,bankRegisterBitCounter,bankRegister = 0
  private var allowZ80ToRead68KRam = false
  private var cart : Cart = uninitialized
  private var rom : Array[Int] = uninitialized
  private val os_rom : Array[Int] = loadOSRom()
  private var extraRam : Array[Int] = uninitialized
  private var extraRamStartAddress, extraRamEndAddress = 0
  private var osRomEnabled = false
  private var tmssActive = false
  private val tmssBuffer = Array(0,0,0,0)
  private var tmssUnlocked = false
  private var lastWordOnBus = 0
  private var lockUpAction : () => Unit = uninitialized

  private val controllers = Array.ofDim[Controller](3)
  private var model : Model = uninitialized

  private var m68k : M68000 = uninitialized
  private var z80 : Z80 = uninitialized
  private var vdp : VDP = uninitialized
  private var psg : PSG = uninitialized
  private var fm : FM = uninitialized

  private var ssf2Rom : Array[Int] = uninitialized
  private var ssf2RomPendingState = false
  private val ssf2Banks = Array.fill[Int](7)(-1)

  private def loadOSRom(): Array[Int] =
    val os = getClass.getResource("/resources/rom/Genesis_OS_ROM.bin")
    if os == null then
      log.error("Cannot find OS rom")
      null
    else
      val rom = os.openStream().readAllBytes().map(_.toInt & 0xFF)
      log.info("OS ROM loaded")
      rom

  override def init(): Unit =
    hardReset()

  override def reset(): Unit =
    bankRegisterShifter = 0
    bankRegisterBitCounter = 0
    bankRegister = 0
    for i <- 0 to 2 do
      if controllers(i) != null then
        controllers(i).resetComponent()

    java.util.Arrays.fill(m68kram,0)
    java.util.Arrays.fill(z80ram,0)
    java.util.Arrays.fill(ssf2Banks,-1)
    ssf2RomPendingState = false
    z80ram(0) = 0x76 // HALT

  override def hardReset(): Unit = {
    reset()

    tmssActive = osRomEnabled
    java.util.Arrays.fill(tmssBuffer,0)

    for i <- 0 to 2 do
      if controllers(i) != null then
        controllers(i).hardResetComponent()
  }

  def get68KRAM: Array[Int] = m68kram
  def getZ80RAM: Array[Int] = z80ram

  def setAudioChips(psg:PSG,fm:FM): Unit =
    this.psg = psg
    this.fm = fm

  def setCPUs(m68k:M68000,z80:Z80): Unit =
    this.m68k = m68k
    this.z80 = z80

  def setCart(cart: Cart): Unit =
    this.cart = cart
    cart.getSystemType match
      case Cart.SYSTEM_TYPE.MEGA_DRIVE_SSF_EXT =>
        ssf2Rom = cart.getROM
        val romLen = math.min(ssf2Rom.length,0x40_0000)
        rom = Array.ofDim[Int](romLen)
        System.arraycopy(ssf2Rom,0,rom,0,romLen)

        if !ssf2RomPendingState then
          java.util.Arrays.fill(ssf2Banks,-1)
        else
          ssf2RomPendingState = false
          for b <- ssf2Banks.indices do
            if ssf2Banks(b) != -1 then
              writeSSF2(0xA1_30F3 + (b << 1),ssf2Banks(b))
      case _ =>
        rom = cart.getROM
    extraRam = null
    cart.getExtraMemoryInfo match
      case Some(info) =>
        extraRam = info.extraRAM
        extraRamStartAddress = info.startAddress
        extraRamEndAddress = info.endAddress
      case None =>

  def setModel(model:Model): Unit =
    this.model = model

  def setController(index:Int,c:Controller): Unit =
    if controllers(index) != null then
      controllers(index).disconnect()
    controllers(index) = c
  def getController(index:Int): Controller = controllers(index)

  def enableZ80ToRead68KRam(enable:Boolean): Unit =
    allowZ80ToRead68KRam = enable

  def enableOSROM(enabled:Boolean): Unit =
    osRomEnabled = enabled
    tmssActive = osRomEnabled
    log.info("OS ROM enabled: %s",enabled)


  def setLockUpAction(action: () => Unit): Unit =
    lockUpAction = action

  def setVDP(vdp:VDP): Unit =
    this.vdp = vdp
    
  def patch(patch:CheatCode): Unit =
    if patch.address >= 0xE0_0000 then
      patch.patch(m68kram,0xFFFF)
    else
      patch.patch(rom,0xFF_FFFF)
  def restore(patch:CheatCode): Unit =
    if patch.address >= 0xE0_0000 then
      patch.restore(m68kram,0xFFFF)
    else
      patch.restore(rom,0xFF_FFFF)

  // ========================= M68000 access ======================================
  override final def read(address: Int, size: Size, readOptions: Int): Int =
    if (readOptions & VDP_MEM_OPTION) != 0 then // VDP is reading for DMA transfer to VRAM
      if address < 0x40_0000 then readROM(address,size)
      else if address >= 0xE0_0000 then
        read_68k_RAM(address,size)
      else
        log.warning("VDP is trying to read from memory address: %X",address)
        lastWordOnBus // TODO ??
    else if address < 0x40_0000 then readROM(address,size)
    else if address < 0x80_0000 then readOpenBUS(address,size)
    else if address < 0xA0_0000 then
      log.warning("Reading from 800000_9FFFFF area: %X M68_PC=%X Z80_PC=%X. Locking up machine ...",address,m68k.getLastInstructionPC,z80.getLastPC)
      if lockUpAction != null then
        lockUpAction()
      0
    else if address < 0xA1_0000 then read_68k_z80_space(address,size)
    else if address < 0xA1_0020 then readVersionOrControllers(address,size)
    else if address < 0xA1_1200 then read_68k_BUSREQ(size)
    else if address < 0xA1_1300 then read_68k_RESETREQ()
    else if address < 0xC0_0000 then
      // TODO some addresses simply return last bus value
      log.warning("Reading from A10020_BFFFFF area: %X. Locking up machine ...",address)
      if lockUpAction != null then
        lockUpAction()
      0
    else if address < 0xE0_0000 then // VDP
      if (address & 0xE7_00E0) == 0xC0_0000 then
        readVDP(address & 0x1F, size, readOptions)
      else
        log.warning("Reading from unconnected VDP address: %x",address)
        0xFF // TODO: here what happens ??
    else if readOptions != Z80_CPU_MEM_OPTION then
      read_68k_RAM(address,size)
    else
      0xFF

  override final def write(address: Int, value: Int, size: Size, writeOptions: Int): Unit = 
    if address < 0x40_0000 then writeROM(address, value, size, writeOptions)
    else if address < 0x80_0000 then log.warning(s"Writing to unused space: %X = %X", address, value)
    else if address < 0xA0_0000 then
      log.warning("Writing to 800000_9FFFFF area: %X. Locking up machine ...", address)
      if lockUpAction != null then
        lockUpAction()
    else if address < 0xA1_0000 then write_68k_z80_space(address, value, size)
    else if address < 0xA1_0020 then writeControllers(address, value, size)
    else if address < 0xA1_1200 then write_68k_BUSREQ(value, size)
    else if address < 0xA1_1300 then write_68k_RESETREQ(value, size)
    else if address >= 0xA1_4000 && address <= 0xA1_4003 then
      if osRomEnabled then
        val offset = address - 0xA1_4000
        size match
          case Byte =>
            tmssBuffer(offset) = value
          case Word =>
            tmssBuffer(offset) = value >> 8
            tmssBuffer(offset + 1) = value & 0xFF
          case Long =>
            tmssBuffer(offset) = value >>> 24
            tmssBuffer(offset + 1) = (value >> 16) & 0xFF
            tmssBuffer(offset + 2) = (value >> 8) & 0xFF
            tmssBuffer(offset + 3) = value & 0xFF
        tmssUnlocked = tmssBuffer(0) == 'S' && tmssBuffer(1) == 'E' && tmssBuffer(2) == 'G' && tmssBuffer(3) == 'A'
    else if address == 0xA1_4101 then
      if osRomEnabled then
        tmssActive = (value & 1) == 0
        log.info("Writing TMSS bankswitch register: tmssActive=%s",tmssActive)
    else if address < 0xC0_0000 then
      if cart.getSystemType == Cart.SYSTEM_TYPE.MEGA_DRIVE_SSF_EXT && address >= 0xA1_30F3 && address <= 0xA1_30FF then
        writeSSF2(address,value)
      /* ignored */
    else if address < 0xE0_0000 then // VDP
      if (address & 0xE7_00E0) == 0xC0_0000 then
        writeVDP(address & 0x1F, value, size, writeOptions)
      else
        log.warning("Writing to unconnected VDP address: %X", address)
    else write_68k_RAM(address, value, size)

  private def writeSSF2(address:Int,value:Int): Unit =
    val ssf2Address = (value & 0x3F) << 19
    val bank = address & 0xF
    val romAddress = bank match
      case 0x3 => /* 080000-0FFFFF */
        0x080000
      case 0x5 => /* 100000-17FFFF */
        0x100000
      case 0x7 => /* 180000-1FFFFF */
        0x180000
      case 0x9 => /* 200000-27FFFF */
        0x200000
      case 0xB => /* 280000-2FFFFF */
        0x280000
      case 0xD => /* 300000-37FFFF */
        0x300000
      case 0xF => /* 380000-3FFFFF */
        0x380000
      case _ =>
        0
    if romAddress > 0 then
      ssf2Banks((bank - 3) >> 1) = value
      val len = if ssf2Address + 0x80000 < ssf2Rom.length then 0x80000 else ssf2Rom.length - ssf2Address
      System.arraycopy(ssf2Rom, ssf2Address, rom, romAddress, len)
    log.info("SSF2 write address %X value = %X", address, value)

  // ========================= Z80 access =========================================
  override final def in(addressHI:Int,addressLO:Int) : Int = 0xFF
  override final def out(addressHI:Int,addressLO:Int,value:Int) : Unit =
    println(s"OUT $addressHI/$addressLO $value")
  
  override final def read(address: Int): Int =
    val read = if address < 0x4000 then readZ80Memory(address,Byte)
    else if address < 0x6000 then readYM2612(address,Byte)
    else if address < 0x6100 then 0xFF // reads from bank register always return FF
    else if address < 0x7F00 then 0xFF // reads always return FF
    else if address < 0x7F20 then readVDP(address & 0x1F,Byte,Z80_CPU_MEM_OPTION)
    else if address < 0x8000 then 0xFF // TODO: ??
    else read_z80_bank(address)

    read & 0xFF

  override final def write(address:Int,value:Int): Unit = 
    if address < 0x4000 then writeZ80Memory(address, value, Byte)
    else if address < 0x6000 then writeYM2612(address, value, Byte)
    else if address < 0x6100 then writeZ80BankRegister(value)
    else if address < 0x7F00 then {
      /* UNUSED */
    }
    else if address < 0x7F20 then
      writeVDP(address & 0x1F, value, Byte, Z80_CPU_MEM_OPTION)
    else if address >= 0x8000 then
      write_z80_bank(address, value)
    else {
      //println(s"Z80 is writing at ${address.toHexString} PC=${z80.getLastPC.toHexString}")
      // TODO
    }

  // ========================== WRITES ============================================
  private def writeVDP(address: Int, value: Int, size: Size, writeOptions: Int): Unit =
    //log.info(s"Writing VDP register ${address.toHexString} value = $value size = $size writeOptions=$writeOptions")

    size match
      /*
       Byte-wide writes
       Writing to the VDP control or data ports is interpreted as a 16-bit
       write, with the LSB duplicated in the MSB. This is regardless of writing
       to an even or odd address
       */
      case Size.Byte =>
        address match
          case 0 | 1 | 2 | 3 =>
            val byte = value & 0xFF
            vdp.writeDataPort(byte << 16 | byte)
          case 4 | 5 | 6 | 7 =>
            val byte = value & 0xFF
            vdp.writeControlPort(byte << 16 | byte)
          case 8 | 9 | 0xA | 0xB | 0xC | 0xD | 0xE | 0xF => // Writing to the HV counter will cause the machine to lock up
            if lockUpAction != null then
              lockUpAction()
          case 0x11 | 0x13 | 0x15 | 0x17 =>
            writePSG(value)
          case 0x1C|0x1E =>
            vdp.writeDebugRegister(value)
          case _ =>
            log.warning("Unrecognized byte write to VDP register: %X = %X",address,value)
      case Size.Word =>
        address match
          case 0 | 2 => vdp.writeDataPort(value)
          case 4 | 6 => vdp.writeControlPort(value)
          case 8 | 0xA | 0xC | 0xE => // Writing to the HV counter will cause the machine to lock up
            if lockUpAction != null then
              lockUpAction()
          case 0x10 | 0x12 | 0x14 | 0x16 => // If you want to write to the PSG via word-wide writes, the data must be in the LSB
            writePSG(value & 0xFF)
          case 0x1C|0x1E =>
            vdp.writeDebugRegister(value)  
          case _ =>
            log.warning("Unrecognized word write to VDP register: %X = %X",address,value)
      case Size.Long =>
        writeVDP(address, value >>> 16, Size.Word, writeOptions)
        writeVDP(address + 2, value & 0xFFFF, Size.Word, writeOptions)

  private def writeROM(address: Int, value: Int, size: Size, writeOptions: Int): Unit =
    if extraRam != null && address >= extraRamStartAddress && address <= extraRamEndAddress then
      println(s"Writing extraram: ${address.toHexString} ...")
      val adr = address - extraRamStartAddress
      size match
        case Byte =>
          extraRam(adr) = value & 0xFF
        case Word =>
          extraRam(adr) = (value >> 8) & 0xFF
          extraRam(adr + 1) = value & 0xFF
        case Long =>
          extraRam(adr) = value >>> 24
          extraRam(adr + 1) = (value >> 16) & 0xFF
          extraRam(adr + 2) = (value >> 8) & 0xFF
          extraRam(adr + 3) = value & 0xFF
    else
      log.warning("Writing to ROM address: %X = %X PC=%X",address,value,m68k.getLastInstructionPC)

  @tailrec private def write_68k_z80_space(address:Int,value:Int,size:Size): Unit =
    m68k.addWaitCycles(M68K_WAIT_CYCLES_Z80_ACCESS)
    if busArbiter.isZ80BUSAcquiredBy68K then
      val adr = address & 0xFFFF
      if adr < 0x4000 then writeZ80Memory(address,value,size)
      else if adr < 0x6000 then writeYM2612(address,value,size)
      else if adr < 0x6100 then writeZ80BankRegister(value)
      else if adr < 0x7F00 then {/* ignored */}
      else if adr < 0x7F20 then
        log.info("write_68k_z80_space access to VDP: address=%X adr=%X",address,adr)
        writeVDP(adr & 0x1F,value,size,0)
      else if adr < 0x7FFF then
        log.info("Writing to 7F20_7FFF area from 68k: %X. Locking up machine ...",address)
        if lockUpAction != null then
          lockUpAction()
      else // Addresses A08000-A0FFFFh mirror A00000-A07FFFh, so the 68000 cannot access it's own banked memory.
        log.warning("write_68k_z80_space writing to banked memory: %X",address)
        write_68k_z80_space(adr & 0x7FFF,value,size) // TODO check
    else
      log.warning("Writing to 68k_z80_space but z80BUSREQ is false")

  inline private def writeZ80Memory(address: Int, value: Int, size: Size): Unit =
    if size == Byte then
      z80ram(address & 0x1FFF) = value
    else // 68000 is writing with Word size (Long ?)
      z80ram(address & 0x1FFF) = (value >> 8) & 0xFF

  inline private def writeYM2612(address:Int,value:Int,size:Size): Unit =
    log.info("Writing to YM2612: %X size=$size value=%X",address,value)
    fm.write(address,value & 0xFF)

  /*
   To specify which 32k section you want to access, write the upper nine
   bits of the complete 24-bit address into bit 0 of the bank address
   register, which is at 6000h (Z80) or A06000h (68000), starting with
   bit 15 and ending with bit 23.
   */
  inline private def writeZ80BankRegister(value:Int): Unit =
    bankRegisterShifter = bankRegisterShifter >> 1 | (value & 1) << 8
    bankRegisterBitCounter += 1
    if bankRegisterBitCounter == 9 then
      bankRegisterBitCounter = 0
      bankRegister = bankRegisterShifter
      log.info("Z80 bank register set to %X => %X",bankRegister,bankRegister << 15)

  private def writeControllers(address: Int,_value:Int,size: Size): Unit = {
    var value = _value
    val adr = size match
      case Byte =>
        if (address & 1) == 1 then address
        else -1 // try to write to even address with byte size
      case Word =>
        if (address & 1) == 0 then
          value = _value & 0xFF // TODO check
          address + 1
        else -1 // must never happen
      case Long =>
        log.error("Try to write controllers area with Long size")
        if (address & 1) == 0 then
          value = _value & 0xFF // TODO check
          address + 1
        else -1 // must never happen

    if adr != -1 then
      adr & 0x1F match
        case 0x01 => // version register
        case 0x03 => // reg_data1
          controllers(0).writeData(value)
        case 0x05 => // reg_data2
          controllers(1).writeData(value)
        case 0x07 => // reg_data3
          controllers(2).writeData(value)
        case 0x09 => // reg_ctrl1
          controllers(0).writeControl(value)
        case 0x0B => // reg_ctrl2
          controllers(1).writeControl(value)
        case 0x0D => // reg_ctrl3
          controllers(2).writeControl(value)
        case _ =>
          log.warning("Reading from unimplemented IO register: %X",address)
  }

    /*
     Bit 0 of A11100h (byte access) or bit 8 of A11100h (word access) controls
     the Z80's /BUSREQ line.

     Writing 1 to this bit will request the Z80 bus. You can then release
     the bus later on by writing 0.
     */
    inline private def write_68k_BUSREQ(_value:Int,size:Size): Unit =
      val value = if size == Size.Word then _value >>> 8 else _value
      val busRequest = (value & 1) != 0
      if busRequest then
        busArbiter.m68kRequestZ80BUS()
      else
        busArbiter.m68kReleaseZ80BUS()

    /*
     Bit 0 of A11200h (byte access) or bit 8 of A11200h (word access) controls
     the Z80's /RESET line.

     Writing 0 to this bit will start the reset process. The Z80 manual says you
     have to assert the /RESET line for three Z80 clock cycles as a reset does
     not happen instantly.

     Writing 1 to this bit will stop the reset process. At this point, the Z80
     will start executing from address 0000h onwards.
     */
    inline private def write_68k_RESETREQ(_value:Int,size:Size): Unit =
      val value = if size == Size.Word then _value >>> 8 else _value
      val resetRequest = (value & 1) == 0
      if resetRequest then
        busArbiter.z80StartResetProcess()
      else
        busArbiter.z80StopResetProcess()

  inline private def write_68k_RAM(address: Int,value:Int, size: Size): Unit =
    //log.info(s"Writing 68k RAM ${address.toHexString} = ${value.toHexString} size=$size")
    val adr = address & 0xFFFF
    size match
      case Byte =>
        m68kram(adr) = value & 0xFF
      case Word =>
        m68kram(adr) = (value >> 8) & 0xFF
        m68kram(adr + 1) = value & 0xFF
      case Long =>
        // check address = 0xFFFE for cross RAM/ROM boundary not sure if correct
        if adr == 0xFFFE then
          m68kram(0xFFFE) = value >>> 24
          m68kram(0xFFFF) = (value >> 16) & 0xFF
          rom(0) = (value >> 8) & 0xFF
          rom(1) = value & 0xFF
        else
          m68kram(adr) = value >>> 24
          m68kram(adr + 1) = (value >> 16) & 0xFF
          m68kram(adr + 2) = (value >> 8) & 0xFF
          m68kram(adr + 3) = value & 0xFF

  inline private def write_z80_bank(address: Int,value:Int): Unit =
    m68k.addWaitCycles(M68K_WAIT_CYCLES_Z80_ACCESS_BANK)
    z80.ctx.setAdditionalClockCycles(Z80_WAIT_CYCLES_ACCESS_BANK)
    busArbiter.z80Request68KBUS()
    val _68kAddress = bankRegister << 15 | address & 0x7FFF
    if (_68kAddress & 0xFF0000) == 0xA00000 then
      log.warning("Writing to A00000_A0FFFF area from Z80 while bank accessing. Locking up machine ...")
      if lockUpAction != null then
        lockUpAction()
    else write(_68kAddress,value,Byte)
    /*else if _68kAddress >= 0xE0_0000 then
      if allowZ80ToRead68KRam then // Z80 cannot access 68k's RAM
        m68kram(_68kAddress & 0xFFFF) = value
    else if _68kAddress < 0x40_0000 then
      writeROM(_68kAddress,value, Byte,Z80_CPU_MEM_OPTION)
    else
      log.warning("Z80 is writing bank area with address %X => 68K address %X",address,_68kAddress)*/

  // ========================== READS =============================================

  /*
   Reading this bit will return 0 if the bus can be accessed by the 68000,
   or 1 if the Z80 is still busy.
   */
  inline private def read_68k_BUSREQ(size:Size): Int =
    //if busArbiter.isZ80BUSAcquiredBy68K then 0 else 0x101
    val r = lastWordOnBus & 0xFE | (if busArbiter.isZ80BUSAcquiredBy68K then 0 else 0x1)
    size match
      case Size.Word =>
        r << 8 | lastWordOnBus & 0xFEFF
      case _ =>
        r

  inline private def read_68k_RESETREQ(): Int = 0xFF

  inline private def read_68k_RAM(address: Int, size: Size): Int =
    //log.info(s"Reading 68k RAM address = ${address.toHexString} size=$size")
    val adr = address & 0xFFFF
    size match
      case Byte =>
        m68kram(adr)
      case Word =>
        m68kram(adr) << 8 | m68kram(adr + 1)
      case Long =>
        // check address = 0xFFFE for cross RAM/ROM boundary
        if adr == 0xFFFE then
          m68kram(0xFFFE) << 24 | m68kram(0xFFFF) << 16 | rom(0) << 8 | rom(1)
        else
          m68kram(adr) << 24 | m68kram(adr + 1) << 16 | m68kram(adr + 2) << 8 | m68kram(adr + 3)

  @tailrec
  private def read_68k_z80_space(address: Int, size: Size): Int =
    m68k.addWaitCycles(M68K_WAIT_CYCLES_Z80_ACCESS)
    if busArbiter.isZ80BUSAcquiredBy68K then
      val adr = address & 0xFFFF
      if adr < 0x4000 then readZ80Memory(address,size)
      else if adr < 0x6000 then readYM2612(address,size)
      else if adr < 0x6100 then 0xFF // reads from bank register always return FF
      else if adr < 0x7F00 then 0xFF // reads always return FF
      else if adr < 0x7F20 then
        log.info("read_68k_z80_space access to VDP address=%X adr=%X",address,adr)
        readVDP(address & 0x1F,size,0) // TODO check
      else if adr < 0x7FFF then
        log.warning("Reading from 7F20_7FFF area from 68k: %X. Locking up machine ...",address)
        if lockUpAction != null then
          lockUpAction()
        0
      else // Addresses A08000-A0FFFFh mirror A00000-A07FFFh, so the 68000 cannot access it's own banked memory.
        log.warning("read_68k_z80_space reading from banked memory: %X",address)
        read_68k_z80_space(adr & 0x7FFF,size)
    else
      log.warning("Reading 68k_z80_space but z80BUSREQ is false")
      readOpenBUS(address, size)

  private def readVersionOrControllers(address:Int,size:Size): Int =
    val adr = size match
      case Byte =>
        if (address & 1) == 0 then return 0 // try to read from even address with byte size
        else address
      case Word =>
        if (address & 1) == 0 then address + 1
        else return 0 // must never happen
      case Long =>
        log.error("Try to read VersionOrController area with Long size")
        return 0 // not supported

    adr & 0x1F match
      case 0x01 => // version register
        log.info("Reading version register")
        model.modelType.ordinal << 7 | model.videoType.ordinal << 6 | 1 << 5 | model.versionNumber & 0xF // 1 << 5 = Expansion unit not connected
      case 0x03 => // reg_data1
        controllers(0).readData()
      case 0x05 => // reg_data2
        controllers(1).readData()
      case 0x07 => // reg_data3
        controllers(2).readData()
      case 0x09 => // reg_ctrl1
        controllers(0).readControl
      case 0x0B => // reg_ctrl2
        controllers(1).readControl
      case 0x0D => // reg_ctrl3
        controllers(2).readControl
      case _ =>
        log.warning("Reading from unimplemented IO register: %X",address)
        0

  inline private def read_z80_bank(address:Int): Int =
    m68k.addWaitCycles(M68K_WAIT_CYCLES_Z80_ACCESS_BANK)
    z80.ctx.setAdditionalClockCycles(Z80_WAIT_CYCLES_ACCESS_BANK)
    busArbiter.z80Request68KBUS()
    val _68kAddress = bankRegister << 15 | address & 0x7FFF
    if (_68kAddress & 0xFF0000) == 0xA00000 then
      log.info("Reading from A00000_A0FFFF area from Z80 while bank accessing. Locking up machine ...")
      if lockUpAction != null then
        lockUpAction()
      0
    else read(_68kAddress,Byte,Z80_CPU_MEM_OPTION)

  /*
   address must be < 0x20

   00h : Data port
   02h : Data port
   04h : Control port (1)
   06h : Control port
   08h : HV counter (2)
   0Ah : HV counter
   0Ch : HV counter
   0Eh : HV counter
   11h : SN76489 PSG (3)
   13h : SN76489 PSG
   15h : SN76489 PSG
   17h : SN76489 PSG
   18h : Unused (4)
   1Ah : Unused
   1Ch : Unused
   1Eh : Unused
   */
  private def readVDP(address:Int,size:Size,readOptions:Int): Int =
    //log.info(s"Reading VDP register ${address.toHexString} size = $size readOptions=$readOptions")
    size match
      case Size.Byte =>
        // Reading from even VDP addresses returns the MSB of the 16-bit data,
        // and reading from odd address returns the LSB
        address match
          case 0|2 => vdp.readDataPort() >> 8
          case 1|3 => vdp.readDataPort() & 0xFF
          case 4|6 => vdp.readControlPort() >> 8
          case 5|7 => vdp.readControlPort() & 0xFF
          case 8|0xA|0xC|0xE => vdp.readHVCounter >> 8
          case 9|0xB|0xD|0xF => vdp.readHVCounter & 0xFF
          case 0x10|0x11|0x12|0x13|0x14|0x15|0x16|0x17 => // Reading the PSG addresses will cause the machine to lock up.
            if lockUpAction != null then
              lockUpAction()
            0
          case _ =>
            readOpenBUS(address,size)
      case Size.Word =>
        address match
          case 0|2 => vdp.readDataPort()
          case 4|6 => vdp.readControlPort()
          case 8|0xA|0xC|0xE => vdp.readHVCounter
          case 0x10 | 0x11 | 0x12 | 0x13 | 0x14 | 0x15 | 0x16 | 0x17 => // Reading the PSG addresses will cause the machine to lock up.
            if lockUpAction != null then
              lockUpAction()
            0
          case _ =>
            readOpenBUS(address, size)
      case Size.Long =>
        readVDP(address,Size.Word,readOptions) << 16 | readVDP(address + 2,Size.Word,readOptions)

  inline private def writePSG(value:Int): Unit =
    //log.info("Writing to PSG %X value=%X",address,value)
    psg.write(value & 0xFF)

  inline private def readYM2612(address:Int,size:Size): Int =
    log.info("Reading from YM2612: %X size=%s",address,size)
    fm.read(address)

  inline private def readZ80Memory(address: Int, size: Size): Int =
    //log.info(s"Reading Z80 RAM address = ${address.toHexString} size = $size")
    val value = z80ram(address & 0x1FFF)
    if size == Byte then
      value
    else // 68000 is reading with Word size (Long ?)
      value << 8 | value

  private def readOpenBUS(address:Int,size:Size): Int =
    log.info("Reading open bus: address = %X",address)
    size match
      case Byte =>
        0
      case Word =>
        lastWordOnBus & 0xFF00
      case Long =>
        (lastWordOnBus & 0xFF00) << 16 | lastWordOnBus & 0xFF00

  inline private def readROM(address: Int, size: Size): Int =
    val rom = if tmssActive then os_rom else this.rom

    if address < rom.length then
      lastWordOnBus = size match
        case Byte =>
          rom(address)
        case Word =>
          if address + 1 >= rom.length then
            log.warning("Reading ROM beyond: %X/%X",address,rom.length)
            0
          else
            rom(address) << 8 | rom(address + 1)
        case Long =>
          if address + 3 >= rom.length then
            log.warning("Reading ROM beyond: %X/%X",address,rom.length)
            0
          else
            rom(address) << 24 | rom(address + 1) << 16 | rom(address + 2) << 8 | rom(address + 3)
    else if extraRam != null && address >= extraRamStartAddress && address <= extraRamEndAddress then
      lastWordOnBus = extraRam(address - extraRamStartAddress)
    else
      //log.warning("Reading from a disconnected rom address %X",address)
      lastWordOnBus = 0x0 // ComradeOj's tiny demo wants 0!!

    lastWordOnBus

  // ===================== State =======================================
  override protected def createState(sb: StateBuilder): Unit =
    sb.
      serialize("m68kram",m68kram,true).
      serialize("z80ram",z80ram,true).
      w("bankRegisterShifter",bankRegisterShifter).
      w("bankRegisterBitCounter",bankRegisterBitCounter).
      w("bankRegister",bankRegister).
      w("lastWordOnBus",lastWordOnBus)

    if cart.getSystemType == Cart.SYSTEM_TYPE.MEGA_DRIVE_SSF_EXT then
      val ssfSB = new StateBuilder()
      ssfSB.w("banks",ssf2Banks)
      sb.w("ssf2",ssfSB.build())

  override protected def restoreState(sb: StateBuilder): Unit =
    import sb.*
    val ram = deserialize[Array[Int]]("m68kram",true)
    System.arraycopy(ram,0,m68kram,0,ram.length)
    val zram = deserialize[Array[Int]]("z80ram",true)
    System.arraycopy(zram,0,z80ram,0,zram.length)
    bankRegisterShifter = r[Int]("bankRegisterShifter")
    bankRegisterBitCounter = r[Int]("bankRegisterBitCounter")
    bankRegister = r[Int]("bankRegister")
    lastWordOnBus = r[Int]("lastWordOnBus")

    sb.subStateBuilder("ssf2") match
      case Some(ssfSB) =>
        ssfSB.r("banks",ssf2Banks)
        ssf2RomPendingState = true
      case None =>
