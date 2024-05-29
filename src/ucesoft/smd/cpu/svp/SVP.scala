package ucesoft.smd.cpu.svp
import RegisterType.*
import ucesoft.smd.Clock.Clockable
import ucesoft.smd.{BusArbiter, Cart, MMU, SMDComponent}

object SVP:
  def main(args:Array[String]): Unit =
    val cart = new Cart(Cart.CartFile("""C:\Users\ealeame\OneDrive - Ericsson\Desktop\Virtua Racing (USA).bin"""))
    val mapper = new SVPMapper(cart)
    val mmu = new MMU(new BusArbiter)
    mmu.setM68KMapper(mapper)
    val svp = mapper.getSVP
    val disa = new SVPDisassembler(mapper)
    disa.useAlternateRegisterNames(true)

    mapper.initComponent()
    mapper.resetComponent()
    val pc = svp.getRegister(RegisterType.PC)
    while true do
      println(disa.disassemble(pc.read))
      println(svp.dumpRegs())
      //if pc.read >= 0xFC1E then
      io.StdIn.readLine(">")
      svp.clock(0)
/**
 * @author Alessandro Abbruzzetti
 *         Created on 24/05/2024 18:20  
 */
class SVP(val mem:SVPMemory) extends SMDComponent with Clockable:
  override protected val smdComponentName = "SVP"
  private inline val A = 0
  private inline val B = 1

  private inline val SSP160x_WRITTEN_XST_MASK = 1
  private inline val M68K_WRITTEN_A15000_MASK = 2

  private val RAM = Array( Array.ofDim[Int](256), Array.ofDim[Int](256) )

  private val pcReg = new ProgramCounter
  private val stackReg = new Stack
  private val stReg = new StatusRegister
  private val xReg = new Register(X)
  private val yReg = new Register(Y)
  private val pReg = new P(xReg,yReg,stReg)

  @volatile private var statusXST = 0

  private val pmcReg = new PMC
  /*
   If this register is blind-accessed, it is "dummy programmed", i.e. nothing
   happens and PMC is reset to "waiting for address" state.
   */
  private val alReg = new Register(AL):
    override def blindAccessedRead(): Unit = pmcReg.resetState()
    override def blindAccessedWrite(): Unit = pmcReg.resetState()
  private val aReg = new Accumulator(alReg)
  /*
    mapped to a15004 on 68k side:
       ???????? ??????10
       0: set, when SSP160x has written something to XST
          (cleared when a15004 is read by 68k)
       1: set, when 68k has written something to a15000 or a15002
          (cleared on PM0 read by SSP160x)
     */
  private val pm0Reg = new ExternalRegister(0,mem,pmcReg,stReg):
    override def get: Int = statusXST
    override protected def externalStatusRegisterRead(readByM68K:Boolean): Int =
      val status = statusXST
      //println(s"Reading PM0 status 68K=$readByM68K $status")
      synchronized {
        if readByM68K then
          statusXST &= ~SSP160x_WRITTEN_XST_MASK
        else
          statusXST &= ~M68K_WRITTEN_A15000_MASK
      }
      status
  /*
    Mapped to a15000 and a15002 on 68k side.
    Affects PM0 when written to.
   */
  private val xstReg = new ExternalRegister(3,mem,pmcReg,stReg):
    //value = 0xFFFF

//    override def reset(): Unit =
//      value = 0xFFFF
    override protected def externalStatusRegisterWrite(value: Int, writeByM68K: Boolean): Unit =
      this.value = value
      synchronized {
        if writeByM68K then
          statusXST |= M68K_WRITTEN_A15000_MASK
        else
          statusXST |= SSP160x_WRITTEN_XST_MASK
      }
      //println(s"Writing XST 68K=$writeByM68K ${value.toHexString} statusXST=$statusXST")

    override protected def externalStatusRegisterRead(readByM68K: Boolean): Int = value

  private val regs = Array(
    new BlindRegister,
    xReg,
    yReg,
    aReg,
    stReg,
    stackReg,
    pcReg,
    pReg,
    pm0Reg,
    new ExternalRegister(1,mem,pmcReg,stReg),
    new ExternalRegister(2,mem,pmcReg,stReg),
    xstReg,
    new ExternalRegister(4,mem,pmcReg,stReg),
    new ExternalRegister(5,mem,pmcReg,stReg),
    pmcReg,
    alReg,
    new PointerRegister(0,mem,RAM(A),stReg),
    new PointerRegister(1,mem,RAM(A),stReg),
    new PointerRegister(2,mem,RAM(A),stReg),
    new PointerRegister(3,mem,RAM(A),stReg),
    new PointerRegister(4,mem,RAM(B),stReg),
    new PointerRegister(5,mem,RAM(B),stReg),
    new PointerRegister(6,mem,RAM(B),stReg),
    new PointerRegister(7,mem,RAM(B),stReg)
  )
  private var halted = false
  // ======================================================================
  def getRegister(rtype:RegisterType): Register = regs(rtype.ordinal)

  override def reset(): Unit =
    regs.foreach(_.reset())
    statusXST = 0
    java.util.Arrays.fill(RAM(A),0)
    java.util.Arrays.fill(RAM(B), 0)

    pcReg.write(mem.svpReadIRamRom(0xFFFC))
    halted = false
  end reset

  def dumpRegs(): String =
    val sb = new StringBuilder()
    for rg <- regs.sliding(4,4) do
      for r <- rg do
        sb.append("%7s  ".format(r.rtype.toString))
        if r.rtype == RegisterType.ACC then
          sb.append("%08X".format(aReg.getA))
        else
          sb.append("%08X".format(r.get))
      sb.append("\n")
    sb.toString()

  final def m68kWriteXST(value:Int): Unit =
    xstReg.write(value,writeByM68K = true)
  final def m68kReadXST(): Int = xstReg.read(readByM68K = true)
  final def m68kReadPM0(): Int =
    pm0Reg.read(readByM68K = true)
  final def halt(enabled:Boolean): Unit =
    halted = enabled

  private def alu(op:Int,operand:Int,_32:Boolean): Unit =
    op match
      case 1 => aReg.aluSUB(operand,stReg,_32)
      case 3 => aReg.aluCMP(operand,stReg,_32)
      case 4 => aReg.aluADD(operand,stReg,_32)
      case 5 => aReg.aluAND(operand,stReg,_32)
      case 6 => aReg.aluOR(operand,stReg,_32)
      case 7 => aReg.aluXOR(operand,stReg,_32)

  inline private def getRI(opcode:Int): PointerRegister =
    regs(RegisterType.R0.ordinal + ((opcode & 0x100) >> 6 | opcode & 3)).asInstanceOf[PointerRegister]

  inline private def isCondition(cccc:Int,f:Int): Boolean =
    cccc match
      case 0 => true // always
      case 4 => if f == 1 then stReg.isSet(StatusRegisterFlag.L) else stReg.isClear(StatusRegisterFlag.L)
      case 5 => if f == 1 then stReg.isSet(StatusRegisterFlag.Z) else stReg.isClear(StatusRegisterFlag.Z)
      case 7 => if f == 1 then stReg.isSet(StatusRegisterFlag.N) else stReg.isClear(StatusRegisterFlag.N)
      case x =>
        println(s"Warning checking unchecked condition: $x")
        false

  final def clock(cycles:Long): Unit =
    if halted then return
    
    import RegisterType.*
    val opcode = mem.svpReadIRamRom(pcReg.getAndInc())

    opcode >> 9 match
      // ALU
      // OP  A, s      ooo0 0000 0000 rrrr
      case op@(0x10|0x30|0x40|0x50|0x60|0x70) =>
        val s = opcode & 0xF
        if s == ACC.ordinal then
          alu(op >> 4,aReg.getA,_32 = true)
        else if s == P.ordinal then
          alu(op >> 4,pReg.multiply(),_32 = true)
        else
          alu(op >> 4,regs(s).read,_32 = false)
      // OP  A, (ri)   ooo0 001j 0000 mmpp
      case op@(0x11|0x31|0x41|0x51|0x61|0x71) =>
        val ri = getRI(opcode)
        val mod = PointerRegisterModifier.fromRI(ri.index,(opcode >> 2) & 3)
        alu(op >> 4,ri.read(PointerRegisterAddressing.Indirect1,mod),_32 = false)
      // OP  A, adr    ooo0 011j aaaa aaaa
      case op@(0x13|0x33|0x43|0x53|0x63|0x73) =>
        val j = (opcode >> 8) & 1
        alu(op >> 4,RAM(j)(opcode & 0xFFFF),_32 = false)
      // OPi A, imm    ooo0 1000 0000 0000 , iiii iiii iiii iiii
      case op@(0x14|0x34|0x44|0x54|0x64|0x74) =>
        val imm = mem.svpReadIRamRom(pcReg.getAndInc())
        alu(op >> 4,imm,_32 = false)
      // op  A, ((ri)) ooo0 101j 0000 mmpp
      case op@(0x15|0x35|0x45|0x55|0x65|0x75) =>
        val ri = getRI(opcode)
        val mod = PointerRegisterModifier.fromRI(ri.index, (opcode >> 2) & 3)
        alu(op >> 4, ri.read(PointerRegisterAddressing.Indirect2, mod), _32 = false)
      // op  A, ri     ooo1 001j 0000 00pp
      case op@(0x19|0x39|0x49|0x59|0x69|0x79) =>
        val ri = getRI(opcode)
        alu(op >> 4, ri.read, _32 = false)
      // OPi simm      ooo1 1000 iiii iiii
      case op@(0x1C|0x3C|0x4C|0x5C|0x6C|0x7C) =>
        alu(op >> 4,opcode & 0xFF,_32 = false)
      // ===================== MOD ==============================
      // mod cond, op  1001 000f cccc 0ooo
      case 0x48 =>
        if isCondition((opcode >> 4) & 0xF,(opcode >> 8) & 1) then
          opcode & 7 match
            case 0 => aReg.aluROR(stReg)
            case 1 => aReg.aluROL(stReg)
            case 2 => aReg.aluSHR(stReg)
            case 3 => aReg.aluSHL(stReg)
            case 4 => aReg.aluINC(stReg)
            case 5 => aReg.aluDEC(stReg)
            case 6 => aReg.aluNEG(stReg)
            case 7 => aReg.aluABS(stReg)
      // mod f, op     1001 0100 0000 oooo
      case 0x4A =>
        opcode & 0xF match
          case 2 => stReg.clearFlag(StatusRegisterFlag.L)
          case 3 => stReg.setFlag(StatusRegisterFlag.L)
          case 4 => stReg.clearFlag(StatusRegisterFlag.IE)
          case 5 => stReg.setFlag(StatusRegisterFlag.IE)
          case 8 => stReg.clearFlag(StatusRegisterFlag.OP)
          case 9 => stReg.setFlag(StatusRegisterFlag.OP)
          case 14 =>
            stReg.clearFlag(StatusRegisterFlag.IE)
            stReg.clearFlag(StatusRegisterFlag.OP)
            stReg.clearFlag(StatusRegisterFlag.L)
          case 15 =>
            stReg.setFlag(StatusRegisterFlag.IE)
            stReg.setFlag(StatusRegisterFlag.OP)
            stReg.setFlag(StatusRegisterFlag.L)
          case x =>
            println(s"Unknown mod f, op: $x")
      // ===================== LD ===============================
      // ld  d, s      0000 0000 dddd ssss
      case 0x00 =>
        if opcode != 0 then // nop
          val d = opcode >> 4
          val s = opcode & 0xF
          if d == ACC.ordinal && s == P.ordinal then // A <- P
            aReg.setA(pReg.multiply(),stReg)
          else if d == ACC.ordinal && s == ACC.ordinal then {/* do nothing */}// A <- A
          else
            val dr = regs(d)
            val sr = regs(s)
            if d == 0 then
              sr.blindAccessedRead()
            else if s == 0 then
              dr.blindAccessedWrite()
            else
              dr.write(sr.read)
      // ld  d, (ri)   0000 001j dddd mmpp
      case 0x01 =>
        val ri = getRI(opcode)
        val mod = PointerRegisterModifier.fromRI(ri.index,(opcode >> 2) & 3)
        regs((opcode >> 4) & 0xF).write(ri.read(PointerRegisterAddressing.Indirect1,mod))
      // ld  (ri), s   0000 010j ssss mmpp
      case 0x02 =>
        val ri = getRI(opcode)
        val mod = PointerRegisterModifier.fromRI(ri.index,(opcode >> 2) & 3)
        ri.write(PointerRegisterAddressing.Indirect1,mod,regs((opcode >> 4) & 0xF).read)
      // ldi d, imm    0000 1000 dddd 0000 , iiii iiii iiii iiii
      case 0x04 =>
        val imm = mem.svpReadIRamRom(pcReg.getAndInc())
        regs((opcode >> 4) & 0xF).write(imm)
      // ld  d, ((ri)) 0000 101j dddd mmpp
      case 0x05 =>
        val ri = getRI(opcode)
        val mod = PointerRegisterModifier.fromRI(ri.index,(opcode >> 2) & 3)
        regs((opcode >> 4) & 0xF).write(ri.read(PointerRegisterAddressing.Indirect2,mod))
      // ldi (ri), imm 0000 110j 0000 mmpp , iiii iiii iiii iiii
      case 0x06 =>
        val ri = getRI(opcode)
        val mod = PointerRegisterModifier.fromRI(ri.index,(opcode >> 2) & 3)
        val imm = mem.svpReadIRamRom(pcReg.getAndInc())
        ri.write(PointerRegisterAddressing.Indirect1,mod,imm)
      // ld  adr, a    0000 111j aaaa aaaa
      case 0x07 =>
        val j = (opcode >> 8) & 1
        RAM(j)(opcode & 0xFFFF) = aReg.read
      // ld  d, ri     0001 001j dddd 00pp
      case 0x09 =>
        val ri = getRI(opcode)
        val d = (opcode >> 4) & 0xF
        if d == 0 then
          ri.blindAccessedRead()
        else
          regs(d).write(ri.read)
      // ld  ri, s     0001 010j ssss 00pp
      case 0x0A =>
        val ri = getRI(opcode)
        val s = (opcode >> 4) & 0xF
        if s == 0 then
          ri.blindAccessedWrite()
        else
          ri.write(regs(s).read)
      // ldi ri, simm  0001 1jpp iiii iiii
      case 0xC|0xD|0xE|0xF =>
        val ri = regs(R0.ordinal + ((opcode >> 8) & 7)).asInstanceOf[PointerRegister]
        ri.write(opcode & 0xFFFF)
      // ld  d, (a)    0100 1010 dddd 0000
      case 0x25 =>
        val read = mem.svpReadIRamRom(aReg.read)
        regs((opcode >> 4) & 0xF).write(read)
      // ===================== CALL/BRA =========================
      // call cond, addr  0100 100f cccc 0000 , aaaa aaaa aaaa aaaa
      case 0x24 =>
        val address = mem.svpReadIRamRom(pcReg.getAndInc())
        if isCondition((opcode >> 4) & 0xF,(opcode >> 8) & 1) then
          stackReg.write(pcReg.read)
          pcReg.write(address)
      // bra  cond, addr  0100 110f cccc 0000 , aaaa aaaa aaaa aaaa
      case 0x26 =>
        val address = mem.svpReadIRamRom(pcReg.getAndInc())
        if isCondition((opcode >> 4) & 0xF,(opcode >> 8) & 1) then
          pcReg.write(address)
      // ===================== MLD/MPYA/MPYS ====================
      // mld  (rj), (ri)  1011 0111 nnjj mmii
      case 0x5B =>
        val ri = regs(R0.ordinal + (opcode & 3)).asInstanceOf[PointerRegister]
        val mi = PointerRegisterModifier.fromRI(ri.index,(opcode >> 2) & 3)
        val rj = regs(R0.ordinal + ((opcode >> 4) & 3) + 4).asInstanceOf[PointerRegister]
        val mj = PointerRegisterModifier.fromRI(rj.index,(opcode >> 6) & 3)
        aReg.setA(0,stReg)
        xReg.write(ri.read(PointerRegisterAddressing.Indirect1,mi))
        yReg.write(rj.read(PointerRegisterAddressing.Indirect1,mj))
      // mpya (rj), (ri)  1001 0111 nnjj mmii
      case 0x4B =>
        val ri = regs(R0.ordinal + (opcode & 3)).asInstanceOf[PointerRegister]
        val mi = PointerRegisterModifier.fromRI(ri.index,(opcode >> 2) & 3)
        val rj = regs(R0.ordinal + ((opcode >> 4) & 3) + 4).asInstanceOf[PointerRegister]
        val mj = PointerRegisterModifier.fromRI(rj.index,(opcode >> 6) & 3)
        aReg.addA(pReg.multiply(),stReg)
        xReg.write(ri.read(PointerRegisterAddressing.Indirect1,mi))
        yReg.write(rj.read(PointerRegisterAddressing.Indirect1,mj))
      // mpys (rj), (ri)  0011 0111 nnjj mmii
      case 0x1B =>
        val ri = regs(R0.ordinal + (opcode & 3)).asInstanceOf[PointerRegister]
        val mi = PointerRegisterModifier.fromRI(ri.index,(opcode >> 2) & 3)
        val rj = regs(R0.ordinal + ((opcode >> 4) & 3) + 4).asInstanceOf[PointerRegister]
        val mj = PointerRegisterModifier.fromRI(rj.index,(opcode >> 6) & 3)
        aReg.addA(-pReg.multiply(),stReg)
        xReg.write(ri.read(PointerRegisterAddressing.Indirect1,mi))
        yReg.write(rj.read(PointerRegisterAddressing.Indirect1,mj))
      case x =>
        println("Unrecognized SVP opcode: %04X".format(x))
  end clock
