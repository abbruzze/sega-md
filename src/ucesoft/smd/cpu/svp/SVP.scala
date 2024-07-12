package ucesoft.smd.cpu.svp
import ucesoft.smd.Clock.Clockable
import ucesoft.smd.{SMDComponent, StateBuilder}
import ucesoft.smd.cpu.svp.RegisterType.*

import scala.annotation.switch

object SVP:
  trait SVPTracer:
    def trace(address:Int): Unit

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
  
  private var tracers : List[SVP.SVPTracer] = Nil
  private var tracing = false
  private var svpEnabled = true
  private val disa = new SVPDisassembler(mem)
  
  disa.useAlternateRegisterNames(true)

  private val RAM = Array( Array.ofDim[Int](256), Array.ofDim[Int](256) )

  private val iramrom = mem.iramRomWord
  private val pcReg = new ProgramCounter
  private val stackReg = new Stack
  private val stReg = new StatusRegister
  private val xReg = new Register(X)
  private val yReg = new Register(Y)
  private val pReg = new P(xReg,yReg,stReg)

  private var statusXST = 0
  private var xstValue = 0

  private val pmcReg = new PMC
  private val aReg = new Accumulator(stReg)
  /*
   If this register is blind-accessed, it is "dummy programmed", i.e. nothing
   happens and PMC is reset to "waiting for address" state.
   */
  private val alReg = new Register(AL):
    override final def read: Int = aReg.readLow
    override final def write(value:Int): Unit = aReg.writeLow(value)
    override def blindAccessedRead(): Unit = pmcReg.resetState()
    override def blindAccessedWrite(): Unit = pmcReg.resetState()

  /*
    mapped to a15004 on 68k side:
       ???????? ??????10
       0: set, when SSP160x has written something to XST
          (cleared when a15004 is read by 68k)
       1: set, when 68k has written something to a15000 or a15002
          (cleared on PM0 read by SSP160x)
     */
  private val pm0Reg = new ExternalRegister(0,mem,pmcReg,stReg):
    override def get: Int = if st.getFlag(StatusRegisterFlag.ST56) > 0 then super.get else statusXST
    override protected def externalStatusRegisterRead: Int =
      val status = statusXST
      statusXST &= ~M68K_WRITTEN_A15000_MASK
      status

    override final protected def externalStatusRegisterWrite(value: Int): Unit =
      statusXST = value
  /*
    Mapped to a15000 and a15002 on 68k side.
    Affects PM0 when written to.
   */
  private val xstReg = new ExternalRegister(3,mem,pmcReg,stReg):
    override def get: Int = xstValue

    override final protected def externalStatusRegisterWrite(value: Int): Unit =
      xstValue = value
      statusXST |= SSP160x_WRITTEN_XST_MASK

    override final protected def externalStatusRegisterRead: Int = xstValue

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
  def disassemble(address:Int): SVPDisassembler.DisassembledInfo =
    disa.disassemble(address)
  override def setComponentEnabled(enabled:Boolean): Unit =
    super.setComponentEnabled(enabled)
    svpEnabled = enabled
  def addTracer(t:SVP.SVPTracer): Unit =
    tracing = true
    tracers ::= t
  def removeTracer(t:SVP.SVPTracer): Unit =
    tracers = tracers.filterNot(tr => tr == t)
    tracing = tracers.nonEmpty
  def getRAM: Array[Array[Int]] = RAM
  def getRegister(rtype:RegisterType): Register = regs(rtype.ordinal)
  def getRegisters: Array[Register] = regs

  override def reset(): Unit =
    regs.foreach(_.reset())
    statusXST = 0
    xstValue = 0
    java.util.Arrays.fill(RAM(A),0)
    java.util.Arrays.fill(RAM(B), 0)

    pcReg.write(iramrom(0xFFFC))
    halted = false
  end reset

  def dumpRamCRC(index:0|1): Int =
    var crc = 0
    var c = 0
    while c < 256 do
      crc += RAM(index)(c)
      c += 1
    crc & 0xFFFF

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
    xstValue = value
    statusXST |= M68K_WRITTEN_A15000_MASK
  final def m68kReadXST(): Int = xstValue
  final def m68kReadPM0(): Int =
    val st = statusXST
    statusXST &= ~SSP160x_WRITTEN_XST_MASK
    st
  final def halt(enabled:Boolean): Unit =
    halted = enabled

  private def alu(op:Int,operand:Int,_32:Boolean): Unit =
    op match
      case 1 => aReg.aluSUB(operand,_32)
      case 3 => aReg.aluCMP(operand,_32)
      case 4 => aReg.aluADD(operand,_32)
      case 5 => aReg.aluAND(operand,_32)
      case 6 => aReg.aluOR(operand,_32)
      case 7 => aReg.aluXOR(operand,_32)

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

    var _cycles = cycles

    while _cycles > 0 && svpEnabled do
      if tracing then
        for t <- tracers do 
          t.trace(pcReg.get)
          
      val opcode = iramrom(pcReg.getAndInc())
      (opcode >> 9 : @switch) match
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
        case 0x03 => // ld a, adr
          val j = (opcode >> 8) & 1
          aReg.write(RAM(j)(opcode & 0xFF))
        case op@(0x13|0x33|0x43|0x53|0x63|0x73) =>
          val j = (opcode >> 8) & 1
          alu(op >> 4,RAM(j)(opcode & 0xFF),_32 = false)
        // OPi A, imm    ooo0 1000 0000 0000 , iiii iiii iiii iiii
        case op@(0x14|0x34|0x44|0x54|0x64|0x74) =>
          val imm = iramrom(pcReg.getAndInc())
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
              case 0 => aReg.aluROR()
              case 1 => aReg.aluROL()
              case 2 => aReg.aluSHR()
              case 3 => aReg.aluSHL()
              case 4 => aReg.aluINC()
              case 5 => aReg.aluDEC()
              case 6 => aReg.aluNEG()
              case 7 => aReg.aluABS()
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
              aReg.setA(pReg.multiply())
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
          val imm = iramrom(pcReg.getAndInc())
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
          val imm = iramrom(pcReg.getAndInc())
          ri.write(PointerRegisterAddressing.Indirect1,mod,imm)
        // ld  adr, a    0000 111j aaaa aaaa
        case 0x07 =>
          val j = (opcode >> 8) & 1
          RAM(j)(opcode & 0xFF) = aReg.read
          //println(s"RAM($j)(${(opcode & 0xFF).toHexString})=${aReg.read}")
        // ld  d, ri     0001 001j dddd 00pp
        case 0x09 =>
          val ri = getRI(opcode)
          val d = (opcode >> 4) & 0xF
          if d == 0 then
            ri.blindAccessedRead() // TODO CHECK
          else
            regs(d).write(ri.read)
        // ld  ri, s     0001 010j ssss 00pp
        case 0x0A =>
          val ri = getRI(opcode)
          val s = (opcode >> 4) & 0xF
          if s == 0 then
            ri.blindAccessedWrite() // TODO CHECK
          else
            ri.write(regs(s).read)
        // ldi ri, simm  0001 1jpp iiii iiii
        case 0xC|0xD|0xE|0xF =>
          val ri = regs(R0.ordinal + ((opcode >> 8) & 7))
          ri.write(opcode & 0xFFFF)
        // ld  d, (a)    0100 1010 dddd 0000
        case 0x25 =>
          val read = iramrom(aReg.read)
          regs((opcode >> 4) & 0xF).write(read)
        // ===================== CALL/BRA =========================
        // call cond, addr  0100 100f cccc 0000 , aaaa aaaa aaaa aaaa
        case 0x24 =>
          val address = iramrom(pcReg.getAndInc())
          if isCondition((opcode >> 4) & 0xF,(opcode >> 8) & 1) then
            stackReg.write(pcReg.read)
            pcReg.write(address)
        // bra  cond, addr  0100 110f cccc 0000 , aaaa aaaa aaaa aaaa
        case 0x26 =>
          val address = iramrom(pcReg.getAndInc())
          if isCondition((opcode >> 4) & 0xF,(opcode >> 8) & 1) then
            pcReg.write(address)
        // ===================== MLD/MPYA/MPYS ====================
        // mld  (rj), (ri)  1011 0111 nnjj mmii
        case 0x5B =>
          val ri = regs(R0.ordinal + (opcode & 3)).asInstanceOf[PointerRegister]
          val mi = PointerRegisterModifier.fromRI(ri.index,(opcode >> 2) & 3)
          val rj = regs(R0.ordinal + ((opcode >> 4) & 3) + 4).asInstanceOf[PointerRegister]
          val mj = PointerRegisterModifier.fromRI(rj.index,(opcode >> 6) & 3)
          aReg.setA(0)
          stReg.write(stReg.read & 0x0FFF)
          xReg.write(ri.read(PointerRegisterAddressing.Indirect1,mi))
          yReg.write(rj.read(PointerRegisterAddressing.Indirect1,mj))
        // mpya (rj), (ri)  1001 0111 nnjj mmii
        case 0x4B =>
          val ri = regs(R0.ordinal + (opcode & 3)).asInstanceOf[PointerRegister]
          val mi = PointerRegisterModifier.fromRI(ri.index,(opcode >> 2) & 3)
          val rj = regs(R0.ordinal + ((opcode >> 4) & 3) + 4).asInstanceOf[PointerRegister]
          val mj = PointerRegisterModifier.fromRI(rj.index,(opcode >> 6) & 3)
          aReg.addA(pReg.multiply())
          xReg.write(ri.read(PointerRegisterAddressing.Indirect1,mi))
          yReg.write(rj.read(PointerRegisterAddressing.Indirect1,mj))
        // mpys (rj), (ri)  0011 0111 nnjj mmii
        case 0x1B =>
          val ri = regs(R0.ordinal + (opcode & 3)).asInstanceOf[PointerRegister]
          val mi = PointerRegisterModifier.fromRI(ri.index,(opcode >> 2) & 3)
          val rj = regs(R0.ordinal + ((opcode >> 4) & 3) + 4).asInstanceOf[PointerRegister]
          val mj = PointerRegisterModifier.fromRI(rj.index,(opcode >> 6) & 3)
          aReg.addA(-pReg.multiply())
          xReg.write(ri.read(PointerRegisterAddressing.Indirect1,mi))
          yReg.write(rj.read(PointerRegisterAddressing.Indirect1,mj))
        case x =>
          println("Unrecognized SVP opcode: %04X at %04X".format(x,pcReg.get - 1))
      _cycles -= 1
    end while
  end clock

  override protected def createState(sb: StateBuilder): Unit =
    sb.
      serialize("RAM_A", RAM(A), zip = true).
      serialize("RAM_B", RAM(B), zip = true).
      w("statusXST",statusXST).
      w("xstValue",xstValue)
    val registers = new StateBuilder()
    for r <- regs do
      val reg = new StateBuilder()
      r.createState(reg)
      registers.w(r.rtype.toString,reg.build())
    sb.w("registers",registers.build())

  override protected def restoreState(sb: StateBuilder): Unit =
    val ra = sb.deserialize[Array[Int]]("RAM_A", zip = true)
    System.arraycopy(ra, 0, RAM(A), 0, ra.length)
    val rb = sb.deserialize[Array[Int]]("RAM_B", zip = true)
    System.arraycopy(rb, 0, RAM(B), 0, rb.length)

    statusXST = sb.r[Int]("statusXST")
    xstValue = sb.r[Int]("xstValue")

    val registers = sb.getSubStateBuilder("registers")
    for r <- regs do
      val reg = registers.getSubStateBuilder(r.rtype.toString)
      r.restoreState(reg)
