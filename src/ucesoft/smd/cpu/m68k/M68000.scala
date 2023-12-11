package ucesoft.smd.cpu.m68k

import ucesoft.smd.cpu.m68k.M6800X0.{BusAccessMode, EventListener, Snapshot}
import ucesoft.smd.cpu.m68k.Memory.{ByteBufferMemory, MapMemory}

import java.io.FileInputStream

object M68000:
  def main(args:Array[String]): Unit =
    val mem = new ByteBufferMemory(512 * 1024)
    val rom = new FileInputStream("""C:\Users\ealeame\OneDrive - Ericsson\Desktop\Genesis_OS_ROM.bin""")//Sonic The Hedgehog (USA, Europe).md""")
    mem.fill(0,rom)
    val cpu = new M68000(new Memory:
      override def read(address: Int, size: Size, readOptions: Int): Int =
        if address < 512 * 1024 then mem.read(address, size, readOptions) else 0x00
      override def write(address: Int, value: Int, size: Size, writeOptions: Int): Unit =
        if address < 512 * 1024 then mem.write(address,value,size,writeOptions)
    )
    cpu.reset(now = true)
    /*
    while cpu.getRegister(RegisterType.PC).get() < 0x8000 do
      val pc = cpu.getRegister(RegisterType.PC).get()
      val dis = cpu.disassemble(pc)
      println(dis)
      cpu.getRegister(RegisterType.PC).set(pc + dis.size, Size.Long)
      io.StdIn.readLine(">")
    end while
    */
    var cycles = 0L
    val monitor = new AbstractDebugger:
      def regDump(cpu: M6800X0): Unit =
        val snap = cpu.getSnapshot
        val sb = new StringBuilder()
        sb.append(s"pc=${snap.regs(s"pc").toHexString} ")
        for (d <- 0 to 7) sb.append(s"d$d=${snap.regs(s"d$d").toHexString} ")
        for (a <- 0 to 6) sb.append(s"a$a=${snap.regs(s"a$a").toHexString} ")
        sb.append(s"usp=${snap.regs("usp").toHexString} ")
        sb.append(s"ssp=${snap.regs("ssp").toHexString} ")
        sb.append(s"sr=${snap.regs("sr").toHexString} ")
        println(sb)

      override def output(text: String): Unit = println("[%10d] %s".format(cycles,text))
      override def onRegisterModified(reg: String, beforeValue: Int, afterValue: Int): Unit =
        println(s"*$reg ${beforeValue.toHexString} -> ${afterValue.toHexString}")
      override def breakEpilogue(cpu: M6800X0): Unit =
        var exit = false
        while !exit do
          io.StdIn.readLine(">") match
            case "r" =>
              regDump(cpu)
            case "b" =>
              println("bus set to N/A")
              cpu.setBUSAvailable(false)
            case "B" =>
              println("bus set to available")
              cpu.setBUSAvailable(true)
            case _ =>
              exit = true

    cpu.addEventListener(monitor)
    monitor.stepByStepEnabled(true)

    while true do
      cycles += cpu.execute()

class M68000(override val mem:Memory) extends M68KCore(mem):
  import ucesoft.smd.cpu.m68k.instructions.*
  override lazy val model: Model = Model._68K

  protected lazy val INSTRUCTION_SET : Array[InstructionGenerator] = Array(
    new ABCD(ctx), new ADD(ctx), new ADDA(ctx), new ADDI(ctx), new ADDQ(ctx), new ADDX(ctx), new AND(ctx), new ANDI(ctx),
    new ANDI2CCR(ctx), new ANDI2SR(ctx), new ASL_ASR(ctx), new Bcc(ctx), new BCHG(ctx), new BCLR(ctx), new BSET(ctx), new BTST(ctx),
    new CHK(ctx), new CLR(ctx), new CMP(ctx), new CMPA(ctx), new CMPI(ctx), new CMPM(ctx), new DBcc(ctx),new DIVS(ctx),
    new DIVU(ctx), new EOR(ctx), new EORI(ctx), new EORI2CCR(ctx), new EORI2SR(ctx), new EXG(ctx), new EXT(ctx),  new ILLEGAL(ctx),
    new JMP(ctx), new JSR(ctx), new LEA(ctx), new LINK(ctx), new LSL_LSR(ctx), new MOVE(ctx), new MOVEA(ctx), new MOVEFromSR(ctx),
    new MOVEToCCR(ctx), new MOVEToSR(ctx), new MOVE_USP(ctx), new MOVEM(ctx), new MOVEP(ctx), new MOVEQ(ctx), new MULS(ctx), new MULU(ctx),
    new NBCD(ctx), new NEG(ctx), new NEGX(ctx), new NOP(ctx), new NOT(ctx), new OR(ctx), new ORI(ctx), new ORI2CCR(ctx), new ORI2SR(ctx),
    new PEA(ctx), new RESET(ctx), new ROL_ROR(ctx), new ROXL_ROXR(ctx), new RTE(ctx), new RTR(ctx), new RTS(ctx), new SBCD(ctx),
    new Scc(ctx), new SUB(ctx), new SUBA(ctx), new SUBI(ctx), new SUBQ(ctx), new SUBX(ctx), new SWAP(ctx), new TAS(ctx),
    new TRAP(ctx), new TRAPV(ctx), new TST(ctx), new UNLK(ctx)
  )

  override protected def buildInstructionSet(): Array[Instruction] =
    val iset = Array.ofDim[Instruction](0x10000)

    for h <- INSTRUCTION_SET do
      h.generate((opcode,i) => {
        if iset(opcode) != null then throw new IllegalArgumentException(s"Opcode ${opcode.toHexString} with instruction ${i.instructionType} already set with ${iset(opcode).instructionType}")
        iset(opcode) = i
      })

    iset

  override protected def buildAddressingModes(): Array[Operand] =
    import ucesoft.smd.cpu.m68k.addressingModes.*
    Array(
      new DataRegisterDirectMode(ctx),
      new AddressRegisterDirectMode(ctx),
      new AddressRegisterIndirectMode(ctx),
      new AddressRegisterIndirectPostIncrementMode(ctx),
      new AddressRegisterIndirectPreDecrementMode(ctx),
      new AddressRegisterIndirectWithDisplacementMode(ctx),
      new AddressRegisterIndirectWithIndexMode(ctx),
      new AbsoluteShortMode(ctx),
      new AbsoluteLongMode(ctx),
      new ProgramCounterWithDisplacementMode(ctx),
      new ProgramCounterWithIndexMode(ctx),
      new ImmediateMode(ctx)
    )

  override def checkMemoryAddress(address: Int, busAccessMode: BusAccessMode, size:Size, isCodeAccess: Boolean, instructionInProgress:Boolean = true): Unit =
    if (size == Size.Word || size == Size.Long) && (address & 1) == 1 then
      throw new AddressBusException(address,busAccessMode,instructionInProgress = instructionInProgress,isCodeAccess)

  override final def execute(): Int =
    if !dtackEnabled || !busAvailable then // on last read/write dtack was not asserted, must wait
      return 1

    lastInstructionElapsedCycles = instructionElapsedCycles
    instructionElapsedCycles = 0
    instructionExceptionNumber = -1

    var opcode = 0
    try
      // check reset condition
      if pendingResetRequest then
        pendingResetRequest = false
        throw new ResetException

      // check interrupt
      if nmiInterruptPending || pendingInterruptMask > statusReg.getInterruptMask then
        serveInterrupt()
        if intAckListener != null then
          intAckListener.intAcknowledged(pendingInterruptMask)
      else {
        if state == CPUState.STOPPED || state == CPUState.HALTED then
          busAccess(BusAccessMode.Idle, Size.Word, codeAccess = false, 0, 0, 2)
          return 2

        pc0 = pcReg.get()

        if busTracing then
          lastBusTraces = Nil

        opcode = prefetchQueue.ird
        lastInstruction = instructionSet(opcode)
        if notifyEventListeners then fireFetchEvent(pc0,opcode,lastInstruction)
        if lastInstruction == null then
          val bit15_12 = opcode >> 12
          if bit15_12 == 10 || bit15_12 == 15 then
            throw new UnimplementedInstruction(bit15_12)
          else throw new IllegalInstruction

        pcReg.increment(Size.Word)

        if lastInstruction.instructionType.needsSupervisorMode && !statusReg.isSupervisorMode then
          throw new PrivilegeViolation

        val trace = statusReg.isTrace
        lastInstruction.execute()

        // check Tracing
        if trace then
          throw new TraceException
      }
    catch
      case _:BUSNotAvailable =>
        // restore PC
        pcReg.set(pc0,Size.Long)
        if notifyEventListeners then fireFetchEvent(pc0,opcode,lastInstruction)
        return 1
      case _:ResetException =>
        resetCPU()
      case ui: UnimplementedInstruction =>
        busAccess(BusAccessMode.Idle, Size.Word, codeAccess = false, 0, 0, 4)
        if ui.pattern == 10 then
          raiseException(10)
        else
          raiseException(11)
        fillPrefetchQueue(true)
      case _: IllegalInstruction =>
        busAccess(BusAccessMode.Idle, Size.Word, codeAccess = false, 0, 0, 4)
        raiseException(4)
        fillPrefetchQueue(true)
      case _: PrivilegeViolation =>
        busAccess(BusAccessMode.Idle, Size.Word, codeAccess = false, 0, 0, 4)
        raiseException(8)
        fillPrefetchQueue(true)
      case e:AddressBusException =>
        handleAddressBusException(e,opcode)
        raiseException(3,pushStackFrame = false)
        fillPrefetchQueue(true)
      case _:TraceException =>
        busAccess(BusAccessMode.Idle, Size.Word, codeAccess = false, 0, 0, 4)
        raiseException(9)
        fillPrefetchQueue(true)

    totalElapsedCycles += instructionElapsedCycles
    instructionElapsedCycles
  end execute
