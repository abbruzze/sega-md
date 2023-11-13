package ucesoft.smd.cpu.m68k.instructions

import ucesoft.smd.cpu.m68k.*

object STOP:
  class STOP(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.STOP

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           STOP       |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
                      |  4(0/0)         |                               n
    */
    final override def execute(): Unit =
      ctx.stopCPU()
      ctx.busIdle(4)

    override def disassemble(address: Int): DisassembledInstruction =
      DisassembledInstruction(address,opcode,instructionType.mnemonic)

class STOP(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 0 | 1 | 0 |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val opcode = genOpcode("0100111001110010")
    instructionSetHandler.registerInstruction(opcode,new STOP.STOP(ctx,opcode))
