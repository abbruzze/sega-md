package ucesoft.smd.cpu.m68k.addressingModes

import ucesoft.smd.cpu.m68k.AddressingMode.AL
import ucesoft.smd.cpu.m68k.{AddressingMode, M6800X0, Operand, Size}

class AbsoluteLongMode(override protected val ctx: M6800X0.Context) extends Operand(ctx):
  override val mode : AddressingMode = AL
  private var ext1,ext2 = 0

  override def init(reg: Int, size: Size,disassemblingAddress:Option[Int],includeIdleBusCycles:Boolean): Unit =
    super.init(reg, size,disassemblingAddress)
    ext1 = disassemblingAddress match
      case None => ctx.fetchWord()
      case Some(address) => ctx.fetchWordForDisassembling(address + 2)
    ext2 = disassemblingAddress match
      case None => ctx.fetchWord()
      case Some(address) => ctx.fetchWordForDisassembling(address + 4)
    address = ext1 << 16 | ext2

  final override def get(size: Size, signExtended: Boolean = false): Int =
    val r = ctx.readMemory(address, size)
    if signExtended then
      extendSign(size, r)
    else
      r
  final override def set(value: Int, size: Size): Unit = ctx.writeMemory(address, value, size)
  
  override def getMnemonic(address:Int): String =
    "%08X".format(this.address)
    //s"(${this.address.toHexString}).l"

  override def getExtensionWords:List[Int] = List(ext1 & 0xFFFF,ext2 & 0xFFFF)