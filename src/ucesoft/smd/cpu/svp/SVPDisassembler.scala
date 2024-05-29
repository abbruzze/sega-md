package ucesoft.smd.cpu.svp

import scala.collection.mutable.ListBuffer

object SVPDisassembler:
  case class DisassembledInfo(address:Int,codes:Array[Int],mnemonic:String,wordSize:Int):
    override def toString: String =
      "%04X %s %s".format(address,codes.map(c => "%04X".format(c)).mkString(" "),mnemonic)

  def main(args:Array[String]): Unit =
    if args.length == 0 then
      println("Usage: <rom> <start address> [<org>]")
      sys.exit(1)
    val rom = java.nio.file.Files.readAllBytes(new java.io.File(args(0)).toPath).sliding(2,2).map(hl => (hl(0).toInt & 0xFF) << 8 | (hl(1).toInt & 0xFF)).toArray
    val org = if args.length == 2 then 0 else Integer.parseInt(args(2),16)
    val dis = new SVPDisassembler(rom)

    var adr = Integer.parseInt(args(1),16)

    while adr - org < rom.length do
      val info = dis.disassemble(adr,org)
      println(info)
      io.StdIn.readLine(">")
      adr += info.wordSize
/**
 * @author Alessandro Abbruzzetti
 *         Created on 20/05/2024 16:42  
 */
class SVPDisassembler(mem:SVPMemory):
  import SVPDisassembler.*
  private val REGS = Array(
    Array("-", "x", "y", "a", "st", "stack", "pc", "p", "ext0", "ext1", "ext2", "ext3", "ext4", "ext5", "ext6", "ext7"),
    Array("-", "x", "y", "a", "st", "stack", "pc", "p", "pm0", "ext1", "ext2", "xst", "ext4", "ext5", "pmc", "al")
  )
  private val POINTER_REGS = Array("r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7")
  private val POINTER_REGS_MOD = Array("", "+!", "-", "+")
  private val POINTER_REGS_R37_MOD = Array("|00", "|01", "|10", "|11")
  private val CONDITIONS = Array("always", "?", "gpi0", "gpi1", "l", "z", "ov", "n", "diof", "gpi2", "gpi3", "?", "?", "?", "?", "?")
  private val ACC_OPS = Array("ror", "rol", "shr", "shl", "inc", "dec", "neg", "abs")
  private val RES_OPS = Array("?", "?", "resl", "setl", "resie", "setie", "?", "?", "resop", "setop", "?", "?", "?", "?", "res", "set")
  private val ALU_OPS = Array("", "sub", "", "cmp", "add", "and", "or", "eor")

  private var regsIndex = 0

  def this(rom:Array[Int]) =
    this(new SVPMemory:
      override def svpReadIRamRom(address: Int): Int = rom(address)
      override def svpWriteIRamRom(address: Int, value: Int): Unit = {}
      override def svpExternalRead(address: Int): Int = 0
      override def svpExternalWrite(address: Int, value: Int): Unit = {}
    )

  def useAlternateRegisterNames(enabled:Boolean): Unit =
    regsIndex = if enabled then 1 else 0

  inline private def ri(opcode:Int,low : Boolean = true,includej : Boolean = true): String =
    val ri = if low then
      if includej then (opcode >> 6) & 4 | opcode & 3 else opcode & 3
    else
      (opcode >> 4) & 3

    val mod = if low then (opcode >> 2) & 3 else (opcode >> 6) & 3
    val mods = if ri == 3 || ri == 7 then POINTER_REGS_R37_MOD else POINTER_REGS_MOD
    s"${POINTER_REGS(ri)}${mods(mod)}"

  inline private def regHi(opcode:Int): String = REGS(regsIndex)((opcode >> 4) & 0xF)
  inline private def regLo(opcode:Int): String = REGS(regsIndex)(opcode & 0xF)
  inline private def alu(opcode:Int): String = ALU_OPS((opcode >> 13) & 7)
  inline private def cond(opcode:Int): String =
    val cccc = (opcode >> 4) & 0xF
    val f = (opcode >> 8) & 1
    if cccc == 0 then CONDITIONS(0) else s"${CONDITIONS(cccc)}=$f"

  def disassemble(_address:Int,org:Int = 0): DisassembledInfo =
    val address = _address - org
    val opcode = mem.svpReadIRamRom(address)
    val opcodes = new ListBuffer[Int]
    opcodes += opcode
    var m = ""
    var size = 1
    opcode >> 9 match
      // ===================== ALU ==============================
      case 0x10|0x30|0x40|0x50|0x60|0x70 => // OP  A, s      ooo0 0000 0000 rrrr
        m = s"${alu(opcode)} a,${regLo(opcode)}"
      case 0x11|0x31|0x41|0x51|0x61|0x71 => // OP  A, (ri)   ooo0 001j 0000 mmpp
        m = s"${alu(opcode)} a,(${ri(opcode)})"
      case 0x13|0x33|0x43|0x53|0x63|0x73 => // OP  A, adr    ooo0 011j aaaa aaaa
        val j = ('A' + ((opcode >> 8) & 1)).toChar
        m = s"${alu(opcode)} a,$j[${"%02X".format(opcode & 0xFF)}]"
      case 0x14|0x34|0x44|0x54|0x64|0x74 => // OPi A, imm    ooo0 1000 0000 0000 , iiii iiii iiii iiii
        size = 2
        val imm = mem.svpReadIRamRom(address + 1)
        opcodes += imm
        m = s"${alu(opcode)}i a,${"%04X".format(imm)}"
      case 0x15|0x35|0x45|0x55|0x65|0x75 => // op  A, ((ri)) ooo0 101j 0000 mmpp
        m = s"${alu(opcode)} a,((${ri(opcode)}))"
      case 0x19|0x39|0x49|0x59|0x69|0x79 => // op  A, ri     ooo1 001j 0000 00pp
        m = s"${alu(opcode)} a,${ri(opcode)}"
      case 0x1C|0x3C|0x4C|0x5C|0x6C|0x7C => // OPi simm      ooo1 1000 iiii iiii
        m = s"${alu(opcode)}i ${"%02X".format(opcode & 0xFF)}"
      // ===================== MOD ==============================
      case 0x48 => // mod cond, op  1001 000f cccc 0ooo
        m = s"mod ${cond(opcode)},${ACC_OPS(opcode & 7)}"
      case 0x4A => // mod f, op     1001 0100 0000 oooo
        m = s"mod f,${RES_OPS(opcode & 0xF)}"
      // ===================== LD ===============================
      case 0x00 =>
        opcode match
          case 0 => // nop
            m = "nop"
          case 0x65 => // ret
            m = "ret"
          case _ => // ld  d, s 0000 0000 dddd ssss
            m = s"ld ${regHi(opcode)},${regLo(opcode)}"
      case 0x01 => // ld  d, (ri)   0000 001j dddd mmpp
        m = s"ld ${regHi(opcode)},(${ri(opcode)})"
      case 0x02 => // ld  (ri), s   0000 010j ssss mmpp
        m = s"ld (${ri(opcode)}),${regHi(opcode)}"
      case 0x04 => // ldi d, imm    0000 1000 dddd 0000 , iiii iiii iiii iiii
        size = 2
        val imm = mem.svpReadIRamRom(address + 1)
        opcodes += imm
        m = s"ldi ${regHi(opcode)},${"%04X".format(imm)}"
      case 0x05 => // ld  d, ((ri)) 0000 101j dddd mmpp
        m = s"ld ${regHi(opcode)},((${ri(opcode)}))"
      case 0x06 => // ldi (ri), imm 0000 110j 0000 mmpp , iiii iiii iiii iiii
        size = 2
        val imm = mem.svpReadIRamRom(address + 1)
        opcodes += imm
        m = s"ldi (${ri(opcode)}),${"%04X".format(imm)}"
      case 0x07 => // ld  adr, a    0000 111j aaaa aaaa
        val j = ('A' + ((opcode >> 8) & 1)).toChar
        m = s"ld $j[${"%02X".format(opcode & 0xFF)}],a"
      case 0x09 => // ld  d, ri     0001 001j dddd 00pp
        m = s"ld ${regHi(opcode)},${ri(opcode)}"
      case 0x0A => // ld  ri, s     0001 010j ssss 00pp
        m = s"ld ${ri(opcode)},${regHi(opcode)}"
      case 0xC|0xD|0xE|0xF => // ldi ri, simm  0001 1jpp iiii iiii
        m = s"ldi ${POINTER_REGS((opcode >> 8) & 7)},${"%02X".format(opcode & 0xFF)}"
      case 0x25 => // ld  d, (a)    0100 1010 dddd 0000
        m = s"ld ${regHi(opcode)},(a)"
      // ===================== CALL/BRA =========================
      case 0x24 => // call cond, addr  0100 100f cccc 0000 , aaaa aaaa aaaa aaaa
        size = 2
        val imm = mem.svpReadIRamRom(address + 1)
        opcodes += imm
        m = s"call ${cond(opcode)},${"%04X".format(imm)}"
      case 0x26 => // bra  cond, addr  0100 110f cccc 0000 , aaaa aaaa aaaa aaaa
        size = 2
        val imm = mem.svpReadIRamRom(address + 1)
        opcodes += imm
        m = s"bra ${cond(opcode)},${"%04X".format(imm)}"
      // ===================== MLD/MPYA/MPYS ====================
      case 0x5B => // mld  (rj), (ri)  1011 0111 nnjj mmii
        m = s"mld (${ri(opcode,low = false,includej = false)}),(${ri(opcode,includej = false)})"
      case 0x4B => // mpya (rj), (ri)  1001 0111 nnjj mmii
        m = s"mpya (${ri(opcode,low = false,includej = false)}),(${ri(opcode,includej = false)})"
      case 0x1B => // mpys (rj), (ri)  0011 0111 nnjj mmii
        m = s"mpys (${ri(opcode,low = false,includej = false)}),(${ri(opcode,includej = false)})"
      // ===================== ?? ===============================
      case _ =>
        m = "dw %04X".format(opcode)

    DisassembledInfo(_address,opcodes.toArray,m,size)

