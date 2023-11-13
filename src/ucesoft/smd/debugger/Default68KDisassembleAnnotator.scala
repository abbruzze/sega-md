package ucesoft.smd.debugger

import ucesoft.smd.Logger
import ucesoft.smd.cpu.m68k.{DisassembledInstruction, M6800X0, Memory, RegisterType, Size}

import java.util.logging.Level

/**
 * @author Alessandro Abbruzzetti
 *         Created on 20/10/2023 16:36  
 */
class Default68KDisassembleAnnotator extends Disassemble68KAnnotator:
  private final val ABS_LONG_MODE_R = """([a-fA-F0-9]{8})""".r
  private final val ABS_SHORT_MODE_R = """([a-fA-F0-9]{4})""".r
  private final val ADDRESS_REG_R = """[aA](\d)""".r
  private final val DATA_REG_R = """[dD](\d)""".r
  private final val INDIRECT_R = """\([aA](\d)\)""".r
  private final val INDIRECT_POST_R = """\([aA](\d)\)\+""".r
  private final val INDIRECT_PRE_R = """-\([aA](\d)\)""".r
  private final val INDIRECT_DISP_R = """\((-?[a-fA-F0-9]+),[aA](\d)\)""".r
  private final val INDIRECT_DISP_INDEX_R = """\((-?[a-fA-F0-9]+),[aA](\d),([aAdD]\d)\)""".r
  private final val IMMEDIATE_R = """#([a-fA-F0-9]+)""".r
  private final val PC_DISP_R = """\((-?[a-fA-F0-9]+),PC\)""".r
  private final val PC_DISP_INDEX_R = """\((-?[a-fA-F0-9]+),PC,([aAdD]\d)\)""".r

  private final val ADDRESS_BOOK : List[(Int,Int,String)]= List(
    (0x00_0000,0x40_0000,"ROM"),
    (0xA0_0000,0xA1_0000,"Z80 AREA"),
    //(0xA0_4000,0xA0_6000,"Z80 YM2612"),
    (0xA1_0000,0xA1_0002,"Version Register"),
    (0xA1_0002,0xA1_0008,"I/O Data"),
    (0xA1_0008,0xA1_000E,"I/O Control"),
    (0xA1_1100,0xA1_1101,"Z80 BUS REQ"),
    (0xA1_1200,0xA1_1201,"Z80 RESET"),
    (0xA1_4000,0xA1_4001,"REG_LOCK"),
    (0xA1_4101,0xA1_4102,"REG_TMSS"),
    (0xC0_0000,0xC0_0004,"VDP Data port"),
    (0xC0_0004,0xC0_0008,"VDP Status/Control port"),
    (0xE0_0000,0x100_0000,"RAM")
  )

  private def searchAddress(a:Int): String =
    ADDRESS_BOOK.find((f,t,l) => a >= f && a < t) match
      case Some((_,_,label)) =>
        s"${a.toHexString.toUpperCase()} ($label)"
      case None =>
        a.toHexString.toUpperCase()

  private def getNoteFor(mnemonic:String,op:String,m68k:M6800X0,memory: Memory): String = ""
    /*val mn = mnemonic.toUpperCase()
    val size = if mn.endsWith(".L") then Size.Long
    else if mn.endsWith(".W") then Size.Word
    else if mn.endsWith(".B") then Size.Byte
    else Size.Word
    Logger.getLogger.log(Level.SEVERE) {
      try
        op match
          case ABS_LONG_MODE_R(address) =>
            val value = searchAddress(memory.read(java.lang.Long.parseLong(address, 16).toInt, size, M6800X0.DEBUG_READ_OPTION))
            s"($op) = $value ${searchAddress(java.lang.Long.parseLong(address, 16).toInt)}"
          case ABS_SHORT_MODE_R(address) =>
            val value = searchAddress(memory.read(java.lang.Long.parseLong(address, 16).toInt, size, M6800X0.DEBUG_READ_OPTION))
            s"($op) = $value ${searchAddress(java.lang.Long.parseLong(address, 16).toInt)}"
          case ADDRESS_REG_R(r) =>
            val value = searchAddress(m68k.getRegister(RegisterType.Address, r.toInt).get())
            s"$op = $value"
          case DATA_REG_R(r) =>
            val value = m68k.getRegister(RegisterType.Data, r.toInt).get().toHexString.toUpperCase()
            s"$op = $value"
          case INDIRECT_R(r) =>
            val value = memory.read(m68k.getRegister(RegisterType.Address, r.toInt).get(), size, M6800X0.DEBUG_READ_OPTION).toHexString.toUpperCase()
            s"$op = $value"
          case INDIRECT_POST_R(r) =>
            val value = memory.read(m68k.getRegister(RegisterType.Address, r.toInt).get(), size, M6800X0.DEBUG_READ_OPTION).toHexString.toUpperCase()
            s"$op = $value"
          case INDIRECT_PRE_R(r) =>
            val value = memory.read((m68k.getRegister(RegisterType.Address, r.toInt).get() - 1) & 0xFFFFFF, size, M6800X0.DEBUG_READ_OPTION).toHexString.toUpperCase()
            s"$op = $value"
          case INDIRECT_DISP_R(disp, r) =>
            val value = memory.read((m68k.getRegister(RegisterType.Address, r.toInt).get() + java.lang.Long.parseLong(disp,16).toInt) & 0xFFFFFF, size, M6800X0.DEBUG_READ_OPTION).toHexString.toUpperCase()
            s"$op = $value"
          case INDIRECT_DISP_INDEX_R(disp, r, i) =>
            "TO DO"
          case IMMEDIATE_R(value) =>
            s"$op = ${searchAddress(java.lang.Long.parseLong(value, 16).toInt)}"
          case _ =>
            ""
      catch
        case t:Throwable =>
          Logger.getLogger.info(s"Error while getting note for $mnemonic $op",t)
          "N/A"
    }*/

  def getNoteFor(dis:DisassembledInstruction,m68k:M6800X0,memory: Memory): String =
    if dis.op1.isEmpty && dis.op2.isEmpty then
      return ""

    Seq(dis.op1,dis.op2).flatten.map(getNoteFor(dis.mnemonic,_,m68k, memory)).mkString(" ; ")//("<html>","<br>","</html>")
