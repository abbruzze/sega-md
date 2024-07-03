package ucesoft.smd.cpu.svp

import ucesoft.smd.StateBuilder
import ucesoft.smd.cpu.svp.RegisterType.*

/**
 * @author Alessandro Abbruzzetti
 *         Created on 10/05/2024 19:41  
 */
class Register(val rtype:RegisterType):
  protected var value = 0 // 16 bit value

  def get: Int = value

  def read: Int = value

  def write(value: Int): Unit =
    this.value = value & 0xFFFF

  def reset(): Unit =
    value = 0

  override def toString: String = get.toHexString

  def blindAccessedRead(): Unit = {}
  def blindAccessedWrite(): Unit =
    value = 0xFFFF
  def createState(sb:StateBuilder): Unit =
    sb.w("value",value)
  def restoreState(sb:StateBuilder): Unit =
    value = sb.r[Int]("value")
// ===========================================================

class BlindRegister extends Register(BLIND):
  value = 0xFFFF

  override def reset(): Unit =
    value = 0xFFFF
  override def write(value: Int): Unit = {}

// ===========================================================
class ProgramCounter extends Register(PC):
  def getAndInc(): Int =
    val pc = value
    value += 1
    pc
// ===========================================================

class Accumulator(val st:StatusRegister) extends Register(ACC):
  private var lowValue = 0

  override def createState(sb: StateBuilder): Unit =
    super.createState(sb)
    sb.w("lowValue",lowValue)
  override def restoreState(sb: StateBuilder): Unit =
    super.restoreState(sb)
    lowValue = sb.r[Int]("lowValue")

  final def readLow: Int = lowValue
  final def writeLow(value:Int): Unit =
    lowValue = value & 0xFFFF

  inline private def write32(value:Int): Unit =
    this.value = value >>> 16
    lowValue = value & 0xFFFF

  inline private def read32: Int = value << 16 | lowValue //AL.read

  final def getA: Int = read32
  final def setA(value:Int): Unit =
    write32(value)
  final def addA(value: Int): Unit =
    val a = read32 + value
    write32(a)
    setNZ(a)

  override def reset(): Unit =
    super.reset()
    lowValue = 0
    //AL.reset()

  // affects 32 bits
  inline private def setNZ(a:Int): Unit =
    if a == 0 then st.setFlag(StatusRegisterFlag.Z) else st.clearFlag(StatusRegisterFlag.Z)
    if (a & 0x8000_0000) != 0 then st.setFlag(StatusRegisterFlag.N) else st.clearFlag(StatusRegisterFlag.N)
  inline private def setL(preA:Int,postA:Int): Unit =
    if (preA & 0x10000) == 0 && (postA & 0x10000) != 0 then st.setFlag(StatusRegisterFlag.L) else st.clearFlag(StatusRegisterFlag.L)

  final def aluADD(value:Int,_32:Boolean): Unit =
    var a = read32
    val aPre = a
    a += (if _32 then value else value << 16)
    write32(a)
    setNZ(a)
    setL(aPre,a)
  final def aluSUB(value: Int, _32: Boolean): Unit =
    var a = read32
    a -= (if _32 then value else value << 16)
    write32(a)
    setNZ(a)
  final def aluCMP(value: Int, _32: Boolean): Unit =
    var a = read32
    a -= (if _32 then value else value << 16)
    setNZ(a)
  final def aluAND(value: Int, _32: Boolean): Unit =
    var a = read32
    a &= (if _32 then value else value << 16/* | 0xFFFF*/)
    write32(a)
    setNZ(a)
  final def aluOR(value: Int, _32: Boolean): Unit =
    var a = read32
    a |= (if _32 then value else value << 16)
    write32(a)
    setNZ(a)
  final def aluXOR(value: Int, _32: Boolean): Unit =
    var a = read32
    a ^= (if _32 then value else value << 16)
    write32(a)
    setNZ(a)
  final def aluROR(): Unit =
    var a = read32
    val lsb = a & 1
    a = a >>> 1 | lsb << 31
    write32(a)
    setNZ(a)
  final def aluROL(): Unit =
    var a = read32
    val msb = a >>> 31
    a = a << 1 | msb
    write32(a)
    setNZ(a)
  final def aluSHR(): Unit =
    var a = read32
    a = a >> 1
    write32(a)
    setNZ(a)
  final def aluSHL(): Unit =
    var a = read32
    a = a << 1
    write32(a)
    setNZ(a)
  final def aluINC(): Unit =
    var a = read32
    a += 1
    write32(a)
    setNZ(a)
  final def aluDEC(): Unit =
    var a = read32
    a -= 1
    write32(a)
    setNZ(a)
  final def aluNEG(): Unit =
    var a = read32
    a = -a
    write32(a)
    setNZ(a)
  final def aluABS(): Unit =
    var a = read32
    if a < 0 then
      a = -a
      write32(a)
    setNZ(a)

end Accumulator


// ===========================================================

class Stack extends Register(STACK):
  private final val stack = Array(0,0,0,0,0,0)
  private var top = -1

  override def createState(sb: StateBuilder): Unit =
    sb.
      w("top", top).
      w("stack",stack)
  override def restoreState(sb: StateBuilder): Unit =
    top = sb.r[Int]("top")
    sb.r("stack",stack)

  override def toString: String = elements.mkString("[",",","]")

  override def get: Int = top
  override def reset(): Unit =
    super.reset()
    top = -1

  override final def read: Int =
    if top == -1 then
      println("Reading empty stack")
      0
    else
      val v = stack(top)
      top -= 1
      v

  override final def write(value: Int): Unit =
    if top == 5 then
      println("Writing full stack")
    else
      top += 1
      stack(top) = value & 0xFFFF

  def elements: Array[Int] =
    val a = Array.ofDim[Int](top + 1)
    System.arraycopy(stack,0,a,0,top + 1)
    a.reverse

// ===========================================================

enum StatusRegisterFlag(val mask:Int,val shift:Int):
  case RPL    extends StatusRegisterFlag(0x07,0)
  case RB     extends StatusRegisterFlag(0x18,3)
  case ST56   extends StatusRegisterFlag(0x60,5)
  case IE     extends StatusRegisterFlag(0x80,7)
  case OP     extends StatusRegisterFlag(0x100,8)
  case MACS   extends StatusRegisterFlag(0x200,9)
  case GPI01  extends StatusRegisterFlag(0xC00,10)
  case L      extends StatusRegisterFlag(0x1000,12)
  case Z      extends StatusRegisterFlag(0x2000,13)
  case OV     extends StatusRegisterFlag(0x4000,14)
  case N      extends StatusRegisterFlag(0x8000,15)

class StatusRegister extends Register(ST):
  override final def read: Int =
    value & ~StatusRegisterFlag.IE.mask
  final def getFlag(f:StatusRegisterFlag): Int =
    (value & f.mask) >> f.shift
  final def setFlag(f:StatusRegisterFlag,value:Int): Unit =
    this.value &= ~f.mask
    this.value |= (value << f.shift) & f.mask
  final def setFlag(f:StatusRegisterFlag): Unit = setFlag(f,1)
  final def clearFlag(f:StatusRegisterFlag): Unit =
    this.value &= ~f.mask
  final def isSet(f:StatusRegisterFlag): Boolean =
    (value & f.mask) != 0
  final def isClear(f: StatusRegisterFlag): Boolean =
    (value & f.mask) == 0

// ===========================================================

class P(val X:Register,val Y:Register,st:StatusRegister) extends Register(RegisterType.P):
  inline private def signExtend32(value:Int): Int =
    val msb = 0x8000
    val mask = 0xFFFF
    var v = value & msb
    if (value & msb) > 0 then
      value | ~mask
    else
      value

  final def multiply(): Int =
    val shift = if st.isClear(StatusRegisterFlag.MACS) then 1 else 0
    value = (signExtend32(X.read) * signExtend32(Y.read)) << shift
    value

// ===========================================================
object PointerRegisterModifier:
  def fromRI(ri: Int, mod: Int): PointerRegisterModifier =
    if ri == 3 || ri == 7 then
      PointerRegisterModifier.fromOrdinal(_00.ordinal + (mod & 3))
    else
      PointerRegisterModifier.fromOrdinal(mod & 3)
enum PointerRegisterModifier:
  case None, PostIncrement,PostDecrementModulo, PostIncrementModulo, _00, _01, _10, _11


enum PointerRegisterAddressing:
  case Direct, Indirect1, Indirect2

class PointerRegister(val index:0|1|2|3|4|5|6|7,val mem:SVPMemory,val ram:Array[Int],val st:StatusRegister) extends Register(RegisterType.fromOrdinal(RegisterType.R0.ordinal + index)):
  private val iramrom = mem.iramRomWord
  override final def read: Int =
    if index == 3 || index == 7 then 0 else value

  override final def write(value: Int): Unit =
    if !(index == 3 || index == 7) then
      this.value = value & 0xFF

  inline private def modulo(inc:Int): Unit =
    if !(index == 3 || index == 7) then
      val rpl = st.getFlag(StatusRegisterFlag.RPL)
      val modulo = if rpl == 0 then
        0xFF
      else
        (1 << rpl) - 1

      value = (value & ~modulo) | (value + inc) & modulo

  private def modWrite(addressing:PointerRegisterAddressing,modifier:PointerRegisterModifier,value:Int): Unit =
    import PointerRegisterModifier.*

    if !(index == 3 || index == 7) then
      modifier match
        case None =>
          ram(this.value) = value & 0xFFFF
        case PostIncrementModulo =>
          ram(this.value) = value & 0xFFFF
          modulo(1)
        case PostDecrementModulo =>
          ram(this.value) = value & 0xFFFF
          modulo(-1)
        case PostIncrement =>
          ram(this.value) = value & 0xFFFF
          this.value = (this.value + 1) & 0xFF
        case _ =>
          println(s"Wrong modifier for addressing $addressing: $modifier")
    else
      modifier match
        case PointerRegisterModifier._00 =>
          ram(0) = value & 0xFFFF
        case PointerRegisterModifier._01 =>
          ram(1) = value & 0xFFFF
        case PointerRegisterModifier._10 =>
          ram(2) = value & 0xFFFF
        case PointerRegisterModifier._11 =>
          ram(3) = value & 0xFFFF
        case _ =>
          println(s"Wrong modifier for addressing $addressing: $modifier")

  private def modRead(addressing:PointerRegisterAddressing,modifier:PointerRegisterModifier): Int =
    import PointerRegisterModifier.*
    if !(index == 3 || index == 7) then
      modifier match
        case None =>
          ram(read)
        case PostIncrementModulo =>
          val ret = ram(read)
          modulo(1)
          ret
        case PostDecrementModulo =>
          val ret = ram(read)
          modulo(-1)
          ret
        case PostIncrement =>
          val ret = ram(read)
          if !(index == 3 || index == 7) then
            value = (value + 1) & 0xFF
          ret
        case _ =>
          println(s"Wrong addressing $addressing with $rtype")
          0
    else
      modifier match
        case PointerRegisterModifier._00 =>
          ram(0)
        case PointerRegisterModifier._01 =>
          ram(1)
        case PointerRegisterModifier._10 =>
          ram(2)
        case PointerRegisterModifier._11 =>
          ram(3)
        case _ =>
          println(s"Wrong addressing $addressing with $rtype")
          io.StdIn.readLine(">")
          0

  final def write(addressing:PointerRegisterAddressing,modifier:PointerRegisterModifier,value:Int): Unit =
    import PointerRegisterAddressing.*
    addressing match
      case Direct =>
        write(value)
      case Indirect1 =>
        modWrite(addressing,modifier, value)
      case Indirect2 =>
        println(s"Invalid $addressing with write operation")

  final def read(addressing:PointerRegisterAddressing,modifier:PointerRegisterModifier): Int =
    import PointerRegisterAddressing.*
    addressing match
      case Direct =>
        if index == 3 || index == 7 then 0 else value
      case Indirect1 =>
        modRead(addressing, modifier)
      case Indirect2 =>
        val oldAddress = read
        val target = modRead(addressing, modifier)
        ram(oldAddress) = (ram(oldAddress) + 1) & 0xFFFF
        iramrom(target)
// ===========================================================
/*
 dsnn nv?? ???a aaaa
   a: bits 16-20 of memory word-address.
   n: auto-increment value. If set, after every access of PMx, word-address
      value related to it will be incremented by (words):
        1 - 1    5 - 16
        2 - 2    6 - 32
        3 - 4    7 - 128
        4 - 8
   d: make auto-increment negative - decrement by count listed above.
   s: special-increment mode. If current address is even (when accessing
      programmed PMx), increment it by 1. Else, increment by 32. It is not
      clear what happens if d and n bits are also set (never done by SVP).
   v: over-write mode when writing, unknown when reading (not used).
      Over-write mode splits the word being written into 4 nibbles and only
      writes out ones which are non zero.
     When auto-increment is performed, it affects all 21 address bits.
 */
class PMC extends Register(PMC):
  private enum State:
    case WaitingForAddress, WaitingForMode

  import State.*

  private var state = WaitingForAddress
  private var pmcSet = false

  override def createState(sb: StateBuilder): Unit =
    super.createState(sb)
    sb.
      w("state",state.toString).
      w("pmcSet",pmcSet)

  override def restoreState(sb: StateBuilder): Unit =
    super.restoreState(sb)
    state = State.valueOf(sb.r[String]("state"))
    pmcSet = sb.r[Boolean]("pmcSet")

  def update(value:Int): Unit =
    this.value = value

  override def reset(): Unit =
    super.reset()
    state = WaitingForAddress
    pmcSet = false

  def resetPMCSet(): Unit =
    pmcSet = false
  def isPMCSet: Boolean = pmcSet

  def resetState(): Unit =
    state = WaitingForAddress
    pmcSet = false

  override final def blindAccessedRead(): Unit = read

  override final def write(value: Int): Unit =
    state match
      case WaitingForAddress =>
        this.value = this.value & 0xFFFF0000 | value & 0xFFFF
        state = WaitingForMode
      case WaitingForMode =>
        this.value = this.value & 0x0000FFFF | (value & 0xFFFF) << 16
        pmcSet = true
        state = WaitingForAddress

  override final def read: Int =
    val address = value & 0xFFFF
    state match
      case WaitingForAddress =>
        state = WaitingForMode
        address
      case WaitingForMode =>
        state = WaitingForAddress
        pmcSet = true
        //println(s"Reading PMC with address set: ${((address << 4) & 0xFFF0 | (address >> 4) & 0xF).toHexString}")
        /*
         If read in "waiting for mode" state, we get the same value as in other state, but rotated by 4
         (or with nibbles swapped, VR always does this to words with both bytes equal,
         like 'abab' to get 'baba' for chessboard dithering effect)
        */
        (address << 4) & 0xFFF0 | (address >> 4) & 0xF

  def isAddressSet: Boolean = state == WaitingForMode
// ===========================================================
class ExternalRegister(val index:0|1|2|3|4|5,val mem:SVPMemory,val pmc:PMC,val st:StatusRegister) extends Register(RegisterType.fromOrdinal(RegisterType.PM0.ordinal + index)):
  private inline val R = 0
  private inline val W = 1
  private inline val SPECIAL_INC = 0xFF
  private val externalModeAddress = Array(0,0)
  private val externalAddressIncrement = Array(0,0) // SPECIAL_INC means special increment mode
  private var externalOverwrite = false
  private var rwmode = R

  override def createState(sb: StateBuilder): Unit =
    super.createState(sb)
    sb.
      w("externalModeAddress",externalModeAddress).
      w("externalAddressIncrement",externalAddressIncrement).
      w("externalOverwrite",externalOverwrite)

  override def restoreState(sb: StateBuilder): Unit =
    super.restoreState(sb)
    sb.r("externalModeAddress",externalModeAddress)
    sb.r("externalAddressIncrement",externalAddressIncrement)
    externalOverwrite = sb.r[Boolean]("externalOverwrite")

  override def get: Int = externalModeAddress(rwmode)

  override def reset(): Unit =
    super.reset()
    externalOverwrite = false
    java.util.Arrays.fill(externalModeAddress,0)
    java.util.Arrays.fill(externalAddressIncrement, 0)


  override final def blindAccessedRead(): Unit =
    if pmc.isPMCSet then
      setMode(R)
    pmc.resetPMCSet()
  override final def blindAccessedWrite(): Unit =
    if pmc.isPMCSet then
      setMode(W)
    pmc.resetPMCSet()

  private def setMode(mode:0|1): Unit =
    this.rwmode = mode
    externalModeAddress(mode) = pmc.get
    val pmcMode = externalModeAddress(mode) >>> 16
    externalAddressIncrement(mode) = if isSpecialIncrementMode(pmcMode) then SPECIAL_INC else getAutoIncrement(pmcMode)
    if mode == W then
      externalOverwrite = isOverwriteMode(pmcMode)

  inline private def isOverwriteMode(mode:Int): Boolean = (mode & 0x400) != 0
  inline private def isSpecialIncrementMode(mode:Int): Boolean = (mode & 0x4000) != 0
  inline private def getAutoIncrement(mode:Int): Int =
    val negative = (mode & 0x8000) != 0
    val nnn = (mode >> 11) & 7
    val autoinc = nnn match
      case 0 => 0
      case 1 => 1
      case 2 => 2
      case 3 => 4
      case 4 => 8
      case 5 => 16
      case 6 => 32
      case 7 => 128
    if negative then -autoinc else autoinc

  protected def externalStatusRegisterRead: Int = 0
  protected def externalStatusRegisterWrite(value:Int): Unit = {}

  private def incrementAddress(mode:0|1): Unit =
    if externalAddressIncrement(mode) == SPECIAL_INC then // special increment
      externalModeAddress(mode) += (if (externalModeAddress(mode) & 1) == 1 then 31 else 1)
    else
      externalModeAddress(mode) += externalAddressIncrement(mode)

  override final def read: Int =
    //pmc.resetPMCSet()
    if index == 4 || st.getFlag(StatusRegisterFlag.ST56) > 0 then
      val value = mem.svpExternalRead(externalModeAddress(R) & 0x1F_FFFF)
      incrementAddress(R)
      pmc.update(externalModeAddress(R))
      value
    else
      externalStatusRegisterRead

  private def overwrite(value:Int): Int =
    if externalOverwrite then
      var currentVal = mem.svpExternalRead(externalModeAddress(W) & 0x1F_FFFF)
      if (value & 0xf000) > 0 then
        currentVal &= ~0xf000
        currentVal |= value & 0xf000
      if (value & 0x0f00) > 0 then
        currentVal &= ~0x0f00
        currentVal |= value & 0x0f00
      if (value & 0x00f0) > 0 then
        currentVal &= ~0x00f0
        currentVal |= value & 0x00f0
      if (value & 0x000f) > 0 then
        currentVal &= ~0x000f
        currentVal |= value & 0x000f

      currentVal
    else
      value

  override final def write(value:Int): Unit =
    //pmc.resetPMCSet()
    if index == 4 || st.getFlag(StatusRegisterFlag.ST56) > 0 then
      mem.svpExternalWrite(externalModeAddress(W) & 0x1F_FFFF,overwrite(value))
      incrementAddress(W)
      pmc.update(externalModeAddress(W))
    else
      externalStatusRegisterWrite(value)


