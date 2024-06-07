package ucesoft.smd.cpu.svp

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
  def blindAccessedWrite(): Unit = {}
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

class Accumulator(val AL:Register) extends Register(ACC):
  inline private def write32(value:Int): Unit =
    this.value = value >>> 16
    AL.write(value & 0xFFFF)

  inline private def read32: Int = value << 16 | AL.read

  final def getA: Int = read32
  final def setA(value:Int,st:StatusRegister): Unit = 
    write32(value)
    setNZ(value,st)
  final def addA(value: Int, st: StatusRegister): Unit =
    write32(read32 + value)
    setNZ(value, st)

  override def reset(): Unit = 
    super.reset()
    AL.reset()

  // affects 32 bits
  inline private def setNZ(a:Int,st:StatusRegister): Unit =
    if a == 0 then st.setFlag(StatusRegisterFlag.Z) else st.clearFlag(StatusRegisterFlag.Z)
    if (a & 0x8000_0000) != 0 then st.setFlag(StatusRegisterFlag.N) else st.clearFlag(StatusRegisterFlag.N)
  inline private def setL(preA:Int,postA:Int,st:StatusRegister): Unit =
    if (preA & 0x10000) == 0 && (postA & 0x10000) != 0 then st.setFlag(StatusRegisterFlag.L) else st.clearFlag(StatusRegisterFlag.L)

  final def aluADD(value:Int,st:StatusRegister,_32:Boolean): Unit =
    var a = read32
    val aPre = a
    a += (if _32 then value else value << 16)
    write32(a)
    setNZ(a,st)
    setL(aPre,a,st)
  final def aluSUB(value: Int, st: StatusRegister, _32: Boolean): Unit =
    var a = read32
    a -= (if _32 then value else value << 16)
    write32(a)
    setNZ(a,st)
  final def aluCMP(value: Int, st: StatusRegister, _32: Boolean): Unit =
    var a = read32
    a -= (if _32 then value else value << 16)
    setNZ(a,st)
  final def aluAND(value: Int, st: StatusRegister, _32: Boolean): Unit =
    var a = read32
    a &= (if _32 then value else value << 16/* | 0xFFFF*/)
    write32(a)
    setNZ(a, st)
  final def aluOR(value: Int, st: StatusRegister, _32: Boolean): Unit =
    var a = read32
    a |= (if _32 then value else value << 16)
    write32(a)
    setNZ(a, st)
  final def aluXOR(value: Int, st: StatusRegister, _32: Boolean): Unit =
    var a = read32
    a ^= (if _32 then value else value << 16)
    write32(a)
    setNZ(a, st)
  final def aluROR(st:StatusRegister): Unit =
    var a = read32
    val lsb = a & 1
    a = a >>> 1 | lsb << 31
    write32(a)
    setNZ(a, st)
  final def aluROL(st: StatusRegister): Unit =
    var a = read32
    val msb = a >>> 31
    a = a << 1 | msb
    write32(a)
    setNZ(a, st)
  final def aluSHR(st: StatusRegister): Unit =
    var a = read32
    a = a >> 1
    write32(a)
    setNZ(a, st)
  final def aluSHL(st: StatusRegister): Unit =
    var a = read32
    a = a << 1
    write32(a)
    setNZ(a, st)
  final def aluINC(st: StatusRegister): Unit =
    var a = read32
    a += 1
    write32(a)
    setNZ(a, st)
  final def aluDEC(st: StatusRegister): Unit =
    var a = read32
    a -= 1
    write32(a)
    setNZ(a, st)
  final def aluNEG(st: StatusRegister): Unit =
    var a = read32
    a = -a
    write32(a)
    setNZ(a, st)
  final def aluABS(st: StatusRegister): Unit =
    var a = read32
    if a < 0 then
      a = -a
      write32(a)
    setNZ(a, st)

end Accumulator


// ===========================================================

class Stack extends Register(STACK):
  private final val stack = Array(0,0,0,0,0,0)
  private var top = -1

  override def toString: String = elements.mkString("[",",","]")

  override def get: Int = top
  override def reset(): Unit = 
    super.reset()
    top = -1

  override final def read: Int =
    if top == -1 then
      println("Reading empty stack")
      io.StdIn.readLine(">")
      0
    else
      val v = stack(top)
      top -= 1
      v

  override final def write(value: Int): Unit =
    if top == 5 then
      println("Writing full stack")
      io.StdIn.readLine(">")
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
  override def read: Int =
    value & ~StatusRegisterFlag.IE.mask
  def getFlag(f:StatusRegisterFlag): Int =
    (value & f.mask) >> f.shift
  def setFlag(f:StatusRegisterFlag,value:Int): Unit =
    this.value &= ~f.mask
    this.value |= (value << f.shift) & f.mask
  def setFlag(f:StatusRegisterFlag): Unit = setFlag(f,1)
  def clearFlag(f:StatusRegisterFlag): Unit =
    this.value &= ~f.mask
  def isSet(f:StatusRegisterFlag): Boolean =
    (value & f.mask) != 0
  def isClear(f: StatusRegisterFlag): Boolean =
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

  def multiply(): Int =
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
  case None, PostIncrementModulo, PostDecrementModulo, PostIncrement, _00, _01, _10, _11


enum PointerRegisterAddressing:
  case Direct, Indirect1, Indirect2

class PointerRegister(val index:0|1|2|3|4|5|6|7,val mem:SVPMemory,val ram:Array[Int],val st:StatusRegister) extends Register(RegisterType.fromOrdinal(RegisterType.R0.ordinal + index)):
  override def read: Int =
    if index == 3 || index == 7 then 0 else value

  override def write(value: Int): Unit =
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
          //println(s"RAM[${if index < 4 then "0" else "1"}](${this.value.toHexString}) = ${value.toHexString}")
        case PostIncrementModulo =>
          ram(this.value) = value & 0xFFFF
          //println(s"RAM[${if index < 4 then "0" else "1"}](${this.value.toHexString}) = ${value.toHexString}")
          modulo(1)
        case PostDecrementModulo =>
          ram(this.value) = value & 0xFFFF
          //println(s"RAM[${if index < 4 then "0" else "1"}](${this.value.toHexString}) = ${value.toHexString}")
          modulo(-1)
        case PostIncrement =>
          ram(this.value) = value & 0xFFFF
          //println(s"RAM[${if index < 4 then "0" else "1"}](${this.value.toHexString}) = ${value.toHexString}")
          this.value = (this.value + 1) & 0xFF
        case _ =>
          println(s"Wrong modifier for addressing $addressing: $modifier")
    else
      modifier match
        case PointerRegisterModifier._00 =>
          ram(0) = value & 0xFFFF
          //println(s"RAM[${if index < 4 then "0" else "1"}](0) = ${value.toHexString}")
        case PointerRegisterModifier._01 =>
          ram(1) = value & 0xFFFF
          //println(s"RAM[${if index < 4 then "0" else "1"}](1) = ${value.toHexString}")
        case PointerRegisterModifier._10 =>
          ram(2) = value & 0xFFFF
          //println(s"RAM[${if index < 4 then "0" else "1"}](2) = ${value.toHexString}")
        case PointerRegisterModifier._11 =>
          ram(3) = value & 0xFFFF
          //println(s"RAM[${if index < 4 then "0" else "1"}](3) = ${value.toHexString}")
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

  def write(addressing:PointerRegisterAddressing,modifier:PointerRegisterModifier,value:Int): Unit =
    import PointerRegisterAddressing.*
    addressing match
      case Direct =>
        write(value)
      case Indirect1 =>
        modWrite(addressing,modifier, value)
      case Indirect2 =>
        println(s"Invalid $addressing with write operation")
        io.StdIn.readLine(">")

  def read(addressing:PointerRegisterAddressing,modifier:PointerRegisterModifier): Int =
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
        mem.svpReadIRamRom(target)
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
    case Address, Mode

  import State.*

  private var state = Address
  private var address = 0
  private var mode = 0

  override def reset(): Unit = 
    super.reset()
    state = Address
    address = 0
    mode = 0

  override def get: Int = mode << 16 | address
  
  def resetState(): Unit =
    state = Address

  override def blindAccessedRead(): Unit =
    state match
      case Address =>
        state = Mode
      case Mode =>
        state = Address

  override def blindAccessedWrite(): Unit =
    state match
      case Address =>
        state = Mode
      case Mode =>
        state = Address

  override def write(value: Int): Unit =
    state match
      case Address =>
        address = value & 0xFFFF
        state = Mode
      case Mode =>
        mode = value & 0xFFFF
        state = Address

  override def read: Int =
    state match
      case Address =>
        state = Mode
        address
      case Mode =>
        state = Address
        /*
           If read in "waiting for mode" state, we get the same value as in other state, but rotated by 4
           (or with nibbles swapped, VR always does this to words with both bytes equal,
           like 'abab' to get 'baba' for chessboard dithering effect)
         */
        (address << 4) & 0xFFF0 | (address >> 4) & 0xF

  def ready(): Boolean = state == Address

  def getTargetAddress: Int =
    address | (mode & 0x1F) << 16
  def getAutoIncrement: Int =
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
  def isSpecialIncrementMode: Boolean =
    (mode & 0x4000) != 0
  def isOverwriteMode: Boolean =
    (mode & 0x400) != 0
// ===========================================================
class ExternalRegister(val index:0|1|2|3|4|5,val mem:SVPMemory,val pmc:PMC,val st:StatusRegister) extends Register(RegisterType.fromOrdinal(RegisterType.PM0.ordinal + index)):
  private inline val R = 0
  private inline val W = 1
  private inline val SPECIAL_INC = 0xFF
  private val externalAddress = Array(0,0)
  private val externalAddressIncrement = Array(0,0) // SPECIAL_INC means special increment mode
  private var externalOverwrite = false
  private var mode = R

  override def get: Int = externalAddress(mode)

  override def reset(): Unit = 
    super.reset()
    externalOverwrite = false
    java.util.Arrays.fill(externalAddress,0)
    java.util.Arrays.fill(externalAddressIncrement, 0)

  override def blindAccessedRead(): Unit =
    if !pmc.ready() then
      println("PMx Blind read with pmc not ready")
      io.StdIn.readLine(">")
    setMode(R)
  override def blindAccessedWrite(): Unit =
    if !pmc.ready() then
      println("PMx Blind write with pmc not ready")
      io.StdIn.readLine(">")
    setMode(W)

  private def setMode(mode:0|1): Unit =
    this.mode = mode
    externalAddress(mode) = pmc.getTargetAddress
    externalAddressIncrement(mode) = if pmc.isSpecialIncrementMode then SPECIAL_INC else pmc.getAutoIncrement
    if mode == W then
      externalOverwrite = pmc.isOverwriteMode

  protected def externalStatusRegisterRead(readByM68K:Boolean): Int = 0
  protected def externalStatusRegisterWrite(value:Int,writeByM68K:Boolean): Unit = {}

  private def incrementAddress(mode:0|1): Unit =
    if externalAddressIncrement(mode) == SPECIAL_INC then // special increment
      externalAddress(mode) += (if (externalAddress(mode) & 1) == 1 then 31 else 1) // Why 31, doc says 32
    else
      externalAddress(mode) += externalAddressIncrement(mode)
    externalAddress(mode) &= 0x1F_FFFF

  override def read: Int = read(readByM68K = false)

  def read(readByM68K:Boolean): Int =
    if index == 4 || st.getFlag(StatusRegisterFlag.ST56) > 0 then
      val value = mem.svpExternalRead(externalAddress(R))
      incrementAddress(R)
      value
    else
      externalStatusRegisterRead(readByM68K)
      
  private def overwrite(value:Int): Int =
    if externalOverwrite then
      var currentVal = mem.svpExternalRead(externalAddress(W))
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

  override def write(value:Int): Unit = write(value,writeByM68K = false)

  def write(value:Int,writeByM68K:Boolean): Unit =
    if index == 4 || st.getFlag(StatusRegisterFlag.ST56) > 0 then
      mem.svpExternalWrite(externalAddress(W),overwrite(value))
      incrementAddress(W)
    else
      externalStatusRegisterWrite(value,writeByM68K)


