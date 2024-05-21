package ucesoft.smd.cpu.svp

import java.io.*
import java.security.MessageDigest
import scala.collection.mutable.ListBuffer

object SVPCompiler:
  def main(arg:Array[String]): Unit =
    if arg.length != 2 && arg.length != 3 then
      println("Usage: [-symbols] <source file> <output file>")
      sys.exit(1)
    val printLabels = arg(0) == "-labels"
    val source = if printLabels then new File(arg(1)) else new File(arg(0))
    val target = if printLabels then new File(arg(2)) else new File(arg(1))
    if !source.exists() then
      println(s"Source file $source does not exist")
      sys.exit(1)

    val comp = new SVPCompiler
    val ts = System.currentTimeMillis()
    comp.compile(source.toString) match
      case Some(err) =>
        println(err)
        sys.exit(1)
      case None =>
    val elapsed = System.currentTimeMillis() - ts
    println(s"Compiled ${comp.getLineNumbers} lines in $elapsed millis, no errors found")
    val code = comp.getCode
    println(s"Compiled into ${code.length} bytes, SHA1 is ${comp.getSHA1}")
    val out = new FileOutputStream(target)
    for w <- code do
      out.write(w >> 8)
      out.write(w & 0xFF)
    out.close()
    println(s"Target file $target written successfully")

    if printLabels then
      println("Symbol table:")
      for l <- comp.getLabels do
          println("%04X %s".format(l._2,l._1))

    /*
    val in = new FileInputStream("""C:\Users\ealeame\Documents\rust\svpdev\tools\ssp16asm\target\debug\svp.bin""")
    in.skip(0x1F800)
    for i <- 0 until 0x400 do
      val w = in.read() << 8 | in.read()
      val v = code(i)
      println("%04X %04X %04X".format(i,w,v))
      if w != v then
        println("Error!")
        sys.exit(1)
    println("Equals!!!!")
     */
/**
 * @author Alessandro Abbruzzetti
 *         Created on 14/05/2024 19:02  
 */
class SVPCompiler:
  private enum Token:
    case DIR_ORG(address:Int)
    case DIR_EQU(label:String,value:Int)
    case Label(label:String)
    case Mnemonic(opcode:String)
    case Register(index:Int)
    case PointerRegister(index:Int,indirection:Int,modifier:Int)
    case Number(n:Int)
    case Ram(j:Int,address:Int)
    case Flag
    case Condition(flag:Int,bit:Int)
    case ResOp(flag:Int)
    case AccOp(op:Int)
    case ProgMem

  private case class Instruction(label:Option[Token.Label],mnemonic:Option[Token.Mnemonic],op1:Option[Token],op2:Option[Token]):
    var pc = 0

  private val MNEMONICS = Array("RET","BRA","CALL","MOD","SUB","CMP","ADD","AND","OR","EOR","SUBI","CMPI","ADDI","ANDI","ORI","EORI","LD","LDI","MLD","MPYA","MPYS","NOP","EQU","ORG","DW")
  private val REGS = Array("-","X","Y","A","ST","STACK","PC","P","EXT0","EXT1","EXT2","EXT3","EXT4","EXT5","EXT6","EXT7")
  private val PTR_REG_RE = """R([01234567])""".r
  private val PTR_REG_IND1_RE = """\(R([012456])(\+|-|\+!)?\)""".r
  private val PTR_REG_IND1_37_RE = """\(R([37])\|(00|01|10|11)?\)""".r
  private val PTR_REG_IND2_37_RE = """\(\(R([37])\|(00|01|10|11)?\)\)""".r
  private val PTR_REG_IND2_RE = """\(\(R([01234567])\)\)""".r
  private val RAM_RE = """([AB])\[(.*)\]""".r
  private val CONDITION_RE = """(L|N|Z|OV)=([01])""".r
  private val CONDITION_ALWAYS_RE = "ALWAYS".r
  private val LABEL_RE = """@([0-9A-Z_\(\)\[\]]+)""".r
  private val RESOP_RE = """RESL|SETL|RESIE|SETIE|RESOP|SETOP|RES|SET""".r
  private val ACC_OP_RE = """ROR|ROL|SHR|SHL|INC|DEC|NEG|ABS""".r
  private val PROG_MEM_RE = """\(A\)""".r

  private val symbols = new collection.mutable.HashMap[String,Int]
  private val labels = new collection.mutable.LinkedHashMap[String,Int]
  private var lastLabel = ""
  private val waitingForLabels = new collection.mutable.HashMap[String,List[Int]]
  private val code = new ListBuffer[Int]
  private var pc = 0
  private var lineNumber = 0

  def getSHA1: String =
    val sh1 = MessageDigest.getInstance("SHA-1")
    for w <- code do
      sh1.update((w >> 8).toByte)
      sh1.update((w & 0xFF).toByte)
    sh1.digest().map(b => "%02X".format(b)).mkString.toUpperCase

  def getCode: List[Int] = code.toList

  def getLabels: List[(String,Int)] = (for e <- labels yield e).toList

  def getLineNumbers: Int = lineNumber

  def compile(fileName:String): Option[String] =
    compile(new FileInputStream(fileName))
  def compile(in:InputStream): Option[String] =
    val cin = new BufferedReader(new InputStreamReader(in))
    try
      code.clear()
      symbols.clear()
      labels.clear()
      waitingForLabels.clear()
      pc = 0

      var keepReading = true
      while keepReading do
        val line = cin.readLine()
        if line == null then
          keepReading = false
        else
          lineNumber += 1
          tokenize(line) match
            case Left(err) =>
              return Some(s"Error on line $lineNumber '$line'\n$err")
            case Right(Instruction(None, None, None, None)) =>
              // comment
            case Right(i@Instruction(label, mnemonic, op1, op2)) =>
              compile(i) match
                case None =>
                case Some(err) =>
                  return Some(s"Error on line $lineNumber '$line'\n$err")
      end while

      // check labels
      fixWaitingLabels() match
        case Some(labels) =>
          return Some(s"Label error: $labels")
        case None =>
      None
    finally
      cin.close()
  end compile

  private def getNumber(op:String): Option[Int] =
    if op == "A" then return None // special case: exadecimal A must be written as 0xA or 0A, 00A, 000A

    if op.charAt(0).isDigit || (op.charAt(0) >= 'A' && op.charAt(0) <= 'F') then
      try
        val ofs = if op.startsWith("0X") then 2 else 0
        Some(Integer.parseInt(op.substring(ofs),16))
      catch
        case _: NumberFormatException =>
          None
    else
      None

  private def tokenizeOperand(_op:String,numberAllowed:Boolean): Either[String,Token] =
    val op = _op.toUpperCase()
    // F
    if op == "F" then return Right(Token.Flag)
    // number
    if numberAllowed then
      getNumber(op) match
        case Some(n) =>
          if n > 0xFFFF then
            return Left(s"Operand '$op' too large (must be < 0x10000)")
          else
            return Right(Token.Number(n))
        case _ =>
    // mnemonics
    val mnPos = MNEMONICS.indexOf(op)
    if mnPos != -1 then
      return Right(Token.Mnemonic(op))
    // register
    val regPos = REGS.indexOf(op)
    if regPos != -1 then
      return Right(Token.Register(regPos))

    op match
      // pointer register
      case PTR_REG_RE(i) => return Right(Token.PointerRegister(i.toInt,0,0))
      case PTR_REG_IND1_RE(i,m) =>
        val mod = m match
          case null => 0
          case "+!" => 1
          case "-" => 2
          case "+" => 3
        return Right(Token.PointerRegister(i.toInt,1,mod))
      case PTR_REG_IND1_37_RE(i,m) =>
        val mod = if m == null then 0 else Integer.parseInt(m,2)
        return Right(Token.PointerRegister(i.toInt,1,mod))
      case PTR_REG_IND2_37_RE(i,m) =>
        val mod = if m == null then 0 else Integer.parseInt(m,2)
        return Right(Token.PointerRegister(i.toInt,2,mod))
      case PTR_REG_IND2_RE(i) =>
        return Right(Token.PointerRegister(i.toInt,2,0))
      // ram
      case RAM_RE(ab,adr) =>
        getNumber(adr) match
          case Some(n) =>
            if n > 0xFF then
              return Left(s"Operand '$op' too large (must be < 0x100)")
            else
              return Right(Token.Ram(if ab == "A" then 0 else 1,n))
          case None =>
            return Left(s"Syntax error on operand '$op': expected a number")
      // condition
      case CONDITION_RE(cond,bit) =>
        val ccc = cond match
          case "L" => 4
          case "Z" => 5
          case "OV" => 6
          case "N" => 7
          case _ =>
            throw new IllegalArgumentException(s"Condition '$cond' not implemented")

        return Right(Token.Condition(ccc,if bit == "0" then 0 else 1))
      case CONDITION_ALWAYS_RE() =>
        return Right(Token.Condition(0,0))
      // label
      case LABEL_RE(label) =>
        return Right(Token.Label(label))
      // resop
      case RESOP_RE() =>
        val flag = op match
          case "RESL" => 2
          case "SETL" => 3
          case "RESIE" => 4
          case "SETIE" => 5
          case "RESOP" => 8
          case "SETOP" => 9
          case "RES" => 14
          case "SET" => 15
        return Right(Token.ResOp(flag))
      case ACC_OP_RE() =>
        val accop = op match
          case "ROR" => 0
          case "ROL" => 1
          case "SHR" => 2
          case "SHL" => 3
          case "INC" => 4
          case "DEC" => 5
          case "NEG" => 6
          case "ABS" => 7
          case _ =>
            throw new IllegalArgumentException(s"Accumulator operation '$op' not implemented")
        return Right(Token.AccOp(accop))
      case PROG_MEM_RE() =>
        return Right(Token.ProgMem)
      case _ =>

    symbols.get(op) match
      case Some(n) =>
        return Right(Token.Number(n))
      case _ =>

    Left("Syntax error")


  private def tokenize(_line:String): Either[String,Instruction] =
    val line = _line.replaceAll("\t"," ")
    var label : Option[Token.Label] = None

    val commentIndex = line.indexOf('#')
    var instr = if commentIndex != -1 then line.substring(0,commentIndex).trim else line.trim
    val labelIndex = instr.indexOf(':')
    if labelIndex != -1 then
      label = Some(Token.Label(instr.substring(0,labelIndex).trim))
      instr = instr.substring(labelIndex + 1).trim
    val spacePos = instr.indexOf(' ')
    if spacePos == -1 && instr.isEmpty then
      return Right(Instruction(label,None,None,None))
    val mnem = if spacePos == -1 then instr else instr.substring(0,spacePos)
    val ops = if spacePos == -1 then "" else instr.substring(spacePos).trim
    tokenizeOperand(mnem,false) match
      case Left(err) =>
        Left(err)
      case Right(m@Token.Mnemonic(opcode)) =>
        if ops.isEmpty then // no operands
          return Right(Instruction(label,Some(m),None,None))
        val operands = ops.split(",")
        if operands.length > 2 then
          return Left(s"Too many operands: ${operands.length}")

        val op1 = tokenizeOperand(operands(0).trim,true) match
            case Right(tokOp1) =>
              tokOp1
            case Left(err) =>
              return Left(s"Invalid operand 1: $err")
        val op2 = if operands.length == 2 then
          tokenizeOperand(operands(1).trim,true) match
            case Right(tokOp2) =>
              Some(tokOp2)
            case Left(err) =>
              return Left(s"Invalid operand 2: $err")
        else
          None
        Right(Instruction(label,Some(m),Some(op1),op2))
      case Right(_) =>
        Left(s"Invalid opcode '$mnem'")

  private def compile(i:Instruction): Option[String] =
    import Token.*
    i.pc = pc
    // check labels
    i.label match
      case Some(Label(label)) =>
        if labels.contains(label) then
          return Some(s"Label $label already defined")
        labels += label.toUpperCase() -> pc
        lastLabel = label.toUpperCase()
      case _ =>

    i.mnemonic match
      // directives
      case Some(Mnemonic("ORG")) =>
        return org(i)
      case Some(Mnemonic("EQU")) =>
        return equ(i)
      case Some(Mnemonic("DW")) =>
        return dw(i)
      // alu
      case Some(Mnemonic("SUB")|Mnemonic("SUBI")) =>
        return alu(i)
      case Some(Mnemonic("ADD")|Mnemonic("ADDI")) =>
        return alu(i)
      case Some(Mnemonic("CMP")|Mnemonic("CMPI")) =>
        return alu(i)
      case Some(Mnemonic("AND")|Mnemonic("ANDI")) =>
        return alu(i)
      case Some(Mnemonic("OR")|Mnemonic("ORI")) =>
        return alu(i)
      case Some(Mnemonic("EOR")|Mnemonic("EORI")) =>
        return alu(i)
      // ld
      case Some(Mnemonic("LD")|Mnemonic("LDI")) =>
        return ld(i)
      // mod
      case Some(Mnemonic("MOD")) =>
        return mod(i)
      // call
      case Some(Mnemonic("CALL")) =>
        return call(i)
      // bra
      case Some(Mnemonic("BRA")) =>
        return bra(i)
      // ret
      case Some(Mnemonic("RET")) =>
        return ld(Instruction(i.label,i.mnemonic,Some(Register(6)),Some(Register(5)))) // 0000 0000 0110 0101  PC = STACK; // same as ld PC, STACK
      // mld
      case Some(Mnemonic("MLD")) =>
        mld(i)
      // mpya
      case Some(Mnemonic("MPYA")) =>
        mpya(i)
      case None =>
      case o =>
        return Some(s"Unimplemented operation $o")

    None
  end compile

  private def fixWaitingLabels(): Option[String] =
    val missing = for (label,offsets) <- waitingForLabels yield
      labels.get(label) match
        case Some(target) =>
          for a <- offsets do
            code(a) = target
          None
        case None =>
            Some(label)
    val missingFlat = missing.toList.flatten
    if missingFlat.nonEmpty then
      Some(s"Missing labels: ${missingFlat.mkString(",")}")
    else
      None

  inline private def encode(e:Int): Unit =
    code += e
    pc += 1

  private def org(i:Instruction): Option[String] =
    import Token.*
    i.op1 match
      case Some(Number(n)) =>
        pc = n
      case Some(_) =>
        return Some("Operand mismatch: expected a number")
      case None =>
        return Some("Invalid number of operands: expected 1")
    i.op2 match
      case None =>
      case Some(_) =>
        return Some("Invalid number of operands: expected 1")
    None

  private def equ(i: Instruction): Option[String] =
    import Token.*
    i.op1 match
      case Some(Number(n)) =>
        symbols(lastLabel) = n
      case Some(_) =>
        return Some("Operand mismatch: expected a number")
      case None =>
        return Some("Invalid number of operands: expected 1")
    i.op2 match
      case None =>
      case Some(_) =>
        return Some("Invalid number of operands: expected 1")
    None

  private def dw(i: Instruction): Option[String] =
    import Token.*
    i.op1 match
      case Some(Number(n)) =>
        encode(n)
      case Some(_) =>
        return Some("Operand mismatch: expected a number")
      case None =>
        return Some("Invalid number of operands: expected 1")
    i.op2 match
      case None =>
      case Some(_) =>
        return Some("Invalid number of operands: expected 1")
    None

  private def alu(i: Instruction): Option[String] =
    import Token.*
    var mnem = i.mnemonic.get.opcode
    var imm = false
    if mnem.endsWith("I") then
      mnem = mnem.substring(0,mnem.length - 1)
      imm = true

    val ooo = mnem match
      case "SUB" => 1
      case "CMP" => 3
      case "ADD" => 4
      case "AND" => 5
      case "OR" => 6
      case "EOR" => 7

    (i.op1,i.op2) match
      // OP  A, s      ooo0 0000 0000 rrrr  A OP r << 16
      case (Some(Register(3)),Some(Register(r))) if !imm =>
        encode(ooo << 13 | r)
      // OP  A, (ri)   ooo0 001j 0000 mmpp  A OP RAMj[pr_modif_read(m,jpp)] << 16
      case (Some(Register(3)),Some(PointerRegister(ptr,1,m))) if !imm =>
        val j = if ptr < 4 then 0 else 1
        encode(ooo << 13 | 1 << 9 | j << 8 | m << 2 | ptr & 3)
      // OP  A, adr    ooo0 011j aaaa aaaa  A OP RAMj[a] << 16
      case (Some(Register(3)), Some(Ram(j,adr))) if !imm =>
        encode(ooo << 13 | 3 << 9 | j << 8 | adr & 0xFF)
      // OPi A, imm    ooo0 1000 0000 0000  A OP i << 16
      //               iiii iiii iiii iiii
      case (Some(Register(3)), Some(Number(i))) if imm =>
        if i > 0xFFFF then return Some("Immediate value too large")
        encode(ooo << 13 | 1 << 11)
        encode(i)
      case (Some(Register(3)), Some(Label(label))) if imm =>
        encode(ooo << 13 | 1 << 11)
        labels.get(label) match
          case Some(address) =>
            encode(address)
          case None =>
            addWaitingLabel(label)
            encode(0)
      // op  A, ((ri)) ooo0 101j 0000 mmpp  tmp = pr_modif_read(m,jpp)
      //                                    A OP program_memory[RAMj[tmp]] << 16
      //                                    RAMj[tmp]++
      case (Some(Register(3)), Some(PointerRegister(ptr,2,m))) if !imm =>
        val j = if ptr < 4 then 0 else 1
        encode(ooo << 13 | 5 << 9 | j << 8 | m << 2 | ptr & 3)
      // op  A, ri     ooo1 001j 0000 00pp  A OP RIJ[jpp] << 16
      case (Some(Register(3)), Some(PointerRegister(ptr, 0, _))) if !imm =>
        val j = if ptr < 4 then 0 else 1
        encode(ooo << 13 | 9 << 9 | j << 8 | ptr & 3)
      // OPi simm      ooo1 1000 iiii iiii  A OP i << 16
      case (Some(Number(i)),None) if imm =>
        if i > 0xFF then return Some("Immediate value too large")
        encode(ooo << 13 | 3 << 11 | i)
      case _ =>
        return Some("Invalid addressing or operand combination for ALU operation")

    None
  end alu

  private def ld(i: Instruction): Option[String] =
    import Token.*
    (i.op1,i.op2) match
      // ld  d, s      0000 0000 dddd ssss  d = s
      case (Some(Register(d)),Some(Register(s))) =>
        encode(d << 4 | s)
      // ld  d, (ri)   0000 001j dddd mmpp  d = RAMj[pr_modif_read(m,jpp)]
      case (Some(Register(d)), Some(PointerRegister(ptr,1,m))) =>
        val j = if ptr < 4 then 0 else 1
        encode(1 << 9 | j << 8 | d << 4 | m << 2 | ptr & 3)
      // ld  (ri), s   0000 010j ssss mmpp  RAMj[pr_modif_read(m,jpp)] = s
      case (Some(PointerRegister(ptr, 1, m)),Some(Register(s))) =>
        val j = if ptr < 4 then 0 else 1
        encode(1 << 10 | j << 8 | s << 4 | m << 2 | ptr & 3)
      // ldi d, imm    0000 1000 dddd 0000  d = i
      //               iiii iiii iiii iiii
      case (Some(Register(d)), Some(Number(i))) =>
        if i > 0xFFFF then return Some("Immediate value too large")
        encode(1 << 11 | d << 4)
        encode(i)
      case (Some(Register(d)), Some(Label(label))) =>
        encode(1 << 11 | d << 4)
        labels.get(label) match
          case Some(address) =>
            encode(address)
          case None =>
            addWaitingLabel(label)
            encode(0)
      // ld  d, ((ri)) 0000 101j dddd mmpp  tmp = pr_modif_read(m,jpp)
      //                                    d = program_memory[RAMj[tmp]]
      //                                    RAMj[tmp]++
      case (Some(Register(d)), Some(PointerRegister(ptr, 2, m))) =>
        val j = if ptr < 4 then 0 else 1
        encode(5 << 9 | j << 8 | d << 4 | m << 2 | ptr & 3)
      // ldi (ri), imm 0000 110j 0000 mmpp  RAMj[pr_modif_read(m,jpp)] = i
      //               iiii iiii iiii iiii
      case (Some(PointerRegister(ptr, 1, m)), Some(Number(i))) =>
        if i > 0xFFFF then return Some("Immediate value too large")
        val j = if ptr < 4 then 0 else 1
        encode(3 << 10 | j << 8 | m << 2 | ptr & 3)
        encode(i)
      case (Some(PointerRegister(ptr, 1, m)), Some(Label(label))) =>
        val j = if ptr < 4 then 0 else 1
        encode(3 << 10 | j << 8 | m << 2 | ptr & 3)
        labels.get(label) match
          case Some(address) =>
            encode(address)
          case None =>
            addWaitingLabel(label)
            encode(0)
      // ld  adr, a    0000 111j aaaa aaaa  RAMj[a] = A
      case (Some(Ram(j,adr)),Some(Register(3))) =>
        if adr > 0xFF then return Some("Immediate value too large")
        encode(7 << 9 | j << 8 | adr)
      // ld  a, adr    0000 111j aaaa aaaa  RAMj[a] = A
      case (Some(Register(3)),Some(Ram(j,adr))) =>
        if adr > 0xFF then return Some("Immediate value too large")
        encode(3 << 9 | j << 8 | adr)
      // ld  d, ri     0001 001j dddd 00pp  d = RIJ[jpp]
      case (Some(Register(d)), Some(PointerRegister(ptr, 0, m))) =>
        val j = if ptr < 4 then 0 else 1
        encode(9 << 9 | j << 8 | d << 4 | ptr & 3)
      // ld  ri, s     0001 010j ssss 00pp  RIJ[jpp] = s
      case (Some(PointerRegister(ptr, 0, m)), Some(Register(s))) =>
        val j = if ptr < 4 then 0 else 1
        encode(5 << 10 | j << 8 | s << 4 | ptr & 3)
      // ldi ri, simm  0001 1jpp iiii iiii  RIJ[jpp] = i
      case (Some(PointerRegister(ptr, 0, m)), Some(Number(i))) =>
        if i > 0xFF then return Some("Immediate value too large")
        val j = if ptr < 4 then 0 else 1
        encode(3 << 11 | j << 10 | (ptr & 3) << 8 | i)
      // ld  d, (a)    0100 1010 dddd 0000  d = program_memory[A[31:16]]
      case (Some(Register(d)), Some(ProgMem)) =>
        encode(37 << 9 | d << 4)
      case _ =>
        return Some("Invalid addressing or operand combination for LD operation")
    None
  end ld

  private def mod(i:Instruction): Option[String] =
    import Token.*
    (i.op1, i.op2) match
      // mod cond, op  1001 000f cccc 0ooo
      case (Some(Condition(flag,bit)), Some(AccOp(op))) =>
        encode(9 << 12 | bit << 8 | flag << 4 | op)
      // mod f, op     1001 0100 0000 oooo
      case (Some(Flag), Some(ResOp(op))) =>
        encode(0x4A << 9 | op)
      case _ =>
        return Some("Invalid addressing or operand combination for MOD operation")
    None
  end mod

  private def addWaitingLabel(label:String): Unit =
    val list = waitingForLabels.getOrElse(label,Nil)
    waitingForLabels += label -> (code.length - 1 :: list)

  private def call(i:Instruction): Option[String] =
    import Token.*
    (i.op1, i.op2) match
      // call cond, addr  0100 100f cccc 0000  if (check_cond(c,f)) {
      //                  aaaa aaaa aaaa aaaa    STACK = next_op_address(); PC = a }
      case (Some(Condition(flag,bit)),Some(Number(n))) =>
        encode(9 << 11 | bit << 8 | flag << 4)
        encode(n)
      case (Some(Condition(flag, bit)), Some(Label(label))) =>
        encode(9 << 11 | bit << 8 | flag << 4)
        labels.get(label) match
          case Some(address) =>
            encode(address)
          case None =>
            addWaitingLabel(label)
            encode(0)
      case _ =>
        return Some("Invalid addressing or operand combination for CALL operation")
    None
  end call

  private def bra(i: Instruction): Option[String] =
    import Token.*
    (i.op1, i.op2) match
      // bra  cond, addr  0100 110f cccc 0000  if (check_cond(c,f)) PC = a
      //                  aaaa aaaa aaaa aaaa
      case (Some(Condition(flag, bit)), Some(Number(n))) =>
        encode(19 << 10 | bit << 8 | flag << 4)
        encode(n)
      case (Some(Condition(flag, bit)), Some(Label(label))) =>
        encode(19 << 10 | bit << 8 | flag << 4)
        labels.get(label) match
          case Some(address) =>
            encode(address)
          case None =>
            addWaitingLabel(label)
            encode(0)
      case _ =>
        return Some("Invalid addressing or operand combination for CALL operation")
    None
  end bra

  private def mld(i: Instruction): Option[String] =
    import Token.*
    (i.op1, i.op2) match
      // mld  (rj), (ri)  1011 0111 nnjj mmii  A = 0; update_flags()
      //                                       X = RAM0[pr_modif_read(m,0ii)]
      //                                       Y = RAM1[pr_modif_read(m,1jj)]
      //                                       P = sign_extend(X) * sign_extend(Y) * 2
      case (Some(PointerRegister(rj,1,mj)),Some(PointerRegister(ri,1,mi))) =>
        if rj > 3 || ri < 4 then return Some("Invalid pointer register index for MLD operation")
        encode(183 << 8 | mj << 6 | (rj & 3) << 4 | mi << 2 | ri & 3)
      case _ =>
        return Some("Invalid addressing or operand combination for MLD operation")
    None
  end mld

  private def mpya(i: Instruction): Option[String] =
    import Token.*
    (i.op1, i.op2) match
      // mpya (rj), (ri)  1001 0111 nnjj mmii  A += P; update_flags()
      //                                       X = RAM0[pr_modif_read(m,0ii)]
      //                                       Y = RAM1[pr_modif_read(m,1jj)]
      //                                       P = sign_extend(X) * sign_extend(Y) * 2
      case (Some(PointerRegister(rj, 1, mj)), Some(PointerRegister(ri, 1, mi))) =>
        if rj > 3 || ri < 4 then return Some("Invalid pointer register index for MPYA operation")
        encode(151 << 8 | mj << 6 | (rj & 3) << 4 | mi << 2 | ri & 3)
      case _ =>
        return Some("Invalid addressing or operand combination for MPYA operation")
    None
  end mpya

  private def mpys(i: Instruction): Option[String] =
    import Token.*
    (i.op1, i.op2) match
      // mpys (rj), (ri)  0011 0111 nnjj mmii  A -= P; update_flags()
      //                                       X = RAM0[pr_modif_read(m,0ii)]
      //                                       Y = RAM1[pr_modif_read(m,1jj)]
      //                                       P = sign_extend(X) * sign_extend(Y) * 2
      case (Some(PointerRegister(rj, 1, mj)), Some(PointerRegister(ri, 1, mi))) =>
        if rj > 3 || ri < 4 then return Some("Invalid pointer register index for MPYA operation")
        encode(55 << 8 | mj << 6 | (rj & 3) << 4 | mi << 2 | ri & 3)
      case _ =>
        return Some("Invalid addressing or operand combination for MPYA operation")
    None
  end mpys