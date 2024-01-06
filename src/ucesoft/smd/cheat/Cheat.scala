package ucesoft.smd.cheat

/**
 * @author Alessandro Abbruzzetti
 *         Created on 06/01/2024 15:53
 *
 * Credits to http://games.technoplaza.net/ggencoder/java/
 */
object Cheat:
  case class CheatCode(address:Int,value:Int):
    private var romOldValue = 0
    private var romPatched = false

    def isROMPatched: Boolean = romPatched
    def patchROM(rom:Array[Int]): Unit =
      romPatched = true
      if value < 256 then
        romOldValue = rom(address)
        rom(address) = value
      else
        romOldValue = rom(address) << 8 | rom(address + 1)
        rom(address) = (value >> 8) & 0xFF
        rom(address + 1) = value & 0xFF
    def restoreROM(rom:Array[Int]): Unit =
      if romPatched then
        if value < 256 then
          rom(address) = romOldValue
        else
          rom(address) = (romOldValue >> 8) & 0xFF
          rom(address + 1) = romOldValue & 0xFF
        romPatched = false

  private val GENESIS_CODES = Array(
    'A', 'B', 'C', 'D', 'E',
    'F', 'G', 'H', 'J', 'K',
    'L', 'M', 'N', 'P', 'R',
    'S', 'T', 'V', 'W', 'X',
    'Y', 'Z', '0', '1', '2',
    '3', '4', '5', '6', '7',
    '8', '9'
  )
  private val CODE_RE = "^([A-Za-z0-9]{4})-([A-Za-z0-9]{4})$".r
  private val RAW_CODE_RE = "^([A-Fa-f0-9]{1,6}):([A-Fa-f0-9]{1,4})$".r

  def decode(code:String): Option[CheatCode] =
    decodeGG(code).orElse(decodeRaw(code))

  private def isValidCode(code:String): Boolean = CODE_RE.matches(code)
  private def toIndex(c:Char): Int = GENESIS_CODES.indexOf(c)

  private def decodeRaw(code:String): Option[CheatCode] =
    code match
      case RAW_CODE_RE(a,v) =>
        Some(CheatCode(Integer.parseInt(a,16),Integer.parseInt(v,16)))
      case _ =>
        None
  private def decodeGG(code:String): Option[CheatCode] =
    if !isValidCode(code) then
      return None

    val ggcode = code.substring(0, 4) + code.substring(5)
    var bitstring = 0L
    var address, value, temp = 0

    for c <- ggcode do
      bitstring <<= 5
      bitstring |= toIndex(c)

      // position abcd
      value = (((bitstring >> 7) & 0xE) | ((bitstring >> 15) & 0x1)).toInt
      // position efgh
      temp = (((bitstring >> 11) & 0xE) | ((bitstring >> 11) & 0x1)).toInt
      value <<= 4
      value |= temp
      // position ijklmnop
      temp = (bitstring >> 32).toInt
      value <<= 8
      value |= temp
      // a-p = value, a-x = addy
      // ijkl mnop IJKL MNOP ABCD EFGH defg habc QRST UVWX
      // position ABCDEFGH
      address = ((bitstring >> 16) & 0xFF).toInt
      // position IJKLMNOP
      temp = ((bitstring >> 24) & 0xFF).toInt
      address <<= 8
      address |= temp
      // position QRSTUVWX
      temp = (bitstring & 0xFF).toInt
      address <<= 8
      address |= temp

    Some(CheatCode(address,value))
