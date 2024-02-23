package ucesoft.smd.cheat

import org.yaml.snakeyaml.Yaml
import ucesoft.smd.Logger

import java.util.zip.GZIPInputStream

/**
 * @author Alessandro Abbruzzetti
 *         Created on 06/01/2024 15:53
 *
 * Credits to http://games.technoplaza.net/ggencoder/java/
 */
object Cheat:
  case class CheatEntry(cheats:List[CheatCode],description:String)
  case class GameCheats(name:String,regions:List[String],cheats:Array[CheatEntry]):
    override def toString: String =
      s"GameCheats($name,$regions,${cheats.mkString("[",",","]")}"
  case class Game(name:String,crc32:String,size:String,year:String)
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

  final val gameCheats = loadGameCheats()
  final val games = loadGames()

  private def loadGames(): Map[String,Game] =
    import java.util.{List as JList, Map as JMap}
    import scala.jdk.CollectionConverters.*

    val yaml = new Yaml()
    val in = classOf[Cheat.type].getResourceAsStream("/resources/cheats/gamesDB.yaml.gz")
    if in == null then
      Logger.getLogger.error("Cannot load cheats file: /resources/cheats/gamesDB.yaml.gz")
      println("Cannot load cheats file")
      return Map.empty
    val gin = new GZIPInputStream(in)
    val db = yaml.load[JList[JMap[String, AnyRef]]](gin).asScala.toList
    val games = for g <- db yield
      val game = g.asScala
      val name = game("game").toString
      val crc32 = game("crc32").toString
      val size = Option(game.getOrElse("size","unknown")).getOrElse("unknown").toString
      val year = Option(game.getOrElse("year","unknown")).getOrElse("unknown").toString
      crc32 -> Game(name,crc32,size,year)

    gin.close()
    games.toMap
  private def loadGameCheats(): List[GameCheats] =
    import java.util.{List as JList, Map as JMap}
    import scala.jdk.CollectionConverters.*

    val yaml = new Yaml()
    val in = classOf[Cheat.type].getResourceAsStream("/resources/cheats/cheatsDB.yaml.gz")
    if in == null then
      Logger.getLogger.error("Cannot load cheats file: /resources/cheats/cheatsDB.yaml.gz")
      println("Cannot load cheats file")
      return Nil
    val gin = new GZIPInputStream(in)
    val db = yaml.load[JList[JMap[String, AnyRef]]](gin).asScala.toList
    val games = for g <- db yield
      val game = g.asScala
      val name = game("game").toString
      val regions = game("regions").asInstanceOf[JList[String]].asScala.toList
      val codes = game("codes").asInstanceOf[JList[JMap[String,AnyRef]]].asScala.toList
      val cheats = for code <- codes yield
        val c = code.asScala.toMap
        val desc = c("desc").toString
        val ccc = c("code").asInstanceOf[JList[String]].asScala.toList
        val cc = c("code").asInstanceOf[JList[String]].get(0).split(",").flatMap(decode).toList
        CheatEntry(cc,desc)
      GameCheats(name,regions,cheats.toArray)

    gin.close()
    games

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
