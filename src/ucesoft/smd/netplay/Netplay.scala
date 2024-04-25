package ucesoft.smd.netplay

import java.io.{DataInputStream, DataOutputStream, IOException}
import java.util.concurrent.{LinkedBlockingDeque, TimeUnit}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 10/04/2024 11:24  
 */
object Netplay:
  case class User(name:String,emulatorVersion:String,passCode:String,gameName:String,gameCRC32:String)

  private inline val JOIN_CMD       = 0x00
  private inline val JOIN_ACK_CMD   = 0x01
  private inline val JOIN_ERR_CMD   = 0x02
  private inline val NETSTOP_CMD    = 0x03
  private inline val PING_CMD       = 0x04
  private inline val PONG_CMD       = 0x05
  private inline val LATENCY_CMD    = 0x06
  private inline val CONTROLLER_CMD = 0x07
  private inline val START_GAME_CMD = 0x08

  sealed trait Command(val cmd:Byte)

  case class JOIN(user:User) extends Command(JOIN_CMD)
  case object JOIN_ACK extends Command(JOIN_ACK_CMD)
  case class JOIN_ERR(error:String) extends Command(JOIN_ERR_CMD)
  case object NETSTOP extends Command(NETSTOP_CMD)
  case object PING extends Command(PING_CMD)
  case object PONG extends Command(PONG_CMD)
  case class LATENCY(latency:Int) extends Command(LATENCY_CMD)
  case class CONTROLLER(index:Int,eventID:Short,value:Byte) extends Command(CONTROLLER_CMD)
  case object START_GAME extends Command(START_GAME_CMD)

  trait CommandListener:
    def ioError(msg:String): Unit
    def joined(user:User): Unit
    def joinAck(): Unit
    def joinError(error:String): Unit
    def disconnected(userName:String): Unit
    def latency(lat:Int): Unit
    def controller(userID:Int,index:Int,eventID:Short,value:Byte): Unit
    def startGame(): Unit

  class CommandSender(out:DataOutputStream,commandListener: CommandListener) extends Thread(s"CommandSender(unknown)"):
    private inline val LATENCY_SAMPLES = 10
    private var running = true
    private val queue = new LinkedBlockingDeque[Command]
    private var pingTS = 0L
    private var latencyAccum = 0
    private var latencyN = 0
    private var lastLatency = -1
    private var userName = ""

    def setUserName(name:String): Unit =
      setName(name)
      userName = name

    def pingLatency: Int = lastLatency
    def pongReceived(): Unit =
      latencyAccum += (System.currentTimeMillis() - pingTS).toInt
      latencyN += 1
      if latencyN == LATENCY_SAMPLES then
        lastLatency = latencyAccum / LATENCY_SAMPLES
        latencyN = 0
        latencyAccum = 0

    def send(cmd:Command): Unit =
      queue.put(cmd)

    def shutdown(): Unit =
      running = false

    override def run(): Unit =
      while running do
        try
          queue.pollFirst(1,TimeUnit.SECONDS) match
            case null =>
              // queue is empty
            case JOIN_ACK =>
              out.write(JOIN_ACK_CMD)
            case JOIN_ERR(error) =>
              out.write(JOIN_ERR_CMD) ; out.writeUTF(error)
            case PING =>
              pingTS = System.currentTimeMillis()
              out.write(PING_CMD)
            case PONG =>
              out.write(PONG_CMD)
            case LATENCY(latency) =>
              out.writeShort(latency)
            case NETSTOP =>
              commandListener.disconnected(userName)
            case CONTROLLER(index,eventID, value) =>
              out.write(CONTROLLER_CMD)
              out.write(index)
              out.writeShort(eventID)
              out.write(value)
            case START_GAME =>
              out.write(START_GAME_CMD)
            case c =>
              println(s"Bad command to send: $c")
        catch
          case io:IOException =>
            running = false
            println(s"I/O error while sending command to $userName")
            io.printStackTrace()
            commandListener.ioError(io.getMessage)
      println(s"CommandSender($userName) stopped")

  class CommandReceiver(userID:Int,in:DataInputStream,commandSender: CommandSender, cmdListener: CommandListener) extends Thread(s"CommandReceiver(unknown)"):
    private var running = true
    private var userName = ""
    private var lastCommandTS = 0L

    def getLastCommandTS: Long = lastCommandTS

    def shutdown(): Unit =
      running = false
      interrupt()

    override def run(): Unit =
      while running do
        try
          in.read() match
            // server
            case JOIN_CMD =>
              val user = User(in.readUTF(),in.readUTF(),in.readUTF(),in.readUTF(),in.readUTF())
              userName = user.name
              setName(userName)
              commandSender.setUserName(userName)
              cmdListener.joined(user)
            case JOIN_ACK_CMD =>
              cmdListener.joinAck()
            case JOIN_ERR_CMD =>
              cmdListener.joinError(in.readUTF())
            case NETSTOP_CMD =>
              cmdListener.disconnected(userName)
            case PING_CMD =>
              commandSender.send(PONG)
            case PONG_CMD =>
              commandSender.pongReceived()
            case LATENCY_CMD =>
              cmdListener.latency(in.readShort())
            case CONTROLLER_CMD =>
              cmdListener.controller(userID,in.read(),in.readShort(),in.readByte())
            case START_GAME_CMD =>
              cmdListener.startGame()
            case -1 =>
              println("No more bytes available on this channel, closing ...")
              running = false
            case c =>
              println(s"Unexpected command (receiving): $c")
          lastCommandTS = System.currentTimeMillis()
        catch
          case _: InterruptedException =>
            running = false
          case io: IOException =>
            running = false
            println(s"I/O error while receiving command from $userName")
            io.printStackTrace()
            cmdListener.ioError(io.getMessage)
      println(s"CommandReceiver($userName) stopped")
