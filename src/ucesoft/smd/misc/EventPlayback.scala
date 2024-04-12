package ucesoft.smd.misc

import ucesoft.smd.Clock
import ucesoft.smd.controller.Controller

import java.io.{DataInputStream, EOFException, FileInputStream}
import scala.compiletime.uninitialized

object EventPlayback:
  case class EventRecordingInfo(crc32:String,gameName:String)
  def checkRecording(file:String): Option[EventRecordingInfo] =
    var in : DataInputStream = null
    try
      in = new DataInputStream(new FileInputStream(file))
      Some(EventRecordingInfo(in.readUTF(),in.readUTF()))
    catch
      case _:Throwable =>
        None
    finally 
      if in != null then
        in.close()
/**
 * @author Alessandro Abbruzzetti
 *         Created on 11/04/2024 19:49  
 */
class EventPlayback(file:String,controllers:Array[Controller],clock:Clock,eofAction: () => Unit):
  private val in = new DataInputStream(new FileInputStream(file))
  private var clockID : Clock.EventID = uninitialized

  def start(): Unit =
    // skip crc32, game name
    in.readUTF()
    in.readUTF()
    clock.schedule(1,_ => scheduleNext())

  def shutdown(): Unit =
    if clockID != null then
      clockID.cancel()
    eofAction()

  private def scheduleNext(): Unit =
    try
      val cycles = in.readLong()
      val index = in.readByte()
      val eventID = in.readShort()
      val value = in.readByte()
      clockID = clock.scheduleAbsolute(cycles,_ => {
        controllers(index).forceChange(eventID,value)
        scheduleNext()
      })
    catch
      case _:EOFException =>
        in.close()
        eofAction()
      case t:Throwable =>
        println("Error while playing back events:")
        t.printStackTrace()
        in.close()
        eofAction()
