package ucesoft.smd.misc

import ucesoft.smd.Clock
import ucesoft.smd.controller.{Controller, ControllerType}

import java.io.{DataOutputStream, FileOutputStream}
import java.util.concurrent.{LinkedBlockingDeque, TimeUnit}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 11/04/2024 19:28  
 */
class EventRecorder(crc32:String,gameName:String,file:String,clock:Clock,closeAction: () => Unit) extends Runnable with Controller.ControllerChangeListener:
  private case class Event(index:Byte,eventID:Short,eventValue:Byte)

  private val queue = new LinkedBlockingDeque[Event]
  private val thread = new Thread(this,"EventRecorder")
  private var running = true

  thread.start()

  def shutdown(): Unit =
    running = false

  override def run(): Unit =
    val out = new DataOutputStream(new FileOutputStream(file))
    out.writeUTF(crc32)
    out.writeUTF(gameName)
    try
      while running do
        queue.pollFirst(1,TimeUnit.SECONDS) match
          case null =>
          case Event(index,id,value) =>
            out.writeLong(clock.cycles)
            out.writeByte(index)
            out.writeShort(id)
            out.writeByte(value)
    finally
      closeAction()
      out.close()

  override def controllerChanged(index: Byte, controllerType: ControllerType, eventID: Short, value: Byte): Unit =
    queue.put(Event(index,eventID,value))
