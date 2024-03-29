package ucesoft.smd

import scala.collection.mutable.ListBuffer

/**
 * @author Alessandro Abbruzzetti
 *         Created on 23/01/2024 16:48  
 */
object MessageBus:
  sealed trait Message:
    val source: AnyRef

  // ======== Messages ==========================
  case class WarpModeMessage(override val source:AnyRef,on:Boolean) extends Message
  case class AudioEnabledMessage(override val source:AnyRef,enabled:Boolean) extends Message
  case class AudioChangeVolume(override val source:AnyRef,up:Boolean) extends Message
  case class ModelChanged(override val source:AnyRef,newModel:Model) extends Message
  case class CartInserted(override val source:AnyRef,cart:Cart) extends Message
  case class CartRemoved(override val source:AnyRef,cart:Cart) extends Message
  case class ControllerConfigurationChanged(override val source:AnyRef) extends Message
  case class StateRestored(override val source:AnyRef,cart:Cart) extends Message
  // ============================================
  trait MessageListener:
    def onMessage(msg:Message): Unit

  private val listeners = new ListBuffer[MessageListener]

  def add(l:MessageListener): Unit =
    if !listeners.contains(l) then
      listeners += l
  def remove(l:MessageListener): Unit =
    listeners -= l

  def send(msg:Message): Unit =
    for l <- listeners.toList do
      l.onMessage(msg)

