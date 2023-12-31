package ucesoft.smd.controller

import net.java.games.input.{Component, Controller, ControllerEnvironment}
import ucesoft.smd.controller.Controller.*
import ucesoft.smd.controller.PadController.*
import ucesoft.smd.{Clock, Logger}

import java.util.Properties

/**
 * @author Alessandro Abbruzzetti
 *         Created on 04/01/2024 20:13  
 */
object USBPadController:
  inline val USB_TYPE = "usb"
  inline val CONTROLLER_NAME_PROP = CONTROLLER_PROP + "name"
  inline val CONTROLLER_POLLING_PROP = CONTROLLER_PROP + "pollingMillis"

  private var controllers: Array[Controller] = Array()
  private var discoverDone = false
  private var discoverSuccessful = false

  private def discoverControllers(millis:Int = 1000): Unit =
    if !discoverDone then
      val thread = new Thread:
        override def run(): Unit =
          System.setProperty("jinput.loglevel", "SEVERE")
          controllers = ControllerEnvironment.getDefaultEnvironment.getControllers
          Logger.getLogger.info("JInput controllers discovery terminated [%d]",controllers.length)

      thread.start()
      thread.join(millis)
      discoverDone = true
      discoverSuccessful = true

  private def getControllers: Array[Controller] = controllers
  private def updateControllers(millis:Int): Unit =
    if discoverSuccessful then
      discoverControllers(millis)
  def getControllersNames: Array[String] = controllers.map(_.getName)

class USBPadController(config:Properties,override val index: Int, override val ctype: ControllerType, override val clock: Clock) extends PadController(index,ctype,clock) with Runnable:
  import USBPadController.*
  private inline val DIR_THRESHOLD = 0.5f

  private var controller : Option[Controller] = None
  private var xAxisComponent : Component = _
  private var yAxisComponent : Component = _
  private var buttonsComponent : List[(Int,Component)] = Nil
  private val thread = new Thread(this,s"USBController($index)")
  private var running = true

  discoverControllers()
  findController(config)
  if controller.isDefined then
    thread.start()

  override def disconnect(): Unit =
    running = false
  def updateConfig(config:Properties): Unit =
    updateControllers(1000)
    findController(config)
    
  def getButtonsMap: Map[Int,String] =
    buttonsComponent.map(kv => (kv._1,kv._2.getName)).toMap

  private def findController(config:Properties): Unit =
    val controllerName = config.getProperty(formatProp(CONTROLLER_NAME_PROP,index))

    val controllers = getControllers
    for c <- controllers do
      println(c.getName)
    controller = controllers.find(c => c.getName.trim == controllerName && (c.getType == Controller.Type.GAMEPAD || c.getType == Controller.Type.STICK))
    controller match
      case Some(c) =>
        log.info("USB controller found: %s",controllerName)
        xAxisComponent = c.getComponent(Component.Identifier.Axis.X)
        yAxisComponent = c.getComponent(Component.Identifier.Axis.Y)
        buttonsComponent = buttonsPropNames.zipWithIndex.map((b, i) =>
          (i,c.getComponents.find(_.getIdentifier.getName == config.getProperty(formatProp(b,index), s"${i + 1}")))
        ).filter(_._2.isDefined).map((i,c) => (i,c.get))
        if ctype == ControllerType.PAD3Buttons then
          buttonsComponent = buttonsComponent.filter(_._1 > S)
        for b <- buttonsComponent do
          log.info(s"Assigned button ${BUTTONS_NAMES(b._1)} to ${b._2}")
      case None =>
        log.warning("USB controller %s not found",controllerName)

  override def run(): Unit =
    val polling = config.getProperty(formatProp(CONTROLLER_POLLING_PROP,index),"10").toInt
    running = true
    log.info("USB controller %d thread started",index)
    val c = controller.get
    while running do
      Thread.sleep(polling)
      c.poll()
      java.util.Arrays.fill(buttons,1)
      // axis
      val x = if xAxisComponent != null then xAxisComponent.getPollData else 0
      val y = if xAxisComponent != null then yAxisComponent.getPollData else 0
      if x < -DIR_THRESHOLD then buttons(L) = 0
      else if x > DIR_THRESHOLD then buttons(R) = 0
      if y < -DIR_THRESHOLD then buttons(U) = 0
      else if y > DIR_THRESHOLD then buttons(D) = 0
      // buttons
      for (b,c) <- buttonsComponent do
        if c.getPollData != 0.0 then buttons(b) = 0

    log.info("USB controller %d thread stopped",index)
