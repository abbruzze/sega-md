package ucesoft.smd.controller

import net.java.games.input.{Component, Controller, ControllerEnvironment}
import ucesoft.smd.controller.Controller.*
import ucesoft.smd.controller.PadController.*
import ucesoft.smd.misc.Preferences
import ucesoft.smd.{Clock, Logger}

import java.util.Properties

/**
 * @author Alessandro Abbruzzetti
 *         Created on 04/01/2024 20:13  
 */
object RealPadController:
  inline val DEVICE_PROP_VALUE = "real_pad"
  inline val CONTROLLER_NAME_PROP = CONTROLLER_PROP + "name"

  private var controllers: Array[Controller] = Array()
  private var discoverDone = false
  private var discoverSuccessful = false

  class WaitButtonTask(controller: Controller, action: String => Unit) extends Runnable:
    private val thread = new Thread(this)
    @volatile private var stopped = false

    override def run(): Unit =
      while !stopped do
        Thread.sleep(100)
        controller.poll()
        val buttons = controller.getComponents filter { c => !c.isAnalog && !c.isRelative && c.getIdentifier.getClass.getName.toUpperCase().endsWith("BUTTON") }
        for c <- buttons do
          if c.getPollData != 0.0 && !stopped then
            action(c.getName)
            stopped = true

    def stop(): Unit = stopped = true
    def start(): Unit = thread.start()

  def discoverControllers(millis:Int = 1000): Unit =
    if !discoverDone then
      val thread = new Thread("DiscoverController"):
        override def run(): Unit =
          System.setProperty("jinput.loglevel", "SEVERE")
          controllers = ControllerEnvironment.getDefaultEnvironment.getControllers
          println(s"Got ${controllers.length} controllers")
          Logger.getLogger.info("JInput controllers discovery terminated [%d]",controllers.length)

      thread.start()
      thread.join(millis)
      discoverDone = true
      discoverSuccessful = true

  private def getControllers: Array[Controller] = controllers
  private def updateControllers(millis:Int): Unit =
    if discoverSuccessful then
      discoverControllers(millis)
  def getControllersNames: Array[String] = controllers.filter(c => c.getType == Controller.Type.GAMEPAD || c.getType == Controller.Type.STICK).map(_.getName.trim)
  def getControllerByName(name:String) : Option[Controller] = controllers.find(_.getName.trim == name)

class RealPadController(config:Properties,pref:Preferences,override val index: Int, override val clock: Clock) extends PadController(index,clock) with Runnable:
  override val device : ControllerDevice = ControllerDevice.RealPad
  import RealPadController.*
  private inline val DIR_THRESHOLD = 0.5f

  private var controller : Option[Controller] = None
  private var xAxisComponent : Component = scala.compiletime.uninitialized
  private var yAxisComponent : Component = scala.compiletime.uninitialized
  private var buttonsComponent : List[(Int,Component)] = Nil
  private val thread = new Thread(this,s"RealController($index)")
  private var running = true

  checkType(config)
  discoverControllers()
  findController(config)
  if controller.isDefined then
    thread.start()

  def waitForButton(action: String => Unit): Option[WaitButtonTask] = controller.map(c => WaitButtonTask(c, action))

  override def disconnect(): Unit =
    running = false
  def updateConfig(config:Properties): Unit =
    updateControllers(1000)
    findController(config)
    
  def getButtonsMap: Map[Int,String] =
    buttonsComponent.map(kv => (kv._1,kv._2.getName)).toMap

  private def findController(config:Properties): Unit =
    val controllerName = config.getProperty(formatProp(CONTROLLER_NAME_PROP,index,DEVICE_PROP_VALUE))

    val controllers = getControllers
//    for c <- controllers do
//      println(c.getName)
    controller = controllers.find(c => c.getName.trim == controllerName && (c.getType == Controller.Type.GAMEPAD || c.getType == Controller.Type.STICK))
    controller match
      case Some(c) =>
        log.info("USB controller found: %s",controllerName)
        xAxisComponent = c.getComponent(Component.Identifier.Axis.X)
        yAxisComponent = c.getComponent(Component.Identifier.Axis.Y)
        buttonsComponent = buttonsPropNames.zipWithIndex.map((b, i) =>
          (i,c.getComponents.find(_.getName.trim == config.getProperty(formatProp(b,index,DEVICE_PROP_VALUE), s"Button ${i + 1}").trim))
        ).filter(_._2.isDefined).map((i,c) => (i,c.get)).toList

        if getControllerType == ControllerType.PAD3Buttons then
          buttonsComponent = buttonsComponent.filterNot(_._1 > S)
        for b <- buttonsComponent do
          log.info(s"Assigned button ${BUTTONS_NAMES(b._1)} to ${b._2}")
      case None =>
        log.warning("USB controller %s not found",controllerName)

  override def run(): Unit =
    val millis = pref.get[Int](Preferences.REAL_PAD_POLLING_MILLIS).map(_.value).getOrElse(10).toString
    val polling = config.getProperty(formatProp(millis,index,DEVICE_PROP_VALUE),"10").toInt
    running = true
    log.info("USB controller %d thread started",index)
    val c = controller.get
    while running do
      Thread.sleep(polling)
      c.poll()
      resetButtons()
      // axis
      val x = if xAxisComponent != null then xAxisComponent.getPollData else 0
      val y = if xAxisComponent != null then yAxisComponent.getPollData else 0
      if x < -DIR_THRESHOLD then setButton(L,0)
      else if x > DIR_THRESHOLD then setButton(R,0)
      if y < -DIR_THRESHOLD then setButton(U,0)
      else if y > DIR_THRESHOLD then setButton(D,0)
      // buttons
      for (b,c) <- buttonsComponent do
        if c.getPollData != 0.0 then
          setButton(b,0)

    log.info("USB controller %d thread stopped",index)
