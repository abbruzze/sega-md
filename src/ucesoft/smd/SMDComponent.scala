package ucesoft.smd

import ucesoft.smd

import scala.collection.mutable.ListBuffer

/**
 * @author Alessandro Abbruzzetti
 *         Created on 03/09/2023 20:14  
 */
trait SMDComponent extends MessageBus.MessageListener:
  private val components = new ListBuffer[SMDComponent]
  protected val smdComponentName : String = ""
  protected var log = Logger.getLogger
  protected var componentEnabled = true
  
  final def add(c:SMDComponent): Unit =
    components += c
  final def remove(c:SMDComponent): Unit =
    components -= c
  
  final def isComponentEnabled: Boolean = componentEnabled
  def setComponentEnabled(enabled:Boolean): Unit =
    this.componentEnabled = enabled
  
  protected def init(): Unit = {}
  protected def reset(): Unit = {}
  protected def hardReset(): Unit = reset()
  
  final def initComponent(): Unit =
    log.info("Initializing %s",smdComponentName)
    init()
    components.foreach(_.initComponent())
    
  final def resetComponent(): Unit =
    log.info("Resetting %s",smdComponentName)
    components.foreach(_.resetComponent())
    reset()

  final def hardResetComponent(): Unit =
    log.info("Hard resetting %s",smdComponentName)
    components.foreach(
      _.hardResetComponent())
    hardReset()

  def setLogger(logger: Logger): Unit =
    log = logger
    components.foreach(_.setLogger(logger))

  override def onMessage(msg: MessageBus.Message): Unit = {}

  final def createComponentState(): StateBuilder =
    try
      val sb = new StateBuilder()
      val csb = new StateBuilder()
      createState(csb)
      sb.w("component",csb.build())
      val compNames = components.map(_.smdComponentName)
      val compNamesSet = compNames.toSet
      if compNames.size != compNamesSet.size || compNamesSet.contains("component") then
        throw StateBuilder.StateBuilderException(s"SMD component $smdComponentName contains a subcomponent with 'component' name or two or more subcomponents have the same name")
      val children = new StateBuilder()
      for comp <- components do
        val state = comp.createComponentState()
        children.w(comp.smdComponentName, state.build())
      sb.w("children",children.build())
      sb
    catch
      case se:StateBuilder.StateBuilderException =>
        se.addPath(smdComponentName)
        throw se
      case t: Throwable =>
        val se = new StateBuilder.StateBuilderException(s"Unexpected error: $t")
        se.addPath(smdComponentName)
        throw se  
  final def restoreComponentState(sb:StateBuilder): Unit =
    try
      sb.subStateBuilder("component") match
        case Some(comp) =>
          sb.subStateBuilder("children") match
            case Some(children) =>
              for c <- components do
                children.subStateBuilder(c.smdComponentName) match
                  case Some(child) =>
                    c.restoreComponentState(child)
                  case None =>
                    throw new StateBuilder.StateBuilderException(s"Error while restoring ${c.smdComponentName} child of $smdComponentName: component not found")
              restoreState(comp)
            case None =>
              throw new StateBuilder.StateBuilderException(s"Error while restoring $smdComponentName: cannot find 'children' attribute")
        case None =>
          throw new StateBuilder.StateBuilderException(s"Error while restoring $smdComponentName: cannot find 'component' attribute")
    catch
      case se: StateBuilder.StateBuilderException =>
        se.addPath(smdComponentName)
        throw se
      case t: Throwable =>
        val se = new StateBuilder.StateBuilderException(s"Unexpected error: $t",t)
        se.addPath(smdComponentName)
        throw se

  protected def createState(sb:StateBuilder): Unit = {}
  protected def restoreState(sb:StateBuilder): Unit = {}