package ucesoft.smd

import scala.collection.mutable.ListBuffer

/**
 * @author Alessandro Abbruzzetti
 *         Created on 03/09/2023 20:14  
 */
trait SMDComponent:
  private val components = new ListBuffer[SMDComponent]
  protected var log = Logger.getLogger
  protected var componentEnabled = true
  
  final def isComponentEnabled: Boolean = componentEnabled
  final def setComponentEnabled(enabled:Boolean): Unit =
    this.componentEnabled = enabled
  
  protected def init(): Unit = {}
  protected def reset(): Unit = {}
  protected def hardReset(): Unit = reset()
  
  final def initComponent(): Unit =
    components.foreach(_.initComponent())
    init()
    
  final def resetComponent(): Unit =
    components.foreach(_.resetComponent())
    reset()

  final def hardResetComponent(): Unit =
    components.foreach(_.hardResetComponent())
    hardReset()

  def setLogger(logger: Logger): Unit =
    log = logger
    components.foreach(_.setLogger(logger))