package ucesoft.smd.controller

import ucesoft.smd.*

enum ControllerType(val counterMask:Int):
  case PAD3Buttons extends ControllerType(1)
  case PAD6Buttons extends ControllerType(7)
  case MouseStartWithCTRLAndLeft extends ControllerType(0)
  case Mouse extends ControllerType(0)

object Controller:
  inline val CONTROLLER_PROP = "controller.%d."
  inline val CONTROLLER_TYPE_PROP = CONTROLLER_PROP + "type"
abstract class Controller extends SMDComponent:
  val index : Int
  protected var control = 0

  def setControllerType(ct:ControllerType): Unit = {}
  
  def disconnect(): Unit = {}

  def readData(): Int
  def writeData(value: Int): Unit
  //
  def readControl: Int = control
  def writeControl(value: Int): Unit =
    control = value

  protected def performDataWrite(data:Int): Unit = {}
  
class EmptyController(override val index: Int) extends Controller:
  override def readData(): Int = 0xFF
  override def writeData(value: Int): Unit = {}


