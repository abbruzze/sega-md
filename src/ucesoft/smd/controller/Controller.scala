package ucesoft.smd.controller

import ucesoft.smd.*

enum ControllerType(val counterMask:Int):
  case PAD3Buttons extends ControllerType(1)
  case PAD6Buttons extends ControllerType(7)
  case MouseStartWithCTRLAndLeft extends ControllerType(0)
  case Mouse extends ControllerType(0)
  case Unknown extends ControllerType(0)
  
enum ControllerDevice:
  case KeyboardPad, RealPad, Mouse, Empty

object Controller:
  inline val CONTROLLER_PROP = "controller.%d."
  inline val CONTROLLER_TYPE_PROP = CONTROLLER_PROP + "type"
  inline val CONTROLLER_DEVICE_PROP = CONTROLLER_PROP + "device"

  def formatProp(s: String, index: Int): String = s.format(index)
abstract class Controller extends SMDComponent:
  val index : Int
  val device : ControllerDevice
  override protected val smdComponentName = s"Controller $index"
  protected var control = 0
  protected var controllerType: ControllerType = ControllerType.Unknown

  def copyStateFrom(p:Controller): Unit =
    control = p.control

  def setControllerType(ct:ControllerType): Unit =
    controllerType = ct
  def getControllerType : ControllerType = controllerType
  
  def disconnect(): Unit = {}

  def readData(): Int
  def writeData(value: Int): Unit
  //
  def readControl: Int = control
  def writeControl(value: Int): Unit =
    control = value

  protected def performDataWrite(data:Int): Unit = {}
  
class EmptyController(override val index: Int) extends Controller:
  override val device : ControllerDevice = ControllerDevice.Empty
  override def readData(): Int = 0xFF
  override def writeData(value: Int): Unit = {}


