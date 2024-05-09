package ucesoft.smd.controller

import ucesoft.smd.*

import scala.compiletime.uninitialized

enum ControllerType(val counterMask:Int):
  case PAD3Buttons extends ControllerType(1)
  case PAD6Buttons extends ControllerType(7)
  case MouseStartWithCTRLAndLeft extends ControllerType(0)
  case Mouse extends ControllerType(0)
  case Menacer extends ControllerType(0)
  case Unknown extends ControllerType(0)
  
enum ControllerDevice:
  case KeyboardPad, RealPad, Mouse, Lightgun, Empty

object Controller:
  inline val CONTROLLER_ROOT_PROP = "controller.%d."
  inline val CONTROLLER_PROP = CONTROLLER_ROOT_PROP + "%s." // index, device
  inline val CONTROLLER_TYPE_PROP = CONTROLLER_ROOT_PROP + "type"
  inline val CONTROLLER_DEVICE_PROP = CONTROLLER_ROOT_PROP + "device"

  trait ControllerChangeListener:
    def controllerChanged(index:Byte, controllerType: ControllerType, eventID:Short, value:Byte): Unit

  def formatProp(s: String, index: Int): String = s.format(index)
  def formatProp(s: String, index: Int,device:String): String = s.format(index,device)

abstract class Controller extends SMDComponent:
  val index : Int
  val device : ControllerDevice
  override protected val smdComponentName = s"Controller $index"
  protected var control = 0
  protected var controllerType: ControllerType = ControllerType.Unknown
  protected var changeListener : Controller.ControllerChangeListener = uninitialized
  protected var delayInMillis = 0f

  override def reset(): Unit =
    delayInMillis = 0

  def setDelayMillis(millis:Float): Unit =
    delayInMillis = millis

  def setChangeListener(cl:Controller.ControllerChangeListener): Unit =
    changeListener = cl

  def forceChange(eventID:Short, value:Byte): Unit = {}

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
  
  def setGameCRC32(crc32:Long): Unit = {}
  
object EmptyController:
  inline val DEVICE_PROP_VALUE = "empty"  
  
class EmptyController(override val index: Int) extends Controller:
  override val device : ControllerDevice = ControllerDevice.Empty
  override def readData(): Int = 0xFF
  override def writeData(value: Int): Unit = {}
  override def writeControl(value: Int): Unit = {}


