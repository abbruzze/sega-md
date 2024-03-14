package ucesoft.smd.controller

import ucesoft.smd.Clock
import ucesoft.smd.Clock.EventID

import java.util.Properties

object PadController:
  import Controller.*
  
  inline val A = 0
  inline val B = 1
  inline val C = 2
  inline val S = 3
  inline val X = 4
  inline val Y = 5
  inline val Z = 6
  inline val M = 7
  inline val U = 8
  inline val D = 9
  inline val L = 10
  inline val R = 11

  final val BUTTONS_NAMES = Array("A","B","C","START","X","Y","Z","MODE","UP","DOWN","LEFT","RIGHT")
  
  inline val CONTROLLER_BUTTON_A = CONTROLLER_PROP + "button.A"
  inline val CONTROLLER_BUTTON_B = CONTROLLER_PROP + "button.B"
  inline val CONTROLLER_BUTTON_C = CONTROLLER_PROP + "button.C"
  inline val CONTROLLER_BUTTON_START = CONTROLLER_PROP + "button.START"
  inline val CONTROLLER_BUTTON_X = CONTROLLER_PROP + "button.X"
  inline val CONTROLLER_BUTTON_Y = CONTROLLER_PROP + "button.Y"
  inline val CONTROLLER_BUTTON_Z = CONTROLLER_PROP + "button.Z"
  inline val CONTROLLER_BUTTON_M = CONTROLLER_PROP + "button.M"
  inline val CONTROLLER_BUTTON_UP = CONTROLLER_PROP + "dir.UP"
  inline val CONTROLLER_BUTTON_DOWN = CONTROLLER_PROP + "dir.DOWN"
  inline val CONTROLLER_BUTTON_RIGHT = CONTROLLER_PROP + "dir.RIGHT"
  inline val CONTROLLER_BUTTON_LEFT = CONTROLLER_PROP + "dir.LEFT"

  final val buttonsPropNames: Array[String] = Array(
    CONTROLLER_BUTTON_A,
    CONTROLLER_BUTTON_B,
    CONTROLLER_BUTTON_C,
    CONTROLLER_BUTTON_START,
    CONTROLLER_BUTTON_X,
    CONTROLLER_BUTTON_Y,
    CONTROLLER_BUTTON_Z,
    CONTROLLER_BUTTON_M
  )
  final val buttonAndDirectionsPropNames: Array[String] = buttonsPropNames ++ Array(CONTROLLER_BUTTON_UP, CONTROLLER_BUTTON_DOWN, CONTROLLER_BUTTON_RIGHT, CONTROLLER_BUTTON_LEFT)
/**
 * @author Alessandro Abbruzzetti
 *         Created on 29/08/2023 20:16  
 */
abstract class PadController(override val index: Int, val clock: Clock) extends Controller:
  import ControllerType.*
  import PadController.*

  private inline val RESET_COUNTER_TIMEOUT_MILLIS = 1.7f // verified with Joystick Test Program
  
  protected var counter6Button = 0
  protected var timeoutID: EventID = _
  protected var lastWrite = 0x40

  protected final val buttons = Array.fill[Int](12)(1) // U,D,L,R,A,B,C,S,X,Y,Z,M

  override def copyStateFrom(c:Controller): Unit =
    c match
      case p:PadController =>
        counter6Button = p.counter6Button
        timeoutID = p.timeoutID
        lastWrite = p.lastWrite
        control = p.control
      case _ =>

  override def disconnect(): Unit =
    if timeoutID != null then
      timeoutID.cancel()

  override def reset(): Unit =
    java.util.Arrays.fill(buttons, 1)
    counter6Button = 0
    control = 0
    lastWrite = 0x40

  override def readData(): Int =
    val read =
      counter6Button match
        case 0 | 2 | 4 => // CBRLDU
          //log.info(s"CBRDLU($counter6Button): ${(buttons(C) << 5 | buttons(B) << 4 | buttons(R) << 3 | buttons(L) << 2 | buttons(D) << 1 | buttons(U)).toHexString}")
          buttons(C) << 5 | buttons(B) << 4 | buttons(R) << 3 | buttons(L) << 2 | buttons(D) << 1 | buttons(U)
        case 1 | 3 => // SA00DU
          //log.info(s"SA00DU($counter6Button): ${(buttons(S) << 5 | buttons(A) << 4 | /* 00 */ buttons(D) << 1 | buttons(U)).toHexString}")
          buttons(S) << 5 | buttons(A) << 4 | /* 00 */ buttons(D) << 1 | buttons(U)
        case 5 => // SA0000
          //log.info(s"SA0000($counter6Button): ${buttons(S) << 5 | buttons(A) << 4}")
          buttons(S) << 5 | buttons(A) << 4
        case 6 => // CBMXYZ
          //log.info(s"CBMXYZ($counter6Button): ${(buttons(C) << 5 | buttons(B) << 4 | buttons(M) << 3 | buttons(X) << 2 | buttons(Y) << 1 | buttons(Z)).toHexString}")
          buttons(C) << 5 | buttons(B) << 4 | buttons(M) << 3 | buttons(X) << 2 | buttons(Y) << 1 | buttons(Z)
        case 7 => // SA1111
          //log.info(s"SA1111($counter6Button): ${(buttons(S) << 5 | buttons(A) << 4 | 0xF).toHexString}")
          buttons(S) << 5 | buttons(A) << 4 | 0xF
        case _ => // can never happens
          0
    lastWrite & 0xC0 | read & 0x3F // (read & ~control & 0x3F) | (lastWrite & control & 0x3F)

  override def writeData(value: Int): Unit =
    if (control & 0x40) > 0 then // TH as output
      if ((lastWrite ^ value) & 0x40) > 0 then
        //log.info(s"lastWrite=${lastWrite.toHexString} value=${value.toHexString} counter6Button=$counter6Button")
        counter6Button = (counter6Button + 1) & controllerType.counterMask

        if controllerType == ControllerType.PAD6Buttons then
          if timeoutID != null then
            timeoutID.cancel()
          timeoutID = clock.scheduleMillis(RESET_COUNTER_TIMEOUT_MILLIS, _ => {
            counter6Button = 0
            lastWrite = 0x40
          })

    lastWrite = value
    performDataWrite(value)
    
  protected def checkType(config:Properties): Unit =
    val ctype = config.getProperty(Controller.CONTROLLER_TYPE_PROP) match
      case null =>
        ControllerType.PAD3Buttons
      case ctype =>
        try
          ControllerType.valueOf(ctype)
        catch
          case _: Exception =>
            ControllerType.PAD3Buttons
    setControllerType(ctype)
