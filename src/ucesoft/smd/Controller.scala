package ucesoft.smd

import ucesoft.smd.Clock.EventID

import java.awt.event.{KeyEvent, KeyListener}

enum ControllerType(val counterMask:Int):
  case PAD3Buttons extends ControllerType(1)
  case PAD6Buttons extends ControllerType(7)

trait Controller:
  val index : Int

  def setControllerType(ct:ControllerType): Unit

  def readData(): Int
  def writeData(value: Int): Unit
  //
  def readControl: Int
  def writeControl(value: Int): Unit

/**
 * @author Alessandro Abbruzzetti
 *         Created on 29/08/2023 20:16  
 */
abstract class PadController(override val index:Int,val ctype:ControllerType,val clock:Clock) extends SMDComponent with Controller:
  import ControllerType.*
  private inline val RESET_COUNTER_TIMEOUT_MILLIS = 1.7f // verified with Joystick Test Program

  protected var controllerType : ControllerType = ctype
  protected var control = 0
  protected var lastWrite = 0
  protected var counter6Button = 0
  protected var timeoutID : EventID = _

  protected final val buttons = Array.fill[Int](12)(1) // U,D,L,R,A,B,C,S,X,Y,Z,M
  protected inline val A = 0
  protected inline val B = 1
  protected inline val C = 2
  protected inline val S = 3
  protected inline val X = 4
  protected inline val Y = 5
  protected inline val Z = 6
  protected inline val M = 7
  protected inline val U = 8
  protected inline val D = 9
  protected inline val L = 10
  protected inline val R = 11

  override def reset(): Unit =
    java.util.Arrays.fill(buttons,1)
    counter6Button = 0
    control = 0
    lastWrite = 0

  override def setControllerType(ct:ControllerType): Unit =
    controllerType = ct

  override def readData() : Int =
    val read =
        counter6Button match
          case 0|2|4 => // CBRLDU
            //log.info(s"CBRDLU($counter6Button): ${(buttons(C) << 5 | buttons(B) << 4 | buttons(R) << 3 | buttons(L) << 2 | buttons(D) << 1 | buttons(U)).toHexString}")
            buttons(C) << 5 | buttons(B) << 4 | buttons(R) << 3 | buttons(L) << 2 | buttons(D) << 1 | buttons(U)
          case 1|3 => // SA00DU
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

  override def writeData(value:Int): Unit =
    if (control & 0x40) > 0 then // TH as output
      if ((lastWrite ^ value) & 0x40) > 0 then
        //log.info(s"lastWrite=${lastWrite.toHexString} value=${value.toHexString} counter6Button=$counter6Button")
        counter6Button += 1
        if counter6Button == controllerType.counterMask + 1 then
          counter6Button = 0
          lastWrite = 0x40

        if controllerType == ControllerType.PAD6Buttons then
          if timeoutID != null then
            timeoutID.cancel()
          timeoutID = clock.scheduleMillis(RESET_COUNTER_TIMEOUT_MILLIS,(cycles,skip) => counter6Button = 0 )

    lastWrite = value
    performDataWrite(value)
  //
  override def readControl: Int = control
  override def writeControl(value:Int): Unit =
    control = value
    lastWrite = 0x40

  protected def performDataWrite(data:Int): Unit = {}

class KeyboardPADController(override val index:Int,override val ctype:ControllerType,override val clock:Clock) extends PadController(index,ctype,clock) with KeyListener:
  import java.awt.event.KeyEvent.*

  private var keyMap = Map(
    VK_A -> A,
    VK_B -> B,
    VK_C -> C,
    VK_S -> S,
    VK_X -> X,
    VK_Y -> Y,
    VK_Z -> Z,
    VK_M -> M,
    VK_UP -> U,
    VK_DOWN -> D,
    VK_RIGHT -> R,
    VK_LEFT -> L
  )
  def setKeyMap(map:Map[Int,Int]): Unit =
    keyMap = map
  override def keyPressed(e: KeyEvent): Unit =
    keyMap.get(e.getExtendedKeyCode) match
      case Some(k) =>
        buttons(k) = 0
      case None =>
  override def keyReleased(e: KeyEvent): Unit =
    keyMap.get(e.getExtendedKeyCode) match
      case Some(k) =>
        buttons(k) = 1
      case None =>
  override def keyTyped(e: KeyEvent): Unit = {}


