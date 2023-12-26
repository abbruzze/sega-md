package ucesoft.smd.controller

import ucesoft.smd.Clock

import java.awt.event.{KeyEvent, KeyListener}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 24/12/2023 17:44  
 */
class KeyboardPADController(override val index: Int, override val ctype: ControllerType, override val clock: Clock) extends PadController(index, ctype, clock) with KeyListener:
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

  def setKeyMap(map: Map[Int, Int]): Unit =
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
