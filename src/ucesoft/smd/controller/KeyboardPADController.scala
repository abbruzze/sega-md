package ucesoft.smd.controller

import ucesoft.smd.Clock

import java.awt.event.{KeyEvent, KeyListener}
import java.util.Properties

/**
 * @author Alessandro Abbruzzetti
 *         Created on 24/12/2023 17:44  
 */
class KeyboardPADController(component:java.awt.Component,config:Properties,override val index: Int, override val ctype: ControllerType, override val clock: Clock) extends PadController(index, ctype, clock) with KeyListener:
  import java.awt.event.KeyEvent.*
  import PadController.*
  
  private val DEFAULT_KEYMAPS = Array(
    Map(
      VK_A -> A,
      VK_S -> B,
      VK_D -> C,
      VK_ENTER -> S,
      VK_Q -> X,
      VK_W -> Y,
      VK_E -> Z,
      VK_R -> M,
      VK_UP -> U,
      VK_DOWN -> D,
      VK_RIGHT -> R,
      VK_LEFT -> L
    ),
    Map(
      VK_J -> A,
      VK_K -> B,
      VK_L -> C,
      VK_BACK_SPACE -> S,
      VK_U -> X,
      VK_I -> Y,
      VK_O -> Z,
      VK_P -> M,
      VK_Y -> U,
      VK_B -> D,
      VK_H -> R,
      VK_G -> L
    )
  )
  
  component.addKeyListener(this)

  private var keyMap = buildFromProperties(config)

  override def disconnect(): Unit =
    component.removeKeyListener(this)
  
  def updateConfig(config:Properties): Unit =
    keyMap = buildFromProperties(config)
    
  def getKeyMap: Map[Int,Int] = keyMap
  
  private def buildFromProperties(config:Properties): Map[Int,Int] =
    val revereseDefaultMap = DEFAULT_KEYMAPS(index).map(kv => (kv._2,kv._1))
    buttonAndDirectionsPropNames.zipWithIndex.map(b => {
      val key = config.getProperty(formatProp(b._1, index))
      if key == null then 
        (revereseDefaultMap(b._2),b._2)
      else
        (key.toInt,b._2)
    }).toMap

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
