package ucesoft.smd.ui

import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import java.awt.{Cursor, Point, Toolkit}
import javax.swing.{JComponent, SwingUtilities, Timer}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 08/01/2024 14:25  
 */
object MouseHider extends MouseMotionListener with MouseListener:
  private inline val DELAY = 2000
  private val emptyCursor = {
    val cursor = new java.awt.image.BufferedImage(16, 16, java.awt.image.BufferedImage.TYPE_INT_ARGB)
    Toolkit.getDefaultToolkit.createCustomCursor(cursor, new Point(0, 0), "null")
  }
  private val timer = new Timer(DELAY,_ => hideCursor())

  private var component: JComponent = _

  private def hideCursor(): Unit =
    SwingUtilities.getWindowAncestor(component).setCursor(emptyCursor)

  private def showCursor(): Unit =
    SwingUtilities.getWindowAncestor(component).setCursor(Cursor.getDefaultCursor)
    if timer.isRunning then
      timer.restart()
    else
      timer.start()

  def hideMouseOn(component:JComponent): Unit =
    component.addMouseListener(this)
    component.addMouseMotionListener(this)
    this.component = component

  def showMouseOn(component:JComponent): Unit =
    component.removeMouseListener(this)
    component.removeMouseMotionListener(this)
    timer.stop()
    SwingUtilities.getWindowAncestor(component).setCursor(Cursor.getDefaultCursor)

  override def mouseDragged(e: MouseEvent): Unit = showCursor()
  override def mouseMoved(e: MouseEvent): Unit = showCursor()
  override def mouseClicked(e: MouseEvent): Unit = showCursor()
  override def mousePressed(e: MouseEvent): Unit = {}
  override def mouseReleased(e: MouseEvent): Unit = {}
  override def mouseEntered(e: MouseEvent): Unit = showCursor()
  override def mouseExited(e: MouseEvent): Unit = timer.stop()
