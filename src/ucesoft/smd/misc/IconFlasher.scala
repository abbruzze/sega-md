package ucesoft.smd.misc

import java.awt.Image
import javax.swing.JFrame

/**
 * @author Alessandro Abbruzzetti
 *         Created on 12/04/2024 19:30  
 */
class IconFlasher(frame:JFrame,periodInMillis:Int,restoreIcon:Image,icons:Image*) extends Runnable:
  private var running = true
  private var index = 0
  private val thread = new Thread(this,"IconFlasher")

  def start(): Unit =
    thread.start()

  def shutdownAndWait(): Unit =
    running = false
    thread.join()

  override def run(): Unit =
    while running do
      Thread.sleep(periodInMillis)
      index = (index + 1) % icons.length
      frame.setIconImage(icons(index))
      
    frame.setIconImage(restoreIcon)
