package ucesoft.smd.misc

import ucesoft.smd.ui.MessageGlassPane

import java.awt.event.{KeyAdapter, KeyEvent, KeyListener, MouseListener}
import java.awt.{Color, Dimension, GraphicsEnvironment}
import javax.imageio.ImageIO
import javax.swing.{JComponent, JFrame, JOptionPane}

object FullScreenMode:
  case class FullscreenWindow(window:JFrame,glassPane:MessageGlassPane,component:JComponent,frame:JFrame,originalSize:Dimension)
  def getScreenDeviceIDs: Array[String] =
    val env = GraphicsEnvironment.getLocalGraphicsEnvironment
    env.getScreenDevices.filter(_.isFullScreenSupported).map(_.getIDstring)
  def goFullScreen(screenDeviceIndex:Int,
                   glassPane:MessageGlassPane,
                   frame:JFrame,
                   component:JComponent,
                   width:Int,
                   height:Int) : Option[FullscreenWindow] =
    val env = GraphicsEnvironment.getLocalGraphicsEnvironment
    val device = env.getScreenDevices()(screenDeviceIndex)
    val conf = device.getDefaultConfiguration
    if device.isFullScreenSupported then
      val window = new JFrame(conf)
      window.getContentPane.setLayout(null)
      window.getContentPane.add(component)
      window.getContentPane.setBackground(Color.BLACK)
      frame.setVisible(false)
      window.setUndecorated(true)
      glassPane.changeFrame(window)
      window.setIconImage(ImageIO.read(getClass.getResourceAsStream("/resources/sonic_ring.png")))
      device.setFullScreenWindow(window)
      val size = conf.getBounds
      val windowWidthFactor = size.width / width.toDouble
      val windowHeightFactor = size.height / height.toDouble
      val factor = math.min(windowWidthFactor,windowHeightFactor)
      val originalSize = component.getSize()
      component.setSize(new Dimension((width * factor).toInt,(height * factor).toInt))
      val vicSize = component.getSize()
      val winSize = window.getSize()
      component.setLocation((winSize.width - vicSize.width) / 2,(winSize.height - vicSize.height) / 2)
      component.invalidate()
      window.validate()
      window.setVisible(true)
      window.toFront()
      
      Some(FullscreenWindow(window,glassPane,component,frame,originalSize))
    else 
      JOptionPane.showMessageDialog(frame,"Your display device does not support full screen mode","Full Screen Mode",JOptionPane.ERROR_MESSAGE)
      None
      
  def restore(w:FullscreenWindow): Unit =
    w.window.dispose()
    w.glassPane.changeFrame(w.frame)
    w.frame.setVisible(true)
    w.component.setSize(w.originalSize)
    w.frame.getContentPane.add("Center", w.component)
    w.frame.pack()