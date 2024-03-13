package ucesoft.smd.misc

import ucesoft.smd.ui.MessageGlassPane

import java.awt.event.{KeyAdapter, KeyEvent, KeyListener, MouseListener}
import java.awt.{Color, Dimension, GraphicsEnvironment}
import javax.imageio.ImageIO
import javax.swing.{JComponent, JFrame, JOptionPane}

object FullScreenMode:
  def getScreenDeviceIDs: Array[String] =
    val env = GraphicsEnvironment.getLocalGraphicsEnvironment
    env.getScreenDevices.filter(_.isFullScreenSupported).map(_.getIDstring)
  def goFullScreen(screenDeviceIndex:Int,
                   glassPane:MessageGlassPane,
                   frame:JFrame,
                   component:JComponent,
                   width:Int,
                   height:Int,
                   menuKeyListener: KeyListener = null) : Unit =
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
      
      for kl <- frame.getKeyListeners do
        window.addKeyListener(kl)
      for ml <- frame.getMouseListeners do
        window.addMouseListener(ml)
      for mml <- frame.getMouseMotionListeners do
        window.addMouseMotionListener(mml)
      if menuKeyListener != null then
        window.addKeyListener(menuKeyListener)
      
      window.addKeyListener(new KeyAdapter:
        override def keyPressed(e:KeyEvent) : Unit =
          e.getKeyCode match
            case java.awt.event.KeyEvent.VK_ENTER if e.isAltDown =>
              window.dispose()
              glassPane.changeFrame(frame)
              frame.setVisible(true)
              component.setSize(originalSize)
              frame.getContentPane.add("Center",component)
              frame.pack()
            case _ =>
      )
    else JOptionPane.showMessageDialog(frame,"Your display device does not support full screen mode","Full Screen Mode",JOptionPane.ERROR_MESSAGE)