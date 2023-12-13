package ucesoft.smd.ui

import java.awt.event.{ComponentAdapter, ComponentEvent, ComponentListener}
import java.awt.{Color, Font}
import java.util.concurrent
import java.util.concurrent.{CountDownLatch, Executors, LinkedBlockingDeque}
import javax.swing.{JFrame, JLabel, JPanel, SwingUtilities}

object MessageGlassPane:
  enum YPOS:
    case TOP, CENTER, BOTTOM
  enum XPOS:
    case LEFT, CENTER, RIGHT

  case class Message(text:String,xpos:XPOS,ypos:YPOS,millis:Int,color:Option[Color],fadingMillis:Option[Int] = None)
/**
 * @author Alessandro Abbruzzetti
 *         Created on 13/12/2023 15:59  
 */
class MessageGlassPane(frame:JFrame) extends ComponentListener with Runnable:
  import MessageGlassPane.*

  private inline val FONT_WINDOW_WIDTH_RATIO = 30.0f
  private var xoff, yoff = 0
  private var msg : Message = _
  private var enabled = true
  private val queue = new LinkedBlockingDeque[Message]
  private val thread = new Thread(this,"MessageGlassThread")
  private var messageOnScreenLock = new CountDownLatch(1)
  private val messageTimerExecutor = Executors.newFixedThreadPool(1)
  private val font = new JLabel().getFont

  frame.addComponentListener(this)
  frame.getRootPane.addComponentListener(new ComponentAdapter:
    override def componentResized(e: ComponentEvent): Unit =
      if enabled then
        renderMessage()
  )
  thread.start()

  def run(): Unit =
    while true do
      messageOnScreenLock.await()
      msg = queue.take()
      messageOnScreenLock = new CountDownLatch(1)
      renderMessage(startTimer = true)


  def add(msg:Message): Unit =
    queue.offer(msg)

  private def initPane(): Unit =
    val insets = frame.getContentPane.getBounds
    val glassPane = frame.getGlassPane.asInstanceOf[JPanel]
    glassPane.setLayout(null)
    glassPane.setVisible(true)
    xoff = insets.x
    yoff = insets.y
    messageOnScreenLock.countDown()

  private def renderMessage(startTimer:Boolean = false): Unit =
    val glassPane = frame.getGlassPane.asInstanceOf[JPanel]
    glassPane.removeAll()

    if msg != null then
      val fsize = frame.getSize
      val fontSize = fsize.width / FONT_WINDOW_WIDTH_RATIO
      val label = new JLabel(msg.text)
      label.setForeground(msg.color.getOrElse(Color.WHITE))
      label.setFont(font.deriveFont(fontSize).deriveFont(Font.BOLD))
      val fm = label.getFontMetrics(label.getFont)
      glassPane.add(label)

      val y = msg.ypos match
        case YPOS.TOP =>
          0
        case YPOS.BOTTOM =>
          fsize.height - fm.getHeight - yoff - fm.getDescent
        case YPOS.CENTER =>
          (fsize.height - fm.getHeight) / 2
      val x = msg.xpos match
        case XPOS.LEFT =>
          0
        case XPOS.RIGHT =>
          fsize.width - (fm.charsWidth(msg.text.toCharArray,0,msg.text.length) * 1.3).toInt
        case XPOS.CENTER =>
          (fsize.width - fm.charsWidth(msg.text.toCharArray,0,msg.text.length)) / 2

      val msgSize = label.getPreferredSize
      label.setBounds(xoff + x,yoff + y,msgSize.width,msgSize.height)
      glassPane.repaint()

      if startTimer then
        messageTimerExecutor.submit(new Runnable:
          override def run(): Unit =
            Thread.sleep(msg.millis)
            val glassPane = frame.getGlassPane.asInstanceOf[JPanel]
            val fadingMillis = msg.fadingMillis.getOrElse(0)
            if fadingMillis > 0 then
              val sleep = fadingMillis / 256
              val label = glassPane.getComponent(0)
              var color = label.getForeground
              var c = 255
              while c >=0 do
                color = new Color(color.getRed,color.getGreen,color.getBlue,c)
                label.setForeground(color)
                Thread.sleep(sleep)
                c -= 1
            msg = null
            messageOnScreenLock.countDown()
            SwingUtilities.invokeLater(() => {
              glassPane.removeAll()
              glassPane.repaint()
            })
        )
  def setGlassPaneEnabled(enabled:Boolean): Unit =
    this.enabled = enabled
    frame.getGlassPane.asInstanceOf[JPanel].setVisible(enabled)

  override def componentResized(e: ComponentEvent): Unit = {}
  override def componentMoved(e: ComponentEvent): Unit = {}
  override def componentShown(e: ComponentEvent): Unit =
    initPane()
    frame.removeComponentListener(this)
  override def componentHidden(e: ComponentEvent): Unit = {}

