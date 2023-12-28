package ucesoft.smd.ui

import java.awt.event.{ComponentAdapter, ComponentEvent, ComponentListener}
import java.awt.{Color, Font, Graphics}
import java.util.concurrent
import java.util.concurrent.{CountDownLatch, Executors, LinkedBlockingDeque}
import javax.swing.{ImageIcon, JFrame, JLabel, JPanel, SwingUtilities}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 13/12/2023 15:59  
 */
class MessageGlassPane(frame:JFrame) extends ComponentListener with Runnable with MessageBoard.MessageBoardListener:
  import MessageBoard.*

  private inline val FONT_WINDOW_WIDTH_RATIO = 30.0f
  private var xoff, yoff = 0
  private var msg : Message = _
  private var enabled = true
  private val queue = new LinkedBlockingDeque[Message]
  private val thread = new Thread(this,"MessageGlassThread")
  private val panelReadyWait = new CountDownLatch(1)
  private val messageTimerExecutor = Executors.newFixedThreadPool(1)
  private val font = new JLabel().getFont
  private val logoImage = new ImageIcon(getClass.getResource("/resources/sonic_ring.png")).getImage
  private var showLogo = false
  private val glassPane = new JPanel:
    setOpaque(false)
    override def paintComponent(g: Graphics): Unit =
      if showLogo then
        val width = logoImage.getWidth(null)
        val height = logoImage.getHeight(null)
        val size = getSize
        if width + xoff > size.width || height + yoff > size.height then
          g.drawImage(logoImage,xoff + 10,yoff + 10,size.width - xoff - 20,size.height - yoff - 20,null)
        else
          g.drawImage(logoImage,xoff + ((size.width - xoff) - width) / 2,yoff + ((size.height - yoff) - height) / 2,width,height,null)
      super.paintComponent(g)

  frame.addComponentListener(this)
  frame.getRootPane.addComponentListener(new ComponentAdapter:
    override def componentResized(e: ComponentEvent): Unit =
      if enabled then
        renderMessage()
  )
  thread.start()

  def run(): Unit =
    panelReadyWait.await()
    while true do
      msg = queue.take()
      msg.showLogo match
        case LOGO.SHOW =>
          showLogo = true
        case LOGO.HIDE =>
          showLogo = false
        case LOGO.IGNORE =>
      renderMessage(startTimer = true)


  override def addMessage(msg:Message): Unit =
    queue.offer(msg)

  private def initPane(): Unit =
    val insets = frame.getContentPane.getBounds
    frame.setGlassPane(glassPane)
    glassPane.setLayout(null)
    glassPane.setVisible(true)
    xoff = insets.x
    yoff = insets.y
    panelReadyWait.countDown()

  private def renderMessage(startTimer:Boolean = false): Unit =
    glassPane.removeAll()

    if msg != null then
      val fsize = frame.getSize
      val fontSize = fsize.width / FONT_WINDOW_WIDTH_RATIO
      val label = new JLabel(msg.text)
      label.setForeground(msg.color.getOrElse(Color.WHITE))
      var labelFont = msg.font match
        case Some(f) =>
          Font.decode(f)
        case None =>
          font
      labelFont = labelFont.deriveFont(fontSize)
      if msg.bold then
        labelFont = labelFont.deriveFont(Font.BOLD)
      if msg.italic then
        labelFont = labelFont.deriveFont(Font.ITALIC)
      label.setFont(labelFont)
      val fm = label.getFontMetrics(label.getFont)
      glassPane.add(label)

      var y = msg.ypos match
        case YPOS.TOP =>
          0
        case YPOS.BOTTOM =>
          fsize.height - fm.getHeight - yoff - fm.getDescent
        case YPOS.CENTER =>
          (fsize.height - fm.getHeight) / 2
      y += msg.yoffset * fm.getHeight
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
        Thread.sleep(msg.millis)
        val fadingMillis = msg.fadingMillis.getOrElse(0)
        if fadingMillis > 0 then
          val sleep = fadingMillis / 256
          var color = label.getForeground
          var c = 255
          while c >= 0 do
            color = new Color(color.getRed, color.getGreen, color.getBlue, c)
            label.setForeground(color)
            Thread.sleep(sleep)
            c -= 1
        panelReadyWait.countDown()
        msg = null
        SwingUtilities.invokeAndWait(() => {
          glassPane.removeAll()
          glassPane.repaint()
        })
  def setGlassPaneEnabled(enabled:Boolean): Unit =
    this.enabled = enabled
    frame.getGlassPane.asInstanceOf[JPanel].setVisible(enabled)

  override def componentResized(e: ComponentEvent): Unit = {}
  override def componentMoved(e: ComponentEvent): Unit = {}
  override def componentShown(e: ComponentEvent): Unit =
    initPane()
    frame.removeComponentListener(this)
  override def componentHidden(e: ComponentEvent): Unit = {}

