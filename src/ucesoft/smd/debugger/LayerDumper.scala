package ucesoft.smd.debugger

import ucesoft.smd.{Palette, VDP}

import java.awt.{BorderLayout, Color, Dimension, FlowLayout, Graphics, Point, Rectangle}
import java.awt.event.{MouseEvent, MouseMotionAdapter}
import java.awt.image.BufferedImage
import javax.swing.*
import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 26/10/2023 18:52  
 */
class LayerDumper(vram:Array[Int],
                  cram:Array[Int],
                  override val title:String,
                  vdp:VDP,
                  override val frame:JFrame,
                  override val windowCloseOperation: () => Unit,
                  activeListener: Boolean => Unit) extends RefreshableDialog(frame,title,windowCloseOperation) with VDP.VDPNewFrameListener with Runnable:
  private inline val LAYER_A = 0
  private inline val LAYER_B = 1
  private inline val LAYER_W = 2
  private inline val LAYER_S = 3

  private val layerNames = Array("Layer A","Layer B","Layer Window","Sprite")

  private case class Sprite(id:String,rec:Rectangle,gfx:String)

  private class LayerCanvas(layer:Int) extends JLabel:
    private var image : BufferedImage = uninitialized
    private val mousePoint = new Point()

    addMouseMotionListener(new MouseMotionAdapter {
      override def mouseMoved(e: MouseEvent): Unit =
        mousePoint.x = e.getX
        mousePoint.y = e.getY
        repaint()
    })
    def setImage(image:BufferedImage): Unit =
      this.image = image
      setPreferredSize(new Dimension(image.getWidth * zoom,image.getHeight * zoom))
      invalidate()
      repaint()

    override def paintComponent(g:Graphics): Unit =
      g.drawImage(image,0,0,image.getWidth * zoom,image.getHeight * zoom,getBackground,null)
      if layer != LAYER_S then
        val cellX = mousePoint.x / (8 * zoom)
        val cellY = mousePoint.y / (8 * zoom)
        if mousePoint.x < image.getWidth * zoom && mousePoint.y < image.getHeight * zoom then
          g.setColor(Color.WHITE)
          g.drawRect(cellX * 8 * zoom, cellY * 8 * zoom, 7 * zoom, 7 * zoom)
          val layerAddress = layer match
            case LAYER_A => vdp.getPatternAAddress
            case LAYER_B => vdp.getPatternBAddress
            case LAYER_W => vdp.getPatternWindowAddress
          val (cellWidth,cellHeight) = vdp.getPlayfieldSize
          val address = layerAddress + (cellY * cellWidth + cellX) * 2
          setToolTipText(s"($cellX,$cellY,${address.toHexString.toUpperCase()})")
        else
          setToolTipText(null)
      else
        var sprite = spriteList
        var found = false
        while !found && sprite != Nil do
          val s = sprite.head
          if s.rec.contains(mousePoint) then
            found = true
            setToolTipText(s"#${s.id} gfx=${s.gfx}")
          else
            sprite = sprite.tail

        if !found then
          setToolTipText(null)

  private var zoom = 1
  private val layerLabels = Array(new LayerCanvas(LAYER_A),new LayerCanvas(LAYER_B),new LayerCanvas(LAYER_W),new LayerCanvas(LAYER_S))
  private var palette = 0
  private val tabbedPane = new JTabbedPane()
  private val layerAddressLabel = new JLabel("000000")
  private val layerAddress = Array(0,0,0,0)
  private var spriteList : List[Sprite] = Nil
  private val thread = new Thread(this,"LayerDumper")
  private val lock = new Object
  private var frameCount = 0
  private var frameCountLimit = 15

  init()

  override def windowActive(on: Boolean): Unit =
    super.windowActive(on)
    activeListener(on)

  override def onNewFrame(): Unit =
    frameCount += 1
    if frameCount >= frameCountLimit then
      frameCount = 0
      lock.synchronized {
        lock.notify()
      }

  override def run(): Unit =
    while true do
      lock.synchronized {
        lock.wait()
      }
      updateModel()

  override protected def init(): Unit =
    super.init()
    thread.start()

    val mainPanel = new JPanel(new BorderLayout())
    val paletteCombo = new JComboBox[String](Array("Palette 0","Palette 1","Palette 2","Palette 3"))
    val zoomCombo = new JComboBox[String](Array("x1","x2","x4"))

    zoomCombo.addActionListener(_ => {
      zoom = 1 << zoomCombo.getSelectedIndex
      updateModel()
    })

    paletteCombo.addActionListener(_ => {
      palette = paletteCombo.getSelectedIndex
      updateModel()
    })

    val buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    buttonPanel.add(new JLabel("Zoom:", SwingConstants.RIGHT))
    buttonPanel.add(zoomCombo)
    buttonPanel.add(new JLabel("Palette:", SwingConstants.RIGHT))
    buttonPanel.add(paletteCombo)
    buttonPanel.add(new JLabel("Address:",SwingConstants.RIGHT))
    buttonPanel.add(layerAddressLabel)
    val spinner = new JSpinner(new SpinnerNumberModel(frameCountLimit,1,20,1))
    buttonPanel.add(new JLabel("Update every frame number:", SwingConstants.RIGHT))
    buttonPanel.add(spinner)
    spinner.addChangeListener(_ => {
      frameCountLimit = spinner.getValue.asInstanceOf[Int]
      println(frameCountLimit)
    })

    for l <- LAYER_A to LAYER_S do
      val sp = new JScrollPane(layerLabels(l))
      tabbedPane.addTab(layerNames(l),sp)

    tabbedPane.addChangeListener(_ => {
      layerAddressLabel.setText("%04X".format(layerAddress(tabbedPane.getSelectedIndex)))
    })

    mainPanel.add("North",buttonPanel)
    mainPanel.add("Center",tabbedPane)

    updateModel()
    dialog.getContentPane.add("Center",mainPanel)
    dialog.pack()

  override protected def updateModel(): Unit =
    for l <- LAYER_A to LAYER_W do
      updateImage(l)
    updateSpriteImage()

  private def updateSpriteImage(): Unit =
    val spriteLayer = new BufferedImage(512,512, BufferedImage.TYPE_INT_RGB)
    var sprite = vdp.getSpritesDump
    val screenSize = vdp.getScreenCells

    val g = spriteLayer.getGraphics
    g.setFont(g.getFont.deriveFont(g.getFont.getSize * 0.8f))
    val fm = g.getFontMetrics
    g.setColor(new Color(50,150,7,100))
    g.fillRect(128,128,screenSize._1 * 8,screenSize._2 * 8)
    g.setColor(Color.WHITE)
    var goNext = true
    val spriteList = new ListBuffer[Sprite]
    while goNext do
      val spriteWidth = (sprite.w + 1) << 3
      val spriteHeight = (sprite.h + 1) << 3
      g.setColor(Color.WHITE)
      g.drawRect(sprite.x,sprite.y,spriteWidth,spriteHeight)
      val id = "%02X".format(sprite.spriteIndex)
      g.setColor(Color.YELLOW)
      g.drawString(id,sprite.x + (spriteWidth - fm.stringWidth(id)) / 2,sprite.y + spriteHeight / 2 + fm.getDescent)
      spriteList += Sprite(id,new Rectangle(sprite.x,sprite.y,spriteWidth,spriteHeight),"%X".format(sprite.gfx << 5))
      sprite.next match
        case Some(next) =>
          sprite = next
        case None =>
          goNext = false

    this.spriteList = spriteList.toList
    layerLabels(LAYER_S).setImage(spriteLayer)

  private def updateImage(layerIndex:Int): Unit =
    layerAddress(layerIndex) = layerIndex match
      case LAYER_A => vdp.getPatternAAddress
      case LAYER_B => vdp.getPatternBAddress
      case LAYER_W => vdp.getPatternWindowAddress

    val (cellWidth,cellHeight) = layerIndex match
      case LAYER_W =>
        val screenSize = vdp.getScreenCells
        if screenSize._1 == 32 then (32,32) else (64,32)
      case _ => vdp.getPlayfieldSize


    tabbedPane.setTitleAt(layerIndex,s"${layerNames(layerIndex)} ${cellWidth}x$cellHeight")

    val yshift = if vdp.isInterlaceMode then 4 else 3
    val ly = (1 << yshift) - 1
    val patternShift = if vdp.isInterlaceMode then 6 else 5
    val layer = new BufferedImage(cellWidth << 3,cellHeight << yshift,BufferedImage.TYPE_INT_RGB)
    for y <- 0 until cellHeight do
      for x <- 0 until cellWidth do
        val address = layerAddress(layerIndex) + ((y * cellWidth + x) << 1)
        val patternInfo = vram(address) << 8 | vram(address + 1)
        val hflip = (patternInfo & 0x800) > 0
        val vflip = (patternInfo & 0x1000) > 0
        val pattern = patternInfo & 0x3FF
        val patternAddress = pattern << patternShift
        for py <- 0 to ly do
          var px = 0
          val Y = if vflip then ly - py else py
          for bx <- 0 to 3 do
            val X = if hflip then 3 - bx else bx
            var twoBits = vram(patternAddress + (Y << 2) + X)
            if hflip then
              twoBits = (twoBits & 0xF) << 4 | (twoBits >> 4) & 0xF
            for c <- 0 to 1 do
              val colorIndex = (twoBits >> 4) & 0xF
              val colorAddress = (palette << 5) + (colorIndex << 1)
              val color = cram(colorAddress) << 8 | cram(colorAddress + 1)
              layer.setRGB((x << 3) + px,(y << yshift) + py,Palette.getColor(color))
              px += 1
              twoBits <<= 4

    layerLabels(layerIndex).setImage(layer)


