package ucesoft.smd.debugger

import ucesoft.smd.{Palette, VDP}

import java.awt.event.{MouseEvent, MouseMotionAdapter, WindowAdapter, WindowEvent}
import java.awt.image.BufferedImage
import java.awt.*
import javax.swing.*

/**
 * @author Alessandro Abbruzzetti
 *         Created on 26/10/2023 18:52  
 */
class LayerDumper(vram:Array[Int],
                  cram:Array[Int],
                  override val title:String,
                  vdp:VDP,
                  override val frame:JFrame,
                  override val windowCloseOperation: () => Unit) extends RefreshableDialog(frame,title,windowCloseOperation):
  private inline val LAYER_A = 0
  private inline val LAYER_B = 1
  private inline val LAYER_W = 2

  private val layerNames = Array("Layer A","Layer B","Layer Window")

  private class LayerCanvas(layer:Int) extends JLabel:
    private var image : BufferedImage = _
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

  private var zoom = 1
  private val layerLabels = Array(new LayerCanvas(LAYER_A),new LayerCanvas(LAYER_B),new LayerCanvas(LAYER_W))
  private var palette = 0
  private val tabbedPane = new JTabbedPane()

  init()

  override protected def init(): Unit =
    super.init()

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

    val buttonPanel = makeRefreshButtonPanel(new JLabel("Zoom:", SwingConstants.RIGHT),zoomCombo,new JLabel("Palette:", SwingConstants.RIGHT),paletteCombo)

    for l <- LAYER_A to LAYER_W do
      val sp = new JScrollPane(layerLabels(l))
      tabbedPane.addTab(layerNames(l),sp)

    mainPanel.add("North",buttonPanel)
    mainPanel.add("Center",tabbedPane)

    updateModel()
    dialog.getContentPane.add("Center",mainPanel)
    dialog.pack()


  override protected def updateModel(): Unit =
    for l <- LAYER_A to LAYER_W do
      updateImage(l)

  private def updateImage(layerIndex:Int): Unit =
    val layerAddress = layerIndex match
      case LAYER_A => vdp.getPatternAAddress
      case LAYER_B => vdp.getPatternBAddress
      case LAYER_W => vdp.getPatternWindowAddress

    val (cellWidth,cellHeight) = vdp.getPlayfieldSize
    /*
      layerIndex match
      case LAYER_W => vdp.getScreenCells
      case _ => vdp.getPlayfieldSize

     */

    tabbedPane.setTitleAt(layerIndex,s"${layerNames(layerIndex)} ${cellWidth}x$cellHeight")


    val layer = new BufferedImage(cellWidth << 3,cellHeight << 3,BufferedImage.TYPE_INT_RGB)
    for y <- 0 until cellHeight do
      for x <- 0 until cellWidth do
        val address = layerAddress + (y * cellWidth + x) * 2
        val patternInfo = vram(address) << 8 | vram(address + 1)
        val hflip = (patternInfo & 0x800) > 0
        val vflip = (patternInfo & 0x1000) > 0
        val pattern = patternInfo & 0x3FF
        val patternAddress = pattern << 5
        for py <- 0 to 7 do
          var px = 0
          val Y = if vflip then 7 - py else py
          for bx <- 0 to 3 do
            val X = if hflip then 3 - bx else bx
            var twoBits = vram(patternAddress + Y * 4 + X)
            if hflip then
              twoBits = (twoBits & 0xF) << 4 | (twoBits >> 4) & 0xF
            for c <- 0 to 1 do
              val colorIndex = (twoBits >> 4) & 0xF
              val colorAddress = (palette << 5) + (colorIndex << 1)
              val color = cram(colorAddress) << 8 | cram(colorAddress + 1)
              layer.setRGB(x * 8 + px,y * 8 + py,Palette.getColor(color))
              px += 1
              twoBits <<= 4

    layerLabels(layerIndex).setImage(layer)


