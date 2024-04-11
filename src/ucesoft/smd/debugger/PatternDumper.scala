package ucesoft.smd.debugger

import ucesoft.smd.Palette
import ucesoft.smd.ui.JCustomTooltip

import java.awt.*
import java.awt.event.{MouseEvent, MouseMotionAdapter, WindowAdapter, WindowEvent}
import java.awt.image.BufferedImage
import javax.swing.*
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer}
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 4/11/2023 18:52
 */
class PatternDumper(vram:Array[Int],
                    cram:Array[Int],
                    override val title:String,
                    override val frame:JFrame,
                    override val windowCloseOperation: () => Unit) extends RefreshableDialog(frame,title,windowCloseOperation):
  private inline val MEM_SIZE = 0x10000
  private inline val CHARS_PER_LINE = 32
  private inline val YCELLS = MEM_SIZE / (CHARS_PER_LINE * 32)

  private val addressLabel = new JLabel("0000")

  private class PatternCellRenderer extends DefaultTableCellRenderer:
    import java.awt.Font

    private val fontSize = getFont.getSize
    private val font = Font.decode(s"monospaced-$fontSize")
    private val fontHighlighted = Font.decode(s"monospaced-bold-$fontSize")
    setHorizontalAlignment(SwingConstants.CENTER)

    override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
      val colorIndex = value.toString.toInt
      val colorAddress = (palette << 5) + (colorIndex << 1)
      val color = cram(colorAddress) << 8 | cram(colorAddress + 1)
      val c = super.getTableCellRendererComponent(table, value, false, hasFocus, row, column)
      setBackground(new Color(Palette.getColor(color)))
      c

  private class PatternTableModel extends AbstractTableModel:
    private val columns = (0 to 7).map(_.toString).toArray
    private var mem = 0

    override def getColumnName(column: Int): String = columns(column)
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = 8
    override def getColumnClass(columnIndex: Int): Class[?] = classOf[String]

    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      val address = mem + (rowIndex << 2) + (columnIndex >> 1)
      val bit = if (columnIndex & 1) == 0 then vram(address) >> 4 else vram(address) & 0xF
      bit.toString

    def update(x:Int,y:Int): Unit =
      mem = (y * CHARS_PER_LINE + x) << 5
      addressLabel.setText("%04X".format(mem))
      fireTableDataChanged()

  private class PatternCanvas extends JLabel:
    private var image : BufferedImage = uninitialized
    private val mousePoint = new Point()
    private var screenPoint : Point = uninitialized
    private val patternModel = new PatternTableModel
    private val table = new JTable(patternModel)
    private val panelTip = new JPanel(new BorderLayout())

    init()

    override def createToolTip(): JToolTip =
      new JCustomTooltip(this,panelTip)

    private def init(): Unit =
      val sp = new JScrollPane(table)
      setToolTipText("")
      table.setDefaultRenderer(classOf[String], new PatternCellRenderer)
      table.getTableHeader.setReorderingAllowed(false)
      val colModel = table.getColumnModel
      for c <- 0 to 7 do
        colModel.getColumn(c).setMinWidth(20)
        colModel.getColumn(c).setMaxWidth(20)

      table.setPreferredScrollableViewportSize(table.getPreferredSize)
      val panel = new JPanel(new BorderLayout())
      panel.add("Center",sp)
      val addressPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      addressPanel.add(new JLabel("Address:",SwingConstants.RIGHT))
      addressPanel.add(addressLabel)
      panel.add("North",addressPanel)
      panelTip.add("Center",panel)

    addMouseMotionListener(new MouseMotionAdapter {
      override def mouseMoved(e: MouseEvent): Unit =
        mousePoint.x = e.getX
        mousePoint.y = e.getY
        screenPoint = e.getLocationOnScreen
        setToolTipText("")
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
        patternModel.update(cellX,cellY)

  private var zoom = 1
  private val canvas = new PatternCanvas
  private var palette = 0

  init()

  override def init(): Unit =
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

    val sp = new JScrollPane(canvas)

    mainPanel.add("North",buttonPanel)
    mainPanel.add("Center",sp)

    updateModel()
    dialog.getContentPane.add("Center",mainPanel)
    dialog.pack()


  override protected def updateModel(): Unit =
    val layer = new BufferedImage(CHARS_PER_LINE << 3,YCELLS << 3,BufferedImage.TYPE_INT_RGB)
    for cy <- 0 until YCELLS do
      for cx <- 0 until CHARS_PER_LINE do
        val mem = (cy * CHARS_PER_LINE + cx) << 5
        for y <- 0 to 7 do
          var px = 0
          for x <- 0 to 3 do
            var twoBits = vram(mem + (y << 2) + x)
            for _ <- 0 to 1 do
              val colorIndex = (twoBits >> 4) & 0xF
              val colorAddress = (palette << 5) + (colorIndex << 1)
              val color = cram(colorAddress) << 8 | cram(colorAddress + 1)
              twoBits <<= 4
              layer.setRGB((cx << 3) + px,(cy << 3) + y,Palette.getColor(color))
              px += 1
    canvas.setImage(layer)


