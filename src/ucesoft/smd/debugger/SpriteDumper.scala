package ucesoft.smd.debugger

import ucesoft.smd.VDP

import java.awt.BorderLayout
import javax.swing.{JFrame, JPanel, JScrollPane, JTable}
import javax.swing.table.AbstractTableModel
import scala.collection.mutable.ListBuffer

/**
 * @author Alessandro Abbruzzetti
 *         Created on 02/12/2023 20:30  
 */
class SpriteDumper(vdp: VDP,
                   override val title: String,
                   override val frame: JFrame,
                   override val windowCloseOperation: () => Unit) extends RefreshableDialog(frame,title,windowCloseOperation):

  private class SpriteTableModel extends AbstractTableModel:
    private val columns = Array("Index", "X", "Y", "Width", "Height", "Gfx", "Hf", "Vf", "Palette", "Priority")
    private var sprites: Array[VDP.VDPSpriteCacheDump] = Array()

    override def getColumnName(column: Int): String = columns(column)

    override def getColumnCount: Int = columns.length

    override def getRowCount: Int = sprites.length

    override def getColumnClass(columnIndex: Int): Class[_] =
      columnIndex match
        case 6 | 7 | 9 => classOf[java.lang.Boolean]
        case _ => classOf[String]

    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      columnIndex match
        case 0 =>
          "%02X".format(sprites(rowIndex).spriteIndex)
        case 1 =>
          "%04X".format(sprites(rowIndex).x)
        case 2 =>
          "%04X".format(sprites(rowIndex).y)
        case 3 =>
          sprites(rowIndex).w.toString
        case 4 =>
          sprites(rowIndex).h.toString
        case 5 =>
          "%03X".format(sprites(rowIndex).gfx)
        case 6 =>
          java.lang.Boolean.valueOf(sprites(rowIndex).hf)
        case 7 =>
          java.lang.Boolean.valueOf(sprites(rowIndex).vf)
        case 8 =>
          sprites(rowIndex).palette.toString
        case 9 =>
          java.lang.Boolean.valueOf(sprites(rowIndex).priority)

    def update(): Unit =
      var spriteDump = vdp.getSpritesDump()
      val buffer = new ListBuffer[VDP.VDPSpriteCacheDump]
      buffer += spriteDump
      while spriteDump.next.isDefined do
        spriteDump = spriteDump.next.get
        buffer += spriteDump
      sprites = buffer.toArray
      fireTableDataChanged()
  end SpriteTableModel

  private val model = new SpriteTableModel

  init()

  override protected def updateModel(): Unit =
    model.update()

  override def init(): Unit =
    super.init()
    val table = new JTable(model)

    val sp = new JScrollPane(table)
    table.getTableHeader.setReorderingAllowed(false)
    //table.setPreferredScrollableViewportSize(table.getPreferredSize)

    val mainPanel = new JPanel(new BorderLayout())
    val buttonPanel = makeRefreshButtonPanel()

    mainPanel.add("North", buttonPanel)
    mainPanel.add("Center", sp)

    updateModel()
    dialog.getContentPane.add("Center", mainPanel)
    dialog.pack()