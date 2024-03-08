package ucesoft.smd.debugger

import ucesoft.smd.Palette

import java.awt.{BorderLayout, Component}
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer}
import javax.swing.{JPanel, JScrollPane, JTable}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 24/10/2023 19:49  
 */
class ColorDumper(cram:Array[Int]) extends JPanel:
  private class ColorRenderer extends DefaultTableCellRenderer:
    private val background = getBackground

    override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
      setToolTipText(null)
      if column == 0 then
        setBackground(background)
        super.getTableCellRendererComponent(table,s"Palette $value",isSelected,hasFocus,row,column)
      else
        val color = value.asInstanceOf[java.lang.Integer].intValue()
        val c = super.getTableCellRendererComponent(table,"",false,hasFocus,row,column)
        c.setBackground(java.awt.Color.getColor("",color))
        c

  private class ColorTableModel extends AbstractTableModel:
    private val columns = Array("Palette") ++ (0 to 0xF).map(_.toHexString.toUpperCase()).toArray

    override def getColumnName(column: Int): String = columns(column)
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = 4

    override def getColumnClass(columnIndex: Int): Class[?] = classOf[java.lang.Integer]

    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      columnIndex match
        case 0 =>
          java.lang.Integer.valueOf(rowIndex)
        case _ =>
          val address = (rowIndex << 5) + ((columnIndex - 1) << 1)
          val color = cram(address) << 8 | cram(address + 1)
          java.lang.Integer.valueOf(Palette.getColor(color))

    def update(): Unit =
      fireTableDataChanged()

  private val model = new ColorTableModel

  init()

  private def init(): Unit =
    setLayout(new BorderLayout())
    val table = new JTable(model)
    table.getTableHeader.setReorderingAllowed(false)
    table.setDefaultRenderer(classOf[java.lang.Integer],new ColorRenderer)
    val colModel = table.getColumnModel
    colModel.getColumn(0).setMinWidth(50)
    colModel.getColumn(0).setMaxWidth(60)
    for col <- 1 to 16 do
      colModel.getColumn(col).setMinWidth(40)
      colModel.getColumn(col).setMaxWidth(40)
    table.setPreferredScrollableViewportSize(table.getPreferredSize)
    val sp = new JScrollPane(table)
    add("Center",sp)

  def update(): Unit =
    model.update()
