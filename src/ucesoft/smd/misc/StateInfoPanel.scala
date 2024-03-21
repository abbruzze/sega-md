package ucesoft.smd.misc

import ucesoft.smd.{Cart, MegaDrive}
import ucesoft.smd.cheat.Cheat
import ucesoft.smd.cheat.similarity.Cosine
import ucesoft.smd.ui.JCustomTooltip

import java.awt.event.MouseEvent
import java.awt.{BorderLayout, GridLayout}
import javax.swing.table.AbstractTableModel
import javax.swing.{BorderFactory, ImageIcon, JLabel, JPanel, JScrollPane, JTable}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 08/03/2024 19:21  
 */
class StateInfoPanel(info:MegaDrive.StateInfo) extends JPanel:
  private val formatter = java.time.format.DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss")
  private class CartModel extends AbstractTableModel:
    override def getColumnName(column: Int): String = ""
    override def getColumnCount: Int = 2
    override def getRowCount: Int = 6
    override def getColumnClass(columnIndex: Int): Class[?] = classOf[String]
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      rowIndex match
        case 0 =>
          columnIndex match
            case 0 => "Timestamp"
            case 1 => info.timestamp.format(formatter)
        case 1 =>
          columnIndex match
            case 0 => "Version"
            case 1 => info.version
        case 2 =>
          columnIndex match
            case 0 => "Build date"
            case 1 => info.buildDate.format(formatter)
        case 3 =>
          columnIndex match
            case 0 => "Cart file name"
            case 1 => info.cartInfo.fileName
        case 4 =>
          columnIndex match
            case 0 => "Cart CRC"
            case 1 => info.cartInfo.crc32.toUpperCase()
        case 5 =>
          columnIndex match
            case 0 => "Model"
            case 1 => s"${info.model.modelType}, ${info.model.videoType} version ${info.model.versionNumber}"
    
  init()

  private def init(): Unit =
    setLayout(new GridLayout(1,2))
    val table = new JTable(new CartModel):
      override def getToolTipText(e:MouseEvent): String =
        val p = e.getPoint
        val rowIndex = rowAtPoint(p)
        val colIndex = columnAtPoint(p)
        getValueAt(rowIndex, colIndex).toString
    table.setPreferredScrollableViewportSize(table.getPreferredSize)
    val colModel = table.getColumnModel
    colModel.getColumn(0).setMinWidth(100)
    colModel.getColumn(0).setMaxWidth(120)
    colModel.getColumn(1).setMinWidth(400)
    colModel.getColumn(1).setMaxWidth(400)
    val sp = new JScrollPane(table)
    add(sp)

    val snap = new JLabel(new ImageIcon(info.snap))
    val snapPanel = new JPanel()
    snapPanel.add(snap)
    snapPanel.setBorder(BorderFactory.createTitledBorder("Preview"))
    add(snapPanel)
