package ucesoft.smd.misc

import ucesoft.smd.Cart

import java.awt.BorderLayout
import javax.swing.{JPanel, JScrollPane, JTable}
import javax.swing.table.AbstractTableModel

/**
 * @author Alessandro Abbruzzetti
 *         Created on 08/03/2024 19:21  
 */
class CartInfoPanel(cart:Cart) extends JPanel:
  private class CartModel extends AbstractTableModel:
    override def getColumnName(column: Int): String = ""
    override def getColumnCount: Int = 2
    override def getRowCount: Int = 9
    override def getColumnClass(columnIndex: Int): Class[?] = classOf[String]
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      rowIndex match
        case 0 =>
          columnIndex match
            case 0 => "File name"
            case 1 => cart.file.originalFile
        case 1 =>
          columnIndex match
            case 0 => "Domestic name"
            case 1 => cart.getDomesticName
        case 2 =>
          columnIndex match
            case 0 => "Oversea name"
            case 1 => cart.getOverseaName
        case 3 =>
          columnIndex match
            case 0 => "System type"
            case 1 => cart.getSystemType.toString
        case 4 =>
          columnIndex match
            case 0 => "Regions"
            case 1 => cart.getRegionList.mkString(", ")
        case 5 =>
          columnIndex match
            case 0 => "Devices"
            case 1 => cart.getDeviceList.mkString(", ")
        case 6 =>
          columnIndex match
            case 0 => "CRC32"
            case 1 => cart.getCRC32
        case 7 =>
          columnIndex match
            case 0 => "Serial number"
            case 1 => cart.getSerialNumber
        case 8 =>
          columnIndex match
            case 0 => "Checksum"
            case 1 => if cart.isChecksumOK then "Ok" else "Wrong"
    
  init()

  private def init(): Unit =
    setLayout(new BorderLayout())
    val table = new JTable(new CartModel)
    val sp = new JScrollPane(table)
    add("Center",sp)
