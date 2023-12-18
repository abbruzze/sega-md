package ucesoft.smd.debugger

import ucesoft.smd.VDP
import ucesoft.smd.cpu.m68k.*
import ucesoft.smd.cpu.z80.Z80

import java.awt.event.{FocusEvent, FocusListener, MouseAdapter, MouseEvent}
import java.awt.{BorderLayout, Color, Component, FlowLayout}
import javax.swing.*
import javax.swing.border.EmptyBorder
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer, TableCellRenderer}
import scala.collection.mutable.ArrayBuffer

/**
 * @author Alessandro Abbruzzetti
 *         Created on 22/11/2023 19:46  
 */
object DebuggerUI {
  private case class Reg(value:Int,modified:Boolean)

  class RegisterRenderer(format: String) extends DefaultTableCellRenderer:
    private val defaultForegroundColor = getForeground
    private val modifiedColor = Color.RED

    override def setValue(value: Any): Unit =
      value match
        case Reg(value, modified) =>
          setHorizontalAlignment(SwingConstants.CENTER)
          setText(format.format(value))
          setForeground(if modified then modifiedColor else defaultForegroundColor)
  end RegisterRenderer

  class Z80RegisterTableModel(ctx:Z80.Context) extends AbstractTableModel:
    private val columns = Array("AF","BC","DE","HL","IX","IY","I","IM","PC","SP")
    private val values = Array.ofDim[Int](columns.length)
    private val modified = Array.ofDim[Boolean](columns.length)

    override def getColumnName(column: Int): String = columns(column)
    override def isCellEditable(row: Int, col: Int): Boolean = false
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = 1
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = Reg(values(columnIndex), modified(columnIndex))
    override def getColumnClass(columnIndex: Int): Class[_] = classOf[String]

    def contentUpdated(): Unit =
      for c <- columns.indices do
        val reg = c match
          case 0 => ctx.AF
          case 1 => ctx.BC
          case 2 => ctx.DE
          case 3 => ctx.HL
          case 4 => ctx.IX
          case 5 => ctx.IY
          case 6 => ctx.I
          case 7 => ctx.im
          case 8 => ctx.PC
          case 9 => ctx.SP
        modified(c) = reg != values(c)
        values(c) = reg

      fireTableDataChanged()
  end Z80RegisterTableModel

  class M68KRegisterTableModel(m68k: M6800X0, data: Boolean) extends AbstractTableModel:
    private val values = Array.ofDim[Int](8)
    private val modified = Array.ofDim[Boolean](8)

    override def getColumnName(column: Int): String = s"${if data then "D" else "A"}$column"
    override def isCellEditable(row: Int, col: Int): Boolean = false
    override def getColumnCount: Int = 8
    override def getRowCount: Int = 1
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = Reg(values(columnIndex), modified(columnIndex))
    override def getColumnClass(columnIndex: Int): Class[_] = classOf[String]
    def contentUpdated(): Unit =
      val rtype = if data then RegisterType.Data else RegisterType.Address
      for r <- 0 to 7 do
        val reg = m68k.getRegister(rtype, r).get()
        modified(r) = reg != values(r)
        values(r) = reg
      fireTableDataChanged()
  end M68KRegisterTableModel

  class StatusRegisterRenderer extends JCheckBox with TableCellRenderer:
    private val noFocusBorder = new EmptyBorder(1, 1, 1, 1)
    setHorizontalAlignment(SwingConstants.CENTER)
    setBorderPainted(true)

    override def getTableCellRendererComponent(table: JTable, _value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
      val (value, modified) = _value match
        case Reg(v, m) => (v, m)
        case _ => (0, false)

      if isSelected then
        setForeground(table.getSelectionForeground)
        super.setBackground(table.getSelectionBackground)
      else
        setForeground(table.getForeground)
        if modified then
          setBackground(Color.RED)
        else
          setBackground(table.getBackground)

      setSelected(value == 1)

      if hasFocus then
        setBorder(UIManager.getBorder("Table.focusCellHighlightBorder"))
      else
        setBorder(noFocusBorder)

      this
  end StatusRegisterRenderer

  class Z80StatusRegisterTableModel(ctx:Z80.Context) extends AbstractTableModel:
    private val columns = Array("S", "Z", "Y", "H", "X", "P", "N", "C")
    private val values = Array.ofDim[Boolean](8)
    private val modified = Array.ofDim[Boolean](8)

    override def getColumnName(column: Int): String = columns(column)
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = 1
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      Reg(if values(columnIndex) then 1 else 0, modified(columnIndex))
    override def getColumnClass(columnIndex: Int): Class[_] = classOf[java.lang.Boolean]

    def contentUpdated(): Unit =
      for c <- columns.indices do
        val value = c match
          case 0 => ctx.sign != 0
          case 1 => ctx.zero != 0
          case 2 => ctx.yf != 0
          case 3 => ctx.half != 0
          case 4 => ctx.xf != 0
          case 5 => ctx.parity != 0
          case 6 => ctx.negative != 0
          case 7 => ctx.carry != 0
        modified(c) = value != values(c)
        values(c) = value

      fireTableDataChanged()
  end Z80StatusRegisterTableModel

  class M68KStatusRegisterTableModel(m68k: M6800X0) extends AbstractTableModel:
    private val columns = Array("T", "S", "Int", "X", "N", "Z", "V", "C")
    private val values = Array.ofDim[Boolean](8)
    private var intValue = 0
    private val modified = Array.ofDim[Boolean](8)

    override def getColumnName(column: Int): String = columns(column)
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = 1
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      columnIndex match
        case 2 =>
          Reg(intValue, modified(columnIndex))
        case _ =>
          Reg(if values(columnIndex) then 1 else 0, modified(columnIndex))
    override def getColumnClass(columnIndex: Int): Class[_] =
      columnIndex match
        case 2 =>
          classOf[String]
        case _ =>
          classOf[java.lang.Boolean]

    def contentUpdated(): Unit =
      import StatusRegister.StatusFlag.*
      val sr = m68k.getRegister(RegisterType.SR).asInstanceOf[StatusRegister]
      for c <- Seq(0, 1, 3, 4, 5, 6, 7) do
        val value = c match
          case 0 => java.lang.Boolean.valueOf(sr.isTrace)
          case 1 => java.lang.Boolean.valueOf(sr.isSupervisorMode)
          case 3 => java.lang.Boolean.valueOf(sr.isFlag(X))
          case 4 => java.lang.Boolean.valueOf(sr.isFlag(N))
          case 5 => java.lang.Boolean.valueOf(sr.isFlag(Z))
          case 6 => java.lang.Boolean.valueOf(sr.isFlag(V))
          case 7 => java.lang.Boolean.valueOf(sr.isFlag(C))
        modified(c) = value != this.values(c)
        this.values(c) = value
      val intV = sr.getInterruptMask
      modified(2) = intV != intValue
      intValue = intV

      fireTableDataChanged()
  end M68KStatusRegisterTableModel

  class M68KPCTableModel(m68k: M6800X0) extends AbstractTableModel:
    private val columns = Array("PC", "USP", "SSP", "Total cycles", "Last instr. cycles")
    private val values = Array.ofDim[Int](5)
    private val modified = Array.ofDim[Boolean](5)

    override def getColumnName(column: Int): String = columns(column)
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = 1
    override def getColumnClass(columnIndex: Int): Class[_] =
      columnIndex match
        case 3 | 4 => classOf[java.lang.Integer]
        case _ => classOf[String]
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = Reg(values(columnIndex), modified(columnIndex))

    def contentUpdated(): Unit =
      for c <- 0 to 4 do
        val value = c match
          case 0 => m68k.getLastInstructionPC
          case 1 => m68k.getRegister(RegisterType.USP).get()
          case 2 => m68k.getRegister(RegisterType.SSP).get()
          case 3 => m68k.getTotalElapsedCycles.toInt
          case 4 => m68k.getLastInstructionElapsedCycles
        modified(c) = value != values(c)
        values(c) = value

      fireTableDataChanged()
  end M68KPCTableModel

  class DisassembledCellRenderer extends DefaultTableCellRenderer:
    import java.awt.Font

    private val fontSize = getFont.getSize
    private val font = Font.decode(s"monospaced-italic-$fontSize")
    private val fontSel = Font.decode(s"monospaced-bold-$fontSize")
    setHorizontalAlignment(SwingConstants.LEFT)

    override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
      val c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
      if column == 4 && value.toString.nonEmpty then
        setToolTipText(value.toString)
      else
        setToolTipText(null)

      if isSelected then
        c.setFont(fontSel)
      else
        c.setFont(font)
      c
  end DisassembledCellRenderer

  class DisassembledTableModel(m68k: M6800X0,
                               m68kMemory:Memory,
                               z80:Z80,
                               addressBreakHandler: Int => Option[String],
                               annotator: Disassemble68KAnnotator,
                               noteEditable: Boolean = false) extends AbstractTableModel:
    private case class DisInfo(numAddress: Int, address: String, opcodes: String, mnemonic: String, var notes: String, disString: String)

    private val columns = Array("Brk", "Address", "Opcodes", "Mnemonic", "Note")
    private val rows = new ArrayBuffer[DisInfo]

    override def getColumnName(column: Int): String = columns(column)
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = rows.size
    override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = noteEditable && columnIndex == 4
    override def getColumnClass(columnIndex: Int): Class[_] = classOf[String]
    override def setValueAt(aValue: Any, rowIndex: Int, columnIndex: Int): Unit =
      rows(rowIndex).notes = aValue.toString
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      val dis = rows(rowIndex)
      columnIndex match
        case 0 => addressBreakHandler(dis.numAddress) match
          case Some(brk) => brk
          case None => ""
        case 1 => dis.address
        case 2 => dis.opcodes
        case 3 => dis.mnemonic
        case 4 => dis.notes

    def clear(): Unit =
      rows.clear()
      fireTableDataChanged()

    def add(d: DisassembledInstruction, busNotAvailable: Boolean): Unit =
      val dis = DisInfo(
        numAddress = d.address,
        address = "%08X".format(d.address),
        opcodes = (if d.isValidOpCode then d.opcode :: d.extendedWords else d.extendedWords).map(a => "%04X".format(a)).mkString(" "),
        mnemonic = if d.isValidOpCode then
          s"${d.mnemonic} ${
            d.op1 match
              case None => ""
              case Some(o1) =>
                d.op2 match
                  case None => o1
                  case Some(o2) => s"$o1,$o2"
          }"
        else "DW",
        notes = s"${if busNotAvailable then "Bus N/A" else ""}${annotator.getNoteFor(d, m68k, m68kMemory)}",
        disString = d.toString
      )
      rows += dis

    def add(d: Z80.DisassembledInfo): Unit =
      rows += DisInfo(d.address, "%04X".format(d.address), d.opcodes.map(o => "%02X".format(o)).mkString(" "), d.mnemonic, "", "")

    def update(): Unit = fireTableDataChanged()

    def getAddressAt(row: Int): Int = Integer.parseInt(rows(row).address, 16)

    def copyToClipboard(): Unit =
      val sb = new StringBuilder()
      for r <- rows do
        sb.append(s"${r.disString}\n")

      java.awt.Toolkit.getDefaultToolkit.getSystemClipboard.setContents(java.awt.datatransfer.StringSelection(sb.toString), null)

  end DisassembledTableModel

  class PropertiesCellRenderer(model: VDPPropertiesTableModel) extends DefaultTableCellRenderer:
    setHorizontalAlignment(SwingConstants.LEFT)

    override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
      super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
      if column == 1 then
        val tooltip = model.getPropertyDesc(row)
        if tooltip.isEmpty then setToolTipText(null) else setToolTipText(tooltip)
      else
        setToolTipText(null)
      this

  class VDPPropertiesTableModel(vdp:VDP, frame:JFrame) extends AbstractTableModel:
    private val columns = Array("Property", "Value", "Details")
    private var properties: VDP.VDPPropertiesDump = vdp.getProperties

    def getPropertyDesc(row: Int): String = properties.properties(row).description
    override def getColumnName(column: Int): String = columns(column)
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = properties.properties.length
    override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean =
      columnIndex == 1 && properties.properties(rowIndex).register.isDefined

    override def getColumnClass(columnIndex: Int): Class[_] = classOf[String]
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      columnIndex match
        case 0 =>
          properties.properties(rowIndex).name
        case 1 =>
          properties.properties(rowIndex).value
        case 2 =>
          properties.properties(rowIndex).valueDetails
    override def setValueAt(aValue: Any, rowIndex: Int, columnIndex: Int): Unit =
      properties.properties(rowIndex).register match
        case Some(r) =>
          try
            val byte = Integer.parseInt(aValue.toString, 16)
            if byte < 0 || byte > 0xFF then
              throw new IllegalArgumentException()
            properties.registerWriter(r, byte)
            update()
          catch
            case _: NumberFormatException =>
              JOptionPane.showMessageDialog(frame, s"Invalid hex byte: $aValue", "Type mismatch", JOptionPane.ERROR_MESSAGE)
            case _: IllegalArgumentException =>
              JOptionPane.showMessageDialog(frame, s"Invalid hex byte: $aValue. Must be between 00 and FF", "Value too large", JOptionPane.ERROR_MESSAGE)
        case None =>

    def update(): Unit =
      properties = vdp.getProperties
      fireTableDataChanged()
  end VDPPropertiesTableModel

  trait DisassemblerBreakHandler:
    def hasBreakAt(address:Int): Boolean
    def addExecuteBreakAt(address:Int): Unit
    def removeBreakAt(address:Int): Unit
    def getBreakStringAt(address:Int): Option[String]

  class DisassemblerPanel(name:String,
                          m68k: M6800X0, // set to null to use z80 instead
                          z80: Z80,
                          frame:JFrame,
                          disassemblerBreakHandler: DisassemblerBreakHandler,
                          override val windowCloseOperation: () => Unit) extends RefreshableDialog(frame, s"$name Disassembler", windowCloseOperation):
    private val model = new DisassembledTableModel(m68k,null,z80,disassemblerBreakHandler.getBreakStringAt,EmptyAnnotator, true)

    init()

    private def initTable(table: JTable, model: DisassembledTableModel): Unit =
      table.getTableHeader.setReorderingAllowed(false)
      table.setDefaultRenderer(classOf[String], new DisassembledCellRenderer)
      val colModel = table.getColumnModel
      colModel.getColumn(0).setMinWidth(25)
      colModel.getColumn(0).setMaxWidth(30)
      colModel.getColumn(1).setMinWidth(70)
      colModel.getColumn(1).setMaxWidth(80)
      colModel.getColumn(2).setMinWidth(130)
      colModel.getColumn(2).setMaxWidth(180)
      colModel.getColumn(3).setMinWidth(160)
      colModel.getColumn(3).setMaxWidth(200)
      table.addMouseListener(new MouseAdapter {
        override def mouseClicked(e: MouseEvent): Unit =
          if e.getClickCount == 2 then
            val row = table.rowAtPoint(e.getPoint)
            val address = model.getAddressAt(row)
            if disassemblerBreakHandler.hasBreakAt(address) then
              disassemblerBreakHandler.removeBreakAt(address)
            else
              disassemblerBreakHandler.addExecuteBreakAt(address)
            model.update()
      })

    private def disassemble(fromS: String, toS: String): Unit =
      try
        val from = java.lang.Integer.parseInt(fromS, 16) & 0xFF_FFFF
        val to = java.lang.Integer.parseInt(toS, 16) & 0xFF_FFFF
        if to < from then
          throw new IllegalArgumentException
        model.clear()
        new Thread(() => {
          var a = from
          while a <= to do
            if m68k != null then
              val dis = m68k.disassemble(a)
              a += dis.size
              model.add(dis, false)
            else
              val dis = z80.getDisassembledInfo(a)
              a += dis.size
              model.add(dis)
          model.update()
        }).start()
      catch
        case _: Exception =>
          JOptionPane.showMessageDialog(dialog, s"Invalid range of addresses", "Address error", JOptionPane.ERROR_MESSAGE)

    override protected def init(): Unit =
      super.init()
      val mainPanel = new JPanel(new BorderLayout())
      dialog.getContentPane.add("Center", mainPanel)
      val buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      val copyClip = new JButton(new ImageIcon(getClass.getResource("/resources/trace/copy.png")))
      val disButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/bug.png")))
      val fromTF = new JTextField("000000", 10)
      val toTF = new JTextField("000000", 10)

      copyClip.addActionListener(_ => model.copyToClipboard())
      copyClip.setToolTipText("Copy to clipboard")
      buttonPanel.add(copyClip)
      buttonPanel.add(new JLabel("From:", SwingConstants.RIGHT))
      buttonPanel.add(fromTF)
      buttonPanel.add(new JLabel("To:", SwingConstants.RIGHT))
      buttonPanel.add(toTF)
      fromTF.addFocusListener(new FocusListener:
        override def focusGained(e: FocusEvent): Unit = {}

        override def focusLost(e: FocusEvent): Unit =
          try
            val from = java.lang.Integer.parseInt(fromTF.getText, 16)
            val to = (from + 1024) & 0xFF_FFFF
            toTF.setText(to.toHexString)
          catch
            case _: NumberFormatException =>
      )
      disButton.addActionListener(_ => disassemble(fromTF.getText, toTF.getText))
      buttonPanel.add(disButton)

      val disPanel = new JPanel(new BorderLayout())
      disPanel.add("North", buttonPanel)
      val disTable = new JTable(model)
      val sp = new JScrollPane(disTable)
      sp.setBorder(BorderFactory.createTitledBorder(s"$name Disassembler"))
      initTable(disTable, model)
      disPanel.add("Center", sp)

      mainPanel.add("Center", disPanel)

      dialog.pack()
  end DisassemblerPanel

  class DMAEventTableModel extends AbstractTableModel:
    private val columns = Array("#","DMA Type", "Memory Type", "Source address","Dest address","Length","Fill value")
    private val events = new collection.mutable.ArrayBuffer[VDP.DMAEvent]

    final def addEvent(event:VDP.DMAEvent): Unit =
      events += event
      fireTableDataChanged()

    final def clearEvents(): Unit =
      events.clear()
      fireTableDataChanged()

    override def getColumnName(column: Int): String = columns(column)
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = events.size
    override def getColumnClass(columnIndex: Int): Class[_] = classOf[String]
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      columnIndex match
        case 0 =>
          "%05d".format(rowIndex + 1)
        case 1 => events(rowIndex).dmaType.toString
        case 2 => events(rowIndex).memoryType.toString
        case 3 => "%06X".format(events(rowIndex).sourceAddress)
        case 4 => "%06X".format(events(rowIndex).destinationAddress)
        case 5 => "%X".format(events(rowIndex).length)
        case 6 =>
          events(rowIndex).fillValue match
            case None =>
              "-"
            case Some(v) =>
              "%02X".format(v)

  class DMAEventPanel(frame:JFrame,
                      vdp:VDP,
                      override val windowCloseOperation: () => Unit) extends RefreshableDialog(frame, "DMA event trace", windowCloseOperation) with VDP.DMAEventListener:
    private val model = new DMAEventTableModel

    init()

    override def onDMAEvent(event: VDP.DMAEvent): Unit =
      model.addEvent(event)

    override protected def init(): Unit =
      super.init()
      val mainPanel = new JPanel(new BorderLayout())
      dialog.getContentPane.add("Center", mainPanel)
      val northPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      val enableCB = new JCheckBox("Enabled")
      northPanel.add(enableCB)
      enableCB.addActionListener(_ => if enableCB.isSelected then vdp.setDMAEventListener(this) else vdp.setDMAEventListener(null))
      val southPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      val clearLog = new JButton(new ImageIcon(getClass.getResource("/resources/trace/clear.png")))
      clearLog.addActionListener(_ => model.clearEvents())
      southPanel.add(clearLog)

      val table = new JTable(model)
      table.getTableHeader.setReorderingAllowed(false)
      val sp = new JScrollPane(table)
      mainPanel.add("North",northPanel)
      mainPanel.add("Center",sp)
      mainPanel.add("South",southPanel)

      dialog.pack()
}
