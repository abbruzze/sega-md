package ucesoft.smd.debugger

import org.fife.ui.rsyntaxtextarea.{RSyntaxTextArea, SyntaxConstants}
import org.fife.ui.rtextarea.RTextScrollPane
import ucesoft.smd.{Cart, Logger, VDP}
import ucesoft.smd.cpu.m68k.*

import java.awt.event.{FocusEvent, FocusListener, MouseAdapter, MouseEvent}
import java.awt.{BorderLayout, Color, Component, Dimension, FlowLayout, GridLayout}
import java.util.concurrent.{Executors, Semaphore}
import javax.swing.*
import javax.swing.border.EmptyBorder
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer, TableCellRenderer}
import javax.swing.text.DefaultCaret
import scala.collection.mutable.ArrayBuffer

/**
 * @author Alessandro Abbruzzetti
 *         Created on 15/10/2023 14:44
 */
class M68kDebugger(m68k:M6800X0,memory:Memory,m68kRAM:Array[Int],vdp:VDP,annotator:Disassemble68KAnnotator) extends AbstractDebugger:
  private case class Reg(value:Int,modified:Boolean)

  private class RegisterRenderer(format:String) extends DefaultTableCellRenderer:
    private val defaultForegroundColor = getForeground
    private val modifiedColor = Color.RED

    override def setValue(value: Any): Unit =
      value match
        case Reg(value,modified) =>
          setHorizontalAlignment(SwingConstants.CENTER)
          setText(format.format(value))
          setForeground(if modified then modifiedColor else defaultForegroundColor)
  end RegisterRenderer

  private class RegisterTableModel(data:Boolean) extends AbstractTableModel:
    private val values = Array.ofDim[Int](8)
    private val modified = Array.ofDim[Boolean](8)

    override def getColumnName(column: Int): String = s"${if data then "D" else "A"}$column"
    override def isCellEditable(row: Int,col: Int): Boolean = false
    override def getColumnCount: Int = 8
    override def getRowCount: Int = 1
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = Reg(values(columnIndex),modified(columnIndex))
    override def getColumnClass(columnIndex: Int): Class[_] = classOf[String]
    def contentUpdated(): Unit =
      val rtype = if data then RegisterType.Data else RegisterType.Address
      for r <- 0 to 7 do
        val reg = m68k.getRegister(rtype,r).get()
        modified(r) = reg != values(r)
        values(r) = reg
      fireTableDataChanged()

  private class StatusRegisterRenderer extends JCheckBox with TableCellRenderer:
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

  private class StatusRegisterTableModel extends AbstractTableModel:
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
      for c <- Seq(0,1,3,4,5,6,7) do
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

  private class PCTableModel extends AbstractTableModel:
    private val columns = Array("PC", "USP", "SSP", "Total cycles","Last instr. cycles")
    private val values = Array.ofDim[Int](5)
    private val modified = Array.ofDim[Boolean](5)

    override def getColumnName(column: Int): String = columns(column)
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = 1
    override def getColumnClass(columnIndex: Int): Class[_] =
      columnIndex match
        case 3|4 => classOf[java.lang.Integer]
        case _ => classOf[String]
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = Reg(values(columnIndex),modified(columnIndex))
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

  private class DisassembledCellRenderer extends DefaultTableCellRenderer:
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

  private class DisassembledTableModel(annotator:Disassemble68KAnnotator,noteEditable:Boolean = false) extends AbstractTableModel:
    private case class DisInfo(numAddress:Int,address:String,opcodes:String,mnemonic:String,var notes:String,disString:String)
    private val columns = Array("Brk","Address","Opcodes","Mnemonic","Note")
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
        case 0 => addressBreaks.get(dis.numAddress) match
          case Some(brk) => brk.breakType.toString.substring(0,1)
          case None => ""
        case 1 => dis.address
        case 2 => dis.opcodes
        case 3 => dis.mnemonic
        case 4 => dis.notes

    def clear(): Unit =
      rows.clear()
      fireTableDataChanged()

    def add(d:DisassembledInstruction,busNotAvailable:Boolean): Unit =
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
        notes = s"${if busNotAvailable then "Bus N/A" else ""}${annotator.getNoteFor(d,m68k,memory)}",
        disString = d.toString
      )
      rows += dis

    def update(): Unit = fireTableDataChanged()
    def getAddressAt(row:Int): Int = Integer.parseInt(rows(row).address,16)

    def copyToClipboard(): Unit =
      val sb = new StringBuilder()
      for r <- rows do
        sb.append(s"${r.disString}\n")

      java.awt.Toolkit.getDefaultToolkit.getSystemClipboard.setContents(java.awt.datatransfer.StringSelection(sb.toString),null)

  private class PropertiesCellRenderer(model:PropertiesTableModel) extends DefaultTableCellRenderer:
    setHorizontalAlignment(SwingConstants.LEFT)

    override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
      super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
      if column == 1 then
        val tooltip = model.getPropertyDesc(row)
        if tooltip.isEmpty then setToolTipText(null) else setToolTipText(tooltip)
      else
        setToolTipText(null)
      this

  private class PropertiesTableModel extends AbstractTableModel:
    private val columns = Array("Property", "Value","Details")
    private var properties: VDP.VDPPropertiesDump = vdp.getProperties()

    def getPropertyDesc(row:Int): String = properties.properties(row).description

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
            properties.registerWriter(r,byte)
            update()
          catch
            case _: NumberFormatException =>
              JOptionPane.showMessageDialog(frame, s"Invalid hex byte: $aValue", "Type mismatch", JOptionPane.ERROR_MESSAGE)
            case _: IllegalArgumentException =>
              JOptionPane.showMessageDialog(frame, s"Invalid hex byte: $aValue. Must be between 00 and FF", "Value too large", JOptionPane.ERROR_MESSAGE)
        case None =>

    def update(): Unit =
      properties = vdp.getProperties()
      fireTableDataChanged()

  private class DisassemblerPanel(override val windowCloseOperation: () => Unit) extends RefreshableDialog(frame,"Disassembler",windowCloseOperation):
    private val model = new DisassembledTableModel(EmptyAnnotator,true)

    init()

    private def initTable(table:JTable,model:DisassembledTableModel): Unit =
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
            addressBreaks.get(address) match
              case Some(_) =>
                addressBreaks -= address
              case None =>
                addressBreaks += address -> AddressBreak(BreakType.EXECUTE, address)
            model.update()
      })

    private def disassemble(fromS:String,toS:String): Unit =
      try
        val from = java.lang.Integer.parseInt(fromS,16) & 0xFF_FFFF
        val to = java.lang.Integer.parseInt(toS,16) & 0xFF_FFFF
        if to < from then
          throw new IllegalArgumentException
        model.clear()
        new Thread(() => {
          var a = from
          while a <= to do
            val dis = m68k.disassemble(a)
            a += dis.size
            model.add(dis,false)
          model.update()
        }).start()
      catch
        case _:Exception =>
          JOptionPane.showMessageDialog(dialog, s"Invalid range of addresses", "Address error", JOptionPane.ERROR_MESSAGE)

    override protected def init(): Unit =
      val mainPanel = new JPanel(new BorderLayout())
      dialog.getContentPane.add("Center",mainPanel)
      val buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      val copyClip = new JButton(new ImageIcon(getClass.getResource("/resources/trace/copy.png")))
      val disButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/bug.png")))
      val fromTF = new JTextField("000000",10)
      val toTF = new JTextField("000000",10)

      copyClip.addActionListener(_ => model.copyToClipboard())
      copyClip.setToolTipText("Copy to clipboard")
      buttonPanel.add(copyClip)
      buttonPanel.add(new JLabel("From:",SwingConstants.RIGHT))
      buttonPanel.add(fromTF)
      buttonPanel.add(new JLabel("To:", SwingConstants.RIGHT))
      buttonPanel.add(toTF)
      fromTF.addFocusListener(new FocusListener:
        override def focusGained(e: FocusEvent): Unit = {}
        override def focusLost(e: FocusEvent): Unit =
          try
            val from = java.lang.Integer.parseInt(fromTF.getText,16)
            val to = (from + 1024) & 0xFF_FFFF
            toTF.setText(to.toHexString)
          catch
            case _:NumberFormatException =>
      )
      disButton.addActionListener(_ => disassemble(fromTF.getText,toTF.getText))
      buttonPanel.add(disButton)

      val disPanel = new JPanel(new BorderLayout())
      disPanel.add("North",buttonPanel)
      val disTable = new JTable(model)
      val sp = new JScrollPane(disTable)
      sp.setBorder(BorderFactory.createTitledBorder("Disassembler"))
      initTable(disTable,model)
      disPanel.add("Center",sp)

      mainPanel.add("Center",disPanel)

      dialog.pack()

  // =============================================================================================

  private enum StepState:
    case NoStep, WaitReturn, WaitTarget

  private var stepOverPending, stepOutPending = StepState.NoStep
  private var stepOverOutStopPending = false
  private var stepOverTargetAddress = 0
  private var stepInstruction : Instruction = _
  private var stepDisassemble : DisassembledInstruction = _
  private val dataRegisterTableModel = new RegisterTableModel(data = true)
  private val addressRegisterTableModel = new RegisterTableModel(data = false)
  private val statusRegisterTableModel = new StatusRegisterTableModel
  private val pcRegisterTableModel = new PCTableModel
  private val disassembledTableModel = new DisassembledTableModel(annotator)
  private val vdpTableMode = new PropertiesTableModel
  private val distable = new JTable(disassembledTableModel)
  private val semaphore = new Semaphore(0)
  private val frame = new JFrame()
  private val logPanel = new RSyntaxTextArea(10,100)
  private val onOffButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/on.png")))

  private val vdpMemDump = vdp.getMemoryDump
  private val vramMemoryDumpItem = new JCheckBoxMenuItem("VDP's VRAM")
  private val cramMemoryDumpItem = new JCheckBoxMenuItem("VDP's CRAM")
  private val vsramMemoryDumpItem = new JCheckBoxMenuItem("VDP's VSRAM")
  private val m68kramMemoryDumpItem = new JCheckBoxMenuItem("68k's RAM")
  private val layerDumpItem = new JCheckBoxMenuItem("Pattern Layers")
  private val patternADumpItem = new JCheckBoxMenuItem("Pattern Dump")
  private val vdpVRAMDialog = new MemoryDumper(vdpMemDump.ram,0,"VRAM",frame,() => vramMemoryDumpItem.setSelected(false),setPreferredScrollableViewportSize = false, showASCII = true).dialog
  private val vdpVSRAMDialog = new MemoryDumper(vdpMemDump.vsram,0,"VSRAM",frame,() => vsramMemoryDumpItem.setSelected(false),setPreferredScrollableViewportSize = false, showASCII = true).dialog
  private val vdpCRAMDialog = new MemoryDumper(vdpMemDump.cram,0,"CRAM",frame,() => cramMemoryDumpItem.setSelected(false),withColorDumper = true).dialog
  private val patternLayersDialog = new LayerDumper(vdpMemDump.ram,vdpMemDump.cram,"Pattern Layers",vdp,frame,() => layerDumpItem.setSelected(false)).dialog
  private val m68KramDialog = new MemoryDumper(m68kRAM,0xFF0000,"68K RAM",frame,() => m68kramMemoryDumpItem.setSelected(false),setPreferredScrollableViewportSize = false, showASCII = true).dialog
  private val romDumpItem = new JCheckBoxMenuItem("Cart's ROM")
  private val patternDialog = new PatternDumper(vdpMemDump.ram,vdpMemDump.cram,"Pattern Dump",frame,() => patternADumpItem.setSelected(false)).dialog
  private var romDialog : JDialog = _
  private val disassemblerItem = new JCheckBoxMenuItem("Disassembler")
  private val disassemblerPanel = new DisassemblerPanel(() => disassemblerItem.setSelected(false))
  private val disassemblerDialog = disassemblerPanel.dialog

  private var cart : Cart = _
  private val romDisaUpdater = Executors.newSingleThreadExecutor()

  def setCart(cart:Cart): Unit =
    this.cart = cart
    if cart == null then
      romDumpItem.setEnabled(false)
      if romDialog != null then
        romDialog.dispose()
    else
      romDumpItem.setEnabled(true)
      romDialog = new MemoryDumper(cart.getROM,0,"ROM",frame,() => romDumpItem.setSelected(false),canUpdate = false,setPreferredScrollableViewportSize = false, showASCII = true).dialog
      romDisaUpdater.submit(new Runnable:
        override def run(): Unit = {}
          // TODO
          //disassemblerPanel.updateROM()
      )

  swing {
    init()
  }

  private def swing(f: => Unit) : Unit =
    if !SwingUtilities.isEventDispatchThread then
      SwingUtilities.invokeAndWait(() => f)
    else f

  private def init(): Unit =
    romDumpItem.setEnabled(false)
    val mainPanel = new JPanel(new BorderLayout())
    val northPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val rightPanel = new JPanel(new BorderLayout())
    mainPanel.add("North",northPanel)

    val toolBar = new JToolBar("Tracer")
    toolBar.setRollover(true)
    northPanel.add(toolBar)

    // buttons
    onOffButton.setToolTipText("Enable tracing")
    onOffButton.addActionListener(_ => enableTracing(onOffButton.isSelected))

    val stepInButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/down.png")))
    stepInButton.setToolTipText("Step in")
    stepInButton.addActionListener(_ => stepIn() )
    val stepOverButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/down_left.png")))
    stepOverButton.addActionListener(_ => stepOver())
    stepOverButton.setToolTipText("Step over")
    val stepOutButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/up.png")))
    stepOutButton.addActionListener(_ => stepOut())
    stepOutButton.setToolTipText("Step out")

    val disaButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/bug.png")))
    disaButton.addActionListener(_ => disassembleGUI())
    disaButton.setToolTipText("Disassemble")

    val readButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/read.png")))
    readButton.addActionListener(_ => readGUI())
    readButton.setToolTipText("Read memory")

    val writeButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/write.png")))
    writeButton.addActionListener(_ => writeGUI())
    writeButton.setToolTipText("Fill memory")

    val clearButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/clear.png")))
    clearButton.addActionListener(_ => disassembledTableModel.clear())
    clearButton.setToolTipText("Clear trace panel")

    toolBar.add(onOffButton)
    toolBar.add(stepInButton)
    toolBar.add(stepOverButton)
    toolBar.add(stepOutButton)
    toolBar.add(disaButton)
    toolBar.add(readButton)
    toolBar.add(writeButton)
    toolBar.add(clearButton)
    // registers
    val registerPanel = new JPanel(new GridLayout(0,1))
    // sr
    val srtable = new JTable(statusRegisterTableModel)
    srtable.getTableHeader.setReorderingAllowed(false)
    srtable.setDefaultRenderer(classOf[java.lang.Boolean], new StatusRegisterRenderer)
    srtable.setDefaultRenderer(classOf[String], new RegisterRenderer("%01X"))
    var sp = new JScrollPane(srtable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
    srtable.setPreferredScrollableViewportSize(srtable.getPreferredSize)
    sp.setBorder(BorderFactory.createTitledBorder("Status register"))
    registerPanel.add(sp)
    // data
    val datatable = new JTable(dataRegisterTableModel)
    datatable.getTableHeader.setReorderingAllowed(false)
    datatable.setDefaultRenderer(classOf[String], RegisterRenderer("%08X"))
    sp = new JScrollPane(datatable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
    datatable.setPreferredScrollableViewportSize(datatable.getPreferredSize)
    sp.setBorder(BorderFactory.createTitledBorder("Data registers"))
    registerPanel.add(sp)
    // address
    val adrtable = new JTable(addressRegisterTableModel)
    adrtable.getTableHeader.setReorderingAllowed(false)
    adrtable.setDefaultRenderer(classOf[String], new RegisterRenderer("%08X"))
    sp = new JScrollPane(adrtable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
    adrtable.setPreferredScrollableViewportSize(adrtable.getPreferredSize)
    sp.setBorder(BorderFactory.createTitledBorder("Address registers"))
    registerPanel.add(sp)
    // misc.
    val pctable = new JTable(pcRegisterTableModel)
    pctable.getTableHeader.setReorderingAllowed(false)
    pctable.setDefaultRenderer(classOf[String], new RegisterRenderer("%08X"))
    pctable.setDefaultRenderer(classOf[java.lang.Integer], new RegisterRenderer("%s"))
    sp = new JScrollPane(pctable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
    pctable.setPreferredScrollableViewportSize(pctable.getPreferredSize)
    sp.setBorder(BorderFactory.createTitledBorder("Misc."))
    registerPanel.add(sp)
    // vdp
    val vdptable = new JTable(vdpTableMode)
    vdptable.getTableHeader.setReorderingAllowed(false)
    vdptable.setDefaultRenderer(classOf[String], new PropertiesCellRenderer(vdpTableMode))
    sp = new JScrollPane(vdptable)
    sp.setBorder(BorderFactory.createTitledBorder("VDP"))
    vdptable.setPreferredScrollableViewportSize(new Dimension(0,200))
    val vdpColModel = vdptable.getColumnModel
    vdpColModel.getColumn(0).setMinWidth(80)
    vdpColModel.getColumn(0).setMaxWidth(150)
    vdpColModel.getColumn(1).setMinWidth(50)
    vdpColModel.getColumn(1).setMaxWidth(50)

    rightPanel.add("Center",sp)
    rightPanel.add("North",registerPanel)
    // log panel
    logPanel.setEditable(false)
    logPanel.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_NONE)
    logPanel.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE)
    val lscroll = new RTextScrollPane(logPanel)
    lscroll.setMinimumSize(new Dimension(0, 70))
    lscroll.setBorder(BorderFactory.createTitledBorder("Log panel"))


    val logButtonPanel = new JPanel(new BorderLayout())
    logButtonPanel.add("Center", lscroll)
    val logToolBar = new JToolBar()
    logButtonPanel.add("South", logToolBar)
    val clearLog = new JButton(new ImageIcon(getClass.getResource("/resources/trace/clear.png")))
    logToolBar.add(clearLog)
    val logSeverityGroup = new ButtonGroup
    val logSeverityInfoButton = new JRadioButton("Info")
    val logSeveritySevereButton = new JRadioButton("Severe")
    logSeverityGroup.add(logSeverityInfoButton)
    logSeverityGroup.add(logSeveritySevereButton)
    val logSeverityPanel = new JPanel()
    logSeverityPanel.setBorder(BorderFactory.createTitledBorder("Log severity"))
    logSeverityPanel.add(logSeverityInfoButton)
    logSeverityPanel.add(logSeveritySevereButton)
    logToolBar.add(logSeverityPanel)
    logSeverityInfoButton.setSelected(true)
    logSeverityInfoButton.addActionListener(_ => Logger.getLogger.setLevel(java.util.logging.Level.INFO))
    logSeveritySevereButton.addActionListener(_ => Logger.getLogger.setLevel(java.util.logging.Level.SEVERE))
    clearLog.addActionListener(_ => logPanel.setText(""))
    // disassemble panel
    distable.getTableHeader.setReorderingAllowed(false)
    distable.setDefaultRenderer(classOf[String], new DisassembledCellRenderer)
    sp = new JScrollPane(distable)
    sp.setBorder(BorderFactory.createTitledBorder("Disassembler"))
    val colModel = distable.getColumnModel
    colModel.getColumn(0).setMinWidth(25)
    colModel.getColumn(0).setMaxWidth(30)
    colModel.getColumn(1).setMinWidth(70)
    colModel.getColumn(1).setMaxWidth(80)
    colModel.getColumn(2).setMinWidth(130)
    colModel.getColumn(2).setMaxWidth(180)
    colModel.getColumn(3).setMinWidth(160)
    colModel.getColumn(3).setMaxWidth(200)
    distable.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit =
        if e.getClickCount == 2 then
          val row = distable.rowAtPoint(e.getPoint)
          val address = disassembledTableModel.getAddressAt(row)
          addressBreaks.get(address) match
            case Some(_) =>
              addressBreaks -= address
            case None =>
              addressBreaks += address -> AddressBreak(BreakType.EXECUTE,address)
          disassembledTableModel.update()
    })

    var splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,sp,rightPanel)
    splitPane.setContinuousLayout(true)
    splitPane.setOneTouchExpandable(true)
    mainPanel.add("Center",splitPane)
    splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, mainPanel, logButtonPanel)
    splitPane.setOneTouchExpandable(true)

    // menu
    val menu = new JMenuBar
    val memoryMenu = new JMenu("Memory")
    val layerMenu = new JMenu("Pattern")
    val disMenu = new JMenu("Disassembler")

    vramMemoryDumpItem.addActionListener(_ => vdpVRAMDialog.setVisible(vramMemoryDumpItem.isSelected) )
    cramMemoryDumpItem.addActionListener(_ => vdpCRAMDialog.setVisible(cramMemoryDumpItem.isSelected) )
    vsramMemoryDumpItem.addActionListener(_ => vdpVSRAMDialog.setVisible(vsramMemoryDumpItem.isSelected) )
    m68kramMemoryDumpItem.addActionListener(_ => m68KramDialog.setVisible(m68kramMemoryDumpItem.isSelected) )
    romDumpItem.addActionListener(_ => romDialog.setVisible(romDumpItem.isSelected) )
    memoryMenu.add(vramMemoryDumpItem)
    memoryMenu.add(cramMemoryDumpItem)
    memoryMenu.add(vsramMemoryDumpItem)
    memoryMenu.add(romDumpItem)
    memoryMenu.add(m68kramMemoryDumpItem)
    menu.add(memoryMenu)

    layerDumpItem.addActionListener(_ => patternLayersDialog.setVisible(layerDumpItem.isSelected) )
    patternADumpItem.addActionListener(_ => patternDialog.setVisible(patternADumpItem.isSelected) )
    layerMenu.add(layerDumpItem)
    layerMenu.add(patternADumpItem)
    menu.add(layerMenu)

    disassemblerItem.addActionListener(_ => disassemblerDialog.setVisible(disassemblerItem.isSelected) )
    disMenu.add(disassemblerItem)

    menu.add(disMenu)

    vdpCRAMDialog.setResizable(false)

    frame.setJMenuBar(menu)

    frame.getContentPane.add("Center",splitPane)
    frame.pack()
  end init

  private def updateModels(): Unit = swing {
    dataRegisterTableModel.contentUpdated()
    addressRegisterTableModel.contentUpdated()
    statusRegisterTableModel.contentUpdated()
    pcRegisterTableModel.contentUpdated()
    disassembledTableModel.update()
    vdpTableMode.update()
    distable.setRowSelectionInterval(0,0)
  }

  def enableTracing(enabled: Boolean): Unit =
    setStepByStep(enabled)

    if enabled then
      onOffButton.setToolTipText("Disable tracing")
    else
      onOffButton.setToolTipText("Enable tracing")
      setStepAlways(false)
      setStepByStep(false)

    onOffButton.setSelected(enabled)
    stepOutPending = StepState.NoStep
    stepOverPending = StepState.NoStep

  private def nextStep(): Unit =
    semaphore.release()

  private def stepIn(): Unit =
    nextStep()

  private def stepOver(): Unit =
    import InstructionType.*
    import StepState.*
    stepInstruction.instructionType match
      case JSR | TRAP | TRAPV | ILLEGAL =>
        stepOverPending = WaitReturn
        setStepAlways(true)
      case DBRA|DBCC|DBCS|DBEQ|DBGE|DBGT|DBHI|DBLE|DBLS|DBMI|DBNE|DBPL|DBVC|DBVS =>
        stepOverTargetAddress = stepDisassemble.address + stepDisassemble.size
        stepOverPending = WaitTarget
        setStepAlways(true)
      case _ =>

    nextStep()

  private def stepOut(): Unit =
    import InstructionType.*
    import StepState.*
    stepInstruction.instructionType match
      case RTR|RTE|RTS =>
        stepOutPending = NoStep
      case _ =>
        stepOutPending = WaitReturn
        setStepAlways(true)
    nextStep()

  private def checkStepOverOut(instruction: Instruction,address:Int): Unit =
    import InstructionType.*
    import StepState.*

    stepOverPending match
      case NoStep =>
      case WaitReturn =>
        instruction.instructionType match
          case RTR|RTE|RTS =>
            stepOverOutStopPending = true
          case _ =>
      case WaitTarget =>
        if address == stepOverTargetAddress then
          stepOutPending = StepState.NoStep
          setStepAlways(false)
    stepOutPending match
      case NoStep =>
      case WaitReturn =>
        instruction.instructionType match
          case RTR|RTE|RTS =>
            stepOverOutStopPending = true
          case _ =>
      case _ =>


  private def disassembleGUI(): Unit = ???
  private def readGUI(): Unit = ???
  private def writeGUI(): Unit = ???

  // abstract debugger
  override def breakEpilogue(cpu: M6800X0): Unit = semaphore.acquire()

  override def onStepByStepChange(stepByStepEnabled: Boolean): Unit =
    if stepByStepEnabled then
      m68k.addEventListener(this)
      frame.setVisible(true)
    else
      nextStep()
      if !existsBreakPending then
        m68k.removeEventListener(this)

  override protected def onFetch(cpu: M6800X0, address: Int, opcode: Int, i: Instruction, busNotAvailable: Boolean, wasBreak: Boolean): Unit =
    if stepOverOutStopPending then
      stepOverOutStopPending = false
      stepOutPending = StepState.NoStep
      setStepAlways(false)

    stepInstruction = i
    checkStepOverOut(i,address)

    if wasBreak then
      val break = addressBreaks(address)
      log(break.toString)
      stepOutPending = StepState.NoStep
      stepOverPending = StepState.NoStep

    if !isStepAlways then
      onOffButton.setSelected(true)
      disassembledTableModel.clear()
      var adr = address
      for a <- 1 to 25 do
        val dis = cpu.disassemble(adr)
        if a == 1 then
          stepDisassemble = dis
        disassembledTableModel.add(dis, busNotAvailable)
        adr += dis.size

      updateModels()
      breakEpilogue(cpu)

  def log(msg: String): Unit = swing {
    logPanel.append(msg)
    logPanel.append("\n")
  }



