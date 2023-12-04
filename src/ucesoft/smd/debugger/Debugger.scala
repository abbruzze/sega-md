package ucesoft.smd.debugger

import org.fife.ui.rsyntaxtextarea.{RSyntaxTextArea, SyntaxConstants}
import org.fife.ui.rtextarea.RTextScrollPane
import ucesoft.smd.cpu.m68k.*
import ucesoft.smd.cpu.z80.Z80
import ucesoft.smd.debugger.DebuggerUI.*
import ucesoft.smd.{Cart, Logger, VDP}

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.{BorderLayout, Dimension, FlowLayout, GridLayout}
import java.util.concurrent.Semaphore
import javax.swing.*
import javax.swing.text.DefaultCaret

/**
 * @author Alessandro Abbruzzetti
 *         Created on 15/10/2023 14:44
 */
class Debugger(m68k:M6800X0,
               m68kMemory:Memory,
               m68kRAM:Array[Int],
               z80:Z80,
               z80Memory:Z80.Memory,
               vdp:VDP,
               annotator:Disassemble68KAnnotator):

  private enum StepState:
    case NoStep, WaitReturn, WaitTarget

  private abstract class InternalDebugger extends JPanel:
    protected var stepOverPending, stepOutPending = StepState.NoStep
    protected var stepOverOutStopPending = false
    protected var stepOverTargetAddress = 0
    protected val semaphore = new Semaphore(0)
    protected val cpuEnabled = {
      val item = new JCheckBoxMenuItem("CPU enabled")
      item.setSelected(true)
      item.addActionListener(_ => onCPUEnabled(item.isSelected))
      item
    }

    protected def onCPUEnabled(enabled:Boolean): Unit = {}

    def enableTracing(enabled: Boolean): Unit
    def stepIn(): Unit
    def stepOver(): Unit
    def stepOut(): Unit
    def updateModels(): Unit
  end InternalDebugger

  private val frame = new JFrame()
  private var cart : Cart = _
  private val logPanel = new RSyntaxTextArea(10,100)
  private val onOffButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/on.png")))
  private val vdpMemDump = vdp.getMemoryDump

  private val vramMemoryDumpItem = new JCheckBoxMenuItem("VDP's VRAM")
  private val cramMemoryDumpItem = new JCheckBoxMenuItem("VDP's CRAM")
  private val vsramMemoryDumpItem = new JCheckBoxMenuItem("VDP's VSRAM")
  private val m68kramMemoryDumpItem = new JCheckBoxMenuItem("68k's RAM")
  private val layerDumpItem = new JCheckBoxMenuItem("Pattern Layers")
  private val patternADumpItem = new JCheckBoxMenuItem("Pattern Dump")
  private val vdpVRAMDialog = new MemoryDumper(vdpMemDump.ram, 0, "VRAM", frame, () => vramMemoryDumpItem.setSelected(false), setPreferredScrollableViewportSize = false, showASCII = true).dialog
  private val vdpVSRAMDialog = new MemoryDumper(vdpMemDump.vsram, 0, "VSRAM", frame, () => vsramMemoryDumpItem.setSelected(false), setPreferredScrollableViewportSize = false, showASCII = true).dialog
  private val vdpCRAMDialog = new MemoryDumper(vdpMemDump.cram, 0, "CRAM", frame, () => cramMemoryDumpItem.setSelected(false), withColorDumper = true).dialog
  private val patternLayersDialog = new LayerDumper(vdpMemDump.ram, vdpMemDump.cram, "Pattern Layers", vdp, frame, () => layerDumpItem.setSelected(false)).dialog
  private val m68KramDialog = new MemoryDumper(m68kRAM, 0xFF0000, "68K RAM", frame, () => m68kramMemoryDumpItem.setSelected(false), setPreferredScrollableViewportSize = false, showASCII = true).dialog
  private val romDumpItem = new JCheckBoxMenuItem("Cart's ROM")
  private val patternDialog = new PatternDumper(vdpMemDump.ram, vdpMemDump.cram, "Pattern Dump", frame, () => patternADumpItem.setSelected(false)).dialog
  private val spriteDumpItem = new JCheckBoxMenuItem("Sprite Cache")
  private val spriteDumpDialog = new SpriteDumper(vdp,"Sprite Cache",frame,() => spriteDumpItem.setSelected(false)).dialog
  private var romDialog: JDialog = _
  private val m68KDisassemblerItem = new JCheckBoxMenuItem("M68K Disassembler")
  private val z80DisassemblerItem = new JCheckBoxMenuItem("Z80 Disassembler")

  private val m68kDebugger = new M68KDebugger
  private val m68kDisassemblerPanel = new DisassemblerPanel("M68K",
    m68k,
    z80,
    frame,
    m68kDebugger,
    () => m68KDisassemblerItem.setSelected(false))
  private val m68kDisassemblerDialog = m68kDisassemblerPanel.dialog
  private val z80Debugger = new Z80Debugger
  private val z80DisassemblerPanel = new DisassemblerPanel("Z80",
    null,
    z80,
    frame,
    m68kDebugger,
    () => z80DisassemblerItem.setSelected(false))
  private val z80DisassemblerDialog = z80DisassemblerPanel.dialog

  private var selectedDebugger : InternalDebugger = m68kDebugger


  private val tabbedPane = new JTabbedPane()

  // =============================================================================================================
  private trait GenericDebugger extends DisassemblerBreakHandler:
    def nextStep(): Unit

  private class Z80Debugger extends InternalDebugger with GenericDebugger with Z80.EventListener:
    private val registerTableModel = new Z80RegisterTableModel(z80.ctx)
    private val statusTableModel = new Z80StatusRegisterTableModel(z80.ctx)
    private val disassembledTableModel = new DisassembledTableModel(null,
      null,
      z80,
      address => getBreakStringAt(address).map(_.substring(0, 1)),
      EmptyAnnotator)
    private val distable = new JTable(disassembledTableModel)
    private var stepByStep = false
    private var stepAlways = false
    private var stepDisassemble : Z80.DisassembledInfo = _

    init()

    private def existsBreakPending: Boolean = false

    override def nextStep(): Unit =
      semaphore.release()

    override def hasBreakAt(address: Int): Boolean = false

    override def addExecuteBreakAt(address: Int): Unit = {}

    override def removeBreakAt(address: Int): Unit = {}

    override def getBreakStringAt(address: Int): Option[String] = None

    override def rw(z80: Z80, address: Int, isRead: Boolean, value: Int = 0): Unit = {/* TODO */}
    override def fetch(z80: Z80, address: Int, opcode: Int): Unit =
      if !stepAlways then
        onOffButton.setSelected(true)
        disassembledTableModel.clear()
        var adr = address
        for a <- 1 to 25 do
          val dis = z80.getDisassembledInfo(adr)
          if a == 1 then
            stepDisassemble = dis
          disassembledTableModel.add(dis)
          adr += dis.size

        updateModels()
      semaphore.acquire()
    override def interrupted(z80: Z80, mode: Int, isNMI: Boolean): Unit = {/* TODO */}
    override def reset(z80: Z80): Unit = {/* TODO */}
    override def halt(z80: Z80, isHalted: Boolean): Unit = {/* TODO */}

    override def enableTracing(enabled: Boolean): Unit = {
      if stepByStep != enabled then
        if enabled then
          z80.addEventListener(this)
          frame.setVisible(true)
        else
          nextStep()
          if !existsBreakPending then
            z80.removeEventListener(this)
      stepByStep = enabled

      if !enabled then
        stepAlways = false
        stepByStep = false

      stepOutPending = StepState.NoStep
      stepOverPending = StepState.NoStep
    }
    override def stepIn(): Unit =
      nextStep()
    override def stepOver(): Unit = stepIn() // TODO
    override def stepOut(): Unit = stepIn() // TODO

    override def updateModels(): Unit = swing {
      registerTableModel.contentUpdated()
      statusTableModel.contentUpdated()
      disassembledTableModel.update()
      distable.setRowSelectionInterval(0, 0)
    }

    private def init(): Unit =
      setLayout(new BorderLayout())
      val northPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      northPanel.add(cpuEnabled)
      add("North",northPanel)
      val rightPanel = new JPanel(new BorderLayout())
      // registers
      val registerPanel = new JPanel(new GridLayout(0, 1))
      // sr
      val srtable = new JTable(statusTableModel)
      srtable.getTableHeader.setReorderingAllowed(false)
      srtable.setDefaultRenderer(classOf[java.lang.Boolean], new StatusRegisterRenderer)
      srtable.setDefaultRenderer(classOf[String], new RegisterRenderer("%01X"))
      var sp = new JScrollPane(srtable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      srtable.setPreferredScrollableViewportSize(srtable.getPreferredSize)
      sp.setBorder(BorderFactory.createTitledBorder("Status register"))
      registerPanel.add(sp)

      val datatable = new JTable(registerTableModel)
      datatable.getTableHeader.setReorderingAllowed(false)
      datatable.setDefaultRenderer(classOf[String], RegisterRenderer("%04X"))
      sp = new JScrollPane(datatable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      datatable.setPreferredScrollableViewportSize(datatable.getPreferredSize)
      sp.setBorder(BorderFactory.createTitledBorder("Registers"))
      registerPanel.add(sp)

      rightPanel.add("Center", sp)
      rightPanel.add("North", registerPanel)

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
            if hasBreakAt(address) then
              removeBreakAt(address)
            else
              addExecuteBreakAt(address)
            disassembledTableModel.update()
      })

      val splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, sp, rightPanel)
      splitPane.setContinuousLayout(true)
      splitPane.setOneTouchExpandable(true)
      add("Center", splitPane)

  end Z80Debugger

  private class M68KDebugger extends InternalDebugger with DisassemblerBreakHandler:
    private var stepInstruction : Instruction = _
    private var stepDisassemble : DisassembledInstruction = _
    private val dataRegisterTableModel = new M68KRegisterTableModel(m68k, data = true)
    private val addressRegisterTableModel = new M68KRegisterTableModel(m68k, data = false)
    private val statusRegisterTableModel = new M68KStatusRegisterTableModel(m68k)
    private val pcRegisterTableModel = new M68KPCTableModel(m68k)
    private val disassembledTableModel = new DisassembledTableModel(m68k,
      m68kMemory,
      null,
      address => getBreakStringAt(address).map(_.substring(0,1)),
      annotator)
    private val vdpTableMode = new VDPPropertiesTableModel(vdp,frame)
    private val distable = new JTable(disassembledTableModel)

    override def hasBreakAt(address: Int): Boolean = debugger.hasBreakAt(address)
    override def addExecuteBreakAt(address: Int): Unit = debugger.addExecuteBreakAt(address)
    override def removeBreakAt(address: Int): Unit = debugger.removeBreakAt(address)
    override def getBreakStringAt(address: Int): Option[String] = debugger.getBreakStringAt(address)

    private val debugger = new AbstractDebugger with GenericDebugger {
      override def hasBreakAt(address: Int): Boolean = m68kAddressBreaks.contains(address)
      override def addExecuteBreakAt(address: Int): Unit = m68kAddressBreaks += address -> AddressBreak(BreakType.EXECUTE, address)
      override def removeBreakAt(address: Int): Unit = m68kAddressBreaks -= address
      override def getBreakStringAt(address: Int): Option[String] = m68kAddressBreaks.get(address).map(_.toString.substring(0, 1))

      override def nextStep(): Unit =
        semaphore.release()

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
        checkStepOverOut(i, address)

        if wasBreak then
          val break = m68kAddressBreaks(address)
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

      private def checkStepOverOut(instruction: Instruction, address: Int): Unit =
        import InstructionType.*
        import StepState.*

        stepOverPending match
          case NoStep =>
          case WaitReturn =>
            instruction.instructionType match
              case RTR | RTE | RTS =>
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
              case RTR | RTE | RTS =>
                stepOverOutStopPending = true
              case _ =>
          case _ =>
    }

    init()

    private def init(): Unit =
      setLayout(new BorderLayout())
      val northPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      northPanel.add(cpuEnabled)
      add("North", northPanel)
      val rightPanel = new JPanel(new BorderLayout())
      // registers
      val registerPanel = new JPanel(new GridLayout(0, 1))
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
      vdptable.setPreferredScrollableViewportSize(new Dimension(0, 200))
      val vdpColModel = vdptable.getColumnModel
      vdpColModel.getColumn(0).setMinWidth(80)
      vdpColModel.getColumn(0).setMaxWidth(150)
      vdpColModel.getColumn(1).setMinWidth(50)
      vdpColModel.getColumn(1).setMaxWidth(50)

      rightPanel.add("Center", sp)
      rightPanel.add("North", registerPanel)

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
            if hasBreakAt(address) then
              removeBreakAt(address)
            else
              addExecuteBreakAt(address)
            disassembledTableModel.update()
      })

      val splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, sp, rightPanel)
      splitPane.setContinuousLayout(true)
      splitPane.setOneTouchExpandable(true)
      add("Center", splitPane)
    end init

    override def updateModels(): Unit = swing {
      dataRegisterTableModel.contentUpdated()
      addressRegisterTableModel.contentUpdated()
      statusRegisterTableModel.contentUpdated()
      pcRegisterTableModel.contentUpdated()
      disassembledTableModel.update()
      vdpTableMode.update()
      distable.setRowSelectionInterval(0, 0)
    }

    override def enableTracing(enabled: Boolean): Unit =
      debugger.setStepByStep(enabled)

      if !enabled then
        debugger.setStepAlways(false)
        debugger.setStepByStep(false)

      stepOutPending = StepState.NoStep
      stepOverPending = StepState.NoStep

    override def stepIn(): Unit =
      debugger.nextStep()

    override def stepOver(): Unit =
      import InstructionType.*
      import StepState.*
      stepInstruction.instructionType match
        case JSR | TRAP | TRAPV | ILLEGAL =>
          stepOverPending = WaitReturn
          debugger.setStepAlways(true)
        case DBRA | DBCC | DBCS | DBEQ | DBGE | DBGT | DBHI | DBLE | DBLS | DBMI | DBNE | DBPL | DBVC | DBVS =>
          stepOverTargetAddress = stepDisassemble.address + stepDisassemble.size
          stepOverPending = WaitTarget
          debugger.setStepAlways(true)
        case _ =>

      debugger.nextStep()

    override def stepOut(): Unit =
      import InstructionType.*
      import StepState.*
      stepInstruction.instructionType match
        case RTR | RTE | RTS =>
          stepOutPending = NoStep
        case _ =>
          stepOutPending = WaitReturn
          debugger.setStepAlways(true)
      debugger.nextStep()

  end M68KDebugger

  // ==================================================================================================

  def setCart(cart:Cart): Unit =
    this.cart = cart
    if cart == null then
      romDumpItem.setEnabled(false)
      if romDialog != null then
        romDialog.dispose()
    else
      romDumpItem.setEnabled(true)
      romDialog = new MemoryDumper(cart.getROM,0,"ROM",frame,() => romDumpItem.setSelected(false),canUpdate = false,setPreferredScrollableViewportSize = false, showASCII = true).dialog

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

    toolBar.add(onOffButton)
    toolBar.add(stepInButton)
    toolBar.add(stepOverButton)
    toolBar.add(stepOutButton)
    toolBar.add(disaButton)
    toolBar.add(readButton)
    toolBar.add(writeButton)

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

    tabbedPane.add("M68K",m68kDebugger)
    tabbedPane.add("Z80",z80Debugger)
    tabbedPane.addChangeListener(e => {
      tabbedPane.getSelectedIndex match
        case 0 =>
          selectedDebugger = m68kDebugger
        case 1 =>
          selectedDebugger = z80Debugger
    })
    mainPanel.add("Center",tabbedPane)

    val splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, mainPanel, logButtonPanel)
    splitPane.setOneTouchExpandable(true)

    // menu
    val menu = new JMenuBar
    val memoryMenu = new JMenu("Memory")
    val layerMenu = new JMenu("Layers")
    val disMenu = new JMenu("Disassembler")
    val spriteMenu = new JMenu("Sprite")

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
    val layerAEnabledItem = new JCheckBoxMenuItem("Layer A enabled")
    layerAEnabledItem.setSelected(true)
    layerAEnabledItem.addActionListener(_ => vdp.setLayerAEnabled(layerAEnabledItem.isSelected))
    val layerBEnabledItem = new JCheckBoxMenuItem("Layer B enabled")
    layerBEnabledItem.setSelected(true)
    layerBEnabledItem.addActionListener(_ => vdp.setLayerBEnabled(layerBEnabledItem.isSelected))
    val layerSEnabledItem = new JCheckBoxMenuItem("Layer Sprite enabled")
    layerSEnabledItem.setSelected(true)
    layerSEnabledItem.addActionListener(_ => vdp.setLayerSEnabled(layerSEnabledItem.isSelected))
    layerMenu.add(layerAEnabledItem)
    layerMenu.add(layerBEnabledItem)
    layerMenu.add(layerSEnabledItem)
    menu.add(layerMenu)

    m68KDisassemblerItem.addActionListener(_ => m68kDisassemblerDialog.setVisible(m68KDisassemblerItem.isSelected) )
    z80DisassemblerItem.addActionListener(_ => z80DisassemblerDialog.setVisible(z80DisassemblerItem.isSelected) )
    disMenu.add(m68KDisassemblerItem)
    disMenu.add(z80DisassemblerItem)

    menu.add(disMenu)

    spriteDumpItem.addActionListener(_ => spriteDumpDialog.setVisible(spriteDumpItem.isSelected) )
    spriteMenu.add(spriteDumpItem)

    menu.add(spriteMenu)

    vdpCRAMDialog.setResizable(false)

    frame.setJMenuBar(menu)

    frame.getContentPane.add("Center",splitPane)
    frame.pack()
  end init

  def enableTracing(enabled: Boolean): Unit =
    selectedDebugger.enableTracing(enabled)

    if enabled then
      onOffButton.setToolTipText("Disable tracing")
    else
      onOffButton.setToolTipText("Enable tracing")

    onOffButton.setSelected(enabled)

  private def stepIn(): Unit =
    selectedDebugger.stepIn()

  private def stepOver(): Unit =
    selectedDebugger.stepOver()

  private def stepOut(): Unit =
    selectedDebugger.stepOut()

  private def disassembleGUI(): Unit = ???
  private def readGUI(): Unit = ???
  private def writeGUI(): Unit = ???

  def log(msg: String): Unit = swing {
    logPanel.append(msg)
    logPanel.append("\n")
  }



