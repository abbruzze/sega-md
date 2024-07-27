package ucesoft.smd.debugger

import org.fife.ui.rsyntaxtextarea.{RSyntaxTextArea, SyntaxConstants}
import org.fife.ui.rtextarea.RTextScrollPane
import ucesoft.smd.cpu.m68k.*
import ucesoft.smd.cpu.m68k.RegisterType.PC
import ucesoft.smd.cpu.svp.{SVP, SVPMapper}
import ucesoft.smd.cpu.z80.Z80
import ucesoft.smd.debugger.DebuggerUI.*
import ucesoft.smd.ui.MessageBoard
import ucesoft.smd.ui.MessageBoard.MessageBoardListener
import ucesoft.smd.{Cart, Logger, VDP, cpu}

import java.awt.event.{MouseAdapter, MouseEvent, WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Dimension, FlowLayout, GridLayout}
import javax.swing.*
import javax.swing.text.DefaultCaret
import scala.compiletime.uninitialized

object Debugger:
  sealed trait BreakType
  case class AddressBreakType(address:Int,execute:Boolean = false,read:Boolean = false,write:Boolean = false,var enabled : Boolean = true) extends BreakType:
    override def toString: String =
      val sb = new StringBuilder()
      if read then sb += 'R'
      if write then sb += 'W'
      if execute then sb += 'E'
      sb.toString()

  case object ResetBreak extends BreakType
  case object HaltBreak extends BreakType
  case object StopBreak extends BreakType
  case class ExceptionBreak(number:Int) extends BreakType
  case class InterruptBreak(number:Int,label:String = "") extends BreakType

/**
 * @author Alessandro Abbruzzetti
 *         Created on 15/10/2023 14:44
 */
class Debugger(m68k:M68000,
               m68kMemory:Memory,
               m68kRAM:Array[Int],
               z80:Z80,
               z80Ram:Array[Int],
               vdp:VDP,
               windowCloseOperation: () => Unit) extends VDP.VDPNewFrameListener:
  import Debugger.*
  private inline val MAX_LOG_LINES = 1000
  private inline val DIS_LINES = 25
  private enum StepState:
    case NoStep, WaitReturn, WaitTarget

  private abstract class InternalDebugger extends JPanel:
    protected var stepOverPending, stepOutPending = StepState.NoStep
    protected var stepOverOutStopPending = false
    protected var stepOverTargetAddress = 0
    protected val semaphore = new Object//new Semaphore(0)
    protected val cpuEnabled = {
      val item = new JCheckBox("CPU enabled")
      item.setSelected(true)
      item.addActionListener(_ => onCPUEnabled(item.isSelected))
      item
    }
    protected val busAvailable = {
      val bus = new JCheckBox("BUS available")
      bus.setEnabled(false)
      bus
    }
    protected var tracingOnFile = false
    protected var tracingListener: TraceListener = scala.compiletime.uninitialized

    protected def onCPUEnabled(enabled:Boolean): Unit = {}

    def startTracingOnFile(tracingListener: TraceListener): Unit =
      this.tracingListener = tracingListener
      tracingOnFile = true
    def stopTracingOnFile(): Unit =
      tracingOnFile = false

    def enableTracing(enabled: Boolean): Unit
    def stepIn(): Unit
    def stepOver(): Unit
    def stepOut(): Unit
    def updateModels(): Unit
  end InternalDebugger

  private val frame = new JFrame("Debugger")
  private var cart : Cart = scala.compiletime.uninitialized
  private val logPanel = new RSyntaxTextArea(10,100)
  private val onOffButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/on.png")))
  private val vdpMemDump = vdp.getMemoryDump

  private var layersDialogActive = false
  private var frameByFrameMode = false
  private val frameByFrameLock = new Object
  private var frameByFrameCond = false
  private var frameCount = 0

  private val svpDRAMDumpItem = new JCheckBoxMenuItem("SVP DRAM")
  private val svpRAMADumpItem = new JCheckBoxMenuItem("SVP RAM A")
  private val svpRAMBDumpItem = new JCheckBoxMenuItem("SVP RAM B")
  private val vramMemoryDumpItem = new JCheckBoxMenuItem("VDP VRAM")
  private val cramMemoryDumpItem = new JCheckBoxMenuItem("VDP CRAM")
  private val vsramMemoryDumpItem = new JCheckBoxMenuItem("VDP VSRAM")
  private val m68kramMemoryDumpItem = new JCheckBoxMenuItem("68k RAM")
  private val z80RamMemoryDumpItem = new JCheckBoxMenuItem("Z80 RAM")
  private val layerDumpItem = new JCheckBoxMenuItem("Pattern Layers")
  private val patternADumpItem = new JCheckBoxMenuItem("Pattern Dump")
  private var svpDRAMDialog : JDialog = uninitialized
  private var svpRAMADialog : JDialog = uninitialized
  private var svpRAMBDialog : JDialog = uninitialized
  private val vdpVRAMDialog = new MemoryDumper(vdpMemDump.ram, 0, "VRAM", frame, () => vramMemoryDumpItem.setSelected(false), setPreferredScrollableViewportSize = false, showASCII = true).dialog
  private val vdpVSRAMDialog = new MemoryDumper(vdpMemDump.vsram, 0, "VSRAM", frame, () => vsramMemoryDumpItem.setSelected(false), setPreferredScrollableViewportSize = false, showASCII = true).dialog
  private val vdpCRAMDialog = new MemoryDumper(vdpMemDump.cram, 0, "CRAM", frame, () => cramMemoryDumpItem.setSelected(false), withColorDumper = true).dialog
  private val patternLayers = new LayerDumper(vdpMemDump.ram, vdpMemDump.cram, "Pattern Layers", vdp, frame, () => layerDumpItem.setSelected(false),active => { layersDialogActive = active ; checkVDPNewFrameState() })
  private val patternLayersDialog = patternLayers.dialog
  private val m68KramDialog = new MemoryDumper(m68kRAM, 0xFF0000, "68K RAM", frame, () => m68kramMemoryDumpItem.setSelected(false), setPreferredScrollableViewportSize = false, showASCII = true).dialog
  private val z80RamDialog = new MemoryDumper(z80Ram, 0x0000, "Z80 RAM", frame, () => z80RamMemoryDumpItem.setSelected(false), setPreferredScrollableViewportSize = false, showASCII = true).dialog
  private val romDumpItem = new JCheckBoxMenuItem("Cart's ROM")
  private val patternDialog = new PatternDumper(vdpMemDump.ram, vdpMemDump.cram, "Pattern Dump", frame, () => patternADumpItem.setSelected(false)).dialog
  private val spriteDumpItem = new JCheckBoxMenuItem("Sprite Cache")
  private val spriteDumpDialog = new SpriteDumper(vdp,"Sprite Cache",frame,() => spriteDumpItem.setSelected(false)).dialog
  private var romDialog: JDialog = uninitialized
  private val m68KDisassemblerItem = new JCheckBoxMenuItem("M68K Disassembler")
  private val z80DisassemblerItem = new JCheckBoxMenuItem("Z80 Disassembler")
  private val svpDisassemblerItem = new JCheckBoxMenuItem("SVP Disassembler")
  private val dmaItem = new JCheckBoxMenuItem("DMA trace")
  private val dmaDialog = new DMAEventPanel(frame,vdp,() => dmaItem.setSelected(false)).dialog
  private val fifoPanel = new VDPFifoPanel(vdp)

  private val m68kDebugger = new M68KDebugger
  private val m68kDisassemblerPanel = new DisassemblerPanel("M68K",
    (model,a) => {
      val dis = m68k.disassemble(a)
      model.add(dis, false)
      a + dis.size
    },
    frame,
    m68kDebugger,
    () => m68KDisassemblerItem.setSelected(false))
  private val m68kDisassemblerDialog = m68kDisassemblerPanel.dialog
  private val z80Debugger = new Z80Debugger
  private val z80DisassemblerPanel = new DisassemblerPanel("Z80",
    (model, a) => {
      val dis = z80.getDisassembledInfo(a)
      model.add(dis)
      a + dis.size
    },
    frame,
    z80Debugger,
    () => z80DisassemblerItem.setSelected(false))
  private val z80DisassemblerDialog = z80DisassemblerPanel.dialog

  private var svpDebugger : SVPDebugger = uninitialized
  private val svpDisassemblerPanel = new DisassemblerPanel("SVP",
    (model, a) => {
      val dis = svpDebugger.svp.disassemble(a)
      model.add(dis)
      a + dis.wordSize
    },
    frame,
    m68kDebugger,
    () => m68KDisassemblerItem.setSelected(false))
  private val svpDisassemblerDialog = svpDisassemblerPanel.dialog

  private var selectedDebugger : InternalDebugger = m68kDebugger

  private val m68kBreakItem = new JCheckBoxMenuItem("68k breaks")
  private val m68kBreakDialog = new BreakMasterPanel(
    "M68K",
    frame,
    6,
    break => m68kDebugger.removeBreakAt(break.address),
    break => m68kDebugger.addBreakAt(break.address,read = break.read,write = break.write,execute = break.execute),
    new M68KBreakEventPanel(m68kDebugger),
    m68kDebugger,
    () => m68kBreakItem.setSelected(false)
  ).dialog

  private val z80BreakItem = new JCheckBoxMenuItem("z80 breaks")
  private val z80BreakDialog = new BreakMasterPanel(
    "Z80",
    frame,
    4,
    break => z80Debugger.removeBreakAt(break.address),
    break => z80Debugger.addBreakAt(break.address, read = break.read, write = break.write, execute = break.execute),
    new Z80BreakEventPanel(z80Debugger),
    z80Debugger,
    () => z80BreakItem.setSelected(false)
  ).dialog

  private val svpBreakItem = new JCheckBoxMenuItem("z80 breaks")
  private var svpBreakDialog : JDialog = uninitialized


  private var messageBoard: MessageBoardListener = scala.compiletime.uninitialized

  private val tabbedPane = new JTabbedPane()

  private var logLines = 0

  // =============================================================================================================
  private trait GenericDebugger extends DisassemblerBreakHandler:
    def nextStep(): Unit
    def updateDisassembly(): Unit
    def isTracing: Boolean

  private class SVPDebugger(val svp:SVP) extends InternalDebugger with GenericDebugger with SVP.SVPTracer:
    private val breaks = new collection.mutable.HashMap[Int,AddressBreakType]

    private val generalRegisterTableModel = new SVPRegisterTableModel(svp,SVPRegsType.General)
    private val pmxRegisterTableModel = new SVPRegisterTableModel(svp,SVPRegsType.PMx)
    private val pointerRegisterTableModel = new SVPRegisterTableModel(svp,SVPRegsType.Pointer)

    private val disassembledTableModel = new DisassembledTableModel(address => getBreakStringAt(address).map(_.substring(0, 1)))
    private val distable = new JTable(disassembledTableModel)

    private var stepByStep = false

    init()

    override protected def onCPUEnabled(enabled:Boolean): Unit =
      svp.setComponentEnabled(enabled)
    override def enableTracing(enabled: Boolean): Unit =
      if stepByStep != enabled then
        if enabled then
          svp.addTracer(this)
          if !frame.isVisible then
            frame.setVisible(true)
        else
          nextStep()
          if breaks.isEmpty then
            svp.removeTracer(this)

      stepByStep = enabled

    override def stepIn(): Unit = nextStep()
    override def stepOver(): Unit = stepIn()
    override def stepOut(): Unit = stepIn()
    override def nextStep(): Unit =
      semaphore.synchronized {
        semaphore.notify()
      }
    override def updateModels(): Unit = swing {
      generalRegisterTableModel.contentUpdated()
      pmxRegisterTableModel.contentUpdated()
      pointerRegisterTableModel.contentUpdated()
      distable.setRowSelectionInterval(0, 0)
    }
    override def updateDisassembly(): Unit =
      disassembledTableModel.clear()
      updateDisassemblyAddress()
      updateModels()
    override def isTracing: Boolean = stepByStep
    override def trace(address: Int): Unit =
      breaks.get(address) match
        case Some(break) if break.execute =>
          log(s"Break $break on address ${address.toHexString}")
          stepByStep = true
        case _ =>

      if stepByStep then
        selectDebugger(2)
        disassembledTableModel.clear()
        updateDisassemblyAddress(address)
        m68kDebugger.updateDisassembly()
        z80Debugger.updateDisassembly()
        checkTracingState(true)
        updateModels()
        semaphore.synchronized {
          semaphore.wait()
        }

    override def hasBreakAt(address: Int): Boolean = breaks.contains(address)
    override def addBreakAt(address: Int, read: Boolean, write: Boolean, execute: Boolean): Unit =
      svp.addTracer(this)
      breaks += address -> AddressBreakType(address, execute = execute, read = read, write = write)
      notifyBreakAdded(AddressBreakType(address, read = read, write = write, execute = execute))
      disassembledTableModel.update()
    override def removeBreakAt(address: Int): Unit =
      breaks -= address
      notifyBreakRemoved(address)
      disassembledTableModel.update()
      if breaks.isEmpty then
        svp.removeTracer(this)
    override def getBreakStringAt(address: Int): Option[String] = breaks.get(address).map(_.toString)
    override def getBreakEvent(eventName: String): Option[AnyRef] = None
    override def addBreakEvent(eventName: String, value: AnyRef): Unit = {}
    override def removeBreakEvent(eventName: String): Unit = {}

    private def updateDisassemblyAddress(address: Int = -1): Unit = swing {
      var adr = if address == -1 then svp.getRegister(cpu.svp.RegisterType.PC).get else address
      for a <- 1 to DIS_LINES do
        val dis = svp.disassemble(adr)
        disassembledTableModel.add(dis)
        adr += dis.wordSize
      disassembledTableModel.update()
    }

    private def init(): Unit =
      setLayout(new BorderLayout())
      val northPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      northPanel.add(cpuEnabled)
      add("North", northPanel)
      val rightPanel = new JPanel(new BorderLayout())
      // registers
      val registerPanel = new JPanel(new GridLayout(0, 1))
      // generic registers
      val gentable = new JTable(generalRegisterTableModel)
      gentable.getTableHeader.setReorderingAllowed(false)
      gentable.setDefaultRenderer(classOf[String], new RegisterRenderer("%04X"))
      var sp = new JScrollPane(gentable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      gentable.setPreferredScrollableViewportSize(gentable.getPreferredSize)
      sp.setBorder(BorderFactory.createTitledBorder("Generic registers"))
      registerPanel.add(sp)
      // pmx registers
      val pmxtable = new JTable(pmxRegisterTableModel)
      pmxtable.getTableHeader.setReorderingAllowed(false)
      pmxtable.setDefaultRenderer(classOf[String], new RegisterRenderer("%04X"))
      sp = new JScrollPane(pmxtable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      pmxtable.setPreferredScrollableViewportSize(pmxtable.getPreferredSize)
      sp.setBorder(BorderFactory.createTitledBorder("PMx registers"))
      registerPanel.add(sp)
      // pointer registers
      val pointertable = new JTable(pointerRegisterTableModel)
      pointertable.getTableHeader.setReorderingAllowed(false)
      pointertable.setDefaultRenderer(classOf[String], new RegisterRenderer("%04X"))
      sp = new JScrollPane(pointertable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      pointertable.setPreferredScrollableViewportSize(pointertable.getPreferredSize)
      sp.setBorder(BorderFactory.createTitledBorder("PMx registers"))
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

  end SVPDebugger



  private class Z80Debugger extends InternalDebugger with GenericDebugger with Z80.EventListener:
    private val registerTableModel = new Z80RegisterTableModel(z80.ctx)
    private val statusTableModel = new Z80StatusRegisterTableModel(z80.ctx)
    private val disassembledTableModel = new DisassembledTableModel(address => getBreakStringAt(address).map(_.substring(0, 1)))
    private val distable = new JTable(disassembledTableModel)
    private var stepByStep = false
    private var stepAlways = false
    private var stepDisassemble : Z80.DisassembledInfo = scala.compiletime.uninitialized
    private val breaks = new collection.mutable.HashMap[Int,AddressBreakType]
    private var breakOnReset = false
    private var breakOnInt,breakOnNMI = false
    private var breakOnHalt = false

    init()

    override def startTracingOnFile(tracingListener: TraceListener): Unit =
      super.startTracingOnFile(tracingListener)
      stepAlways = true
      z80.addEventListener(this)
      nextStep()

    override def stopTracingOnFile(): Unit =
      super.stopTracingOnFile()
      if !existsBreakPending then
        z80.removeEventListener(this)
      stepAlways = false

    override protected def onCPUEnabled(enabled:Boolean): Unit =
      z80.setComponentEnabled(enabled)

    override def getBreakEvent(eventName: String): Option[AnyRef] =
      eventName match
        case "reset" => if breakOnReset then Some(java.lang.Boolean.TRUE) else None
        case "halt" => if breakOnHalt then Some(java.lang.Boolean.TRUE) else None
        case "int" => if breakOnInt then Some(java.lang.Boolean.TRUE) else None
        case "nmi" => if breakOnNMI then Some(java.lang.Boolean.TRUE) else None
        case _ => None
    override def addBreakEvent(eventName: String, value: AnyRef): Unit =
      z80.addEventListener(this)
      eventName match
        case "reset" => breakOnReset = true
        case "halt" => breakOnHalt = true
        case "int" => breakOnInt = true
        case "nmi" => breakOnNMI = true
        case _ =>
    override def removeBreakEvent(eventName: String): Unit =
      if !existsBreakPending then
        z80.removeEventListener(this)
      eventName match
        case "reset" => breakOnReset = false
        case "halt" => breakOnHalt = false
        case "int" => breakOnInt = false
        case "nmi" => breakOnNMI = false
        case _ =>

    private def existsBreakPending: Boolean =
      breaks.nonEmpty || breakOnReset || breakOnInt || breakOnNMI || breakOnHalt

    override def nextStep(): Unit =
      semaphore.synchronized {
        semaphore.notify()
      }

    override def hasBreakAt(address: Int): Boolean = breaks.contains(address)

    override def addBreakAt(address:Int,read:Boolean,write:Boolean,execute:Boolean): Unit =
      z80.addEventListener(this)
      breaks += address -> AddressBreakType(address, execute = execute, read = read, write = write)
      notifyBreakAdded(AddressBreakType(address, read = read, write = write, execute = execute))
      disassembledTableModel.update()

    override def removeBreakAt(address: Int): Unit =
      breaks -= address
      notifyBreakRemoved(address)
      disassembledTableModel.update()
      if !existsBreakPending then
          z80.removeEventListener(this)

    override def getBreakStringAt(address: Int): Option[String] = breaks.get(address).map(_.toString)

    override def rw(z80: Z80, address: Int, isRead: Boolean, value: Int = 0): Unit =
      breaks.get(address) match
        case Some(break) if (break.read && isRead) || (break.write && !isRead) =>
          log(s"Break on Z80 ${if isRead then "READ" else "WRITE"} on address ${address.toHexString}")
          stepByStep = true
          disassembledTableModel.clear()
          updateDisassembly(z80, address)

          checkTracingState(true)
          updateModels()
          semaphore.synchronized {
            semaphore.wait()
          }
        case _ =>

    override def fetch(z80: Z80, address: Int, opcode: Int): Unit =
      breaks.get(address) match
        case Some(break) if break.execute =>
          log(s"Break $break on address ${address.toHexString}")
          stepByStep = true
        case _ =>

      checkStepOverOut(address)

      if !stepAlways && stepByStep then
        selectDebugger(1)
        disassembledTableModel.clear()
        updateDisassembly(z80,address)
        m68kDebugger.updateDisassembly()
        if svpDebugger != null then svpDebugger.updateDisassembly()
        checkTracingState(true)
        updateModels()
        semaphore.synchronized {
          semaphore.wait()
        }
    end fetch

    override def isTracing: Boolean = stepByStep

    private def checkStepOverOut(address: Int): Unit =
      stepOverPending match
        case StepState.WaitTarget =>
          if address == stepOverTargetAddress then
            stepOutPending = StepState.NoStep
            stepAlways = false
        case _ =>

    override def updateDisassembly(): Unit =
      disassembledTableModel.clear()
      updateDisassembly(z80)
      updateModels()

    private def updateDisassembly(z80:Z80,address:Int = -1): Unit = swing {
      var adr = if address == -1 then z80.ctx.PC else address
      for a <- 1 to DIS_LINES do
        val dis = z80.getDisassembledInfo(adr)
        if a == 1 then
          stepDisassemble = dis
        disassembledTableModel.add(dis)
        adr += dis.size
      disassembledTableModel.update()
    }
    override def interrupted(z80: Z80, mode: Int, isNMI: Boolean): Unit =
      log(s"Break on Z80 ${if isNMI then "NMI" else "INT"}")
      stepByStep = true
      stepOutPending = StepState.NoStep
      stepOverPending = StepState.NoStep
      checkTracingState(true)
      updateDisassembly(z80)
      updateModels()
      semaphore.synchronized {
        semaphore.wait()
      }
    override def reset(z80: Z80): Unit =
      log(s"Break on Z80 RESET")
      stepByStep = true
      stepOutPending = StepState.NoStep
      stepOverPending = StepState.NoStep
      checkTracingState(true)
      updateDisassembly(z80)
      updateModels()
      semaphore.synchronized {
        semaphore.wait()
      }
    override def halt(z80: Z80, isHalted: Boolean): Unit =
      log(s"Break on Z80 HALT")
      stepByStep = true
      stepOutPending = StepState.NoStep
      stepOverPending = StepState.NoStep
      checkTracingState(true)
      updateDisassembly(z80)
      updateModels()
      semaphore.synchronized {
        semaphore.wait()
      }

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
    override def stepOver(): Unit =
      stepOverTargetAddress = stepDisassemble.address + stepDisassemble.size
      stepOverPending = StepState.WaitTarget
      stepAlways = true
      nextStep()

    override def stepOut(): Unit = stepIn() // TODO

    override def updateModels(): Unit = swing {
      registerTableModel.contentUpdated()
      statusTableModel.contentUpdated()
      disassembledTableModel.update()
      distable.setRowSelectionInterval(0, 0)
      busAvailable.setSelected(!z80.isBUSRequested)
    }

    private def init(): Unit =
      setLayout(new BorderLayout())
      val northPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      northPanel.add(cpuEnabled)
      northPanel.add(busAvailable)
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

  private class M68KDebugger extends InternalDebugger with GenericDebugger:
    private var stepInstruction : Instruction = scala.compiletime.uninitialized
    private var stepDisassemble : DisassembledInstruction = scala.compiletime.uninitialized
    private val dataRegisterTableModel = new M68KRegisterTableModel(m68k, data = true)
    private val addressRegisterTableModel = new M68KRegisterTableModel(m68k, data = false)
    private val statusRegisterTableModel = new M68KStatusRegisterTableModel(m68k)
    private val pcRegisterTableModel = new M68KPCTableModel(m68k)
    private val disassembledTableModel = new DisassembledTableModel(address => getBreakStringAt(address))
    private val vdpTableMode = new VDPPropertiesTableModel(vdp,frame)
    private val distable = new JTable(disassembledTableModel)

    override def startTracingOnFile(tracingListener: TraceListener): Unit =
      super.startTracingOnFile(tracingListener)
      debugger.setStepAlways(true)
      m68k.addEventListener(debugger)
      debugger.nextStep()

    override def stopTracingOnFile(): Unit =
      super.stopTracingOnFile()
      if !debugger.existsBreakPending then
        m68k.removeEventListener(debugger)
      debugger.setStepAlways(false)

    override protected def onCPUEnabled(enabled:Boolean): Unit =
      m68k.setComponentEnabled(enabled)
    override def getBreakEvent(eventName: String): Option[AnyRef] = debugger.getBreakEvent(eventName)
    override def addBreakEvent(eventName: String, value: AnyRef): Unit =
      debugger.addBreakEvent(eventName, value)
      
    override def removeBreakEvent(eventName: String): Unit =
      debugger.removeBreakEvent(eventName)
      if !debugger.existsBreakPending then
        enableTracing(false)
    override def hasBreakAt(address: Int): Boolean = debugger.hasBreakAt(address)
    override def addBreakAt(address:Int,read:Boolean,write:Boolean,execute:Boolean): Unit =
      debugger.addBreakAt(address,read,write,execute)
      notifyBreakAdded(AddressBreakType(address,read = read,write = write, execute = execute))
      disassembledTableModel.update()
      m68k.addEventListener(debugger)
    override def removeBreakAt(address: Int): Unit =
      debugger.removeBreakAt(address)
      notifyBreakRemoved(address)
      disassembledTableModel.update()
      if !debugger.existsBreakPending then
        m68k.removeEventListener(debugger)
    override def getBreakStringAt(address: Int): Option[String] = debugger.getBreakStringAt(address)

    override def isTracing: Boolean = debugger.isTracing

    private val debugger = new AbstractDebugger with GenericDebugger {
      override def getBreakEvent(eventName: String): Option[AnyRef] =
        eventName match
          case "reset" => if isBreakOnReset then Some(java.lang.Boolean.TRUE) else None
          case "halt" => if isBreakOnHalt then Some(java.lang.Boolean.TRUE) else None
          case "stop" => if isBreakOnStop then Some(java.lang.Boolean.TRUE) else None
          case "interrupt" =>
            val interrupt = getBreakOnInterruptLevel
            if interrupt == -1 then None else Some(Integer.valueOf(interrupt))
          case "exception" =>
            val ex = getBreakOnExceptionNumber
            if ex == -1 then None else Some(Integer.valueOf(ex))
          case _ => None
      override def addBreakEvent(eventName: String, value: AnyRef): Unit =
        m68k.addEventListener(this)
        eventName match
          case "reset" => setBreakOnReset(true)
          case "halt" => setBreakOnHalt(true)
          case "stop" => setBreakOnStop(true)
          case "interrupt" => setBreakOnInterrupt(value.asInstanceOf[Integer].intValue())
          case "exception" => setBreakOnExceptionNumber(value.asInstanceOf[Integer].intValue())
      override def removeBreakEvent(eventName: String): Unit =
        if !existsBreakPending then
          m68k.removeEventListener(this)
        eventName match
          case "reset" => setBreakOnReset(false)
          case "halt" => setBreakOnHalt(false)
          case "stop" => setBreakOnStop(false)
          case "interrupt" => setBreakOnInterrupt(-1)
          case "exception" => setBreakOnExceptionNumber(-1)
      override def hasBreakAt(address: Int): Boolean = m68kAddressBreaks.contains(address)
      override def addBreakAt(address:Int,r:Boolean,w:Boolean,e:Boolean): Unit =
        m68kAddressBreaks += address -> AddressBreak(BreakType(execute = e,read = r, write = w), address)
      override def removeBreakAt(address: Int): Unit = m68kAddressBreaks -= address
      override def getBreakStringAt(address: Int): Option[String] = m68kAddressBreaks.get(address).map(_.breakType.toString)

      override def nextStep(): Unit =
        //semaphore.release()
        semaphore.synchronized {
          semaphore.notify()
        }

      override def isTracing: Boolean = isStepByStep

      override def breakEpilogue(cpu: M6800X0): Unit = semaphore.synchronized {
        semaphore.wait()
      }//semaphore.acquire()

      override def onStepByStepChange(stepByStepEnabled: Boolean): Unit =
        if stepByStepEnabled then
          m68k.addEventListener(this)
          checkTracingState(true)
        else
          nextStep()
          if !existsBreakPending then
            m68k.removeEventListener(this)

      override protected def onInterrupted(cpu: M6800X0, level: Int): Unit =
        if !tracingOnFile then
          log(s"Break on 68K interrupt $level")
          stepOutPending = StepState.NoStep
          stepOverPending = StepState.NoStep
          checkTracingState(true)
          updateDisassembly(cpu)
          updateModels()
          breakEpilogue(cpu)
      override protected def onException(cpu: M6800X0, level: Int): Unit =
        if !tracingOnFile then
          log(s"Break on 68K exception $level")
          stepOutPending = StepState.NoStep
          stepOverPending = StepState.NoStep
          checkTracingState(true)
          updateDisassembly(cpu)
          updateModels()
          breakEpilogue(cpu)
      override protected def onReset(cpu: M6800X0): Unit =
        if !tracingOnFile then
          log(s"Break on 68K RESET")
          stepOutPending = StepState.NoStep
          stepOverPending = StepState.NoStep
          checkTracingState(true)
          updateDisassembly(cpu)
          updateModels()
          breakEpilogue(cpu)
      override protected def onHalt(cpu: M6800X0): Unit =
        if !tracingOnFile then
          log(s"Break on 68K HALT")
          stepOutPending = StepState.NoStep
          stepOverPending = StepState.NoStep
          checkTracingState(true)
          updateDisassembly(cpu)
          updateModels()
          breakEpilogue(cpu)
      override protected def onStop(cpu: M6800X0): Unit =
        if !tracingOnFile then
          log(s"Break on 68K STOP")
          stepOutPending = StepState.NoStep
          stepOverPending = StepState.NoStep
          checkTracingState(true)
          updateDisassembly(cpu)
          updateModels()
          breakEpilogue(cpu)
      override protected def onRw(cpu: M6800X0, address: Int, size: Size, read: Boolean, value: Int): Unit =
        if !tracingOnFile then
          log(s"Break on M68K ${if read then "READ" else "WRITE"} with size $size${if read then s"value=$value" else ""}")
          stepOutPending = StepState.NoStep
          stepOverPending = StepState.NoStep
          checkTracingState(true)
          updateDisassembly(cpu)
          updateModels()
          breakEpilogue(cpu)

      override protected def onFetch(cpu: M6800X0, address: Int, opcode: Int, i: Instruction, busNotAvailable: Boolean, wasBreak: Boolean): Unit =
        if tracingOnFile then
          tracingListener.onTrace(cpu.disassemble(address).toString,address)
          return

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
          checkTracingState(true)
          disassembledTableModel.clear()
          updateDisassembly(cpu,address)
          z80Debugger.updateDisassembly()
          if svpDebugger != null then svpDebugger.updateDisassembly()
          updateModels()
          breakEpilogue(cpu)
        end if
      end onFetch

      override def updateDisassembly(): Unit =
        //disassembledTableModel.clear()
        updateDisassembly(m68k)
        updateModels()

      private def updateDisassembly(cpu: M6800X0,address:Int = -1): Unit =
        disassembledTableModel.clear()
        val startAddress = if address == -1 then cpu.getRegister(PC).get() else address
        swing {
          var adr = startAddress
          for a <- 1 to DIS_LINES do
            val dis = cpu.disassemble(adr)
            if a == 1 then
              stepDisassemble = dis
            disassembledTableModel.add(dis, false)
            adr += dis.size
          disassembledTableModel.update()
        }

      private def checkStepOverOut(instruction: Instruction, address: Int): Unit =
        import InstructionType.*
        import StepState.*

        stepOverPending match
          case WaitTarget =>
            if address == stepOverTargetAddress then
              stepOutPending = StepState.NoStep
              setStepAlways(false)
          case _ =>
        stepOutPending match
          case NoStep =>
          case WaitReturn =>
            instruction.instructionType match
              case RTR | RTE | RTS =>
                stepOverOutStopPending = true
              case _ =>
          case _ =>
    }

    override def updateDisassembly(): Unit = debugger.updateDisassembly()
    override def nextStep(): Unit = debugger.nextStep()

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
      stepOverTargetAddress = stepDisassemble.address + stepDisassemble.size
      stepOverPending = StepState.WaitTarget
      debugger.setStepAlways(true)

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


    init()

    private def init(): Unit =
      setLayout(new BorderLayout())
      val northPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      northPanel.add(cpuEnabled)
      northPanel.add(busAvailable)
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
      val vdpPanel = new JTabbedPane()
      val vdptable = new JTable(vdpTableMode)
      vdptable.getTableHeader.setReorderingAllowed(false)
      vdptable.setDefaultRenderer(classOf[String], new PropertiesCellRenderer(vdpTableMode))
      sp = new JScrollPane(vdptable)
      vdptable.setPreferredScrollableViewportSize(new Dimension(0, 200))
      val vdpColModel = vdptable.getColumnModel
      vdpColModel.getColumn(0).setMinWidth(80)
      vdpColModel.getColumn(0).setMaxWidth(150)
      vdpColModel.getColumn(1).setMinWidth(50)
      vdpColModel.getColumn(1).setMaxWidth(50)

      vdpPanel.add("VDP properties",sp)
      vdpPanel.add("VDP FIFO",fifoPanel)

      rightPanel.add("Center", vdpPanel)
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
      colModel.getColumn(2).setMinWidth(180)
      colModel.getColumn(2).setMaxWidth(200)
      colModel.getColumn(3).setMinWidth(220)
      colModel.getColumn(3).setMaxWidth(250)
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
      fifoPanel.updateModel()
      busAvailable.setSelected(m68k.isBUSAvailable)
    }

  end M68KDebugger

  // ==================================================================================================
  def enableSVP(enabled:Boolean,mapper:SVPMapper): Unit =
    if enabled then
      svpDRAMDumpItem.setEnabled(true)
      svpRAMADumpItem.setEnabled(true)
      svpRAMBDumpItem.setEnabled(true)
      svpDisassemblerItem.setEnabled(true)
      svpBreakItem.setEnabled(true)
      svpDRAMDialog = new MemoryDumper(mapper.getDRAM, 0, "SVP DRAM", frame, () => svpDRAMDumpItem.setSelected(false), setPreferredScrollableViewportSize = false, showASCII = false, wordValues = true).dialog
      svpRAMADialog = new MemoryDumper(mapper.getSVP.getRAM(0), 0, "SVP RAM A", frame, () => svpDRAMDumpItem.setSelected(false), setPreferredScrollableViewportSize = false, showASCII = false, wordValues = true).dialog
      svpRAMBDialog = new MemoryDumper(mapper.getSVP.getRAM(1), 0, "SVP RAM B", frame, () => svpDRAMDumpItem.setSelected(false), setPreferredScrollableViewportSize = false, showASCII = false, wordValues = true).dialog
      svpDebugger = new SVPDebugger(mapper.getSVP)
      tabbedPane.addTab("SVP",svpDebugger)
      svpBreakDialog = new BreakMasterPanel(
        "SVP",
        frame,
        4,
        break => svpDebugger.removeBreakAt(break.address),
        break => svpDebugger.addBreakAt(break.address, read = break.read, write = break.write, execute = break.execute),
        new JPanel,
        svpDebugger,
        () => svpBreakItem.setSelected(false)
      ).dialog
    else
      svpDRAMDumpItem.setEnabled(false)
      svpRAMADumpItem.setEnabled(false)
      svpRAMBDumpItem.setEnabled(false)
      svpDisassemblerItem.setEnabled(false)
      svpBreakItem.setEnabled(false)
      svpBreakDialog = null
      svpDebugger = null
      if tabbedPane.getTabCount == 3 then
        tabbedPane.removeTabAt(2)
  def setMessageBoard(mb:MessageBoardListener): Unit =
    messageBoard = mb
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
      SwingUtilities.invokeLater(() => f)
    else f

  private def init(): Unit =
    frame.addWindowListener(new WindowAdapter:
      override def windowClosing(e: WindowEvent): Unit =
        windowCloseOperation()
    )
    frame.setIconImage(new ImageIcon(getClass.getResource("/resources/sonic_ring.png")).getImage)
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

    val enableButtons = (enabled:Boolean) => {
      onOffButton.setEnabled(enabled)
      stepInButton.setEnabled(enabled)
      stepOverButton.setEnabled(enabled)
      stepOutButton.setEnabled(enabled)
    }

    val disaButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/bug.png")))
    disaButton.addActionListener(_ => disassembleGUI())
    disaButton.setToolTipText("Disassemble")

    val writeButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/write.png")))
    writeButton.addActionListener(_ => memoryGUI())
    writeButton.setToolTipText("Memory")

    val nextFrame = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/nextFrame.png")))
    nextFrame.addActionListener(_ => advanceByOneFrame())
    nextFrame.setToolTipText("Advance by one frame")
    nextFrame.setEnabled(false)

    val frameByFrameMode = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/frameByFrameMode.png")))
    frameByFrameMode.addActionListener(_ => {
      nextFrame.setEnabled(frameByFrameMode.isSelected)
      setFrameByFrameMode(frameByFrameMode.isSelected)
    })
    frameByFrameMode.setToolTipText("Frame by frame mode")

    val breakPoint = new JButton(new ImageIcon(getClass.getResource("/resources/trace/red_breakpoint.png")))
    breakPoint.addActionListener(_ => breakGUI())
    breakPoint.setToolTipText("Breakpoints")

    val saveTrace = new JButton(new ImageIcon(getClass.getResource("/resources/trace/save.png")))
    saveTrace.addActionListener(_ => saveTraceUI())
    saveTrace.setToolTipText("Save live disassembly on file")

    toolBar.add(onOffButton)
    toolBar.add(stepInButton)
    toolBar.add(stepOverButton)
    toolBar.add(stepOutButton)
    toolBar.add(disaButton)
    toolBar.add(writeButton)
    toolBar.add(frameByFrameMode)
    toolBar.add(nextFrame)
    toolBar.add(breakPoint)
    toolBar.add(saveTrace)

    // log panel
    logPanel.setEditable(false)
    logPanel.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_NONE)
    logPanel.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE)
    val lscroll = new RTextScrollPane(logPanel)
    lscroll.setMinimumSize(new Dimension(0, 70))
    lscroll.setBorder(BorderFactory.createTitledBorder("Log panel"))


    val logButtonPanel = new JPanel(new BorderLayout())
    logButtonPanel.add("Center", lscroll)
    val logToolBar = new JPanel(new FlowLayout(FlowLayout.LEFT))
    logButtonPanel.add("South", logToolBar)
    val clearLog = new JButton(new ImageIcon(getClass.getResource("/resources/trace/clear.png")))
    clearLog.setToolTipText("Clear log panel")
    logToolBar.add(clearLog)
    val logSeverityGroup = new ButtonGroup
    val logSeverityInfoButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/log_info.png")))
    logSeverityInfoButton.setToolTipText("Set log level to INFO")
    val logSeverityWarningButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/log_warning.png")))
    logSeverityWarningButton.setToolTipText("Set log level to WARNING")
    val logSeverityOffButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/log_off.png")))
    logSeverityOffButton.setToolTipText("Set log level to OFF, log disabled")
    logSeverityGroup.add(logSeverityInfoButton)
    logSeverityGroup.add(logSeverityWarningButton)
    logSeverityGroup.add(logSeverityOffButton)
    val logSeverityPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    logSeverityPanel.add(logSeverityInfoButton)
    logSeverityPanel.add(logSeverityWarningButton)
    logSeverityPanel.add(logSeverityOffButton)
    logToolBar.add(logSeverityPanel)
    logSeverityOffButton.setSelected(true)
    logSeverityInfoButton.addActionListener(_ => {
      Logger.getLogger.setLevel(java.util.logging.Level.INFO)
      vdp.enableLogging(true)
    })
    logSeverityWarningButton.addActionListener(_ => {
      Logger.getLogger.setLevel(java.util.logging.Level.WARNING)
      vdp.enableLogging(true)
    })
    logSeverityOffButton.addActionListener(_ => {
      Logger.getLogger.setLevel(java.util.logging.Level.OFF)
      vdp.enableLogging(false)
    })
    clearLog.addActionListener(_ => {
      logLines = 0
      logPanel.setText("")
    })

    tabbedPane.add("M68K",m68kDebugger)
    tabbedPane.add("Z80",z80Debugger)
    tabbedPane.addChangeListener(e => {
      tabbedPane.getSelectedIndex match
        case 0 =>
          selectedDebugger = m68kDebugger
          enableButtons(!(z80Debugger.isTracing || (svpDebugger != null && svpDebugger.isTracing)))
        case 1 =>
          selectedDebugger = z80Debugger
          enableButtons(!(m68kDebugger.isTracing || (svpDebugger != null && svpDebugger.isTracing)))
        case 2 =>
          selectedDebugger = svpDebugger
          enableButtons(!(m68kDebugger.isTracing || z80Debugger.isTracing))
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
    val dmaMenu = new JMenu("DMA")
    val breakMenu = new JMenu("Breaks")

    svpDRAMDumpItem.addActionListener(_ => svpDRAMDialog.setVisible(svpDRAMDumpItem.isSelected))
    svpRAMADumpItem.addActionListener(_ => svpRAMADialog.setVisible(svpRAMADumpItem.isSelected))
    svpRAMBDumpItem.addActionListener(_ => svpRAMBDialog.setVisible(svpRAMBDumpItem.isSelected))
    svpDRAMDumpItem.addActionListener(_ => svpDRAMDialog.setVisible(svpDRAMDumpItem.isSelected))
    vramMemoryDumpItem.addActionListener(_ => vdpVRAMDialog.setVisible(vramMemoryDumpItem.isSelected) )
    cramMemoryDumpItem.addActionListener(_ => vdpCRAMDialog.setVisible(cramMemoryDumpItem.isSelected) )
    vsramMemoryDumpItem.addActionListener(_ => vdpVSRAMDialog.setVisible(vsramMemoryDumpItem.isSelected) )
    m68kramMemoryDumpItem.addActionListener(_ => m68KramDialog.setVisible(m68kramMemoryDumpItem.isSelected) )
    z80RamMemoryDumpItem.addActionListener(_ => z80RamDialog.setVisible(z80RamMemoryDumpItem.isSelected))
    romDumpItem.addActionListener(_ => romDialog.setVisible(romDumpItem.isSelected) )
    memoryMenu.add(vramMemoryDumpItem)
    memoryMenu.add(cramMemoryDumpItem)
    memoryMenu.add(vsramMemoryDumpItem)
    memoryMenu.add(romDumpItem)
    memoryMenu.add(m68kramMemoryDumpItem)
    memoryMenu.add(z80RamMemoryDumpItem)
    memoryMenu.add(svpDRAMDumpItem)
    memoryMenu.add(svpRAMADumpItem)
    memoryMenu.add(svpRAMBDumpItem)
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

    spriteDumpItem.addActionListener(_ => spriteDumpDialog.setVisible(spriteDumpItem.isSelected) )
    val spriteDrawBoundariesItem = new JCheckBoxMenuItem("Draw boundaries")
    spriteDrawBoundariesItem.addActionListener(_ => vdp.enableDrawSpriteBoundaries(spriteDrawBoundariesItem.isSelected))
    spriteMenu.add(spriteDumpItem)
    spriteMenu.add(spriteDrawBoundariesItem)

    menu.add(spriteMenu)

    vdpCRAMDialog.setResizable(false)

    dmaItem.addActionListener(_ => dmaDialog.setVisible(dmaItem.isSelected))
    dmaMenu.add(dmaItem)
    menu.add(dmaMenu)

    m68KDisassemblerItem.addActionListener(_ => m68kDisassemblerDialog.setVisible(m68KDisassemblerItem.isSelected))
    z80DisassemblerItem.addActionListener(_ => z80DisassemblerDialog.setVisible(z80DisassemblerItem.isSelected))
    svpDisassemblerItem.addActionListener(_ => svpDisassemblerDialog.setVisible(svpDisassemblerItem.isSelected))
    disMenu.add(m68KDisassemblerItem)
    disMenu.add(z80DisassemblerItem)
    disMenu.add(svpDisassemblerItem)

    menu.add(disMenu)

    m68kBreakItem.addActionListener(_ => m68kBreakDialog.setVisible(m68kBreakItem.isSelected))
    z80BreakItem.addActionListener(_ => z80BreakDialog.setVisible(z80BreakItem.isSelected))
    svpBreakItem.addActionListener(_ => svpBreakDialog.setVisible(svpBreakItem.isSelected))

    breakMenu.add(m68kBreakItem)
    breakMenu.add(z80BreakItem)
    breakMenu.add(svpBreakItem)

    menu.add(breakMenu)

    frame.setJMenuBar(menu)

    frame.getContentPane.add("Center",splitPane)
    frame.pack()
  end init

  def enableTracing(enabled: Boolean): Unit =
    selectedDebugger.enableTracing(enabled)
    checkTracingState(enabled)
    if !enabled then
      windowCloseOperation()

  protected def selectDebugger(index:Int): Unit =
    swing {
      tabbedPane.setSelectedIndex(index)
      selectedDebugger = index match
        case 0 => m68kDebugger
        case 1 => z80Debugger
        case 2 => svpDebugger
    }

  def showDebugger(show:Boolean): Unit =
    frame.setVisible(show)

  private def checkTracingState(enabled: Boolean): Unit =
    if enabled then
      onOffButton.setToolTipText("Disable tracing")
      if !frame.isVisible then
        frame.setVisible(true)
    else
      onOffButton.setToolTipText("Enable tracing")
      m68kDebugger.enableTracing(false)
      m68kDebugger.nextStep()
      z80Debugger.nextStep()
      z80Debugger.enableTracing(false)
    onOffButton.setSelected(enabled)

  private def stepIn(): Unit =
   selectedDebugger.stepIn()

  private def stepOver(): Unit =
    selectedDebugger.stepOver()

  private def stepOut(): Unit =
    selectedDebugger.stepOut()

  private def disassembleGUI(): Unit =
    tabbedPane.getSelectedIndex match
      case 0 =>
        m68KDisassemblerItem.setSelected(true)
        m68kDisassemblerDialog.setVisible(true)
      case 1 =>
        z80DisassemblerItem.setSelected(true)
        z80DisassemblerDialog.setVisible(true)
      case 2 =>
        svpDisassemblerItem.setSelected(true)
        svpDisassemblerDialog.setVisible(true)

  private def memoryGUI(): Unit =
    if tabbedPane.getSelectedIndex == 1 then
      z80RamMemoryDumpItem.setSelected(true)
      z80RamDialog.setVisible(true)
    else
      m68kramMemoryDumpItem.setSelected(true)
      m68KramDialog.setVisible(true)

  def log(msg: String): Unit = swing {
    if logLines == MAX_LOG_LINES then
      logPanel.append("Hey, too many logs here, please clear this panel to keep reading new logs")
      logLines += 1
    else if logLines < MAX_LOG_LINES then
      logPanel.append(msg)
      logPanel.append("\n")
      logLines += 1
  }

  private def checkVDPNewFrameState(): Unit =
    if frameByFrameMode || layersDialogActive then
      vdp.setNewFrameListener(this)
    else
      vdp.setNewFrameListener(null)
      advanceByOneFrame()

  override def onNewFrame(): Unit =
    patternLayers.onNewFrame()
    if frameByFrameMode then
      frameByFrameLock.synchronized {
        while frameByFrameCond do
          frameByFrameLock.wait(1000)
      }
      frameByFrameCond = true

  private def setFrameByFrameMode(on:Boolean): Unit =
    frameByFrameMode = on
    frameByFrameCond = on
    frameCount = 0
    checkVDPNewFrameState()
  private def advanceByOneFrame(): Unit =
    frameByFrameLock.synchronized {
      frameByFrameCond = false
      frameByFrameLock.notify()
    }
    frameCount += 1
    if messageBoard != null then
      messageBoard.addMessage(MessageBoard.builder.message(s"$frameCount  ").ytop().xright().delay(500).fadingMilliseconds(100).adminLevel().build())

  private def breakGUI(): Unit =
    tabbedPane.getSelectedIndex match
      case 0 =>
        m68kBreakItem.setSelected(true)
        m68kBreakDialog.setVisible(true)
      case 1 =>
        z80BreakItem.setSelected(true)
        z80BreakDialog.setVisible(true)
      case 2 =>
        svpBreakItem.setSelected(true)
        svpBreakDialog.setVisible(true)

  private def saveTraceUI(): Unit =
    val std = new SaveTraceDialog(frame,tl => selectedDebugger.startTracingOnFile(tl),() => selectedDebugger.stopTracingOnFile())
    std.setLocationRelativeTo(frame)
    std.setVisible(true)



