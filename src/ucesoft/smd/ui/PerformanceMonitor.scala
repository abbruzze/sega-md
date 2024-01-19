package ucesoft.smd.ui

import org.jfree.chart.axis.AxisSpace
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.{ChartFactory, ChartPanel, StandardChartTheme}
import org.jfree.data.time.{DynamicTimeSeriesCollection, Second}
import ucesoft.smd.Clock
import ucesoft.smd.cpu.m68k.M6800X0
import ucesoft.smd.cpu.z80.Z80

import java.awt.{Color, Paint}
import java.awt.event.{ActionEvent, ActionListener, WindowAdapter, WindowEvent}
import java.lang.management.ManagementFactory
import javax.swing.*

/**
 * @author Alessandro Abbruzzetti
 *         Created on 19/01/2024 13:01  
 */
class PerformanceMonitor(frame:JFrame,m68k:M6800X0,z80:Z80,clock:Clock,closeAction: () => Unit) extends JPanel with ActionListener:
  private enum PerfState:
    case LOW_RES,NORMAL_RES

  import PerfState.*

  private final val CHART_COLOR = Color.YELLOW
  private final val LOW_RES_CHART_COLOR = Color.RED

  private val overallPerfDataset = new DynamicTimeSeriesCollection(1, 2000, new Second())
  private val m68kPerfDataset = new DynamicTimeSeriesCollection(1, 2000, new Second())
  private val z80PerfDataset = new DynamicTimeSeriesCollection(1, 2000, new Second())
  private val sysPerfDataset = new DynamicTimeSeriesCollection(1, 2000, new Second())
  private var lastM68kCycles = 0L
  private var lastZ80Cycles = 0L
  final val dialog = new JDialog(frame,"Performance monitor")
  private val timer = new Timer(1000,this)
  private val darkTheme = StandardChartTheme.createDarknessTheme()
  private var emuPlot : XYPlot = _
  private var emuPlotPaint : Paint = _
  private var state = NORMAL_RES
  private var lowResThreshold = 80
  private var lowResObservationPeriodInSec = 10
  private var resCounter = 0
  private var firstSample = true

  init()

  def setLowResThreshold(t:Int): Unit =
    lowResThreshold = t
  def setLowResObservationPeriodInSec(p:Int): Unit =
    lowResObservationPeriodInSec = p

  private def init(): Unit =
    dialog.addWindowListener(new WindowAdapter:
      override def windowClosing(e: WindowEvent): Unit =
        timer.stop()
        closeAction()
    )
    setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS))
    overallPerfDataset.setTimeBase(new Second(new java.util.Date()))
    overallPerfDataset.addSeries(Array(0f),0,"Overall performance")
    m68kPerfDataset.setTimeBase(new Second(new java.util.Date()))
    m68kPerfDataset.addSeries(Array(0f), 0, "m68k million cycles/sec")
    z80PerfDataset.setTimeBase(new Second(new java.util.Date()))
    z80PerfDataset.addSeries(Array(0f), 0, "z80 million cycles/sec")
    sysPerfDataset.setTimeBase(new Second(new java.util.Date()))
    sysPerfDataset.addSeries(Array(0f), 0, "system load")
    emuPlot = addChart("Emulator","Performance",overallPerfDataset)
    addChart("M68000", "cycles/sec", m68kPerfDataset)
    addChart("Z80", "cycles/sec", z80PerfDataset)
    addChart("Host system load", "load", sysPerfDataset)
    dialog.getContentPane.add("Center",this)
    dialog.setSize(500,700)
    timer.setRepeats(true)
    timer.start()

  private def addChart(title:String,yLabel:String,series:DynamicTimeSeriesCollection): XYPlot =
    val chart = ChartFactory.createTimeSeriesChart(title, "Time", yLabel, series, true, true, false)
    darkTheme.apply(chart)
    val plot = chart.getXYPlot
    val space = new AxisSpace()
    space.setLeft(50)
    plot.setFixedRangeAxisSpace(space)
    plot.getRenderer.setSeriesPaint(0, CHART_COLOR)
    val axis = plot.getDomainAxis
    axis.setAutoRange(true)
    axis.setFixedAutoRange(200000)
    val chartPanel = new ChartPanel(chart)
    add(chartPanel)
    plot

  override def actionPerformed(e:ActionEvent): Unit =
    overallPerfDataset.advanceTime()
    val perf = clock.getLastPerformance
    overallPerfDataset.appendData(Array(perf.toFloat))
    m68kPerfDataset.advanceTime()
    val m68kCycles = m68k.getTotalElapsedCycles
    var elapsed = if firstSample then 0 else ((m68kCycles - lastM68kCycles) / 1_000_000.0).toFloat
    m68kPerfDataset.appendData(Array(elapsed))
    lastM68kCycles = m68kCycles
    z80PerfDataset.advanceTime()
    val z80Cycles = z80.getTotalElapsedCycles
    elapsed = if firstSample then 0 else ((z80Cycles - lastZ80Cycles) / 1_000_000.0).toFloat
    z80PerfDataset.appendData(Array(elapsed))
    lastZ80Cycles = z80Cycles
    sysPerfDataset.advanceTime()
    val load = ManagementFactory.getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean]).getProcessCpuLoad * 100
    sysPerfDataset.appendData(Array(load.toFloat))

    state match
      case NORMAL_RES =>
        if perf < lowResThreshold then
          resCounter += 1
          if resCounter == lowResObservationPeriodInSec then
            state = LOW_RES
            resCounter = 0
            emuPlot.getRenderer.setSeriesPaint(0,LOW_RES_CHART_COLOR)
        else
          resCounter = 0
      case LOW_RES =>
        if perf > lowResThreshold then
          resCounter += 1
          if resCounter == lowResObservationPeriodInSec then
            state = NORMAL_RES
            resCounter = 0
            emuPlot.getRenderer.setSeriesPaint(0,CHART_COLOR)
        else
          resCounter = 0

    firstSample = false


