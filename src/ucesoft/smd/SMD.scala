package ucesoft.smd

import com.formdev.flatlaf.FlatLightLaf
import org.yaml.snakeyaml.{DumperOptions, Yaml}
import ucesoft.smd.Clock.Clockable
import ucesoft.smd.VDP.SCREEN_WIDTH
import ucesoft.smd.audio.{FM, PSG}
import ucesoft.smd.controller.ControllerType.MouseStartWithCTRLAndLeft
import ucesoft.smd.controller.{ControllerType, EmptyController, KeyboardPADController, MouseController, RealPadController}
import ucesoft.smd.cpu.m68k.M68000
import ucesoft.smd.cpu.z80.Z80
import ucesoft.smd.debugger.Debugger
import ucesoft.smd.ui.MessageBoard.MessageLevel.NORMAL
import ucesoft.smd.ui.{AudioVolumePanel, MessageBoard, MessageGlassPane, MouseHider, PerformanceMonitor}

import java.awt.Color
import java.io.{FileOutputStream, PrintWriter, StringReader, StringWriter}
import java.util.Properties
import javax.swing.*

/**
 * @author Alessandro Abbruzzetti
 *         Created on 20/10/2023 18:28  
 */
object SMD:
  def main(args:Array[String]): Unit =
    inline val VDP_CLOCK_INDEX = 0
    inline val M68_CLOCK_INDEX = 1
    inline val Z80_CLOCK_INDEX = 2

    inline val VDP_CLOCK_DIVIDER = 5 // start with H32
    inline val M68_CLOCK_DIVIDER = 7
    inline val Z80_CLOCK_DIVIDER = 15
    inline val FM_CLOCK_DIVIDER = M68_CLOCK_DIVIDER * 6

    inline val MEGA_DRIVE_VERSION = 0

    Logger.getLogger.setLevel(java.util.logging.Level.SEVERE)

    SwingUtilities.invokeAndWait(() => {
      FlatLightLaf.setup()
      JFrame.setDefaultLookAndFeelDecorated(false)
      JDialog.setDefaultLookAndFeelDecorated(false)
      UIManager.setLookAndFeel("com.formdev.flatlaf.FlatDarculaLaf")
    })

    val vmodel = VideoType.NTSC
    val masterClock = new Clock("master",vmodel.clockFrequency)
    val busArbiter = new BusArbiter

    val vdp = new VDP(busArbiter)
    val model = Model(ModelType.Oversea, vmodel, MEGA_DRIVE_VERSION)
    vdp.setModel(model)

    val f = new JFrame("Test SMD")

    f.setIconImage(new ImageIcon(getClass.getResource("/resources/sonic_ring.png")).getImage)
    val display = new Display(SCREEN_WIDTH, vmodel.totalLines, "Test SMD", f, masterClock)
    vdp.setDisplay(display)
    
    display.setClipArea(model.videoType.getClipArea(h40 = false).getTuple)
    display.setPreferredSize(model.videoType.getClipArea(h40 = true).getPreferredSize(2))
    f.getContentPane.add("Center", display)
    f.pack()

    vdp.initComponent()

    val mmu = new MMU(busArbiter)
    //mmu.enableOSROM(true)
    val m68k = new M68000(mmu)
    val z80 = new Z80(mmu,mmu)
    z80.setPCMask(0x3FFF)
    z80.initComponent()
    vdp.set68KMemory(mmu)
    vdp.setCPUs(m68k,z80)
    mmu.setVDP(vdp)

    val fmAudio = new FM(vmodel.clockFrequency / (FM_CLOCK_DIVIDER * 24),"Ym3438")
    fmAudio.setBufferInMillis(15)

    val psgAudio = new PSG(44100,"PSG")
    psgAudio.setCPUFrequency(vmodel.clockFrequency / Z80_CLOCK_DIVIDER)
    psgAudio.setBufferInMillis(15)

    mmu.setAudioChips(psgAudio,fmAudio)

    val masterLoop = new Clockable with VDP.VDPChangeClockRateListener:
      private inline val MULTIPLIER = 65536
      private final var m68Div = 0
      private final var z80Div = 0
      private var m68Acc = 0
      private var z80Acc = 0
      private val psgSampleCycles = psgAudio.getCyclesPerSample
      private var psgCycles = 0
      private var fmCycles = 0
      private var m68WaitCycles = 0
      private var z80WaitCycles = 0
      private final val DIV_68K = M68_CLOCK_DIVIDER.toDouble
      private final val Z80_68K = Z80_CLOCK_DIVIDER.toDouble

      clockRateChanged(VDP_CLOCK_DIVIDER)

      override final def clockRateChanged(rate: Int): Unit =
        if m68Div != 0 then
          masterClock.setClockDivider(0,rate)
        m68Div = math.round((rate / DIV_68K) * MULTIPLIER).toInt
        z80Div = math.round((rate / Z80_68K) * MULTIPLIER).toInt

      override final def clock(cycles: Long): Unit =
        vdp.clock(cycles) // VDP
        if m68k.isComponentEnabled then
          m68Acc += m68Div
          if m68Acc >= MULTIPLIER then // M68 clock
            m68Acc -= MULTIPLIER
            if m68WaitCycles == 0 then
              m68WaitCycles = m68k.execute() - 1 //math.round((m68k.execute() - 1) * 1.05f)
            else
              m68WaitCycles -= 1
            fmCycles += 1
            if fmCycles == 6 then
              fmCycles = 0
              fmAudio.clock()
          end if // M68
        if z80.isComponentEnabled then
          z80Acc += z80Div
          if z80Acc >= MULTIPLIER then // Z80 clock
            z80Acc -= MULTIPLIER
            if z80WaitCycles == 0 then
              z80WaitCycles = z80.clock() - 1
            else
              z80WaitCycles -= 1
            psgCycles += 1
            if psgCycles == psgSampleCycles then
              psgCycles = 0
              psgAudio.clock()
          end if

    vdp.setClockRateChangeListener(masterLoop)
    masterClock.setClockables(masterLoop)
    masterClock.setClockDivider(0, VDP_CLOCK_DIVIDER)

    // *******************************************************************
    busArbiter.set(m68k,z80,fmAudio)

    masterClock.setErrorHandler(t => {
      t.printStackTrace()
      sys.exit(1)
    })

    val keyController = new KeyboardPADController(f,new Properties(),0,masterClock)
    val empty1Controller = new EmptyController(1)//new MouseController(2,display)
    val empty2Controller = new EmptyController(2)
    val usbProp = new Properties
    usbProp.setProperty("controller.0.name","Wireless Controller")
    val usbController = new RealPadController(usbProp,0,masterClock)//new KeyboardPADController(f,new Properties(),1,ControllerType.PAD6Buttons,masterClock)
    mmu.setController(0,keyController)
    mmu.setController(1,empty1Controller)
    mmu.setController(2,empty2Controller)

    //c2.mouseEnabled(true)
    //c2.setControllerType(MouseStartWithCTRLAndLeft)

    var glassPane: MessageGlassPane = null
    SwingUtilities.invokeAndWait(() => {
      f.setVisible(true)
      glassPane = new MessageGlassPane(f)
    })

    glassPane.setLevel(NORMAL)
    vdp.setMessageListener(glassPane)

    val deb = new Debugger(m68k,mmu,mmu.get68KRAM,z80,mmu.getZ80RAM,vdp,() => {})
    deb.setMessageBoard(glassPane)
    deb.enableTracing(true)
    val logger = Logger.setLogger(deb.log)
    logger.setLevel(java.util.logging.Level.INFO)
    vdp.setLogger(Logger.getLogger)
    mmu.setLogger(Logger.getLogger)
    m68k.setLogger(Logger.getLogger)
    keyController.setLogger(Logger.getLogger)
    busArbiter.setLogger(Logger.getLogger)
    fmAudio.setLogger(Logger.getLogger)
    psgAudio.setLogger(Logger.getLogger)


    val cart = new Cart(Cart.CartFile("""G:\My Drive\Emulatori\Sega Mega Drive\Sonic 3D Blast ~ Sonic 3D Flickies' Island (USA, Europe, Korea).md"""))
    println(cart)

    glassPane.addMessage(MessageBoard.builder.message("Scala Mega Drive Emulator").adminLevel().italic().bold().xcenter().ycenter().delay(2000).fadingMilliseconds(500).showLogo().color(Color.YELLOW).build())
    glassPane.addMessage(MessageBoard.builder.
      message(cart.getOverseaName).
      adminLevel().
      bold().
      xcenter().
      ybottom().
      delay(2000).
      fadingMilliseconds(2000).
      hideLogo().
      build()
    )
    glassPane.addMessage(MessageBoard.builder.hideLogo().build())
    mmu.setCart(cart)
    deb.setCart(cart)
    mmu.setModel(model)
    mmu.setCPUs(m68k,z80)

    m68k.reset()

    fmAudio.start()
    psgAudio.start()

    m68k.setComponentEnabled(false)
    z80.setComponentEnabled(false)
    // The execution delay between the VDP and the M68000
    // is measured to be approximately 13 milliseconds with a logic analyzer.
    masterClock.scheduleMillis(10,_ => {
      m68k.setComponentEnabled(true)
      z80.setComponentEnabled(true)
    })

    MouseHider.hideMouseOn(display)

    val options = new DumperOptions
    options.setIndent(2)
    options.setPrettyFlow(true)
    options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
    val yaml = new Yaml(options)
    val sw = new StringWriter()
    //yaml.dump(psgAudio.createComponentState().build(),sw)
    val rootSB = new StateBuilder()
    vdp.createState(rootSB)
//    Cart.createState(cart,rootSB)
    yaml.dump(rootSB.build(),sw)
    val out = new PrintWriter(new FileOutputStream("""C:\Users\ealeame\OneDrive - Ericsson\Desktop\vdp.yaml"""))
    out.println(sw.toString)
    out.close()
    //println(sw)

    //val state = yaml.load[java.util.Map[String,AnyRef]](new StringReader(sw.toString))
    //psgAudio.restoreComponentState(new StateBuilder(state))
    //println(Cart.restoreState(new StateBuilder(state),false))

    masterClock.start()

    //val pm = new PerformanceMonitor(f,m68k,z80,masterClock,Array(fmAudio,psgAudio),() => println("Perf closed"))
    //pm.dialog.setVisible(true)
    //val audioPanel = new AudioVolumePanel(f,Array(fmAudio,psgAudio),() => {})
    //audioPanel.dialog.setVisible(true)