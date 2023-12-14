package ucesoft.smd

import com.formdev.flatlaf.FlatLightLaf
import ucesoft.smd.Clock.Clockable
import ucesoft.smd.VDP.SCREEN_WIDTH
import ucesoft.smd.audio.{FM, PSG}
import ucesoft.smd.cpu.m68k.M68000
import ucesoft.smd.cpu.z80.Z80
import ucesoft.smd.debugger.Debugger
import ucesoft.smd.ui.MessageGlassPane

import java.awt.Color
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

    inline val VDP_CLOCK_DIVIDER = 4
    inline val M68_CLOCK_DIVIDER = 7
    inline val Z80_CLOCK_DIVIDER = 15
    inline val FM_CLOCK_DIVIDER = M68_CLOCK_DIVIDER * 6

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
    val model = Model(ModelType.Oversea, vmodel, 0)
    vdp.setModel(model)
    vdp.setMasterClock(masterClock)

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
    val m68k = new M68000(mmu)
    val z80 = new Z80(mmu,Z80.EmptyIOMemory)
    z80.setPCMask(0x3FFF)
    z80.initComponent()
    vdp.set68KMemory(mmu)
    vdp.setCPUs(m68k,z80)
    mmu.setVDP(vdp)

    val fmAudio = new FM(vmodel.clockFrequency / (FM_CLOCK_DIVIDER * 24),"FM")
    fmAudio.setBufferInMillis(25)

    val psgAudio = new PSG(44100,"PSG")
    psgAudio.setCPUFrequency(vmodel.clockFrequency / Z80_CLOCK_DIVIDER)
    psgAudio.setBufferInMillis(25)

    mmu.setAudioChips(psgAudio.sn76489,fmAudio)

    val masterLoop = new Clockable:
      private final val m68Div = 4.0 / 7.0
      private final val z80Div = 4.0 / 15.0
      private var m68Acc = 0.0
      private var z80Acc = 0.0
      private val psgSampleCycles = psgAudio.getCyclesPerSample
      private var psgCycles = 0
      private var fmCycles = 0
      private var m68WaitCycles = 0
      private var z80WaitCycles = 0

      override final def clock(cycles: Long): Unit =
        vdp.clock(cycles) // VDP
        m68Acc += m68Div
        if m68Acc >= 1 then // M68 clock
          m68Acc -= 1
          if m68WaitCycles == 0 then
            m68WaitCycles = m68k.execute() - 1
          else
            m68WaitCycles -= 1
          fmCycles += 1
          if fmCycles == 6 then
            fmCycles = 0
            fmAudio.clock()
        end if // M68
        z80Acc += z80Div
        if z80Acc >= 1 then // Z80 clock
          z80Acc -= 1
          if z80WaitCycles == 0 then
            z80WaitCycles = z80.clock() - 1
          else
            z80WaitCycles -= 1
          psgCycles += 1
          if psgCycles == psgSampleCycles then
            psgCycles = 0
            psgAudio.clock()
        end if

    masterClock.setClockables(masterLoop)
    masterClock.setClockDivider(0,VDP_CLOCK_DIVIDER)
    // *******************************************************************
    busArbiter.set(m68k,z80,fmAudio)

    masterClock.setErrorHandler(t => {
      t.printStackTrace()
      sys.exit(1)
    })

    val c1 = new KeyboardPADController(0,ControllerType.PAD6Buttons,masterClock)
    val c2 = new KeyboardPADController(1,ControllerType.PAD6Buttons,masterClock)
    val c3 = new KeyboardPADController(2,ControllerType.PAD6Buttons,masterClock)
    mmu.setController(0,c1)
    mmu.setController(1,c2)
    mmu.setController(2,c3)

    f.addKeyListener(c1)

    val deb = new Debugger(m68k,mmu,mmu.get68KRAM,z80,mmu.getZ80RAM,vdp)
    deb.enableTracing(true)
    val logger = Logger.setLogger(deb.log)
    logger.setLevel(java.util.logging.Level.INFO)
    vdp.setLogger(Logger.getLogger)
    mmu.setLogger(Logger.getLogger)
    m68k.setLogger(Logger.getLogger)
    c1.setLogger(Logger.getLogger)
    busArbiter.setLogger(Logger.getLogger)
    fmAudio.setLogger(Logger.getLogger)
    psgAudio.setLogger(Logger.getLogger)

    var glassPane : MessageGlassPane = null
    val cart = new Cart("""G:\My Drive\Emulatori\Sega Mega Drive\Sonic The Hedgehog (USA, Europe).md""")
    SwingUtilities.invokeAndWait(() => {
      f.setVisible(true)
      glassPane = new MessageGlassPane(f)
    })
    glassPane.add(MessageGlassPane.Message(cart.getDomesticName, MessageGlassPane.XPOS.CENTER, MessageGlassPane.YPOS.BOTTOM, 2000, None, Some(2000)))
    mmu.setCart(cart)
    deb.setCart(cart)
    mmu.setModel(model)
    mmu.setCPUs(m68k,z80)

    m68k.reset()

    fmAudio.start()
    psgAudio.start()

    masterClock.start()
