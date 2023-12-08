package ucesoft.smd

import com.formdev.flatlaf.FlatLightLaf
import ucesoft.smd.Clock.Clockable
import ucesoft.smd.VDP.SCREEN_WIDTH
import ucesoft.smd.audio.PSG
import ucesoft.smd.cpu.m68k.M68000
import ucesoft.smd.cpu.z80.Z80
import ucesoft.smd.debugger.{Debugger, Default68KDisassembleAnnotator}

import javax.swing.{JDialog, JFrame, UIManager}

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

    FlatLightLaf.setup()
    JFrame.setDefaultLookAndFeelDecorated(false)
    JDialog.setDefaultLookAndFeelDecorated(false)
    UIManager.setLookAndFeel("com.formdev.flatlaf.FlatDarculaLaf")

    val vmodel = VideoType.NTSC
    val masterClock = new Clock("master",vmodel.clockFrequency)
    val busArbiter = new BusArbiter

    val vdp = new VDP(busArbiter)
    val model = Model(ModelType.Oversea, vmodel, 0)
    vdp.setModel(model)
    vdp.setMasterClock(masterClock)

    val f = new JFrame("Test SMD")
    val display = new Display(SCREEN_WIDTH, vmodel.totalLines, "Test SMD", f, masterClock)
    vdp.setDisplay(display)
    
    display.setClipArea(model.videoType.getClipArea(h40 = false).getTuple)
    display.setPreferredSize(model.videoType.getClipArea(h40 = true).getPreferredSize(2))
    f.getContentPane.add("Center", display)
    f.pack()

    vdp.initComponent()

    //val z80MasterClock = new Clock("master-z80", vmodel.clockFrequency)
    val psgMasterClock = new Clock("master-psg", vmodel.clockFrequency / Z80_CLOCK_DIVIDER)

    val mmu = new MMU(busArbiter)
    val m68k = new M68000(mmu)
    val z80 = new Z80(mmu,Z80.EmptyIOMemory)
    z80.setPCMask(0x3FFF)
    busArbiter.set(m68k,z80)
    z80.initComponent()
    vdp.set68KMemory(mmu)
    vdp.setCPUs(m68k,z80)
    mmu.setVDP(vdp)
    // M680000 ===========================================================
    val m68kClock = new Clockable:
      //private var remainingCycles = 0
      override def clock(cycles: Long): Unit =
        masterClock.setWait(M68_CLOCK_INDEX,m68k.execute() - 1)
        /*if remainingCycles == 0 then
          remainingCycles = m68k.execute() - 1
        else
          remainingCycles -= 1*/
    // ===================================================================
    // VDP ===============================================================
    val vdpClock = new Clockable:
      override def clock(cycles: Long): Unit = vdp.clock(cycles)
    // ===================================================================
    // PSG ===============================================================
    val psgAudio = new PSG(44100,"PSG")
    mmu.setPSG(psgAudio.sn76489)
    psgAudio.setCPUFrequency(vmodel.clockFrequency / 15)
    psgAudio.setBufferInMillis(25)
    psgAudio.start()
    val psgClock = new Clockable:
      override def clock(cycles: Long): Unit = psgAudio.externalClock()
    // ===================================================================
    // Z80 ===============================================================
    val z80Clock = new Clockable:
      //private var remainingCycles = 0
      override def clock(cycles:Long): Unit =
        masterClock.setWait(Z80_CLOCK_INDEX, z80.clock() - 1)
        /*if remainingCycles == 0 then
          remainingCycles = z80.clock() - 1
        else
          remainingCycles -= 1*/

        //psgAudio.clock()
    // ===================================================================

    psgMasterClock.setClockables(psgClock)
    psgMasterClock.setClockDivider(0,psgAudio.getCyclesPerSample)

    //z80MasterClock.setClockables(z80Clock)
    //z80MasterClock.setClockDivider(0,15)

    masterClock.setClockables(vdpClock,m68kClock,z80Clock)
    masterClock.setClockDivider(VDP_CLOCK_INDEX,VDP_CLOCK_DIVIDER)
    masterClock.setClockDivider(M68_CLOCK_INDEX,M68_CLOCK_DIVIDER)
    masterClock.setClockDivider(Z80_CLOCK_INDEX,Z80_CLOCK_DIVIDER)

    /*z80MasterClock.setErrorHandler(t => {
      t.printStackTrace()
      sys.exit(1)
    })*/
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

    val deb = new Debugger(m68k,mmu,mmu.get68KRAM,z80,mmu,vdp,dualClock = false)
    deb.enableTracing(true)
    val logger = Logger.setLogger(deb.log)
    logger.setLevel(java.util.logging.Level.INFO)
    vdp.setLogger(Logger.getLogger)
    mmu.setLogger(Logger.getLogger)
    m68k.setLogger(Logger.getLogger)
    c1.setLogger(Logger.getLogger)
    busArbiter.setLogger(Logger.getLogger)

    f.setVisible(true)

    val cart = new Cart("""G:\My Drive\Emulatori\Sega Mega Drive\R-Type DEMO 1.2.bin""")
    mmu.setCart(cart)
    deb.setCart(cart)
    mmu.setModel(model)
    mmu.setCPUs(m68k,z80)

    m68k.reset()

    masterClock.start()
    psgMasterClock.start()
    //z80MasterClock.start()
