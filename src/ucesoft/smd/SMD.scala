package ucesoft.smd

import ucesoft.smd.Clock.Clockable
import ucesoft.smd.VDP.SCREEN_WIDTH
import ucesoft.smd.cpu.m68k.M68000
import ucesoft.smd.cpu.z80.Z80
import ucesoft.smd.debugger.{Default68KDisassembleAnnotator, Debugger}

import javax.swing.JFrame

/**
 * @author Alessandro Abbruzzetti
 *         Created on 20/10/2023 18:28  
 */
object SMD:
  def main(args:Array[String]): Unit =
    val masterClock = new Clock("master",53_693_175)
    //masterClock.setWarpMode(true)
    //masterClock.setM68KClockDivider(7)
    //masterClock.setZ80ClockDivider(15)
    //masterClock.setVDPClockDivider(5)

    val vdp = new VDP
    val vmodel = VideoType.NTSC
    val model = Model(ModelType.Oversea, vmodel, 0)
    vdp.setModel(model)
    vdp.setMasterClock(masterClock)

    val f = new JFrame("Test SMD")
    val display = new Display(SCREEN_WIDTH, vmodel.totalLines, "Test SMD", f, masterClock)
    //java.util.Arrays.fill(display.displayMem, java.awt.Color.RED.getRGB)
    vdp.setDisplay(display)
    
    //display.setPreferredSize(new java.awt.Dimension(SCREEN_WIDTH * 2, vmodel.totalLines * 2))
    display.setClipArea(model.videoType.getClipArea(h40 = false).getTuple)
    display.setPreferredSize(model.videoType.getClipArea(h40 = true).getPreferredSize(2))
    f.getContentPane.add("Center", display)
    f.pack()

    vdp.initComponent()

    val mmu = new MMU
    val m68k = new M68000(mmu)
    val z80 = new Z80(mmu,Z80.EmptyIOMemory)
    z80.initComponent()
    vdp.set68KMemory(mmu)
    vdp.setM68K(m68k)
    mmu.setVDP(vdp)

    val n68kClock = new Clockable:
      private var remainingCycles = 0
      override def clock(cycles: Long): Unit =
        if remainingCycles == 0 then
          remainingCycles = m68k.execute() - 1
        else
          remainingCycles -= 1
    val vdpClock = new Clockable:
      override def clock(cycles: Long): Unit = vdp.clock(cycles)

    masterClock.setClockables(vdpClock,n68kClock)
    masterClock.setClockDivider(0,4)
    masterClock.setClockDivider(1,7)

    masterClock.setErrorHandler(t => {
      t.printStackTrace()
      sys.exit(1)
    })

    //masterClock.setComponents(m68k,z80,vdp)

    val c1 = new KeyboardPADController(0,ControllerType.PAD6Buttons,masterClock)
    val c2 = new KeyboardPADController(1,ControllerType.PAD6Buttons,masterClock)
    val c3 = new KeyboardPADController(2,ControllerType.PAD6Buttons,masterClock)
    mmu.setController(0,c1)
    mmu.setController(1,c2)
    mmu.setController(2,c3)

    f.addKeyListener(c1)

    val deb = new Debugger(m68k,mmu,mmu.get68KRAM,z80,mmu,vdp,new Default68KDisassembleAnnotator)
    deb.enableTracing(true)
    val logger = Logger.setLogger(deb.log)
    logger.setLevel(java.util.logging.Level.INFO)
    vdp.setLogger(Logger.getLogger)
    mmu.setLogger(Logger.getLogger)
    m68k.setLogger(Logger.getLogger)
    c1.setLogger(Logger.getLogger)

    f.setVisible(true)

    val cart = new Cart("""G:\My Drive\Emulatori\Sega Mega Drive\Sonic The Hedgehog (USA, Europe).md""")
    mmu.setCart(cart)
    deb.setCart(cart)
    mmu.setModel(model)
    mmu.setCPUs(m68k,z80)

    m68k.reset()

    masterClock.start()
