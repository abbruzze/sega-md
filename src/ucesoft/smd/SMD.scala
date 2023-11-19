package ucesoft.smd

import ucesoft.smd.VDP.SCREEN_WIDTH
import ucesoft.smd.cpu.m68k.M68000
import ucesoft.smd.cpu.z80.Z80
import ucesoft.smd.debugger.{Default68KDisassembleAnnotator, M68kDebugger}

import javax.swing.JFrame

/**
 * @author Alessandro Abbruzzetti
 *         Created on 20/10/2023 18:28  
 */
object SMD:
  def main(args:Array[String]): Unit =
    val masterClock = new Clock("master",53_693_175)
    //masterClock.setWarpMode(true)
    masterClock.setM68KClockDivider(7)
    masterClock.setZ80ClockDivider(15)
    masterClock.setVDPClockDivider(5)

    val vdp = new VDP
    val vmodel = VideoType.NTSC
    val model = Model(ModelType.Oversea, vmodel, 0)
    vdp.setModel(model)
    vdp.setMasterClock(masterClock)

    val f = new JFrame("Test SMD")
    val display = new Display(SCREEN_WIDTH, vmodel.totalLines, "Test SMD", f, masterClock)
    //java.util.Arrays.fill(display.displayMem, java.awt.Color.RED.getRGB)
    vdp.setDisplay(display)

    //display.setClipArea(32,13,379,256)
    display.setClipArea(model.videoType.getClipArea(h40 = false).getTuple)
    //display.setPreferredSize(new java.awt.Dimension(SCREEN_WIDTH * 2, vmodel.totalLines * 2))
    //display.setPreferredSize(new java.awt.Dimension((13 + 320 + 14) * 2,(11 + 224 + 8) * 2))
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

    masterClock.setComponents(m68k,z80,vdp)

    val c1 = new KeyboardPADController(0,ControllerType.PAD6Buttons,masterClock)
    val c2 = new KeyboardPADController(1,ControllerType.PAD6Buttons,masterClock)
    val c3 = new KeyboardPADController(2,ControllerType.PAD6Buttons,masterClock)
    mmu.setController(0,c1)
    mmu.setController(1,c2)
    mmu.setController(2,c3)

    f.addKeyListener(c1)

    val deb = new M68kDebugger(m68k,mmu,mmu.get68KRAM,vdp,new Default68KDisassembleAnnotator)
    deb.enableTracing(true)
    val logger = Logger.setLogger(deb.log)
    logger.setLevel(java.util.logging.Level.INFO)
    vdp.setLogger(Logger.getLogger)
    mmu.setLogger(Logger.getLogger)
    m68k.setLogger(Logger.getLogger)
    c1.setLogger(Logger.getLogger)

    f.setVisible(true)

    val cart = new Cart("""G:\My Drive\Emulatori\Sega Mega Drive\testrom\TEST_COLOR_1536.bin""")
    mmu.setCart(cart)
    deb.setCart(cart)
    mmu.setModel(model)
    mmu.setCPUs(m68k,z80)

    m68k.reset()

    masterClock.start()
