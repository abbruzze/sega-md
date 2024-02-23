package ucesoft.smd.ui

import com.formdev.flatlaf.FlatLightLaf
import ucesoft.smd.VDP.SCREEN_WIDTH
import ucesoft.smd.controller.{Controller, EmptyController, KeyboardPADController, MouseController, RealPadController}
import ucesoft.smd.debugger.Debugger
import ucesoft.smd.misc.Preferences
import ucesoft.smd.ui.MessageBoard.MessageLevel.ADMIN
import ucesoft.smd.{Cart, Display, Logger, MegaDrive, MessageBus, ui}

import java.awt.event.{WindowAdapter, WindowEvent}
import java.io.{File, FileInputStream}
import java.nio.file.StandardCopyOption
import java.util.zip.ZipInputStream
import javax.swing.filechooser.FileFilter
import javax.swing.{ImageIcon, JCheckBoxMenuItem, JDialog, JFileChooser, JFrame, JMenu, JMenuBar, JMenuItem, JOptionPane, KeyStroke, SwingUtilities, UIManager}
import scala.collection.mutable.ListBuffer

object MegaDriveUI:
  def main(args:Array[String]): Unit =
    if System.getProperty("swing.defaultlaf") == null then
      FlatLightLaf.setup()
      JFrame.setDefaultLookAndFeelDecorated(false)
      JDialog.setDefaultLookAndFeelDecorated(false)
      UIManager.setLookAndFeel("com.formdev.flatlaf.FlatDarculaLaf")

    val preBuildLogs = new ListBuffer[String]
    Logger.setLogger(msg => preBuildLogs += msg)
    val md = new MegaDriveUI
    preBuildLogs.toList.foreach(Logger.getLogger.addLog)
    try
      md.boot()
      md.configure(args)
      md.run()
    catch
      case i: Preferences.PreferenceIllegalArgumentException =>
        println(s"Bad command line argument: ${i.getMessage}")
        sys.exit(100)
      case t: Throwable =>
        md.errorHandler(t)
        sys.exit(1)
end MegaDriveUI
/**
 * @author Alessandro Abbruzzetti
 *         Created on 20/02/2024 15:49  
 */
class MegaDriveUI:
  // motherboard
  private val megaDrive = new MegaDrive

  private var frame : JFrame = _
  private var glassPane: MessageGlassPane = _
  private var debugger : Debugger = _
  private var cart : Cart = _

  private var audioPanel : AudioVolumePanel = _
  private var performanceMonitor : PerformanceMonitor = _

  private var lastDirectory = new File(scala.util.Properties.userHome)
  private var fixChecksum = false

  // MenuItems
  private val cartMenu = new JMenu("Cart")
  private val debugMenu = new JMenu("Debug")
  private val debuggerCB = new JCheckBoxMenuItem("Debugger")
  private val warpModeCB = new JCheckBoxMenuItem("Warp mode")
  private val audioPanelCB = new JCheckBoxMenuItem("Audio settings")
  private val pauseCB = new JCheckBoxMenuItem("Pause")

  def boot(): Unit =
    // main frame
    frame = new JFrame(s"Mega Drive emulator v1.0") // TODO add Version generated class info
    frame.setResizable(false)
    frame.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = shutdown()
    })
    frame.setIconImage(new ImageIcon(getClass.getResource("/resources/sonic_ring.png")).getImage)
    // display
    val display = new Display(SCREEN_WIDTH, megaDrive.model.videoType.totalLines, frame.getTitle, frame, megaDrive.masterClock)
    megaDrive.setDisplay(display)
    display.setClipArea(megaDrive.model.videoType.getClipArea(h40 = false).getTuple)                // starts with H32
    display.setPreferredSize(megaDrive.model.videoType.getClipArea(h40 = true).getPreferredSize(2)) // preferred size is relative to H40, double size
    frame.getContentPane.add("Center",display)
    frame.pack()

    debugger = new Debugger(megaDrive.m68k,megaDrive.mmu,megaDrive.mmu.get68KRAM,megaDrive.z80,megaDrive.mmu.getZ80RAM,megaDrive.vdp,glassPane,() => debuggerCB.setSelected(false))
    val logger = Logger.setLogger(debugger.log)
    megaDrive.setLogger(logger)

    audioPanel = new AudioVolumePanel(frame,Array(megaDrive.fmAudio,megaDrive.psgAudio),() => audioPanelCB.setSelected(false))
    MessageBus.add(audioPanel)

    buildMenuBar()

    // DND
    frame.setTransferHandler(new DNDHandler(handleDND))
  end boot

  protected def makeController(pos: Int): Controller =
    megaDrive.conf.getProperty(Controller.formatProp(Controller.CONTROLLER_DEVICE_PROP, pos)) match
      case KeyboardPADController.DEVICE_PROP_VALUE | null =>
        new KeyboardPADController(frame,megaDrive.conf,pos,megaDrive.masterClock)
      case RealPadController.DEVICE_PROP_VALUE =>
        new RealPadController(megaDrive.conf,pos,megaDrive.masterClock)
      case MouseController.DEVICE_PROP_VALUE =>
        new MouseController(pos,megaDrive.display)
      case unknown =>
        Logger.getLogger.warning("Cannot make controller %d from configuration file: unknown device %s", pos, unknown)
        EmptyController(pos)

  private def swing(action : => Unit) : Unit = SwingUtilities.invokeLater(() => action)

  private def pause(): Unit =
    megaDrive.masterClock.pause()
    pauseCB.setSelected(true)
    glassPane.addMessage(MessageBoard.builder.message("Paused").adminLevel().bold().xleft().ytop().delay().fadingMilliseconds(500).build())

  private def play(): Unit =
    glassPane.interrupt()
    pauseCB.setSelected(false)
    megaDrive.masterClock.play()

  private def shutdown(): Unit = ??? // TODO

  private def errorHandler(t:Throwable): Unit =
    t.printStackTrace()
    JOptionPane.showOptionDialog(
      frame,
      s"Unexpected error: $t",
      "Unexpected error",
      JOptionPane.YES_NO_OPTION,
      JOptionPane.ERROR_MESSAGE,
      null,
      Array("Ignore","Open debugger"),
      "Ignore"
    ) match
      case JOptionPane.NO_OPTION =>
        openDebugger()
      case _ => // do nothing
  end errorHandler
  private def openDebugger(): Unit =
    debugger.enableTracing(true)

  private def configure(args:Array[String]): Unit = {} // TODO

  private def run(): Unit =
    val log = Logger.getLogger
    log.info("Building the system ...")

    megaDrive.initComponent()

    log.setLevel(java.util.logging.Level.WARNING)

    swing {
      // setting old coordinates
      val xy = megaDrive.conf.getProperty(Preferences.XY_PREF)
      if xy == null then
        frame.setLocationByPlatform(true)
      else
        try
          val Array(x, y) = xy.split(",").map(_.toInt)
          frame.setLocation(x, y)
        catch
          case _: Throwable =>
            frame.setLocationByPlatform(true)

      frame.setVisible(true)
      glassPane = new MessageGlassPane(frame)
      glassPane.setLevel(ADMIN)
      showWelcome()
    }
  end run

  private def showWelcome(): Unit =
    // TODO
    glassPane.addMessage(MessageBoard.builder.message("Welcome").adminLevel().bold().xcenter().ycenter().delay(2000).fadingMilliseconds(500).build())

  private def reset(hard:Boolean): Unit =
    if !megaDrive.masterClock.isPaused then
      pause()
    try
      if hard then
        megaDrive.hardResetComponent()
      else
        megaDrive.resetComponent()
    finally
      if !megaDrive.masterClock.isRunning then
        megaDrive.masterClock.start()
        megaDrive.fmAudio.start()
        megaDrive.psgAudio.start()
      play()

  private def buildMenuBar(): Unit =
    val menubar = new JMenuBar
    frame.setJMenuBar(menubar)

    val fileMenu = new JMenu("File")
    debugMenu.setEnabled(false)
    val toolsMenu = new JMenu("Tools")
    cartMenu.setEnabled(false)
    val helpMenu = new JMenu("Help")

    menubar.add(fileMenu)
    menubar.add(debugMenu)
    menubar.add(toolsMenu)
    menubar.add(cartMenu)
    menubar.add(helpMenu)

    buildFileMenu(fileMenu)
    buildToolsMenu(toolsMenu)
    buildDebugMenu(debugMenu)
  end buildMenuBar

  private def buildFileMenu(fileMenu:JMenu): Unit =
    val loadCartItem = new JMenuItem("Load cart ...")
    fileMenu.add(loadCartItem)
    loadCartItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L,java.awt.event.InputEvent.ALT_DOWN_MASK))
    loadCartItem.addActionListener(_ => attachCart(None))
  private def buildDebugMenu(debugMenu:JMenu): Unit =
    debugMenu.add(debuggerCB)
    debuggerCB.addActionListener(_ => debugger.showDebugger(debuggerCB.isSelected))
    debuggerCB.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_D, java.awt.event.InputEvent.ALT_DOWN_MASK))
  private def buildToolsMenu(toolsMenu:JMenu): Unit =
    toolsMenu.add(pauseCB)
    pauseCB.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P, java.awt.event.InputEvent.ALT_DOWN_MASK))
    pauseCB.addActionListener(_ => if pauseCB.isSelected then pause() else play())
    pauseCB.setEnabled(false)
    toolsMenu.add(audioPanelCB)
    audioPanelCB.addActionListener(_ => audioPanel.dialog.setVisible(audioPanelCB.isSelected))
    audioPanelCB.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A, java.awt.event.InputEvent.ALT_DOWN_MASK))
    val perfMonitorItem = new JCheckBoxMenuItem("Performance monitor")
    toolsMenu.add(perfMonitorItem)
    perfMonitorItem.addActionListener(_ => {
      if perfMonitorItem.isSelected then
        performanceMonitor = new PerformanceMonitor(frame,
          megaDrive.m68k,
          megaDrive.z80,
          megaDrive.masterClock,
          Array(megaDrive.fmAudio,megaDrive.psgAudio),
          () => perfMonitorItem.setSelected(false))
        performanceMonitor.dialog.setVisible(true)
      else
        performanceMonitor.shutdown()
        performanceMonitor.dialog.dispose()
        performanceMonitor = null
    })
    toolsMenu.add(warpModeCB)
    warpModeCB.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, java.awt.event.InputEvent.ALT_DOWN_MASK))
    warpModeCB.addActionListener(_ => setWarpMode(warpModeCB.isSelected))

  private def handleDND(file:File) : Unit = attachCart(Some(file))

  private def attachCart(file:Option[File]): Unit =
    if cart != null then
      if !megaDrive.masterClock.isPaused then
        pause()

    val fileToLoad = file match
      case None =>
        chooseCart()
      case Some(_) =>
        file

    fileToLoad match
      case Some(f) =>
        loadCart(f) match
          case Some(newCart) =>
            playCart(newCart)
          case None =>
            if cart != null then
              play()
      case None =>

  private def playCart(newCart:Cart): Unit =
    if cart != null then
      detachCart()
    cart = newCart
    MessageBus.send(MessageBus.CartInserted(this,cart))
    glassPane.addMessage(MessageBoard.builder.
      message(cart.getOverseaName).
      adminLevel().
      bold().
      xcenter().
      ybottom().
      delay(2000).
      fadingMilliseconds(2000).
      build()
    )
    debugger.setCart(cart)
    debugMenu.setEnabled(true)
    pauseCB.setEnabled(true)
    MouseHider.hideMouseOn(megaDrive.display)

    reset(hard = true)

  private def detachCart(): Unit =
    cart = null
    debugMenu.setEnabled(false)
    debugger.enableTracing(false)
    debugger.showDebugger(false)
    pauseCB.setEnabled(false)
    MouseHider.showMouseOn(megaDrive.display)
    // TODO

  private def chooseCart(): Option[File] =
    val fc = new JFileChooser()
    fc.setFileFilter(new FileFilter {
      override def accept(f: File): Boolean =
        val fileName = f.getName.toUpperCase()
        fileName.endsWith(".BIN") || fileName.endsWith(".MD") || fileName.endsWith(".SMD") || fileName.endsWith(".ZIP")

      override def getDescription: String = "Mega Drive cart"
    })
    fc.setCurrentDirectory(lastDirectory)
    fc.showOpenDialog(frame) match
      case JFileChooser.APPROVE_OPTION =>
        Some(fc.getSelectedFile)
      case _ =>
        None

  private def extractFromZIP(file:File): Option[File] =
    try
      val zis = new ZipInputStream(new FileInputStream(file))
      val entry = zis.getNextEntry
      if entry != null && !entry.isDirectory then
        val fileName = entry.getName.toUpperCase()
        if fileName.endsWith(".BIN") || fileName.endsWith(".MD") || fileName.endsWith(".SMD") then
          val tmpFile = File.createTempFile(fileName,null)
          tmpFile.deleteOnExit()
          java.nio.file.Files.copy(zis,tmpFile.toPath,StandardCopyOption.REPLACE_EXISTING)
          zis.close()
          Some(tmpFile)
        else
          JOptionPane.showMessageDialog(frame,s"Error while extracting from zip file '${file.getName}': no suitable cartridge extension found","Zip error",JOptionPane.ERROR_MESSAGE)
          None
      else
        JOptionPane.showMessageDialog(frame,s"Error while extracting from zip file '${file.getName}': directory found","Zip error",JOptionPane.ERROR_MESSAGE)
        None
    catch
      case t:Throwable =>
        JOptionPane.showMessageDialog(frame,s"Error while extracting from zip file '${file.getName}': $t","Zip error",JOptionPane.ERROR_MESSAGE)
        None
  private def loadCart(file:File): Option[Cart] =
    var f = file
    if file.getName.toUpperCase().endsWith(".ZIP") then
      extractFromZIP(file) match
        case Some(zf) =>
          f = zf
        case None =>
          return None
    val loadingDialog = new LoadingDialog(frame, s"Loading cart ${file.getName} ...")
    loadingDialog.setVisible(true)
    try
      val cart = new Cart(f.toString, None, fixChecksum = fixChecksum)
      println(cart)
      if cart.getSystemType == Cart.SYSTEM_TYPE.UNKNOWN then
        JOptionPane.showMessageDialog(frame, s"Unknown system for cartridge '${file.getName}'", "Cartridge loading warning", JOptionPane.WARNING_MESSAGE)
      Some(cart)
    catch
      case t: Throwable =>
        JOptionPane.showMessageDialog(frame, s"Error while loading cartridge '${file.getName}': $t", "Cartridge loading error", JOptionPane.ERROR_MESSAGE)
        None
    finally
      loadingDialog.dispose()

  private def setWarpMode(on:Boolean) : Unit =
    MessageBus.send(MessageBus.WarpModeMessage(this,on))
    megaDrive.masterClock.setWarpMode(on)
    glassPane.addMessage(MessageBoard.builder.message(s"Warp mode ${if on then "on" else "off"}").adminLevel().bold().xleft().ybottom().delay(1000).fadingMilliseconds(500).build())
  end setWarpMode

  private def setAudio(on:Boolean): Unit =
    MessageBus.send(MessageBus.AudioEnabledMessage(this,on))


