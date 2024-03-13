package ucesoft.smd.ui

import com.formdev.flatlaf.FlatLightLaf
import org.yaml.snakeyaml.{DumperOptions, Yaml}
import ucesoft.smd.VDP.SCREEN_WIDTH
import ucesoft.smd.controller.*
import ucesoft.smd.debugger.Debugger
import ucesoft.smd.misc.{CartInfoPanel, FullScreenMode, GIFPanel, Preferences}
import ucesoft.smd.ui.MessageBoard.MessageLevel.ADMIN
import ucesoft.smd.*
import ucesoft.smd.cheat.Cheat.CheatCode
import ucesoft.smd.cheat.CheatManager

import java.awt.event.{KeyAdapter, KeyEvent, WindowAdapter, WindowEvent}
import java.io.*
import java.util.Properties
import java.util.logging.Level
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import javax.swing.filechooser.FileFilter
import javax.swing.*
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
class MegaDriveUI extends MessageBus.MessageListener with CheatManager:
  import scala.compiletime.uninitialized

  private inline val MESSAGE_STD_WAIT = 2000
  // motherboard
  private val megaDrive = new MegaDrive

  private var frame : JFrame = uninitialized
  private var glassPane: MessageGlassPane = uninitialized
  private var debugger : Debugger = uninitialized
  private var cart : Cart = uninitialized

  private var audioPanel : AudioVolumePanel = uninitialized
  private var performanceMonitor : PerformanceMonitor = uninitialized

  private var lastDirectory = new File(scala.util.Properties.userHome)
  private var lastSaveStateFile : String = uninitialized
  private var fixChecksum = false

  private var extraRAMDirectory : File = uninitialized

  // MenuItems
  private val stateMenu = new JMenu("State")
  private val cartMenu = new JMenu("Cart")
  private val debugMenu = new JMenu("Debug")
  private val quickSaveStateItem = new JMenuItem("Quick save state")
  private val debuggerCB = new JCheckBoxMenuItem("Debugger")
  private val warpModeCB = new JCheckBoxMenuItem("Warp mode")
  private val audioPanelCB = new JCheckBoxMenuItem("Audio settings")
  private val pauseCB = new JCheckBoxMenuItem("Pause")
  private val fullScreenMode = new JMenuItem("Full screen mode")

  // cheats
  private val cheatList = new ListBuffer[CheatCode]

  override def onMessage(msg: MessageBus.Message): Unit =
    msg match
      case MessageBus.CartRemoved(_,cart) =>
        cart.getExtraMemoryInfo match
          case Some(mf) =>
            saveCartExtraMemory(cart)
          case None =>
      case MessageBus.CartInserted(_,cart) =>
        cart.getExtraMemoryInfo match
          case Some(mf) =>
            loadCartExtraMemory(cart)
          case None =>
      case _ =>

  private def saveCartExtraMemory(cart:Cart): Unit =
    cart.getExtraMemoryInfo match
      case Some(mf) =>
        val mem = mf.extraRAM.map(_.toByte)
        val dir = Option(extraRAMDirectory).getOrElse(new File(cart.file.originalFile).getParentFile)
        var cartFile = new File(cart.file.originalFile).getName
        val extPos = cartFile.lastIndexOf('.')
        if extPos != -1 then
          cartFile = cartFile.substring(0,extPos)
        cartFile += ".ram"
        val file = new File(dir,cartFile)
        val out = new FileOutputStream(file)
        out.write(mem)
        out.close()
        glassPane.addMessage(MessageBoard.builder.message("Extra ram saved").adminLevel().bold().xleft().ybottom().delay(MESSAGE_STD_WAIT).fadingMilliseconds(500).build())
      case None =>
  private def loadCartExtraMemory(cart:Cart): Unit =
    cart.getExtraMemoryInfo match
      case Some(mf) =>
        val dir = Option(extraRAMDirectory).getOrElse(new File(cart.file.originalFile).getParentFile)
        var cartFile = new File(cart.file.originalFile).getName
        val extPos = cartFile.lastIndexOf('.')
        if extPos != -1 then
          cartFile = cartFile.substring(0,extPos)
        cartFile += ".ram"
        val file = new File(dir,cartFile)

        if file.exists() then
          val in = new FileInputStream(file)
          val mem = in.readAllBytes().map(_.toInt & 0xFF)
          in.close()
          System.arraycopy(mem,0,mf.extraRAM,0,mf.extraRAM.length)
      case None =>

  def boot(): Unit =
    // main frame
    frame = new JFrame(s"Mega Drive emulator v${Version.VERSION}")
    frame.setResizable(false)
    frame.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = shutdown()
    })
    frame.setIconImage(new ImageIcon(getClass.getResource("/resources/sonic_ring.png")).getImage)
    // display
    val display = new Display(SCREEN_WIDTH, megaDrive.model.videoType.totalLines, frame.getTitle, frame, megaDrive.masterClock)
    display.setFocusable(true)
    megaDrive.setDisplay(display)
    display.setClipArea(megaDrive.model.videoType.getClipArea(h40 = false).getTuple)                // starts with H32
    display.setPreferredSize(megaDrive.model.videoType.getClipArea(h40 = true).getPreferredSize(2)) // preferred size is relative to H40, double size
    frame.getContentPane.add("Center",display)
    frame.pack()

    debugger = new Debugger(megaDrive.m68k,megaDrive.mmu,megaDrive.mmu.get68KRAM,megaDrive.z80,megaDrive.mmu.getZ80RAM,megaDrive.vdp,() => debuggerCB.setSelected(false))
    val logger = Logger.setLogger(debugger.log)
    megaDrive.setLogger(logger)

    audioPanel = new AudioVolumePanel(frame,Array(megaDrive.fmAudio,megaDrive.psgAudio),megaDrive.pref,() => audioPanelCB.setSelected(false))
    MessageBus.add(audioPanel)

    buildMenuBar()

    // Real pad
    RealPadController.discoverControllers()

    // DND
    frame.setTransferHandler(new DNDHandler(handleDND))
  end boot

  protected def makeController(conf:Properties,pos: Int): Controller =
    conf.getProperty(Controller.formatProp(Controller.CONTROLLER_DEVICE_PROP, pos)) match
      case KeyboardPADController.DEVICE_PROP_VALUE | null =>
        Logger.getLogger.info("Controller %d set as keyboard pad",pos + 1)
        new KeyboardPADController(megaDrive.display,conf,pos,megaDrive.masterClock)
      case RealPadController.DEVICE_PROP_VALUE =>
        Logger.getLogger.info("Controller %d set as real pad",pos + 1)
        new RealPadController(conf,megaDrive.pref,pos,megaDrive.masterClock)
      case MouseController.DEVICE_PROP_VALUE =>
        Logger.getLogger.info("Controller %d set as mouse",pos + 1)
        new MouseController(pos,megaDrive.display)
      case unknown =>
        Logger.getLogger.warning("Cannot make controller %d from configuration file: unknown device %s", pos, unknown)
        EmptyController(pos)

  private def swing(action : => Unit) : Unit = SwingUtilities.invokeLater(() => action)

  private def pause(): Unit =
    if !megaDrive.masterClock.isPaused then
      megaDrive.masterClock.pause()
      pauseCB.setSelected(true)
      glassPane.addMessage(MessageBoard.builder.message("Paused").adminLevel().bold().xleft().ytop().delay().fadingMilliseconds(500).build())

  private def play(): Unit =
    if cart != null then
      glassPane.interrupt()
      pauseCB.setSelected(false)
      megaDrive.masterClock.play()

  private def shutdown(): Unit =
    val loadingDialog = new LoadingDialog(frame, s"Shutting down emulator ...")
    loadingDialog.setVisible(true)
    try
      frame.dispose()
      megaDrive.cart.foreach(cart => {
        MessageBus.send(MessageBus.CartRemoved(this,cart))
      })
      savePreferences()
    finally
      loadingDialog.dispose()

  private def savePreferences(): Unit = ???

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

  private def configure(args:Array[String]): Unit =
    // check controllers
    megaDrive.mmu.setController(0,makeController(megaDrive.conf,0))
    megaDrive.mmu.setController(1,makeController(megaDrive.conf,1))

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
      debugger.setMessageBoard(glassPane)
      showWelcome()
    }
  end run

  private def showWelcome(): Unit =
    // TODO
    glassPane.addMessage(MessageBoard.builder.message("Welcome").adminLevel().bold().xcenter().ycenter().delay(MESSAGE_STD_WAIT).fadingMilliseconds(500).build())

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
    stateMenu.setEnabled(false)
    debugMenu.setEnabled(false)
    val toolsMenu = new JMenu("Tools")
    cartMenu.setEnabled(false)
    val helpMenu = new JMenu("Help")

    menubar.add(fileMenu)
    menubar.add(stateMenu)
    menubar.add(debugMenu)
    menubar.add(toolsMenu)
    menubar.add(cartMenu)
    menubar.add(helpMenu)

    buildFileMenu(fileMenu)
    buildStateMenu(stateMenu)
    buildToolsMenu(toolsMenu)
    buildDebugMenu(debugMenu)
    buildCartMenu(cartMenu)
  end buildMenuBar

  private def buildFileMenu(fileMenu:JMenu): Unit =
    val loadCartItem = new JMenuItem("Load cart ...")
    fileMenu.add(loadCartItem)
    loadCartItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L,java.awt.event.InputEvent.ALT_DOWN_MASK))
    loadCartItem.addActionListener(_ => attachCart(None))
  private def buildStateMenu(stateMenu:JMenu): Unit =
    val saveStateItem = new JMenuItem("Save state ...")
    stateMenu.add(saveStateItem)
    saveStateItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, java.awt.event.InputEvent.ALT_DOWN_MASK))
    saveStateItem.addActionListener(_ => saveState(None))
    quickSaveStateItem.setEnabled(false)
    stateMenu.add(quickSaveStateItem)
    quickSaveStateItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Q, java.awt.event.InputEvent.ALT_DOWN_MASK))
    quickSaveStateItem.addActionListener(_ => saveState(Some(lastSaveStateFile)))

  private def buildDebugMenu(debugMenu:JMenu): Unit =
    debugMenu.add(debuggerCB)
    debuggerCB.addActionListener(_ => debugger.showDebugger(debuggerCB.isSelected))
    debuggerCB.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_D, java.awt.event.InputEvent.ALT_DOWN_MASK))
  private def buildToolsMenu(toolsMenu:JMenu): Unit =
    toolsMenu.add(pauseCB)
    pauseCB.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P, java.awt.event.InputEvent.ALT_DOWN_MASK))
    pauseCB.addActionListener(_ => if pauseCB.isSelected then pause() else play())
    pauseCB.setEnabled(false)
    toolsMenu.addSeparator()
    toolsMenu.add(audioPanelCB)
    audioPanelCB.addActionListener(_ => audioPanel.dialog.setVisible(audioPanelCB.isSelected))
    audioPanelCB.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A, java.awt.event.InputEvent.ALT_DOWN_MASK))
    val volUpItem = new JMenuItem("Increase volume")
    toolsMenu.add(volUpItem)
    volUpItem.addActionListener(_ => MessageBus.send(MessageBus.AudioChangeVolume(this,up = true)))
    volUpItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_PLUS,0))
    val volDownItem = new JMenuItem("Decrease volume")
    toolsMenu.add(volDownItem)
    volDownItem.addActionListener(_ => MessageBus.send(MessageBus.AudioChangeVolume(this, up = false)))
    volDownItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_MINUS,0))
    toolsMenu.addSeparator()
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

    fullScreenMode.setEnabled(false)
    fullScreenMode.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER, java.awt.event.InputEvent.ALT_DOWN_MASK))
    fullScreenMode.addActionListener(_ => enableFullScreenMode(true))
    toolsMenu.add(fullScreenMode)

    val controllerItem = new JMenuItem("Controllers ...")
    controllerItem.addActionListener(_ => openControllersPanel())
    controllerItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_J, java.awt.event.InputEvent.ALT_DOWN_MASK))
    toolsMenu.add(controllerItem)

    val cheatItem = new JMenuItem("Cheats ...")
    toolsMenu.add(cheatItem)
    cheatItem.addActionListener(_ => showCheatPanel())

    val gifItem = new JMenuItem("GIF recording ...")
    toolsMenu.add(gifItem)
    gifItem.addActionListener(_ => showGIFRecordingPanel())
  end buildToolsMenu

  private def buildCartMenu(cartMenu:JMenu): Unit =
    val cartInfoItem = new JMenuItem("Cart info ...")
    cartMenu.add(cartInfoItem)
    cartInfoItem.addActionListener(_ => showCartInfo())

  private def handleDND(file:File) : Unit = attachCart(Some(file))

  // =======================================================================
  private def showGIFRecordingPanel(): Unit =
    val gifDialog = GIFPanel.createGIFPanel(frame,Array(megaDrive.display),Array("main"))
    gifDialog.setVisible(true)

  private def showCheatPanel(): Unit =
    pause()
    try
      val cp = new CheatPanel(frame,this,megaDrive.cart.map(_.getOverseaName),megaDrive.pref,cart != null)
      cp.dialog.setVisible(true)
    finally
      play()

  private def openControllersPanel(): Unit =
    pause()
    try
      Logger.getLogger.log(Level.INFO) {
        val panel = new ControllerConfigPanel(frame, megaDrive.conf, megaDrive.mmu.getController, megaDrive.mmu.setController, makeController)
        panel.dialog.setVisible(true)
      }
    finally
      play()
  private def enableFullScreenMode(selectScreen:Boolean): Unit =
    var selectedScreenIndex = 0
    val devices = FullScreenMode.getScreenDeviceIDs
    if devices.length > 1 then
      pause()
      try
        JOptionPane.showInputDialog(frame,"Select screen","Screen selection for full screen mode",JOptionPane.INFORMATION_MESSAGE,null,devices.asInstanceOf[Array[Object]],devices(0)) match
          case null =>
            return
          case sel =>
            val index = devices.indexOf(sel.toString)
            if index != -1 then
              selectedScreenIndex = index
      finally
        play()
    FullScreenMode.goFullScreen(selectedScreenIndex,glassPane,frame,megaDrive.display,SCREEN_WIDTH, megaDrive.model.videoType.totalLines,new KeyAdapter:
      override def keyPressed(e: KeyEvent): Unit =
        {/* Add keys */} // TODO
    )

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
      delay(MESSAGE_STD_WAIT).
      fadingMilliseconds(2000).
      build()
    )
    debugger.setCart(cart)
    stateMenu.setEnabled(true)
    debugMenu.setEnabled(true)
    pauseCB.setEnabled(true)
    fullScreenMode.setEnabled(true)
    cartMenu.setEnabled(true)
    MouseHider.hideMouseOn(megaDrive.display)

    if cheatList.nonEmpty then
      JOptionPane.showConfirmDialog(frame,s"Apply ${cheatList.size} configured cheats on this cartridge ?","Cheats confirm",JOptionPane.YES_NO_OPTION) match
        case JOptionPane.YES_OPTION =>
          for c <- cheatList do
            c.reset()
            megaDrive.mmu.patch(c)
        case _ =>

    reset(hard = true)

  private def detachCart(): Unit =
    cart = null
    stateMenu.setEnabled(true)
    debugMenu.setEnabled(false)
    debugger.enableTracing(false)
    debugger.showDebugger(false)
    pauseCB.setEnabled(false)
    MouseHider.showMouseOn(megaDrive.display)
    fullScreenMode.setEnabled(false)
    cartMenu.setEnabled(false)

    for c <- cheatList do
      c.reset()
    // TODO

  private def chooseCart(): Option[File] =
    val fc = new JFileChooser()
    fc.setDialogTitle("Choose cartridge to play")
    fc.setFileFilter(new FileFilter {
      override def accept(f: File): Boolean =
        val fileName = f.getName.toUpperCase()
        fileName.endsWith(".BIN") || fileName.endsWith(".MD") || fileName.endsWith(".SMD") || fileName.endsWith(".ZIP")

      override def getDescription: String = "Mega Drive cart"
    })
    fc.setCurrentDirectory(lastDirectory)
    fc.showOpenDialog(frame) match
      case JFileChooser.APPROVE_OPTION =>
        lastDirectory = fc.getSelectedFile.getParentFile
        Some(fc.getSelectedFile)
      case _ =>
        None

  private def extractFromZIP(file:File): Option[File] =
    try
      Cart.extractFromZIP(file.toString) match
        case Right(f) =>
          Some(f)
        case Left(Cart.UNZIP_ERROR.NO_SUITABLE_CART) =>
          JOptionPane.showMessageDialog(frame,s"Error while extracting from zip file '${file.getName}': no suitable cartridge extension found","Zip error",JOptionPane.ERROR_MESSAGE)
          None
        case Left(Cart.UNZIP_ERROR.DIRECTORY_FOUND) =>
          JOptionPane.showMessageDialog(frame, s"Error while extracting from zip file '${file.getName}': directory found", "Zip error", JOptionPane.ERROR_MESSAGE)
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
      val cart = new Cart(Cart.CartFile(file.toString,f.toString), None, fixChecksum = fixChecksum)
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

  private def showCartInfo(): Unit =
    JOptionPane.showMessageDialog(frame, new CartInfoPanel(cart), "Command options", JOptionPane.INFORMATION_MESSAGE)

  private def saveState(file:Option[String]): Unit =
    pause()
    try
      val outFileName = file match
        case Some(f) => f
        case None =>
          val fc = new JFileChooser()
          fc.setDialogTitle("Choose where to store the state")
          fc.setCurrentDirectory(lastDirectory)
          fc.showSaveDialog(frame) match
            case JFileChooser.APPROVE_OPTION =>
              var file = fc.getSelectedFile
              if !file.getName.toUpperCase().endsWith(".YAML.GZ") then
                file = new File(file.toString + ".yaml.gz")
              if file.exists() then
                JOptionPane.showConfirmDialog(frame,"File already exists, do you want to overwrite it ?","File exists",JOptionPane.YES_NO_OPTION) match
                  case JOptionPane.YES_OPTION =>
                    file.toString
                  case _ =>
                    return
              else
                file.toString
            case _ =>
              return

      val sb = megaDrive.createComponentState()
      saveStateAsYAML(sb,outFileName)
      lastSaveStateFile = outFileName
      quickSaveStateItem.setEnabled(true)
      glassPane.addMessage(MessageBoard.builder.message("State saved").adminLevel().bold().xleft().ybottom().delay(1000).fadingMilliseconds(500).build())
    catch
      case e:StateBuilder.StateBuilderException =>
        JOptionPane.showMessageDialog(frame,s"Error while saving state. Component [${e.getComponentPath}]: ${e.getMessage}","State error",JOptionPane.ERROR_MESSAGE)
      case t:Throwable =>
        t.printStackTrace()
        JOptionPane.showMessageDialog(frame,s"Unexpected error while saving state: $t","State error",JOptionPane.ERROR_MESSAGE)
    finally
      play()
  end saveState

  private def saveStateAsYAML(sb:StateBuilder,fileName:String): Unit =
    val loadingDialog = new LoadingDialog(frame, s"Backing up state $fileName ...")
    loadingDialog.setVisible(true)
    try
      val options = new DumperOptions
      options.setIndent(2)
      options.setPrettyFlow(true)
      options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
      val yaml = new Yaml(options)
      val out = new PrintWriter(new GZIPOutputStream(new FileOutputStream(fileName)))
      yaml.dump(sb.build(), out)
      out.close()
    finally
      loadingDialog.dispose()

  private def loadStateAsYAML(fileName:String): StateBuilder =
    val loadingDialog = new LoadingDialog(frame, s"Restoring state $fileName ...")
    loadingDialog.setVisible(true)
    try
      val yaml = new Yaml()
      val in = new InputStreamReader(new GZIPInputStream(new FileInputStream(fileName)))
      val state = yaml.load[java.util.Map[String,AnyRef]](in)
      in.close()
      new StateBuilder(state)
    finally
      loadingDialog.dispose()

  // cheat manager
  override def addCheat(_cheat: CheatCode): Unit =
    val cheat = _cheat.copy()
    cheatList += cheat
    megaDrive.mmu.patch(cheat)
  override def removeCheat(_cheat: CheatCode): Unit =
    val cheat = _cheat.copy()
    cheatList -= cheat
    megaDrive.mmu.restore(cheat)
  override def removeAllCheats(): Unit =
    for c <- cheatList do
      megaDrive.mmu.restore(c)
    cheatList.clear()
  override def getCheats: List[CheatCode] = cheatList.toList


