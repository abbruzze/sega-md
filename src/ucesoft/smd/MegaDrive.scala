package ucesoft.smd

import ucesoft.smd.Clock.Clockable
import ucesoft.smd.audio.{FM, PSG}
import ucesoft.smd.controller.{ControllerDevice, ControllerType, EmptyController}
import ucesoft.smd.cpu.m68k.M68000
import ucesoft.smd.cpu.z80.Z80
import ucesoft.smd.misc.Preferences

import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File, FileReader, FileWriter, IOException}
import java.time.LocalDateTime
import java.util.Properties
import javax.imageio.ImageIO
import scala.compiletime.uninitialized

object MegaDrive:
  case class StateInfo(model:Model,timestamp:LocalDateTime,version:String,buildDate:LocalDateTime,cartInfo:Cart.CartInfo,snap:BufferedImage)
/**
 * @author Alessandro Abbruzzetti
 *         Created on 15/02/2024 15:24  
 */
class MegaDrive extends SMDComponent with Clockable with VDP.VDPChangeClockRateListener:
  override protected val smdComponentName: String = "MegaDrive"
  inline private val VDP_CLOCK_DIVIDER = 5 // start with H32
  inline private val M68_CLOCK_DIVIDER = 7
  inline private val Z80_CLOCK_DIVIDER = 15
  inline private val FM_CLOCK_DIVIDER = M68_CLOCK_DIVIDER * 6
  inline private val DIV_68K = 7.0
  inline private val DIV_Z80 = 14.0

  private var _model = Model(ModelType.Oversea,VideoType.NTSC,0)
  private var _display : Display = uninitialized
  private var _cart : Cart = uninitialized

  private var configurationFile : File = uninitialized

  private var fixChecksum = false
  private var mapper : MMU.M68KMapper = uninitialized
  // ============== Public =======================================================

  final val masterClock = new Clock("masterClock",model.videoType.clockFrequency)
  final val busArbiter = new BusArbiter
  final val vdp = new VDP(busArbiter)
  final val mmu = new MMU(busArbiter)
  final val m68k = new M68000(mmu)
  final val z80 = new Z80(mmu,mmu)
  final val fmAudio = new FM(model.videoType.clockFrequency / (FM_CLOCK_DIVIDER * 24),"Ym3438")
  final val psgAudio = new PSG(44100,"PSG")

  final val pref = new Preferences
  final val conf = new Properties()
  final val originalConf = new Properties()

  // =============================================================================

  private inline val MULTIPLIER = 65536
  private var m68Div = 0
  private var z80Div = 0
  private var m68Acc = 0
  private var z80Acc = 0
  private var psgSampleCycles = psgAudio.getCyclesPerSample
  private var psgCycles = 0
  private var fmCycles = 0
  private var m68WaitCycles = 0
  private var z80WaitCycles = 0

  private var makeController: (Int,ControllerType,ControllerDevice) => Unit = scala.compiletime.uninitialized

  // CONSTRUCTOR ===============================================================
  configure()
  // ===========================================================================

  private def configure(): Unit =
    // LOAD configuration
    // MD HOME ==========================================================================
    var mdHome = System.getProperty("scalagen.home")
    if mdHome == null then
      mdHome = scala.util.Properties.userHome
      println(s"Warning: scalagen.home env variable not set. Default emulator's home is $mdHome")
    else
      val mdHomeDir = new File(new File(mdHome), "conf")
      mdHome = mdHomeDir.toString
      if !mdHomeDir.exists() then
        if !mdHomeDir.mkdir() then
          mdHome = scala.util.Properties.userHome
          println(s"Cannot create directory $mdHomeDir, using $mdHome as home directory")
    // PROPERTIES =======================================================================
    configurationFile = new File(new File(mdHome), "config.properties")
    if configurationFile.exists then
      var fr : FileReader = null
      try
        fr = new FileReader(configurationFile)
        conf.load(fr)
        originalConf.putAll(conf)
      catch
        case io : IOException =>
          println(s"Cannot load configuration file $configurationFile. Using default configuration.")
          io.printStackTrace()
      finally
        if fr != null then
          fr.close()
  end configure
  
  def setMapper(mapper:MMU.M68KMapper): Unit =
    this.mapper = mapper

  def setMakeController(mkController:(Int,ControllerType,ControllerDevice) => Unit): Unit =
    makeController = mkController

  def saveConfiguration(conf:Properties): Unit =
    val out = new FileWriter(configurationFile)
    conf.store(out, "ScalaGen configuration file")
    out.close()

  override def reset(): Unit =
    m68Div = 0
    z80Div = 0
    m68Acc = 0
    z80Acc = 0
    psgSampleCycles = psgAudio.getCyclesPerSample
    psgCycles = 0
    fmCycles = 0
    m68WaitCycles = 0
    z80WaitCycles = 0
  
  override def init(): Unit =
    add(masterClock)
    add(fmAudio)
    add(psgAudio)
    add(mmu)
    add(m68k)
    add(z80)
    add(busArbiter)
    add(vdp)

    setLogger(log)

    masterClock.setClockables(this)
    masterClock.setClockDivider(0, VDP_CLOCK_DIVIDER)

    z80.setPCMask(0x3FFF)
    vdp.setModel(_model)
    vdp.set68KMemory(mmu)
    vdp.setCPUs(m68k, z80)
    vdp.setClockRateChangeListener(this)
    clockRateChanged(VDP_CLOCK_DIVIDER)
    mmu.setVDP(vdp)
    mmu.setAudioChips(psgAudio,fmAudio)
    mmu.setModel(_model)
    mmu.setCPUs(m68k, z80)

    fmAudio.setBufferInMillis(15)
    psgAudio.setCPUFrequency(_model.videoType.clockFrequency / Z80_CLOCK_DIVIDER)
    psgAudio.setBufferInMillis(15)

    busArbiter.set(m68k, z80, fmAudio)
    
    for c <- 0 to 2 do
      if mmu.getController(c) == null then
        mmu.setController(c,new EmptyController(c))
      
    MessageBus.add(this)
    MessageBus.add(vdp)
  end init

  override def hardReset(): Unit =
    reset()
    m68k.setComponentEnabled(false)
    z80.setComponentEnabled(false)
    // The execution delay between the VDP and the M68000
    // is measured to be approximately 13 milliseconds with a logic analyzer.
    masterClock.scheduleMillis(10, _ => {
      m68k.setComponentEnabled(true)
      z80.setComponentEnabled(true)
    })
    clockRateChanged(VDP_CLOCK_DIVIDER)

  // ===============================================================================
  def setDisplay(display:Display): Unit =
    _display = display
    vdp.setDisplay(display)
  def display : Display = _display
  def model : Model = _model
  
  def cart : Option[Cart] = Option(_cart)
  
  private def modelChanged(newModel:Model): Unit =
    _model = newModel
    psgAudio.setCPUFrequency(_model.videoType.clockFrequency / Z80_CLOCK_DIVIDER)
    psgSampleCycles = psgAudio.getCyclesPerSample
    vdp.setModel(_model)
    mmu.setModel(_model)
    clockRateChanged(VDP_CLOCK_DIVIDER)
    // TODO
    
  private def cartInserted(cart:Cart): Unit =
    _cart = cart
    mmu.setCart(cart)

  override def onMessage(msg: MessageBus.Message): Unit =
    import MessageBus.*
    msg match
      case ModelChanged(_,newModel) =>
        if newModel != _model then
          modelChanged(newModel)
      case CartInserted(_,cart) =>
        cartInserted(cart)
      case _ =>

  // =================== Main Loop ==============================
  override final def clockRateChanged(rate: Int): Unit =
    if m68Div != 0 then
      masterClock.setClockDivider(0, rate)
      
    m68Div = math.round((rate / DIV_68K) * MULTIPLIER).toInt
    z80Div = math.round((rate / DIV_Z80) * MULTIPLIER).toInt
  final override def clock(cycles: Long): Unit =
    vdp.clock(cycles)
    if mapper != null then
      mapper.clock(cycles)
      
    if m68k.isComponentEnabled then
      m68Acc += m68Div
      if m68Acc >= MULTIPLIER then
        m68Acc -= MULTIPLIER
        if m68WaitCycles == 0 then
          m68WaitCycles = m68k.execute() - 1
        else
          m68WaitCycles -= 1
        fmCycles += 1
        if fmCycles == 6 then
          fmCycles = 0
          fmAudio.clock()
      end if
    if z80.isComponentEnabled then
      z80Acc += z80Div
      if z80Acc >= MULTIPLIER then
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
  end clock

  def isChecksumFixed: Boolean = fixChecksum
  def setChecksumFixed(enabled:Boolean): Unit = fixChecksum = enabled
  // ================= State ====================================
  def getStateInfo(_sb: StateBuilder): Option[MegaDrive.StateInfo] =
    try
      val sb = _sb.getSubStateBuilder("component")
      val dateFormatter = java.time.format.DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss")
      val ts = LocalDateTime.from(dateFormatter.parse(sb.r[String]("timestamp")))
      val version = sb.r[String]("version")
      val buildDate = LocalDateTime.from(dateFormatter.parse(sb.r[String]("buildDate")))
      val snapBuffer = sb.deserialize[Array[Byte]]("snapshot",zip = true)
      val snap = ImageIO.read(new ByteArrayInputStream(snapBuffer))

      val modelSB = sb.getSubStateBuilder("model")
      val videoType = VideoType.valueOf(modelSB.r[String]("videoType"))
      val modelType = ModelType.valueOf(modelSB.r[String]("modelType"))
      val modelVersion = modelSB.r[Int]("version")
      val stateModel = Model(modelType, videoType, modelVersion)

      Cart.getInfo(sb.getSubStateBuilder("cart")) match
        case Some(cartInfo) =>
          Some(MegaDrive.StateInfo(stateModel,ts,version,buildDate,cartInfo,snap))
        case None =>
          None
    catch
      case _:StateBuilder.StateBuilderException =>
        None
  override protected def createState(sb: StateBuilder): Unit =
    // Internal state
    val snap = display.getSnapshot(320,-1)
    val buffer = new ByteArrayOutputStream()
    ImageIO.write(snap,"png",buffer)
    sb.w("timestamp",LocalDateTime.now().format(java.time.format.DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss"))).
      w("version",Version.VERSION).
      w("buildDate",Version.BUILD_DATE).
      serialize("snapshot",buffer.toByteArray,zip = true)

    val modelSB = new StateBuilder()
    modelSB.w("videoType",model.videoType.toString).
      w("modelType",model.modelType.toString).
      w("version",model.versionNumber)
    sb.w("model",modelSB.build())

    val controllersSB = new StateBuilder()
    for c <- 0 to 1 do
      val cSB = new StateBuilder()
      val controller = mmu.getController(c)
      val cs = controller.createComponentState()
      cs.w("device",controller.device.toString)
      cs.w("type",controller.getControllerType.toString)
      controllersSB.w(c.toString, cs.build())
    sb.w("controllers",controllersSB.build())

    val loopSB = new StateBuilder()
    loopSB.w("m68Div",m68Div).
      w("z80Div",z80Div).
      w("m68Acc",m68Acc).
      w("z80Acc",z80Acc).
      w("psgCycles",psgCycles).
      w("fmCycles",fmCycles).
      w("m68WaitCycles",m68WaitCycles).
      w("z80WaitCycles",z80WaitCycles)
    sb.w("loop",loopSB.build())
    
    // Cart
    Cart.createState(_cart,sb)
  override protected def restoreState(sb: StateBuilder): Unit =
    val modelSB = sb.getSubStateBuilder("model")
    val videoType = VideoType.valueOf(modelSB.r[String]("videoType"))
    val modelType = ModelType.valueOf(modelSB.r[String]("modelType"))
    val version = modelSB.r[Int]("version")

    val controllersSB = sb.getSubStateBuilder("controllers")
    for c <- 0 to 1 do
      val controller = controllersSB.getSubStateBuilder(c.toString)
      val device = ControllerDevice.valueOf(controller.r[String]("device"))
      val ctype = ControllerType.valueOf(controller.r[String]("type"))
      makeController(c,ctype,device)
      mmu.getController(c).restoreComponentState(controller)

    val stateModel = Model(modelType,videoType,version)
    val loopSB = sb.getSubStateBuilder("loop")
    import loopSB.*
    m68Div = r[Int]("m68Div")
    z80Div = r[Int]("z80Div")
    m68Acc = r[Int]("m68Acc")
    z80Acc = r[Int]("z80Acc")
    psgCycles = r[Int]("psgCycles")
    fmCycles = r[Int]("fmCycles")
    m68WaitCycles = r[Int]("m68WaitCycles")
    z80WaitCycles = r[Int]("z80WaitCycles")

    if stateModel != _model then
      MessageBus.send(MessageBus.ModelChanged(this,stateModel))

    val cart = Cart.restoreState(sb,fixChecksum)
    MessageBus.send(MessageBus.CartInserted(this,cart))
    MessageBus.send(MessageBus.StateRestored(this,cart))


