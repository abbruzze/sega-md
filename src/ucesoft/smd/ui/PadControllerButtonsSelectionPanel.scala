package ucesoft.smd.ui

import com.formdev.flatlaf.FlatLightLaf
import ucesoft.smd.controller.{Controller, KeyboardPADController, RealPadController}

import java.awt.event.{KeyEvent, KeyListener}
import java.awt.{BorderLayout, Color, FlowLayout, GridLayout}
import java.util.Properties
import javax.swing.*
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 16/01/2024 14:29  
 */
class PadControllerButtonsSelectionPanel(frame:JDialog,
                                         index:Int,
                                         config:Properties,
                                         realPadControllerName:Option[String],
                                         is3Buttons:Boolean,
                                         action: Option[Properties] => Unit) extends JPanel:
  import ucesoft.smd.controller.PadController.*
  private val workingConfig = new Properties()
  private val keyLabels = Array.ofDim[JLabel](12)
  private val keyboardMode = realPadControllerName.isEmpty
  val dialog = new JDialog(frame,s"Controller $index configuration",true)
  private var waitingDialog : JDialog = uninitialized
  private val BUTTONS = if keyboardMode then 11 else 7
  private val device = if realPadControllerName.isDefined then RealPadController.DEVICE_PROP_VALUE else KeyboardPADController.DEVICE_PROP_VALUE

  init()
  
  private def init(): Unit =
    import scala.jdk.CollectionConverters.*
    for e <- config.keys().asScala do
      workingConfig.setProperty(e.toString,config.getProperty(e.toString))

    val deviceProp = realPadControllerName match
      case Some(_) => RealPadController.DEVICE_PROP_VALUE
      case None => KeyboardPADController.DEVICE_PROP_VALUE
    workingConfig.setProperty(Controller.formatProp(Controller.CONTROLLER_DEVICE_PROP,index,device),deviceProp)
      
    setLayout(new BorderLayout())
    val devicePanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    devicePanel.add(new JLabel(s"Device: ${realPadControllerName.getOrElse("Keyboard")}"))
    add("North",devicePanel)

    val keysPanel = new JPanel(new GridLayout(BUTTONS + 1,2,20,10))
    keysPanel.setBorder(BorderFactory.createTitledBorder("Controller buttons"))
    add("Center",keysPanel)

    for r <- 0 to BUTTONS do
      val button = new JButton(BUTTONS_NAMES(r))
      if is3Buttons && (BUTTONS_NAMES(r) == "X" || BUTTONS_NAMES(r) == "Y" || BUTTONS_NAMES(r) == "Z" || BUTTONS_NAMES(r) == "MODE") then
        button.setEnabled(false)
      keysPanel.add(button)
      button.addActionListener(_ => configureButton(r))
      var value = workingConfig.getProperty(Controller.formatProp(buttonAndDirectionsPropNames(r),index,device))
      if value == null then
        value = "EMPTY"
      else if realPadControllerName.isEmpty then
        value = KeyEvent.getKeyText(value.toInt)
      keyLabels(r) = new JLabel(value)
      if value == "EMPTY" then
        keyLabels(r).setForeground(Color.RED)
      else
        keyLabels(r).setForeground(Color.WHITE)
      keysPanel.add(keyLabels(r))

    val cancelButton = new JButton("Cancel")
    cancelButton.addActionListener(_ => {
      dialog.dispose()
      action(None)
    })
    val okButton = new JButton("Ok")
    okButton.addActionListener(_ => {
      dialog.dispose()
      action(Some(workingConfig))
    })
    val buttonPanel = new JPanel(new FlowLayout())
    buttonPanel.add(okButton)
    buttonPanel.add(cancelButton)
    add("South",buttonPanel)
    dialog.getContentPane.add("Center",this)
    dialog.setLocationRelativeTo(frame)
    dialog.pack()

    if !keyboardMode then
      RealPadController.discoverControllers()

  private def configureButton(b: Int): Unit =
    waitingDialog = new JDialog(dialog,true)
    waitingDialog.setFocusable(true)
    waitingDialog.requestFocus()
    var buttonTask : RealPadController.WaitButtonTask = null
    val cancelButton = new JButton("Cancel")
    cancelButton.addActionListener(_ => {
      waitingDialog.dispose()
      if buttonTask != null then
        buttonTask.stop()
    })
    val southPanel = new JPanel(new FlowLayout())
    southPanel.add(cancelButton)
    waitingDialog.getContentPane.add("South", southPanel)
    val messagePanel = new JPanel(new BorderLayout())
    waitingDialog.getContentPane.add("Center",messagePanel)
    val labelPanel = new JPanel(new FlowLayout())
    messagePanel.add("Center",labelPanel)
    if keyboardMode then
      labelPanel.add(new JLabel("Press a key to configure"))
    else
      labelPanel.add(new JLabel("Press a button on your controller"))
      RealPadController.getControllerByName(realPadControllerName.get) match
        case Some(controller) =>
          buttonTask = new RealPadController.WaitButtonTask(controller,button => {
            waitingDialog.dispose()
            keyLabels(b).setText(button)
            keyLabels(b).setForeground(Color.WHITE)
            workingConfig.setProperty(Controller.formatProp(buttonAndDirectionsPropNames(b), index,device), button)
          })
          buttonTask.start()
        case None =>
          JOptionPane.showMessageDialog(dialog,s"Controller '${realPadControllerName.get}' not found","Controller error",JOptionPane.ERROR_MESSAGE)
    waitingDialog.pack()
    waitingDialog.addKeyListener(new KeyListener:
      override def keyTyped(e: KeyEvent): Unit = {}
      override def keyReleased(e: KeyEvent): Unit = {}
      override def keyPressed(e: KeyEvent): Unit =
        waitingDialog.dispose()
        val keyName = s"${e.getExtendedKeyCode}"
        keyLabels(b).setText(KeyEvent.getKeyText(e.getExtendedKeyCode))
        keyLabels(b).setForeground(Color.WHITE)
        workingConfig.setProperty(Controller.formatProp(buttonAndDirectionsPropNames(b),index,device),keyName)
    )
    waitingDialog.setLocationRelativeTo(dialog)
    waitingDialog.setVisible(true)

