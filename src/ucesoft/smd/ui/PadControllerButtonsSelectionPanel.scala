package ucesoft.smd.ui

import com.formdev.flatlaf.FlatLightLaf
import ucesoft.smd.controller.RealPadController

import java.awt.event.{KeyEvent, KeyListener}
import java.awt.{BorderLayout, Color, FlowLayout, GridLayout}
import java.util.Properties
import javax.swing.*

object PadControllerButtonsSelectionPanel:
  def main(args:Array[String]): Unit =
    FlatLightLaf.setup()
    JFrame.setDefaultLookAndFeelDecorated(false)
    JDialog.setDefaultLookAndFeelDecorated(false)
    UIManager.setLookAndFeel("com.formdev.flatlaf.FlatDarculaLaf")
    val panel = new PadControllerButtonsSelectionPanel(null,1,new Properties(),None,prop => println(prop))
    panel.dialog.setVisible(true)
/**
 * @author Alessandro Abbruzzetti
 *         Created on 16/01/2024 14:29  
 */
class PadControllerButtonsSelectionPanel(frame:JFrame,index:Int,config:Properties,realPadControllerName:Option[String],action: Option[Properties] => Unit) extends JPanel:
  import ucesoft.smd.controller.PadController.*
  private val workingConfig = new Properties()
  private val keyLabels = Array.ofDim[JLabel](12)
  private val keyboardMode = realPadControllerName.isEmpty
  val dialog = new JDialog(frame,s"Controller $index configuration",true)
  private var waitingDialog : JDialog = _
  private val BUTTONS = if keyboardMode then 11 else 7

  init()
  
  private def init(): Unit =
    import scala.jdk.CollectionConverters.*
    for e <- config.keys().asScala do
      workingConfig.setProperty(e.toString,config.getProperty(e.toString))
      
    setLayout(new BorderLayout())
    val devicePanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    devicePanel.add(new JLabel(s"Device: ${realPadControllerName.getOrElse("Keyboard")}"))
    add("North",devicePanel)

    val keysPanel = new JPanel(new GridLayout(BUTTONS + 1,2,20,10))
    keysPanel.setBorder(BorderFactory.createTitledBorder("Controller buttons"))
    add("Center",keysPanel)

    for r <- 0 to BUTTONS do
      val button = new JButton(BUTTONS_NAMES(r))
      keysPanel.add(button)
      button.addActionListener(_ => configureButton(r))
      val value = workingConfig.getProperty(formatProp(buttonAndDirectionsPropNames(r),index),"EMPTY")
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
    dialog.pack()

    if !keyboardMode then
      RealPadController.discoverControllers()
      for c <- RealPadController.getControllersNames do
        println(c)

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
            workingConfig.setProperty(formatProp(buttonAndDirectionsPropNames(b), index), button)
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
        val keyName = s"${KeyEvent.getKeyText(e.getKeyCode)},${e.getKeyCode}"
        keyLabels(b).setText(KeyEvent.getKeyText(e.getKeyCode))
        keyLabels(b).setForeground(Color.WHITE)
        workingConfig.setProperty(formatProp(buttonAndDirectionsPropNames(b),index),keyName)
    )
    waitingDialog.setLocationRelativeTo(dialog)
    waitingDialog.setVisible(true)

