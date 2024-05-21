package ucesoft.smd.ui

import ucesoft.smd.MessageBus
import ucesoft.smd.controller.{Controller, ControllerDevice, ControllerType, EmptyController, LightgunController, MouseController, RealPadController}
import ucesoft.smd.controller.Controller.{CONTROLLER_DEVICE_PROP, CONTROLLER_TYPE_PROP, formatProp}
import ucesoft.smd.controller.ControllerDevice.*
import ucesoft.smd.controller.RealPadController.CONTROLLER_NAME_PROP

import java.awt.{BorderLayout, GridLayout}
import java.util.Properties
import javax.swing.*

/**
 * @author Alessandro Abbruzzetti
 *         Created on 01/03/2024 17:47  
 */
class ControllerConfigPanel(frame:JFrame,
                            config:Properties,
                            getController:Int => Controller,
                            setController:(Int,Controller) => Unit,
                            makeController: (Properties,Int) => Controller) extends JPanel:
  val dialog = new JDialog(frame,s"Controller configuration panel",true)
  private val workingProps = new Properties()
  private val applyButton = new JButton("Apply")
  private val combos = Array.ofDim[JComboBox[String]](2)
  init()

  private def init(): Unit =
    import scala.jdk.CollectionConverters.*
    for prop <- config.keys().asScala do
      workingProps.setProperty(prop.toString, config.getProperty(prop.toString))

    setLayout(new BorderLayout())
    val controllersPane = new JPanel(new GridLayout(2,0))
    for tab <- 0 to 1 do
      val panel = new JPanel(new BorderLayout())
      controllersPane.add(panel)
      panel.setBorder(BorderFactory.createTitledBorder(s"Controller #${tab + 1}"))
      var dummyPanel = new JPanel()
      val controller = getController(tab)

      combos(tab) = new JComboBox[String](Array(
        "Pad - Keyboard 3 buttons", // 0
        "Pad - Keyboard 6 buttons", // 1
        "Pad - real pad 3 buttons", // 2
        "Pad - real pad 6 buttons", // 3
        "Mouse", // 4
        "Mouse with start on CTRL", // 5
        "Lightgun Menacer",         // 6
        "Empty") // 7
      )

      controller.device match
        case Empty =>
          combos(tab).setSelectedIndex(7)
        case KeyboardPad =>
          combos(tab).setSelectedIndex(if controller.getControllerType == ControllerType.PAD3Buttons then 0 else 1)
        case Mouse =>
          combos(tab).setSelectedIndex(if controller.getControllerType == ControllerType.Mouse then 4 else 5)
        case RealPad =>
          combos(tab).setSelectedIndex(if controller.getControllerType == ControllerType.PAD3Buttons then 2 else 3)
        case Lightgun =>
          combos(tab).setSelectedIndex(6)
      dummyPanel.add(new JLabel("Device:"))
      dummyPanel.add(combos(tab))
      panel.add("Center",dummyPanel)
      dummyPanel = new JPanel()
      val confButton = new JButton("Configure")
      //confButton.setEnabled(controller.device != Empty)
      dummyPanel.add(confButton)
      panel.add("South",dummyPanel)

      confButton.addActionListener(_ => {
        val (device,dtype) = getDeviceAndType(tab)
        configure(tab,device,dtype)
      })
      combos(tab).addActionListener(_ => {
        //confButton.setEnabled(combos(tab).getSelectedIndex != 7)
        applyButton.setEnabled(true)
      })

    add("Center",controllersPane)
    val dummyPanel = new JPanel()
    applyButton.setEnabled(false)
    applyButton.addActionListener(_ => apply())
    val cancelButton = new JButton("Cancel")
    cancelButton.addActionListener(_ => dialog.dispose())
    dummyPanel.add(applyButton)
    dummyPanel.add(cancelButton)
    dialog.getContentPane.add("Center",this)
    dialog.getContentPane.add("South",dummyPanel)
    dialog.setResizable(false)
    dialog.setLocationRelativeTo(frame)
    dialog.setSize(300,300)

  private def getDeviceAndType(i:Int): (ControllerDevice,ControllerType) =
    combos(i).getSelectedIndex match
      case 0 => (ControllerDevice.KeyboardPad, ControllerType.PAD3Buttons)
      case 1 => (ControllerDevice.KeyboardPad, ControllerType.PAD6Buttons)
      case 2 => (ControllerDevice.RealPad, ControllerType.PAD3Buttons)
      case 3 => (ControllerDevice.RealPad, ControllerType.PAD6Buttons)
      case 4 => (ControllerDevice.Mouse, ControllerType.Mouse)
      case 5 => (ControllerDevice.Mouse, ControllerType.MouseStartWithCTRLAndLeft)
      case 6 => (ControllerDevice.Lightgun, ControllerType.Menacer)
      case 7 => (ControllerDevice.Empty, ControllerType.Unknown)
  private def apply(): Unit =
    applyButton.setEnabled(false)
    import scala.jdk.CollectionConverters.*
    for prop <- workingProps.keys().asScala do
      config.setProperty(prop.toString, workingProps.getProperty(prop.toString))

    for c <- 0 to 1 do
      val old = getController(0)
      val newC = makeController(config,c)
      val (_,deviceType) = getDeviceAndType(c)
      newC.setControllerType(deviceType)
      newC.copyStateFrom(old)
      setController(c,newC)

    MessageBus.send(MessageBus.ControllerConfigurationChanged(this))
    JOptionPane.showMessageDialog(this,"Changes applied","Changes applied",JOptionPane.INFORMATION_MESSAGE)

  private def applyProp(prop:Option[Properties]): Unit =
    prop match
      case Some(p) =>
        applyButton.setEnabled(true)
        import scala.jdk.CollectionConverters.*
        for prop <- p.keys().asScala do
          workingProps.setProperty(prop.toString,p.getProperty(prop.toString))
      case None =>

  private def configure(index:Int,controllerDevice: ControllerDevice,controllerType: ControllerType): Unit =
    workingProps.setProperty(formatProp(CONTROLLER_TYPE_PROP, index),controllerType.toString)
    controllerDevice match
      case KeyboardPad =>
        val selectionPanel = new PadControllerButtonsSelectionPanel(dialog,index,workingProps,None,controllerType == ControllerType.PAD3Buttons,applyProp)
        selectionPanel.dialog.setVisible(true)
      case RealPad =>
        RealPadController.discoverControllers()
        val controllerNames = RealPadController.getControllersNames
        if controllerDevice == RealPad && controllerNames.isEmpty then
          JOptionPane.showMessageDialog(dialog,"No real pads available","Real pad error",JOptionPane.ERROR_MESSAGE)
          return
        JOptionPane.showInputDialog(dialog,"Select device","Real Pad selection",JOptionPane.QUESTION_MESSAGE,null,controllerNames.asInstanceOf[Array[Object]],controllerNames(0)) match
          case null =>
            dialog.dispose()
          case device =>
            workingProps.setProperty(formatProp(CONTROLLER_NAME_PROP,index,RealPadController.DEVICE_PROP_VALUE),device.toString)
            val selectionPanel = new PadControllerButtonsSelectionPanel(dialog, index, workingProps, Some(device.toString),controllerType == ControllerType.PAD3Buttons, applyProp)
            selectionPanel.dialog.setVisible(true)
      case Mouse =>
        workingProps.setProperty(formatProp(CONTROLLER_DEVICE_PROP,index,MouseController.DEVICE_PROP_VALUE),MouseController.DEVICE_PROP_VALUE)
        JOptionPane.showMessageDialog(dialog,"Mouse controller configured","Mouse configuration",JOptionPane.INFORMATION_MESSAGE)
      case Lightgun =>
        workingProps.setProperty(formatProp(CONTROLLER_DEVICE_PROP,index,LightgunController.DEVICE_PROP_VALUE),LightgunController.DEVICE_PROP_VALUE)
        JOptionPane.showMessageDialog(dialog,"Light gun controller configured","Light gun configuration",JOptionPane.INFORMATION_MESSAGE)
      case Empty =>
        workingProps.setProperty(formatProp(CONTROLLER_DEVICE_PROP, index,MouseController.DEVICE_PROP_VALUE), EmptyController.DEVICE_PROP_VALUE)
        JOptionPane.showMessageDialog(dialog, "Empty controller configured", "Empty controller configuration", JOptionPane.INFORMATION_MESSAGE)