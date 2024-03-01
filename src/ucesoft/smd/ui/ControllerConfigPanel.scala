package ucesoft.smd.ui

import ucesoft.smd.controller.{Controller, ControllerDevice, RealPadController}
import ucesoft.smd.controller.ControllerDevice.{Empty, KeyboardPad, Mouse, RealPad}

import java.awt.BorderLayout
import java.util.Properties
import javax.swing.{JButton, JComboBox, JDialog, JFrame, JLabel, JOptionPane, JPanel, JTabbedPane}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 01/03/2024 17:47  
 */
class ControllerConfigPanel(frame:JFrame,config:Properties,getController:Int => Controller,setController:(Int,Controller) => Unit) extends JPanel:
  val dialog = new JDialog(frame,s"Controller configuration panel",true)
  init()

  private def init(): Unit =
    setLayout(new BorderLayout())
    val tabbedPane = new JTabbedPane()
    for tab <- 0 to 1 do
      val panel = new JPanel(new BorderLayout())
      tabbedPane.add(s"Controller #${tab + 1}",panel)
      var dummyPanel = new JPanel()
      val controller = getController(tab)
      val deviceCombo = new JComboBox[String](Array("Pad - Keyboard","Pad - real pad","Mouse","Empty"))
      controller.device match
        case Empty =>
          deviceCombo.setSelectedIndex(3)
        case KeyboardPad =>
          deviceCombo.setSelectedIndex(0)
        case Mouse =>
          deviceCombo.setSelectedIndex(2)
        case RealPad =>
          deviceCombo.setSelectedIndex(1)
      dummyPanel.add(new JLabel("Device:"))
      dummyPanel.add(deviceCombo)
      panel.add("Center",dummyPanel)
      dummyPanel = new JPanel()
      val confButton = new JButton("Configure")
      confButton.setEnabled(controller.device != Empty)
      dummyPanel.add(confButton)
      panel.add("South",dummyPanel)

      confButton.addActionListener(_ => configure(tab,ControllerDevice.fromOrdinal(deviceCombo.getSelectedIndex)))
      deviceCombo.addActionListener(_ => confButton.setEnabled(deviceCombo.getSelectedIndex != 3))

    add("Center",tabbedPane)
    dialog.getContentPane.add("Center",this)
    dialog.setResizable(false)
    dialog.setLocationRelativeTo(frame)
    dialog.setSize(300,200)

  private def applyProp(prop:Option[Properties]): Unit =
    prop match
      case Some(p) =>
        import scala.jdk.CollectionConverters._
        for prop <- p.keys().asScala do
          config.setProperty(prop.toString,p.getProperty(prop.toString))
      case None =>

  private def configure(index:Int,controllerDevice: ControllerDevice): Unit =
    val controllerConfig = if controllerDevice == getController(index).device then config else new Properties()
    controllerDevice match
      case KeyboardPad =>
        val selectionPanel = new PadControllerButtonsSelectionPanel(dialog,index,controllerConfig,None,applyProp)
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
            val selectionPanel = new PadControllerButtonsSelectionPanel(dialog, index, controllerConfig, Some(device.toString), applyProp)
            selectionPanel.dialog.setVisible(true)