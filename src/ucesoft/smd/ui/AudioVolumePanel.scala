package ucesoft.smd.ui

import ucesoft.smd.MessageBus
import ucesoft.smd.audio.AudioDevice

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Dimension, FlowLayout}
import javax.swing.*
/**
 * @author Alessandro Abbruzzetti
 *         Created on 23/01/2024 10:59  
 */
class AudioVolumePanel(frame:JFrame,audioDevices:Array[AudioDevice],closeAction:() => Unit) extends JPanel with MessageBus.MessageListener:
  final val dialog = new JDialog(frame,"Audio controls")
  private var sliders : Array[(JSlider,AudioDevice,JCheckBox)] = scala.compiletime.uninitialized
  init()

  override def onMessage(msg: MessageBus.Message): Unit =
    import MessageBus.*
    msg match
      case WarpModeMessage(_,on) =>
        muteAll(on)
      case AudioEnabledMessage(_,enabled) =>
        muteAll(!enabled)
      case AudioChangeVolume(_,up) =>
        for (slider,device,_) <- sliders do
          val vol = slider.getValue
          if up then
            if vol < slider.getMaximum then
              slider.setValue(vol + 1)
              device.setMasterVolume(vol + 1)
          else
            if vol > slider.getMinimum then
              slider.setValue(vol - 1)
              device.setMasterVolume(vol - 1)
      case _ =>
  private def muteAll(mute:Boolean): Unit =
    for (_,device,cb) <- sliders do
      cb.setSelected(mute)
      device.mute(mute)

  private def init(): Unit  =
    inline val XGAP = 15
    setLayout(new BorderLayout())
    val controlPanel = new JPanel()
    val boxLayout = new BoxLayout(controlPanel,BoxLayout.X_AXIS)
    controlPanel.setLayout(boxLayout)
    controlPanel.add(Box.createRigidArea(new Dimension(XGAP, 0)))

    add("Center",controlPanel)
    val setAllCB = new JCheckBox("Set all")
    setAllCB.setSelected(true)
    val northPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    northPanel.add(setAllCB)
    add("North",northPanel)

    sliders = for device <- audioDevices yield
      val muteCB = new JCheckBox("Mute")
      val volumeSlider = new JSlider(SwingConstants.VERTICAL,0,100,AudioDevice.INITIAL_VOLUME)
      val devicePanel = new JPanel(new BorderLayout())
      devicePanel.add("North",muteCB)
      devicePanel.add("Center",volumeSlider)
      devicePanel.setBorder(BorderFactory.createTitledBorder(device.name))

      muteCB.addActionListener(_ => device.mute(muteCB.isSelected))
      volumeSlider.addChangeListener(e => {
        if !volumeSlider.getValueIsAdjusting then
          device.setMasterVolume(volumeSlider.getValue)
          if setAllCB.isSelected then
            sliders.filterNot(_._1 == e.getSource).foreach(s => {
              s._1.setValue(volumeSlider.getValue)
              s._2.setMasterVolume(volumeSlider.getValue)
            })
      })
      volumeSlider.setMajorTickSpacing(50)
      volumeSlider.setPaintTicks(true)
      volumeSlider.setPaintLabels(true)

      controlPanel.add(devicePanel)
      controlPanel.add(Box.createRigidArea(new Dimension(XGAP, 0)))

      (volumeSlider,device,muteCB)


    dialog.getContentPane.add("Center",this)
    dialog.pack()
    dialog.setResizable(false)
    dialog.addWindowListener(new WindowAdapter:
      override def windowClosing(e: WindowEvent): Unit =
        closeAction()
    )



