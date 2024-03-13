package ucesoft.smd.ui

import ucesoft.smd.MessageBus
import ucesoft.smd.audio.AudioDevice
import ucesoft.smd.misc.Preferences

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Dimension, FlowLayout}
import javax.swing.*
/**
 * @author Alessandro Abbruzzetti
 *         Created on 23/01/2024 10:59  
 */
class AudioVolumePanel(frame:JFrame,audioDevices:Array[AudioDevice],pref:Preferences,closeAction:() => Unit) extends JPanel with MessageBus.MessageListener:
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
    inline val XGAP = 5
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
      val audioBufferPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      val buffers = Array("5","10","20","50","100")
      val audioBufferCombo = new JComboBox[String](buffers)
      audioBufferPanel.add(new JLabel("Buffer in millis:",SwingConstants.RIGHT))
      audioBufferPanel.add(audioBufferCombo)
      devicePanel.add("South",audioBufferPanel)
      val buffer = device.name match
        case "PSG" =>
          pref.get[String](Preferences.PSG_AUDIO_BUFF_MILLIS).map(_.value).getOrElse("10")
        case "FM" =>
          pref.get[String](Preferences.FM_AUDIO_BUFF_MILLIS).map(_.value).getOrElse("10")
        case _ =>
          "10"
      audioBufferCombo.setSelectedItem(buffer)
      audioBufferCombo.addActionListener(_ => {
        val millis = audioBufferCombo.getSelectedItem.toString.toInt
        device.setBufferInMillis(millis)
        device.name match
          case "PSG" =>
            pref.update(Preferences.PSG_AUDIO_BUFF_MILLIS, millis)
          case "FM" =>
            pref.update(Preferences.FM_AUDIO_BUFF_MILLIS, millis)
          case _ =>
      })

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



