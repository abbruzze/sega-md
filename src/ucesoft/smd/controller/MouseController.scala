package ucesoft.smd.controller

import ucesoft.smd.Display

import java.awt.{Point, Robot, Toolkit}
import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import javax.swing.{SwingUtilities, Timer}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 24/12/2023 17:36
 *
 *
 * The protocol works as follows: TH and TR are outputs which tell the microcontroller to stop or start a data transfer, and to
  acknowledge received data. TL is an input which returns a busy flag for the microcontroller. D3-D0 are inputs that return the data.

  Here's a table showing the communication process:
  Write TH TR TL D3 D2 D1 D0 Description
  $60 1 1 1 0 0 0 0 Request data
  $20 0 1 1 0 0 0 0 ID #0 ($0)
  $00 0 0 1 1 0 1 1 ID #1 ($B)
  $20 0 1 0 1 1 1 1 ID #2 ($F)
  $00 0 0 1 1 1 1 1 ID #3 ($F)
  $20 0 1 0 Y Over X Over Y Sign X Sign Axis sign and overflow
  $00 0 0 1 Start Middle Right Left Button state
  $20 0 1 0 X7 X6 X5 X4 X axis MSN
  $00 0 0 1 X3 X2 X1 X0 X axis LSN
  $20 0 1 0 Y7 Y6 Y5 Y4 Y axis MSN
  $00 0 0 1 Y3 Y2 Y1 Y0 Y axis LSN

  Write #$60 when you are done polling to stop the data transfer. If you continue to poll the mouse after collecting all the data by
  writing $20 / $00, the Y axis LSN will always be returned. Sega advises polling beyond this point will make the mouse behave
  abnormally. It's possible that the PIC code in some versions of the Mega Mouse may have problems if the poll sequence is too
  long.
  The X/Y overflow flags are supposed to be set if the mouse is moved over a distance greater than can be measured. I can't get
  them to become set, maybe the mouse supports a fairly wide range of movement.
  You need a considerable delay between writing new values to TH/TR. I think the intention is to poll TL until the microcontroller
  returns 'not busy', but I can't get this to work reliably. Sega's mouse reading code also has a timeout when checking TL which
  would indicate it may get stuck in a certain state.
  All buttons (start, left, middle, right) are active-high logic, so they return '1' when pressed and '0' when released.
 */
class MouseController(override val index: Int,display:Display) extends Controller with MouseMotionListener with MouseListener:
  private inline val LEFT_BUTTON_MASK = 0x1
  private inline val RIGHT_BUTTON_MASK = 0x2
  private inline val MIDDLE_BUTTON_MASK = 0x4
  private inline val START_BUTTON_MASK = 0x8

  private var buttonsState = 0
  private var state = 0x60
  private var counter = 0
  private var mx, my = 0
  private var waitHandshake = 0
  private val robot = new Robot()
  private val emptyCursor = {
    val cursor = new java.awt.image.BufferedImage(16, 16, java.awt.image.BufferedImage.TYPE_INT_ARGB)
    Toolkit.getDefaultToolkit.createCustomCursor(cursor,new Point(0, 0),"null")
  }
  private var mouseStartWithCTRLandLeftEnabled = false
  override def setControllerType(ct: ControllerType): Unit =
    ct match
      case ControllerType.MouseStartWithCTRLAndLeft =>
        mouseStartWithCTRLandLeftEnabled = true
      case _ =>
        mouseStartWithCTRLandLeftEnabled = false

  def mouseEnabled(enabled:Boolean): Unit =
    val window = SwingUtilities.getWindowAncestor(display)
    if enabled then
      window.setCursor(emptyCursor)
      display.addMouseListener(this)
      display.addMouseMotionListener(this)
    else
      window.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR))
      display.removeMouseListener(this)
      display.removeMouseMotionListener(this)

  override def disconnect(): Unit =
    mouseEnabled(false)

  override def readData(): Int =
    var read = 0
    counter match
      case 0 =>
        read = 0
      case 1 => // xxxx1011
        read = 0xB
      case 2|3 => // xxxx1111
        read = 0xF
      case 4 => // Axis sign & overflow
        read = 0
        if mx < 0 then read |= 1
        if my < 0 then read |= 2
      case 5 => // START, A, B, C buttons state (active high)
        read = buttonsState & 0x0F
      case 6 => // X Axis MSB
        read = (mx >> 4) & 0x0F
      case 7 => // X Axis LSB
        read = mx & 0x0F
      case 8 => // Y Axis MSB
        read = (my >> 4) & 0x0F
      case 9 => // Y Axis LSB
        read = my & 0x0F

      if waitHandshake > 0 then
        waitHandshake -= 1
        // TL = !TR (handshake in progress)
        read |= (~state & 0x20) >> 1
      else
        // TL = TR (handshake completed)
        read |= (state & 0x20) >> 1

    read

  override def writeData(_value: Int): Unit =
    val value = (state & ~control) | (_value & control)
    if ((state ^ value) & 0x20) != 0 then // TR transition
      if counter > 0 && counter < 9 then
        counter += 1
        // TL handshake latency
        waitHandshake = 2

    if ((state ^ value) & 0x40) != 0 then // TH transition
      counter = (state >> 6) & 1

    state = value

  // =============================================================
  override def mouseDragged(e: MouseEvent): Unit = {}

  override def mouseMoved(e: MouseEvent): Unit =
    val loc = display.getLocationOnScreen
    val cx = loc.x + (display.getWidth >> 1)
    val cy = loc.y + (display.getHeight >> 1)
    mx = e.getX - (display.getWidth >> 1)
    my = (display.getHeight >> 1) - e.getY
    val timer = new Timer(1,_ => robot.mouseMove(cx,cy))
    timer.setRepeats(false)
    timer.start()

  override def mouseClicked(e: MouseEvent): Unit = {}

  override def mousePressed(e: MouseEvent): Unit =
    if SwingUtilities.isLeftMouseButton(e) then
      if e.isControlDown then
        buttonsState |= START_BUTTON_MASK
      else
        buttonsState |= LEFT_BUTTON_MASK
    else if SwingUtilities.isRightMouseButton(e) then
      buttonsState |= RIGHT_BUTTON_MASK
    else if SwingUtilities.isMiddleMouseButton(e) then
      buttonsState |= MIDDLE_BUTTON_MASK

  override def mouseReleased(e: MouseEvent): Unit =
    if SwingUtilities.isLeftMouseButton(e) then
      buttonsState &= ~LEFT_BUTTON_MASK
      buttonsState &= ~START_BUTTON_MASK
    else if SwingUtilities.isRightMouseButton(e) then
      buttonsState &= ~RIGHT_BUTTON_MASK
    else if SwingUtilities.isMiddleMouseButton(e) then
      buttonsState &= ~MIDDLE_BUTTON_MASK

  override def mouseEntered(e: MouseEvent): Unit = {}

  override def mouseExited(e: MouseEvent): Unit = {}


