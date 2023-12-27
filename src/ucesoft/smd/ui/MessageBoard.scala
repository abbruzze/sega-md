package ucesoft.smd.ui

import ucesoft.smd.ui.MessageBoard.XPOS.CENTER

import java.awt.{Color, Font}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 27/12/2023 19:22  
 */
object MessageBoard:
  enum YPOS:
    case TOP, CENTER, BOTTOM

  enum XPOS:
    case LEFT, CENTER, RIGHT
  enum LOGO:
    case SHOW, HIDE, IGNORE

  case class Message(text: String, xpos: XPOS, ypos: YPOS, millis: Int, color: Option[Color], fadingMillis: Option[Int], font: Option[Font], showLogo: LOGO, yoffset: Int)  
  
  trait MessageBoardListener:
    def addMessage(msg:Message): Unit
    
  def builder: MessageBuilder = new MessageBuilder
    
  class MessageBuilder:
    private var text = ""
    private var xpos = XPOS.CENTER
    private var ypos = YPOS.CENTER
    private var color : Option[Color] = None
    private var fadingMillis : Option[Int] = None
    private var font : Option[Font] = None
    private var delay = 0
    private var showLogoImage = LOGO.IGNORE
    private var yoffset = 0
    
    def showLogo(): MessageBuilder =
      showLogoImage = LOGO.SHOW ; this
    def hideLogo(): MessageBuilder =
      showLogoImage = LOGO.HIDE; this
    def message(text:String): MessageBuilder =
      this.text = text ; this
    def xcenter(): MessageBuilder =
      xpos = XPOS.CENTER ; this
    def xleft(): MessageBuilder =
      xpos = XPOS.LEFT ; this
    def xright(): MessageBuilder =
      xpos = XPOS.RIGHT ; this
    def ycenter(): MessageBuilder =
      ypos = YPOS.CENTER;
      this
    def ytop(): MessageBuilder =
      ypos = YPOS.TOP
      this
    def ybottom(): MessageBuilder =
      ypos = YPOS.BOTTOM
      this
    def color(color:Color): MessageBuilder =
      this.color = Some(color) ; this
    def font(font:Font): MessageBuilder =
      this.font = Some(font) ; this
    def fadingMilliseconds(millis:Int): MessageBuilder =
      this.fadingMillis = Some(millis) ; this  
    def delay(millis:Int): MessageBuilder =
      this.delay = millis ; this
    def yoffset(offset:Int): MessageBuilder =
      yoffset = offset ; this
    def build(): Message = Message(text,xpos,ypos,delay,color,fadingMillis,font,showLogoImage,yoffset)
      
    
