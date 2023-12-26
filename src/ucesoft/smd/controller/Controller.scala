package ucesoft.smd.controller

import ucesoft.smd.*

enum ControllerType(val counterMask:Int):
  case PAD3Buttons extends ControllerType(1)
  case PAD6Buttons extends ControllerType(7)

abstract class Controller extends SMDComponent:
  val index : Int
  protected var control = 0

  def setControllerType(ct:ControllerType): Unit

  def readData(): Int
  def writeData(value: Int): Unit
  //
  def readControl: Int = control
  def writeControl(value: Int): Unit =
    control = value

  protected def performDataWrite(data:Int): Unit = {}


