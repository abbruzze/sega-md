package ucesoft.smd

import java.io.*

/**
 * @author Alessandro Abbruzzetti
 *         Created on 28/12/2023 18:08  
 */
class StateHandler:
  private var outputStream : ObjectOutputStream = _
  private var inputStream : ObjectInputStream = _
  def writeComponent(name:String): StateHandler =
    if outputStream == null then throw IllegalStateException()
    outputStream.writeUTF(name)
    this
  def readComponent(checkName:String): StateHandler =
    if inputStream == null then throw IllegalStateException()
    val find = inputStream.readUTF()
    if find != checkName then
      throw new IllegalArgumentException(s"Bad component: find $find, expected $checkName")
    this
  def close(): Unit =
    if outputStream != null then
      outputStream.close()
      outputStream = null
    else if inputStream != null then
      inputStream.close()
      inputStream = null

  def openOutput(file:String): Unit =
    outputStream = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(file)))
  def openInput(file:String): Unit =
    inputStream = new ObjectInputStream(new BufferedInputStream(new FileInputStream(file)))

  // ====================== writes ======================================
  def w(b:Byte): StateHandler =
    if outputStream == null then throw IllegalStateException()
    outputStream.writeByte(b)
    this
  def w(i: Int): StateHandler =
    if outputStream == null then throw IllegalStateException()
    outputStream.writeInt(i)
    this
  def w(l: Long): StateHandler =
    if outputStream == null then throw IllegalStateException()
    outputStream.writeLong(l)
    this
  def w(f: Float): StateHandler =
    if outputStream == null then throw IllegalStateException()
    outputStream.writeFloat(f)
    this
  def w(d: Double): StateHandler =
    if outputStream == null then throw IllegalStateException()
    outputStream.writeDouble(d)
    this
  def w(s: String): StateHandler =
    if outputStream == null then throw IllegalStateException()
    outputStream.writeUTF(s)
    this
  def ser(o: Object): StateHandler =
    if outputStream == null then throw IllegalStateException()
    outputStream.writeObject(o)
    this
  def w[T](a: Array[T]): StateHandler =
    if outputStream == null then throw IllegalStateException()
    outputStream.writeObject(a)
    this
  // ====================== reads =======================================
  def fillArray(dest:Array[_]): Unit =
    dest match
      case a: Array[Array[Array[_]]] =>
        val src = inputStream.readObject().asInstanceOf[Array[Array[Array[_]]]]
        for i <- src.indices do
          val src2 = src(i)
          for j <- src2.indices do
            val src3 = src2(j)
            System.arraycopy(src3,0,a(i)(j),0,src3.length)
      case a: Array[Array[_]] =>
        val src = inputStream.readObject().asInstanceOf[Array[Array[_]]]
        for i <- src.indices do
          val src2 = src(i)
          System.arraycopy(src2,0,a(i),0,src2.length)
      case a: Array[_] =>
        val src = inputStream.readObject().asInstanceOf[Array[_]]
        System.arraycopy(src,0,a,0,src.length)
      case _ =>
        throw new IllegalArgumentException("Invalid array structure")