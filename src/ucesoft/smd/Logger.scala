package ucesoft.smd

import java.io.{PrintWriter, StringWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.logging.Level

object Logger:
  private var logger = new Logger

  def getLogger : Logger = logger
  
  def setLogger(logAction: String => Unit): Logger =
    logger = new Logger:
      override def logImpl(log: String): Unit = logAction(log)
    logger


class Logger private () :
  import Logger.*
  private val dateFormatter = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss")

  private var level : Level = Level.INFO

  def setLevel(level:Level): Unit = this.level = level

  protected def format(level:Level,msg:String,t:Throwable): String =
    val now = LocalDateTime.now()
    val formatted = s"${now.format(dateFormatter)} [${"%7s".format(level)}] $msg"
    if t == null then
      formatted
    else
      val sw = new StringWriter()
      t.printStackTrace(new PrintWriter(sw))
      s"$formatted :\n$sw"

  def log(level:Level,msg:String,t:Throwable = null): Unit =
    if level.intValue() >= this.level.intValue() then
      logImpl(format(level,msg,t))

  def log[T](tmpLevel:Level)(body : => T): T =
    val currentLevel = level
    setLevel(tmpLevel)
    try
      body
    finally
      setLevel(currentLevel)

  def debug(msg: => String,t:Throwable = null): Unit = log(Level.FINE,msg,t)
  def info(msg: => String,t:Throwable = null): Unit = log(Level.INFO,msg,t)
  def warning(msg: => String,t:Throwable = null): Unit = log(Level.WARNING,msg,t)
  def error(msg: => String,t:Throwable = null): Unit = log(Level.SEVERE,msg,t)

  protected def logImpl(log:String): Unit =
    println(log)

