package ucesoft.smd

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.logging.Level

object Logger:
  private var logger = new Logger

  def getLogger : Logger = logger
  
  def setLogger(logAction: String => Unit): Logger =
    logger = new Logger:
      override def addLog(log: String): Unit = logAction(log)
    logger


class Logger private () :
  private val dateFormatter = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss")
  private var level : Level = Level.INFO

  def setLevel(level:Level): Unit = this.level = level

  inline private def format(level:Level,msg:String): String =
    val now = LocalDateTime.now()
    s"${now.format(dateFormatter)} [${"%7s".format(level)}] $msg"

  final def log(level:Level,fmt:String,pars:Any*): Unit =
    if level.intValue() >= this.level.intValue() then
      addLog(format(level,fmt.format(pars*)))

  def log[T](tmpLevel:Level)(body : => T): T =
    val currentLevel = level
    setLevel(tmpLevel)
    try
      body
    finally
      setLevel(currentLevel)

  final def debug(format:String,pars:Any*): Unit = log(Level.FINE,format,pars*)
  final def info(format:String,pars:Any*): Unit = log(Level.INFO,format,pars*)
  final def warning(format:String,pars:Any*): Unit = log(Level.WARNING,format,pars*)
  final def error(format:String,pars:Any*): Unit = log(Level.SEVERE,format,pars*)

  def addLog(log:String): Unit =
    println(log)

