/**
  * Created by mvdcamme on 11/04/16.
  */
object Logger {

  private val logFilePath: String = "log.txt"

  {
    /* Reset the log file upon creation of the Logger object. */
    resetLogFile()
  }

  /*
   * All debug levels. Higher levels correspond to more important messages.
   */
  object LogLevel extends Enumeration {
    type LogLevel = Value
    val Debug = Value(1)
    val Verbose = Value(2)
    val Info = Value(3)
    val Essential = Value(4)
    val Urgent = Value(5)
    val NoLogging = Value(Integer.MAX_VALUE)
  }

  /*
   * resetLogFile is called in the Logger-object's constructor-block. As this function uses the logFilePath-field,
   * make sure to define this field _before_ the constructor-block, or else the field will be null.
   */
  private def resetLogFile(): Unit = {
    val fileTemp = new java.io.File(logFilePath)
    if (fileTemp.exists) {
      fileTemp.delete()
    }
  }
  private def logToFile(str: String): Unit = {
    val writer = new java.io.BufferedWriter(new java.io.FileWriter(logFilePath, true))
    writer.write(str)
    writer.write("\n")
    writer.close()
  }

  val D = LogLevel.Debug
  val V = LogLevel.Verbose
  val I = LogLevel.Info
  val E = LogLevel.Essential
  val U = LogLevel.Urgent
  val N = LogLevel.NoLogging

  private def internalLog(message: => String, messageLevel: LogLevel.Value): Unit = {
    if (messageLevel >= GlobalFlags.level) {
      println(message)
      logToFile(message)
    }
  }

  def log(message: => Any, messageLevel: LogLevel.Value): Unit = {
    internalLog(message.toString, messageLevel)
  }
  def D(message: => Any): Unit = log(message, D)
  def V(message: => Any): Unit = log(message, V)
  def I(message: => Any): Unit = log(message, I)
  def E(message: => Any): Unit = log(message, E)
  def U(message: => Any): Unit = log(message, U)
  /* We don't define a method for the N-level, since that level is not intended to actually log messages in. */

}
