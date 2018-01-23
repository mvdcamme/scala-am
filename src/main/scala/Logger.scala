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

  def log(messageObject: => Object, messageLevel: LogLevel.Value): Unit = {
    internalLog(messageObject.toString, messageLevel)
  }

}
