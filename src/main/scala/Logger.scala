/**
  * Created by mvdcamme on 11/04/16.
  */
object Logger {

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

  val D = LogLevel.Debug
  /* Executing traces */
  val V = LogLevel.Verbose
  /* Starting and ending tracing */
  val I = LogLevel.Info
  val E = LogLevel.Essential
  val U = LogLevel.Urgent
  val N = LogLevel.NoLogging

  def log(message: String, messageLevel: LogLevel.Value): Unit = {
    if (messageLevel >= GlobalFlags.level) {
      println(message)
    }
  }

  def log(messageObject: Object, messageLevel: LogLevel.Value): Unit = {
    log(messageObject.toString, messageLevel)
  }

}
