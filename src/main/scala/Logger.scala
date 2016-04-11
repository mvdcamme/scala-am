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
  }

  val D = LogLevel.Debug
  val V = LogLevel.Verbose
  val I = LogLevel.Info
  val E = LogLevel.Essential

  /*
   * The current message level. All messages that are logged must have a level greater than or equal to this level
   * in order to be printed.
   */
  val level = E

  def log(message: String, messageLevel: LogLevel.Value): Unit = {
    if (messageLevel >= level) {
      println(message)
    }
  }

  def log(messageObject: Object, messageLevel: LogLevel.Value): Unit = {
    log(messageObject.toString, messageLevel)
  }

}
