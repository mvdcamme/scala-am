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

  def log(message: String, messageLevel: LogLevel.Value): Unit = {
    if (messageLevel >= TracerFlags.level) {
      println(message)
    }
  }

  def log(messageObject: Object, messageLevel: LogLevel.Value): Unit = {
    log(messageObject.toString, messageLevel)
  }

}
