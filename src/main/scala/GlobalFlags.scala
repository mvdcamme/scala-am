/**
  * Created by mvdcamme on 26/02/16.
  */
object GlobalFlags {

  val PRINT_ENTIRE_TRACE = true
  val PRINT_ACTIONS_EXECUTED = false

  val PRINT_EXECUTION_TIME = true

  /*
   * The current message level. All messages that are logged must have a level greater than or equal to this level
   * in order to be printed.
   */
  val level = Logger.D
}
