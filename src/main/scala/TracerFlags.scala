/**
  * Created by mvdcamme on 26/02/16.
  */
object TracerFlags {

  val DO_TRACING = true
  val TRACING_THRESHOLD = 5

  val APPLY_OPTIMIZATIONS = true
  val SWITCH_ABSTRACT = false

  val PRINT_ENTIRE_TRACE = false
  val PRINT_ACTIONS_EXECUTED = false

  val PRINT_EXECUTION_TIME = false

  /*
   * The current message level. All messages that are logged must have a level greater than or equal to this level
   * in order to be printed.
   */
  val level = Logger.E
}
