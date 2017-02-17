object GlobalFlags {

  val PRINT_EDGE_ANNOTATIONS_FULL = false
  val PRINT_ACTIONS_EXECUTED = false
  val PRINT_ENTIRE_TRACE = false
  val PRINT_EXECUTION_TIME = false

  var ANALYSIS_RESULTS_OUTPUT: Option[String] = None

  /*
   * The current message level. All messages that are logged must have a level greater than or equal to this level
   * in order to be printed.
   */
  val level = Logger.U

  var APPLY_OPTIMIZATION_ENVIRONMENTS_LOADING = false
  var APPLY_OPTIMIZATION_CONTINUATIONS_LOADING = false
  var APPLY_OPTIMIZATION_CONSTANT_FOLDING = false
  var APPLY_OPTIMIZATION_TYPE_SPECIALIZED_ARITHMETICS = false
  var APPLY_OPTIMIZATION_VARIABLE_FOLDING = false
  var APPLY_OPTIMIZATION_MERGE_ACTIONS = false
}