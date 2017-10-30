object GlobalFlags {

  val PRINT_EDGE_ANNOTATIONS_FULL = false
  val PRINT_EXECUTION_TIME = true

  var ANALYSIS_RESULTS_OUTPUT: Option[String] = None

  /*
   * The current message level. All messages that are logged must have a level greater than or equal to this level
   * in order to be printed.
   */
  val level = Logger.E

  val ANALYSIS_PATH = "../scala-am misc/Analysis/"

  val AAM_CHECK_SUBSUMES = true

  var CURRENT_PROGRAM: String = ""
}