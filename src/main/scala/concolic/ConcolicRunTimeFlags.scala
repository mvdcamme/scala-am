object ConcolicRunTimeFlags {

  val MAX_CONCOLIC_ITERATIONS = 100

  val checkAnalysis: Boolean = true
  val checkRunTimeAnalysis: Boolean = true

  def useRunTimeAnalyses: Boolean = checkAnalysis && checkRunTimeAnalysis

}
