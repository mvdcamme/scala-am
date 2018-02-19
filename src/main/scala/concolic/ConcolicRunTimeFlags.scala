object ConcolicRunTimeFlags {

  private var randomConsId = 0

  val MAX_CONCOLIC_ITERATIONS = 100

  val checkAnalysis: Boolean = true
  val checkRunTimeAnalysis: Boolean = true

  def useRunTimeAnalyses: Boolean = checkAnalysis && checkRunTimeAnalysis

  def newRandomConsId: Int = {
    randomConsId += 1
    randomConsId
  }

}
