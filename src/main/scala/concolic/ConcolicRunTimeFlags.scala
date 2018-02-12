object ConcolicRunTimeFlags {

  private var randomConsId = 0

  val MAX_CONCOLIC_ITERATIONS = 100

  val checkAnalysis: Boolean = false
  val checkRunTimeAnalysis: Boolean = false

  def useRunTimeAnalyses: Boolean = checkAnalysis && checkRunTimeAnalysis

  def newRandomConsId: Int = {
    randomConsId += 1
    randomConsId
  }

}
