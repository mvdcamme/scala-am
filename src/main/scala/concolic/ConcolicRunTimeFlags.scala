object ConcolicRunTimeFlags {

  private var randomConsId = 0

  val MAX_CONCOLIC_ITERATIONS = 100

  val checkAnalysis: Boolean = true
  val checkRunTimeAnalysis: Boolean = false

  private var hasCompletedAnalysis: Boolean = false

  def setHasCompletedAnalysis(): Unit = {
    hasCompletedAnalysis = true
  }

  def checkHasCompletedAnalysis: Boolean = {
    val temp = hasCompletedAnalysis
    hasCompletedAnalysis = false
    temp
  }

  def useRunTimeAnalyses: Boolean = checkAnalysis && checkRunTimeAnalysis

  def newRandomConsId: Int = {
    randomConsId += 1
    randomConsId
  }

}
