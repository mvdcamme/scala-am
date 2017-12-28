object ConcolicRunTimeFlags {

  private var randomConsId = 0

  val MAX_CONCOLIC_ITERATIONS = 100

  val checkAnalysis: Boolean = false
  val checkRunTimeAnalysis: Boolean = false

  private var startRunTimeAnalysis: Boolean = false

  def setStartRunTimeAnalysis(): Unit = {
    startRunTimeAnalysis = true
  }

  def shouldStartRunTimeAnalysis: Boolean = {
    val temp = startRunTimeAnalysis
    startRunTimeAnalysis = false
    temp
  }

  def useRunTimeAnalyses: Boolean = checkAnalysis && checkRunTimeAnalysis

  def newRandomConsId: Int = {
    randomConsId += 1
    randomConsId
  }

}
