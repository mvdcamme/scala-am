object ConcolicRunTimeFlags {

  val MAX_CONCOLIC_ITERATIONS = 100

  val checkAnalysis: Boolean = true
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

}
