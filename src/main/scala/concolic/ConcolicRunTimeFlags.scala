object ConcolicRunTimeFlags {

  val MAX_CONCOLIC_ITERATIONS = 100

  val checkAnalysis: Boolean = false
  val checkRunTimeAnalysis: Boolean = false

  private var ifEncountered: Boolean = false

  def setIfEncountered(): Unit = {
    ifEncountered = true
  }

  def wasIfEncountered: Boolean = {
    val temp = ifEncountered
    ifEncountered = false
    temp
  }

}
