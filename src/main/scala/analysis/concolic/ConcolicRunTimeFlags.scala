object ConcolicRunTimeFlags {

  val checkAnalysis: Boolean = true
  val checkRunTimeAnalysis: Boolean = true

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
