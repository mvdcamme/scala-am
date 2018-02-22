case class ConcolicRunTimeFlags(maxNrIterations: Int = 100,
                                checkAnalysis: Boolean = true,
                                checkRunTimeAnalysis: Boolean = true) {
  def useRunTimeAnalyses: Boolean = checkAnalysis && checkRunTimeAnalysis
}
