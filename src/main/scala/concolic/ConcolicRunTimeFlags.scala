sealed trait ConcolicEndCondition {
  def shouldStop(nrIterations: Int): Boolean
}
case class ConcolicTimeout(timeout: Timeout) extends ConcolicEndCondition {
  def shouldStop(nrIterations: Int): Boolean = {
    timeout.reached
  }
}
case class ConcolicMaxIterations(maxNrIterations: Int) extends ConcolicEndCondition {
  def shouldStop(nrIterations: Int): Boolean = {
    nrIterations >= maxNrIterations
  }
}

case class ConcolicRunTimeFlags(endCondition: ConcolicEndCondition = ConcolicMaxIterations(100),
                                checkAnalysis: Boolean = true, checkRunTimeAnalysis: Boolean = true,
                                tryMergePaths: Boolean = true) {
  def useRunTimeAnalyses: Boolean = checkAnalysis && checkRunTimeAnalysis
}
