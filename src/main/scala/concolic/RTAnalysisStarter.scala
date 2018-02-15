import backend.PathConstraint

trait RTAnalysisStarter {
  def stepCount: Int
  def startAnalysisFromCurrentState(thenBranchTaken: Boolean, pathConstraint: PathConstraint): AnalysisResult
}
