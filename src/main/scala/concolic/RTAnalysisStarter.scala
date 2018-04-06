import backend.PathConstraint

trait RTAnalysisStarter[RTAnalysisInitialState] {

  def stepCount: Int
  def abstractCurrentState(pathConstraint: PathConstraint): RTAnalysisInitialState
  def startAnalysisFromCurrentState(thenBranchTaken: Boolean, pathConstraint: PathConstraint): AnalysisResult
}
