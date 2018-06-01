import backend.PathConstraint

trait RTAnalysisStarter[RTAnalysisInitialState, RTAnalysisNormalState] {
  def stepCount: Int
  def abstractCurrentState(pathConstraint: PathConstraint): RTAnalysisInitialState
  def startAnalysisFromCurrentState(thenBranchTaken: Boolean, pathConstraint: PathConstraint): AnalysisResult[RTAnalysisNormalState]
  def getCurrentState: ConcolicMachineState
}
