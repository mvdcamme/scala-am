import backend.path_filtering.PartialRegexMatcher
import backend.tree.Constraint

trait RTAnalysisStarter {
  def stepCount: Int
  def startAnalysisFromCurrentState(thenBranchTaken: Boolean, pathConstraint: List[(Constraint, Boolean)]): AnalysisResult
}
