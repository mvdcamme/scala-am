import backend.path_filtering.PartialRegexMatcher
import backend.tree.Constraint

trait RTAnalysisStarter {
  def startAnalysisFromCurrentState(thenBranchTaken: Boolean, pathConstraint: List[(Constraint, Boolean)]): AnalysisResult
}
