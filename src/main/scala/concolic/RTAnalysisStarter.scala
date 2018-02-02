import backend.PathConstraint
import backend.path_filtering.PartialRegexMatcher

trait RTAnalysisStarter {
  def startAnalysisFromCurrentState(thenBranchTaken: Boolean, pathConstraint: PathConstraint): Option[PartialRegexMatcher]
}
