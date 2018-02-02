import backend.path_filtering.PartialRegexMatcher

trait RTAnalysisStarter {
  def startAnalysisFromCurrentState(thenBranchTaken: Boolean): Option[PartialRegexMatcher]
}
