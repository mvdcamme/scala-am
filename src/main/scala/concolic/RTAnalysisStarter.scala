import backend.path_filtering.PartialRegexMatcher

trait RTAnalysisStarter {

  def discardSavedState(): Unit
  def saveCurrentState(): Unit
  def currentStateSaved: Boolean
  def startAnalysisFromSavedState(thenBranchTaken: Boolean): Option[PartialRegexMatcher]

}
