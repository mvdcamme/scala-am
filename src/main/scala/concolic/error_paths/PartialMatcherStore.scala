import backend.path_filtering.PartialRegexMatcher

object PartialMatcherStore {
  private var maybeInitialPartialMatcher: Option[PartialRegexMatcher] = None
  private var maybeCurrentPartialMatcher: Option[PartialRegexMatcher] = None
  def get: Option[PartialRegexMatcher] = maybeCurrentPartialMatcher
  def getInitial: Option[PartialRegexMatcher] = maybeInitialPartialMatcher
  def setInitial(partialMatcher: PartialRegexMatcher): Unit = {
    /* The initial error partial matcher should only be set once */
    assert(maybeInitialPartialMatcher.isEmpty)
    maybeInitialPartialMatcher = Some(partialMatcher)
    reset()
  }
  def setCurrentMatcher(partialMatcher: PartialRegexMatcher): Unit = {
    maybeCurrentPartialMatcher = Some(partialMatcher)
  }
  def reset(): Unit = maybeInitialPartialMatcher match {
    case Some(initialPartialMatcher) => setCurrentMatcher(initialPartialMatcher)
    case None =>
  }
}