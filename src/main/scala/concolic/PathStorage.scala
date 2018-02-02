import backend.{Path, PathConstraint}
import backend.path_filtering.PartialRegexMatcher
import backend.tree.Constraint
import backend.tree.path.{ElseBranchTaken, ThenBranchTaken}

class PathStorage {

  private var currentPath: Path = Nil
  private var currentReport: PathConstraint = Nil

  def getCurrentPath: Path = currentPath

  /**
    * Adds an element to the current path, without actually replacing the current path.
    * @param thenBranchTaken
    * @return
    */
  def addToPath(thenBranchTaken: Boolean): Path = {
    val newPathElement = if (thenBranchTaken) backend.tree.path.ThenBranchTaken else backend.tree.path.ElseBranchTaken
    currentPath :+ newPathElement
  }
  def resetCurrentPath(): Unit = {
    currentPath = Nil
  }

  /**
    * Destructively adds an element to the current path.
    * @param thenBranchTaken
    */
  def updateCurrentPath(thenBranchTaken: Boolean): Unit = {
    currentPath = addToPath(thenBranchTaken)
  }

  def getCurrentReport: PathConstraint = currentReport

  /**
    * Adds the given parameters as an element to the current path constraint, without actually replacing the current
    * path constraint.
    * @param constraint
    * @param thenBranchTaken
    * @param maybePartialMatcher
    * @return
    */
  private def addToReport(constraint: Constraint, thenBranchTaken: Boolean, maybePartialMatcher: Option[PartialRegexMatcher]): PathConstraint = {
    currentReport :+ (constraint, thenBranchTaken, maybePartialMatcher)
  }
  def resetCurrentReport(): Unit = {
    currentReport = Nil
  }

  /**
    * Destructively adds the given parameters as an element to the current path constraint.
    * @param constraint
    * @param thenBranchTaken
    */
  def updateReport(constraint: Constraint, thenBranchTaken: Boolean): Unit = {

    /* If a new partial matcher was constructed before executing the condition, the matcher is included in the triple.
     * NOTE: this partial matcher starts at a state corresponding to BEFORE the branch. */
    currentReport = addToReport(constraint, thenBranchTaken, maybeIncludePartialMatcher)
  }

  private def maybeIncludePartialMatcher: Option[PartialRegexMatcher] = {
    if (ConcolicRunTimeFlags.useRunTimeAnalyses && ConcolicRunTimeFlags.checkHasCompletedAnalysis) {
      PartialMatcherStore.getCurrent
    } else {
      None
    }
  }

}
