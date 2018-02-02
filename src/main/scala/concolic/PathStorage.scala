import backend.{Path, PathConstraint}
import backend.path_filtering.PartialRegexMatcher
import backend.tree.Constraint
import backend.tree.path.{ElseBranchTaken, ThenBranchTaken}

class PathStorage {

  /*
   * For performance reasons, new constraints are added to the FRONT of the list, so when returning the path,
   * the list should be reversed first.
   */
  private var currentPath: Path = Nil
  /*
   * For performance reasons, new constraints are added to the FRONT of the list, so when returning the report,
   * the list should be reversed first.
   */
  private var currentReport: PathConstraint = Nil

  def getCurrentPath: Path = currentPath.reverse
  def resetCurrentPath(): Unit = {
    currentPath = Nil
  }
  def addToCurrentPath(thenBranchTaken: Boolean): Unit = {
    /* New constraints are added to the front of the list for performance reasons. */
    currentPath ::= (if (thenBranchTaken) backend.tree.path.ThenBranchTaken else backend.tree.path.ElseBranchTaken)
  }

  /* Report is reversed first, as new constraints are added to the front of the list for performance reasons. */
  def getCurrentReport: PathConstraint = currentReport.reverse
  def resetCurrentReport(): Unit = {
    currentReport = Nil
  }

  def addConstraint(constraint: Constraint, thenBranchTaken: Boolean): Unit = {
    /* New constraints are added to the front of the list for performance reasons. */

    /* If a new partial matcher was constructed before executing the condition, the matcher is included in the triple.
     * NOTE: this partial matcher starts at a state corresponding to BEFORE the branch. */
    currentReport ::= (constraint, thenBranchTaken, maybeIncludePartialMatcher)
  }

  private def maybeIncludePartialMatcher: Option[PartialRegexMatcher] = {
    if (ConcolicRunTimeFlags.useRunTimeAnalyses && ConcolicRunTimeFlags.checkHasCompletedAnalysis) {
      PartialMatcherStore.getCurrent
    } else {
      None
    }
  }

}
