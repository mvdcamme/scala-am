import backend.{Path, PathConstraint}
import backend.path_filtering.PartialRegexMatcher
import backend.tree.Constraint

class PathStorage {

  private var currentPath: Path = Nil
  private var currentReport: PathConstraint = Nil

  def getCurrentPath: Path = currentPath.reverse /* Elements were added in reverse order, so reverse list now. */

  /**
    * Adds an element to the current path, without actually replacing the current path.
    * @param thenBranchTaken
    * @return
    */
  def addToPath(thenBranchTaken: Boolean): Path = {
    /* New path symbols are added in reverse order, i.e., to the front, for performance reasons. */
    val newPathElement = if (thenBranchTaken) backend.tree.path.ThenBranchTaken else backend.tree.path.ElseBranchTaken
    newPathElement :: currentPath
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

  def getCurrentReport: PathConstraint = currentReport.reverse /* Constraints were added in reverse order, so reverse list now. */

  /**
    * Adds the given parameters as an element to the current path constraint, without actually replacing the current
    * path constraint.
    * @param constraint
    * @param thenBranchTaken
    * @param maybePartialMatcher
    * @return
    */
  def addToReport(constraint: Constraint, thenBranchTaken: Boolean, maybePartialMatcher: Option[PartialRegexMatcher]): PathConstraint = {
    /* New constraints are added in reverse order, i.e., to the front, for performance reasons. */
    (constraint, thenBranchTaken, maybePartialMatcher) :: currentReport
  }
  def resetCurrentReport(): Unit = {
    currentReport = Nil
  }

  /**
    * Destructively adds the given parameters as an element to the current path constraint.
    * @param constraint
    * @param thenBranchTaken
    */
  def updateReport(constraint: Constraint, thenBranchTaken: Boolean, maybePartialMatcher: Option[PartialRegexMatcher]): Unit = {

    /* If a new partial matcher was constructed before executing the condition, the matcher is included in the triple.
     * NOTE: this partial matcher starts at a state corresponding to AFTER the branch. */
    currentReport = addToReport(constraint, thenBranchTaken, maybePartialMatcher)
  }

}
