import backend._
import backend.tree._
import backend.tree.path._

case object AbortConcolicRunException extends Exception

object ScalaAMReporter {

  private var doConcolic: Boolean = false

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


  import scala.language.implicitConversions
  implicit def pathToString(path: Path): String = {
    path.map({
      case ElseBranchTaken => "e"
      case ThenBranchTaken => "t"
    }).mkString("")
  }

  def getCurrentPath: Path = currentPath.reverse
  private def resetCurrentPath(): Unit = {
    currentPath = Nil
  }
  def addToCurrentPath(thenBranchTaken: Boolean): Unit = {
    /* New constraints are added to the front of the list for performance reasons. */
    currentPath ::= (if (thenBranchTaken) backend.tree.path.ThenBranchTaken else backend.tree.path.ElseBranchTaken)
  }

  /* Report is reversed first, as new constraints are added to the front of the list for performance reasons. */
  def getCurrentReport: PathConstraint = currentReport.reverse
  private def resetCurrentReport(): Unit = {
    currentReport = Nil
  }
  private def addConstraint(constraint: Constraint, thenBranchTaken: Boolean): Unit = {
    /* New constraints are added to the front of the list for performance reasons. */
    currentReport ::= (constraint, thenBranchTaken)
  }

  private def testCurrentPath: Boolean = {
    Logger.log(s"Current pathstring is ${pathToString(getCurrentPath)}", Logger.V)
    val (matched, newPartialMatcher) = PartialMatcherStore.get.get.incrementalMatch(getCurrentPath)
    PartialMatcherStore.setCurrentMatcher(newPartialMatcher)
    if (matched) {
      /* We're using the incremental match for performance reasons, so the string should be reset if there is a match. */
      resetCurrentPath()
    }
    matched
  }

  def doErrorPathsDiverge: Boolean = {
    val currentPathFollowingElse = (ElseBranchTaken :: currentPath).reverse
    val currentPathFollowingThen = (ThenBranchTaken :: currentPath).reverse
    val isErrorViaElse = PartialMatcherStore.get.get.tentativeIncrementalMatch(currentPathFollowingElse)
    val isErrorViaThen = PartialMatcherStore.get.get.tentativeIncrementalMatch(currentPathFollowingThen)
    isErrorViaElse && isErrorViaThen
  }

  def enableConcolic(): Unit = {
    doConcolic = true
  }
  def disableConcolic(): Unit = {
    doConcolic = false
  }
  def isConcolicEnabled: Boolean = doConcolic
  def clear(isFirstClear: Boolean): Unit = {
    Reporter.clear()
    InputVariableStore.reset()
    resetCurrentPath()
    resetCurrentReport()
    GlobalSymbolicEnvironment.reset()
    GlobalSymbolicStore.reset()
    PartialMatcherStore.reset()
  }

  def addBranchConstraint(constraint: BranchConstraint, thenBranchTaken: Boolean): Unit = {
    if (!doConcolic) {
      return
    }

    val optimizedConstraint = ConstraintOptimizer.optimizeConstraint(constraint)
    if (! ConstraintOptimizer.isConstraintConstant(constraint)) {
      /*
       * If the constraint is constant, don't bother adding it to the currentReport as it will always be either true or false anyway.
       * The currentPath should still be updated, because this path is compared with the path computed via static analyses,
       * which don't (or can't) check whether some condition is constant or not.
       */
      addConstraint(optimizedConstraint, thenBranchTaken)
      if (ConcolicRunTimeFlags.checkAnalysis) {
        assert(PartialMatcherStore.get.isDefined)
        val result: Boolean = testCurrentPath
        if (!result) {
          Logger.log("Execution no longer follows an errorpath, aborting this concolic run", Logger.U)
          throw AbortConcolicRunException
        }
      }
    } else {
      addUnusableConstraint(thenBranchTaken)
    }
  }

  def addUnusableConstraint(thenBranchTaken: Boolean): Unit = {
    if (doConcolic) {
      addConstraint(UnusableConstraint, thenBranchTaken)
    }
  }

  def printReports(): Unit = {
    Logger.log(s"Reporter recorded path: ${getCurrentReport.filter({
      case (_: BranchConstraint, _, _) => true
      case _ => false
    }).mkString("; ")}", Logger.U)
  }
}
