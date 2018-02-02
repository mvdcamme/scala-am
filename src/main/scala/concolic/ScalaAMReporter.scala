import backend._
import backend.tree._
import backend.tree.path._

case object AbortConcolicRunException extends Exception

object ScalaAMReporter {

  private var doConcolic: Boolean = false

  val pathStorage = new PathStorage

  import scala.language.implicitConversions
  implicit private def pathToString(path: Path): String = {
    path.map({
      case ElseBranchTaken => "e"
      case ThenBranchTaken => "t"
    }).mkString("")
  }

  private def testCurrentPath: Boolean = {
    Logger.log(s"Current pathstring is ${pathToString(pathStorage.getCurrentPath)}", Logger.V)
    val (matched, newPartialMatcher) = PartialMatcherStore.getCurrent.get.incrementalMatch(pathStorage.getCurrentPath)
    PartialMatcherStore.setCurrentMatcher(newPartialMatcher)
    if (matched) {
      /* We're using the incremental match for performance reasons, so the string should be reset if there is a match. */
      pathStorage.resetCurrentPath()
    }
    matched
  }

  def doErrorPathsDiverge: Boolean = {
    val currentPathFollowingElse = pathStorage.getCurrentPath :+ ElseBranchTaken
    val currentPathFollowingThen = pathStorage.getCurrentPath :+ ThenBranchTaken
    val isErrorViaElse = PartialMatcherStore.getCurrent.get.tentativeIncrementalMatch(currentPathFollowingElse)
    val isErrorViaThen = PartialMatcherStore.getCurrent.get.tentativeIncrementalMatch(currentPathFollowingThen)
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
    pathStorage.resetCurrentPath()
    pathStorage.resetCurrentReport()
    GlobalSymbolicStore.reset()
    PartialMatcherStore.reset()
  }

  def addBranchConstraint(constraint: BranchConstraint, thenBranchTaken: Boolean, rTAnalysisStarter: RTAnalysisStarter): Unit = {

    def checkWithPartialMatcher(constraint: BranchConstraint): Unit = {
      assert(PartialMatcherStore.getCurrent.isDefined)
      val currentMatcher = PartialMatcherStore.getCurrent.get // TODO Debugging
      val result: Boolean = testCurrentPath
      if (!result) {
        Logger.log("Execution no longer follows an errorpath, aborting this concolic run", Logger.U)
        throw AbortConcolicRunException
      }
    }

    if (!doConcolic) {
      return
    }

    val optimizedConstraint = ConstraintOptimizer.optimizeConstraint(constraint)
    if (ConstraintOptimizer.isConstraintConstant(optimizedConstraint)) {
      /*
       * If the constraint is constant, don't bother adding it to the currentReport as it will always be either true or false anyway.
       * The currentPath should still be updated, because this path is compared with the path computed via static analyses,
       * which don't (or can't) check whether some condition is constant or not.
       */
      addUnusableConstraint(thenBranchTaken, rTAnalysisStarter)
    } else if (ConcolicRunTimeFlags.useRunTimeAnalyses) {

      pathStorage.updateReport(constraint, thenBranchTaken)
      rTAnalysisStarter.startAnalysisFromCurrentState(thenBranchTaken, pathStorage.getCurrentReport)
      checkWithPartialMatcher(optimizedConstraint)
    } else if (ConcolicRunTimeFlags.checkAnalysis) {
      checkWithPartialMatcher(optimizedConstraint)
    } else {
      /* Constraint is not constant and checkAnalysis is false */
      pathStorage.updateReport(optimizedConstraint, thenBranchTaken)
    }
  }

  def addUnusableConstraint(thenBranchTaken: Boolean, rTAnalysisStarter: RTAnalysisStarter): Unit = {
    if (doConcolic) {
      pathStorage.updateReport(UnusableConstraint, thenBranchTaken)
    }
  }

  def printReports(): Unit = {
    Logger.log(s"Reporter recorded path: ${pathStorage.getCurrentReport.filter({
      case (_: BranchConstraint, _, _) => true
      case _ => false
    }).map({
      case (constraint, constraintTrue, _) => (constraint, constraintTrue)
    }).mkString("; ")}", Logger.U)
  }
}
