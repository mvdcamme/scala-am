import backend._
import backend.path_filtering.PartialRegexMatcher
import backend.tree._
import backend.tree.path._

case object AbortConcolicIterationException extends Exception

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

  private def testCurrentPath: (Boolean, PartialRegexMatcher) = {
    Logger.log(s"Current pathstring is ${pathToString(pathStorage.getCurrentPath)}", Logger.V)
    val (matched, newPartialMatcher) = PartialMatcherStore.getCurrent.get.incrementalMatch(pathStorage.getCurrentPath)
    (matched, newPartialMatcher)
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

    def abortConcolicIteration(): Unit = {
      Logger.log("Execution no longer follows an errorpath, aborting this concolic run", Logger.U)
      throw AbortConcolicIterationException
    }

    /**
      * Updates [[PartialMatcherStore]]'s current partial matcher and resets the current path of the pathStorage.
      * @param updatedMatcher
      */
    def updatePartialMatcher(updatedMatcher: PartialRegexMatcher): Unit = {
      PartialMatcherStore.setCurrentMatcher(updatedMatcher)
      /* We're using the incremental match for performance reasons, so the string should be reset if there is a match. */
      pathStorage.resetCurrentPath()
    }

    def checkWithPartialMatcher: PartialRegexMatcher = {
      assert(PartialMatcherStore.getCurrent.isDefined)
      val currentMatcher = PartialMatcherStore.getCurrent.get // TODO Debugging
      val (result, partialMatcher) = testCurrentPath
      if (!result) {
        abortConcolicIteration()
      }
      partialMatcher
    }

    if (!doConcolic) {
      return
    }

    val optimizedConstraint = ConstraintOptimizer.optimizeConstraint(constraint)
    if (ConstraintOptimizer.isConstraintConstant(optimizedConstraint)) {
      /*
       * If the constraint is constant, don't bother adding it to the currentReport as it will always be either true or
       * false anyway. The currentPath should still be updated, because this path is compared with the path computed
       * via static analyses, which don't (or can't) check whether some condition is constant or not.
       */
      addUnusableConstraint(thenBranchTaken, rTAnalysisStarter)
    } else if (ConcolicRunTimeFlags.useRunTimeAnalyses) { // Final non-constant BranchConstraint is evaluated at stepCount 565560; 1st iteration takes 571048 concrete steps
      val constraintAddedPC = pathStorage.addToReport(optimizedConstraint, thenBranchTaken, None).map(triple => (triple._1, triple._2))
      val analysisResult = rTAnalysisStarter.startAnalysisFromCurrentState(thenBranchTaken, constraintAddedPC)
      /*
       * PartialMatcherStore's current matcher now refers to a matcher starting from *after* this constraint.
       * The current path is empty.
       */
      pathStorage.updateReport(optimizedConstraint, thenBranchTaken, Some(analysisResult.partialMatcher))
      /*
       * If the run-time analyis (which was started after having already evaluated the condition of the constraint)
       * cannot find any error states, no errors are reachable from this point in the execution tree, so we may
       * abort this concolic run.
       */
      if (! analysisResult.containsErrorStates) {
        abortConcolicIteration()
      }
    } else if (ConcolicRunTimeFlags.checkAnalysis) {
      pathStorage.updateReport(optimizedConstraint, thenBranchTaken, None)
      val updatedMatcher = checkWithPartialMatcher
      updatePartialMatcher(updatedMatcher)
    } else {
      /* Constraint is not constant and checkAnalysis is false */
      pathStorage.updateReport(optimizedConstraint, thenBranchTaken, None)
    }
  }

  def addUnusableConstraint(thenBranchTaken: Boolean, rTAnalysisStarter: RTAnalysisStarter): Unit = {
    if (doConcolic) {
      pathStorage.updateReport(UnusableConstraint, thenBranchTaken, None)
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
