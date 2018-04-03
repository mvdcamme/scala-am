import backend.path_filtering.PartialRegexMatcher
import backend.tree._
import backend.tree.path._
import backend._

case object AbortConcolicIterationException extends Exception

class ScalaAMReporter(val concolicFlags: ConcolicRunTimeFlags) {


  private var doConcolic: Boolean = false

  val solver: ScalaAMConcolicSolver = if (concolicFlags.checkAnalysis) new RegularConcolicSolver else new PartialMatcherConcolicSolver
  val inputVariableStore = new InputVariableStore(solver)
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
  def clear(): Unit = {
    Reporter.clear()
    inputVariableStore.reset()
    pathStorage.resetCurrentPath()
    pathStorage.resetCurrentReport()
    GlobalSymbolicStore.reset()
    PartialMatcherStore.reset()
  }

  private def abortConcolicIteration(): Unit = {
    Logger.log("Execution no longer follows an errorpath, aborting this concolic run", Logger.E)
    throw AbortConcolicIterationException
  }

  /**
    * Updates [[PartialMatcherStore]]'s current partial matcher and resets the current path of the pathStorage.
    * @param updatedMatcher
    */
  private def updatePartialMatcher(updatedMatcher: PartialRegexMatcher): Unit = {
    PartialMatcherStore.setCurrentMatcher(updatedMatcher)
    /* We're using the incremental match for performance reasons, so the string should be reset if there is a match. */
    pathStorage.resetCurrentPath()
  }

  private def checkWithPartialMatcher: PartialRegexMatcher = {
    assert(PartialMatcherStore.getCurrent.isDefined)
    val currentMatcher = PartialMatcherStore.getCurrent.get // TODO Debugging
    val (result, partialMatcher) = testCurrentPath
    if (!result) {
      abortConcolicIteration()
    }
    updatePartialMatcher(partialMatcher)
    partialMatcher
  }

  def addBranchConstraint(constraint: BranchConstraint, thenBranchTaken: Boolean, rTAnalysisStarter: RTAnalysisStarter): Unit = {

    if (!doConcolic) {
      return
    }

    val optimizedConstraint = ConstraintOptimizer.optimizeConstraint(constraint)
    lazy val constraintAddedPC = pathStorage.addToReport(optimizedConstraint, thenBranchTaken, None)
    val inputVariableFinder = new InputVariableFinder
    val (exactInputVars, inexactInputVars) = inputVariableFinder.findInputVariablesDefinitions(constraintAddedPC)
    if (ConstraintOptimizer.isConstraintConstant(optimizedConstraint)) {
      /*
       * If the constraint is constant, don't bother adding it to the currentReport as it will always be either true or
       * false anyway. The currentPath should still be updated, because this path is compared with the path computed
       * via static analyses, which don't (or can't) check whether some condition is constant or not.
       */
      addUnusableConstraint(thenBranchTaken, rTAnalysisStarter)
    } else if (concolicFlags.useRunTimeAnalyses && inexactInputVars.isEmpty) {
      Logger.log(s"EXACT INPUT VARIABLES IN PATH ARE $exactInputVars", Logger.E)
      val analysisResult = rTAnalysisStarter.startAnalysisFromCurrentState(thenBranchTaken, constraintAddedPC)
      /*
       * PartialMatcherStore's current matcher now refers to a matcher starting from *after* this constraint.
       * The current path is empty.
       */
      pathStorage.updateReport(optimizedConstraint, thenBranchTaken, Some(analysisResult.partialMatcher))
      checkWithPartialMatcher
      /*
       * If the run-time analyis (which was started after having already evaluated the condition of the constraint)
       * cannot find any error states, no errors are reachable from this point in the execution tree, so we may
       * abort this concolic run.
       */
      if (! analysisResult.shouldContinueTesting) {
        abortConcolicIteration()
      }
      Logger.log("Continuing Testing", Logger.E)
    } else if (concolicFlags.checkAnalysis) {
      if (concolicFlags.useRunTimeAnalyses && inexactInputVars.nonEmpty) {
        Logger.log("SKIPPING RT ANALYSIS BECAUSE OF AN INEXACT INPUT VARIABLE", Logger.U)
      }
      pathStorage.updateReport(optimizedConstraint, thenBranchTaken, None)
      checkWithPartialMatcher
    } else {
      /* Constraint is not constant and checkAnalysis is false */
      pathStorage.updateReport(optimizedConstraint, thenBranchTaken, None)
    }
  }

  def addUnusableConstraint(thenBranchTaken: Boolean, rTAnalysisStarter: RTAnalysisStarter): Unit = {
    if (doConcolic) {
      pathStorage.updateReport(UnusableConstraint, thenBranchTaken, None)
      if (concolicFlags.checkAnalysis) {
        checkWithPartialMatcher
      }
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

  def writeSymbolicTree(path: String): Unit = {
    Reporter.writeSymbolicTree(path)
  }
}
