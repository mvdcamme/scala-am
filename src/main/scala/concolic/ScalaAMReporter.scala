import backend._
import backend.tree._
import backend.tree.path._

case object AbortConcolicRunException extends Exception

object ScalaAMReporter {

  private var doConcolic: Boolean = false

  private var currentPath: Path = Nil
  private var currentReport: PathConstraint = Nil


  import scala.language.implicitConversions
  implicit def pathToString(path: Path): String = {
    path.map({
      case ElseBranchTaken => "e"
      case ThenBranchTaken => "t"
    }).mkString("")
  }

  def getCurrentPath: Path = currentPath
  private def resetCurrentPath(): Unit = {
    currentPath = Nil
  }

  def getCurrentReport: PathConstraint = currentReport
  private def resetCurrentReport(): Unit = {
    currentReport = Nil
  }
  def addToCurrentPath(thenBranchTaken: Boolean): Unit = {
    currentPath :+= (if (thenBranchTaken) backend.tree.path.ThenBranchTaken else backend.tree.path.ElseBranchTaken)
  }

  private def testCurrentPath: Boolean = {
    Logger.log(s"Current pathstring is ${pathToString(currentPath)}", Logger.V)
    val (matched, newPartialMatcher) = PartialMatcherStore.get.get.incrementalMatch(currentPath)
    PartialMatcherStore.setCurrentMatcher(newPartialMatcher)
    if (matched) {
      /* We're using the incremental match for performance reasons, so the string should be reset if there is a match. */
      resetCurrentPath()
    }
    matched
  }

  def doErrorPathsDiverge: Boolean = {
    val currentPathFollowingElse = currentPath :+ ElseBranchTaken
    val currentPathFollowingThen = currentPath :+ ThenBranchTaken
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

  private def addConstraint(constraint: Constraint, thenBranchTaken: Boolean): Unit = {
    currentReport :+= (constraint, thenBranchTaken)
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
    Logger.log(s"Reporter recorded path: ${currentReport.filter({
      case (_: BranchConstraint, _) => true
      case _ => false
    }).mkString("; ")}", Logger.U)
  }
}
