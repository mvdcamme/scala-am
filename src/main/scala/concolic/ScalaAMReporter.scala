import java.util.regex.{Matcher, Pattern}

import backend._
import backend.tree._
import backend.tree.path._

case object AbortConcolicRunException extends Exception

object ScalaAMReporter {

  private var doConcolic: Boolean = false

  private var currentPath: Path = Nil
  private var currentReport: PathConstraint = Nil

  private def testCurrentPath: Boolean = {
    val pathString = currentPath.map({
      case ElseBranchTaken => "e"
      case ThenBranchTaken => "t"
    }).mkString("")
    Logger.log(s"Current pathstring is $pathString", Logger.D)

    InitialErrorPaths.get.get.partialMatch(pathString)
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
    currentPath = Nil
    currentReport = Nil
    GlobalSymbolicEnvironment.reset()
    GlobalSymbolicStore.reset()
  }

  private case class SplitErrorPaths(thenPaths: Set[Path], elsePaths: Set[Path])

  private def splitErrorPaths(errorPaths: Set[Path]): SplitErrorPaths = {
    // If node does not follow a path along which an error is located, make the corresponding branch ineligable for testing
    val nonEmptyPaths = errorPaths.filter(_.nonEmpty)
    val startsWithThen = nonEmptyPaths.filter(_.head == backend.tree.path.ThenBranchTaken)
    val startsWithElse = nonEmptyPaths.filter(_.head == backend.tree.path.ElseBranchTaken)
    SplitErrorPaths(startsWithThen, startsWithElse)
  }

  private def addConstraint(constraint: BranchConstraint, thenBranchTaken: Boolean): Unit = {
    if (! ConstraintOptimizer.isConstraintConstant(constraint)) {
      /*
       * If the constraint is constant, don't bother adding it to the currentReport as it will always be either true or false anyway.
       * The currentPath should still be updated, because this path is compared with the path computed via static analyses,
       * which don't (or can't) check whether some condition is constant or not.
       */
      currentReport :+= (constraint, thenBranchTaken)
    }
  }

  def addBranchConstraint(constraint: BranchConstraint, thenBranchTaken: Boolean): Unit = {
    if (!doConcolic) {
      return
    }

    val optimizedConstraint = ConstraintOptimizer.optimizeConstraint(constraint)
    addConstraint(optimizedConstraint, thenBranchTaken)
    if (ConcolicRunTimeFlags.checkAnalysis) {
      assert(InitialErrorPaths.get.isDefined)
      val result: Boolean = testCurrentPath
      if (! result) {
        Logger.log("Execution no longer follows an errorpath, aborting this concolic run", Logger.U)
        throw AbortConcolicRunException
      }
    }
  }

  def getCurrentPath: Path = currentPath
  def getCurrentReport: PathConstraint = currentReport

  def addToCurrentPath(thenBranchTaken: Boolean): Unit = {
    currentPath :+= (if (thenBranchTaken) backend.tree.path.ThenBranchTaken else backend.tree.path.ElseBranchTaken)
  }

  def printReports(): Unit = {
    Logger.log(s"Reporter recorded path: ${currentReport.mkString("; ")}", Logger.U)
  }

  def doErrorPathsDiverge: Boolean = ??? // TODO
}
