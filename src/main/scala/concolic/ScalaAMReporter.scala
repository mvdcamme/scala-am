import backend._
import backend.expression._
import backend.tree._

case object AbortConcolicRunException extends Exception

object ScalaAMReporter {

  private var doConcolic: Boolean = false

  private var currentPath: Path = Nil
  private var currentReport: PathConstraint = Nil

  private var optCurrentErrorPaths: Option[List[Path]] = None

  def setCurrentErrorPaths(newCurrentErrorPaths: List[Path]): Unit = {
    optCurrentErrorPaths = Some(newCurrentErrorPaths)
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
    optCurrentErrorPaths = InitialErrorPaths.get
    GlobalSymbolicEnvironment.reset()
    GlobalSymbolicStore.reset()
  }

  private case class SplitErrorPaths(thenPaths: List[Path], elsePaths: List[Path])

  private def splitErrorPaths(errorPaths: List[Path]): SplitErrorPaths = {
    // If node does not follow a path along which an error is located, make the corresponding branch ineligable for testing
    val nonEmptyPaths = errorPaths.filter(_.nonEmpty)
    val startsWithThen = nonEmptyPaths.filter(_.head == backend.tree.path.ThenBranchTaken)
    val startsWithElse = nonEmptyPaths.filter(_.head == backend.tree.path.ElseBranchTaken)
    SplitErrorPaths(startsWithThen, startsWithElse)
  }

  private def addConstraint(constraint: BranchConstraint, thenBranchTaken: Boolean): Unit = {
    currentPath :+= (if (thenBranchTaken) backend.tree.path.ThenBranchTaken else backend.tree.path.ElseBranchTaken)
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
      optCurrentErrorPaths match {
        case Some(currentErrorPaths) =>
          println(s"In ScalaAMReporter, currentErrorPaths are ${currentErrorPaths}")
          val SplitErrorPaths(startsWithThen, startsWithElse) = splitErrorPaths(currentErrorPaths)
          if (thenBranchTaken) {
            if (startsWithThen.isEmpty) {
              Logger.log("Execution no longer follows an errorpath, aborting this concolic run", Logger.U)
              // The current path does not follow an existing errorpath, so abort this run
              throw AbortConcolicRunException
            }
            // Continue with paths that follow the then-branch.
            val tailStartsWithThen = startsWithThen.map(_.tail)
            optCurrentErrorPaths = Some(tailStartsWithThen)
          } else {
            if (startsWithElse.isEmpty) {
              Logger.log("Execution no longer follows an errorpath, aborting this concolic run", Logger.U)
              // The current path does not follow an existing errorpath, so abort this run
              throw AbortConcolicRunException
            }
            // Continue with paths that follow the else-branch.
            val tailStartsWithElse = startsWithElse.map(_.tail)
            optCurrentErrorPaths = Some(tailStartsWithElse)
          }
        case None =>
          Logger.log("Reporter not doing anything with error paths", Logger.U)
          // Assuming concolic testing is only really started if at least one errorpath is defined,
          // and assuming we check for every branch that was encountered whether execution still
          // follows _some_ errorpath (as is done in this function), this should never happen.
          throw AbortConcolicRunException
      }
    }
  }

  def getCurrentPath: Path = currentPath
  def getCurrentReport: PathConstraint = currentReport

  def printReports(): Unit = {
    Logger.log(s"Reporter recorded path: ${currentReport.mkString("; ")}", Logger.U)
  }

  def doErrorPathsDiverge: Boolean = optCurrentErrorPaths match {
    case Some(errorPaths) =>
      val SplitErrorPaths(startsWithThen, startsWithElse) = splitErrorPaths(errorPaths)
      startsWithThen.nonEmpty && startsWithElse.nonEmpty
    case None =>
      assert(false, "Should not happen: some errorpaths should be defined")
      false
  }
}
