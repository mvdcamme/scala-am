import backend._
import backend.expression.ConcolicInput
import backend.path_filtering.PartialRegexMatcher
import backend.solvers._

object ScalaAMConcolicSolver {

  private var latestInputs: List[(ConcolicInput, Int)] = Nil

  private def resetInputs(): Unit = {
    latestInputs = Nil
  }
  def getInputs: List[(ConcolicInput, Int)] = latestInputs

  private def handleAnalysisResult[Abs: IsSchemeLattice]
  (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult): Option[PartialRegexMatcher] = {
    if (ConcolicRunTimeFlags.checkAnalysis) {
      result match {
        case outputGraph: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, errorPathDetector.aam.State] =>
          // TODO can't just pass the current partial matcher, because that one starts from some cached final state instead of the initial state
          // TODO and the ENTIRE path is passed to the matcher (so including the part it has actually already matched).
          // TODO Just passing the initial matcher would mean the current matcher wouldn't be used at all though.
          // TODO Solution: when an AbortConcolicExecution-error is thrown, pass the path that was already created
          // TODO to the backend and ask to invalidate that one?
          val maybePartialMatcher = errorPathDetector.detectErrors(outputGraph.hasGraph.graph)
//          Logger.log(s"### Concolic got error paths $automaton", Logger.U)
          maybePartialMatcher
        case _ =>
          Logger.log(s"### Concolic did not get expected graph, got $result instead", Logger.U)
          None
      }
    } else {
      None
    }
  }

  private def convertInputs(inputs: Map[ConcolicInput, Int]) = {
    inputs.toList.sortBy(_._1.id)
  }

  /**
    * If no [[PartialRegexMatcher]] was defined for the first constraint, adds the given [[PartialRegexMatcher]]
    * to this constraint.
    * @param report
    * @param toPatchWith
    * @return A copy of the given report, where the [[PartialRegexMatcher]] of the first constraint was replaced by
    *         the given [[PartialRegexMatcher]], if the first constraint didn't have a [[PartialRegexMatcher]] yet.
    */
  private def patchInitialPartialMatcher(report: PathConstraint, toPatchWith: PartialRegexMatcher): PathConstraint = report match {
    case Nil => Nil
    case (constraint, constraintTrue, None) :: rest =>
      /* If no PartialMatcher was included when adding this constraint, add it now. */
      (constraint, constraintTrue, Some(toPatchWith)) :: rest
    case list => list
  }

  def solve(): Boolean = {
    resetInputs()
    val report = ScalaAMReporter.getCurrentReport
    PartialMatcherStore.getInitial match {
      case Some(initialPartialMatcher) =>
        val patchedReport = patchInitialPartialMatcher(report, initialPartialMatcher)
        Reporter.addExploredPathWithPartialMatcher(patchedReport)
      case None => Reporter.addExploredPath(report)
    }
    val result = ConcolicSolver.solve
    result match {
      case NewInput(inputs) =>
        latestInputs = convertInputs(inputs)
        true
      case SymbolicTreeFullyExplored => false
    }
  }

  def handleInitialAnalysisResult[Abs: IsSchemeLattice]
  (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult): Unit = {
    val maybePartialMatcher = handleAnalysisResult[Abs](errorPathDetector)(result)
    PartialMatcherStore.setInitial(maybePartialMatcher.get)
  }


  def handleRunTimeAnalysisResult[Abs: IsSchemeLattice]
  (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T], result: StaticAnalysisResult): Unit = {
    val maybePartialMatcher = handleAnalysisResult[Abs](errorPathDetector)(result)
//    val initialErrorPathsNotStartingWithPrefix = InitialErrorPaths.get.get.filterNot(_.startsWith(prefixErrorPath))
//    val newInitialErrorPaths = initialErrorPathsNotStartingWithPrefix ++ automaton.map(prefixErrorPath ++ _)
    PartialMatcherStore.setCurrentMatcher(maybePartialMatcher.get)
//    ScalaAMReporter.setCurrentErrorPaths(automaton)
  }

}