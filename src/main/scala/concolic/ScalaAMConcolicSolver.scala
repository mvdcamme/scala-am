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

  def solve(): Boolean = {
    resetInputs()
    val report = ScalaAMReporter.getCurrentReport
    PartialMatcherStore.get match {
      case Some(partialMatcher) => Reporter.addExploredPathWithPartialMatcher(report, partialMatcher)
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
    PartialMatcherStore.setRunTime(maybePartialMatcher.get)
//    ScalaAMReporter.setCurrentErrorPaths(automaton)
  }

}