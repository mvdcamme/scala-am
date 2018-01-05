import backend._
import backend.expression.ConcolicInput
import backend.solvers._
import dk.brics.automaton.{ Automaton, RunAutomaton }

object InitialErrorPaths {
  private var automaton: Option[RunAutomaton] = None
  def get: Option[RunAutomaton] = automaton
  def set(newAutomaton: RunAutomaton): Unit = {
    automaton = Some(newAutomaton)
//    automaton = Some(paths)
//    Reporter.replaceWhitelistedPaths(paths) TODO Trying to use automaton-approach now...
  }

  def testString(s: String): Boolean = {
    automaton.get.run(s)
  }
}

object ScalaAMConcolicSolver {

  private var latestInputs: List[(ConcolicInput, Int)] = Nil

  private def resetInputs(): Unit = {
    latestInputs = Nil
  }
  def getInputs: List[(ConcolicInput, Int)] = latestInputs

  private def handleAnalysisResult[Abs: IsSchemeLattice]
  (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult): Option[RunAutomaton] = {
    if (ConcolicRunTimeFlags.checkAnalysis) {
      result match {
        case outputGraph: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, errorPathDetector.aam.State] =>
          val automaton = errorPathDetector.detectErrors(outputGraph.hasGraph.graph)
//          Logger.log(s"### Concolic got error paths $automaton", Logger.U)
          automaton
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
    Reporter.addExploredPath(report)
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
    (result: StaticAnalysisResult): Option[RunAutomaton] = {
    val automaton = handleAnalysisResult[Abs](errorPathDetector)(result)
    InitialErrorPaths.set(automaton.get)
    automaton
  }


  def handleRunTimeAnalysisResult[Abs: IsSchemeLattice]
  (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult, prefixErrorPath: Path): Option[RunAutomaton] = {
    val automaton = handleAnalysisResult[Abs](errorPathDetector)(result)
    // TODO MV Using automaton approach now...
//    val initialErrorPathsNotStartingWithPrefix = InitialErrorPaths.get.get.filterNot(_.startsWith(prefixErrorPath))
//    val newInitialErrorPaths = initialErrorPathsNotStartingWithPrefix ++ automaton.map(prefixErrorPath ++ _)
    InitialErrorPaths.set(automaton.get)
//    ScalaAMReporter.setCurrentErrorPaths(automaton)
    automaton
  }

}