import backend._
import backend.expression.ConcolicInput
import backend.solvers._

object InitialErrorPaths {
  private var regexes: Option[Set[Regex]] = None
  def get: Option[Set[Regex]] = regexes
  def set(newRegexes: Set[Regex]): Unit = {
    regexes = Some(newRegexes)
//    regexes = Some(paths)
//    Reporter.replaceWhitelistedPaths(paths) TODO Trying to use regexes-approach now...
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
    (result: StaticAnalysisResult): Option[Set[Regex]] = {
    if (ConcolicRunTimeFlags.checkAnalysis) {
      result match {
        case outputGraph: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, errorPathDetector.aam.State] =>
          val optRegexes = errorPathDetector.detectErrors(outputGraph.hasGraph.graph)
//          Logger.log(s"### Concolic got error paths $automaton", Logger.U)
          optRegexes
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
    (result: StaticAnalysisResult): Option[Set[Regex]] = {
    val regexes = handleAnalysisResult[Abs](errorPathDetector)(result)
    InitialErrorPaths.set(regexes.get)
    regexes
  }


  def handleRunTimeAnalysisResult[Abs: IsSchemeLattice]
  (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult, prefixErrorPath: Path): Option[Set[Regex]] = {
    val regexes = handleAnalysisResult[Abs](errorPathDetector)(result)
    // TODO MV Using automaton approach now...
//    val initialErrorPathsNotStartingWithPrefix = InitialErrorPaths.get.get.filterNot(_.startsWith(prefixErrorPath))
//    val newInitialErrorPaths = initialErrorPathsNotStartingWithPrefix ++ automaton.map(prefixErrorPath ++ _)
    InitialErrorPaths.set(regexes.get)
//    ScalaAMReporter.setCurrentErrorPaths(automaton)
    regexes
  }

}