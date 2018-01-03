import backend._
import backend.expression.ConcolicInput
import backend.solvers._

object InitialErrorPaths {
  private var initialErrorPaths: Option[Set[Path]] = None
  def get: Option[Set[Path]] = initialErrorPaths
  def set(paths: Set[Path]): Unit = {
    initialErrorPaths = Some(paths)
    Reporter.replaceWhitelistedPaths(paths)
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
    (result: StaticAnalysisResult): Set[Path] = {
    if (ConcolicRunTimeFlags.checkAnalysis) {
      result match {
        case outputGraph: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, errorPathDetector.aam.State] =>
          val errorPaths = errorPathDetector.detectErrors(outputGraph.hasGraph.graph)
          Logger.log(s"### Concolic got error paths $errorPaths", Logger.U)
          errorPaths
        case _ =>
          Logger.log(s"### Concolic did not get expected graph, got $result instead", Logger.U)
          Set()
      }
    } else {
      Set()
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
    (result: StaticAnalysisResult): Set[Path] = {
    val errorPaths = handleAnalysisResult[Abs](errorPathDetector)(result)
    InitialErrorPaths.set(errorPaths)
    errorPaths
  }


  def handleRunTimeAnalysisResult[Abs: IsSchemeLattice]
  (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult, prefixErrorPath: Path): Set[Path] = {
    val errorPaths = handleAnalysisResult[Abs](errorPathDetector)(result)
    val initialErrorPathsNotStartingWithPrefix = InitialErrorPaths.get.get.filterNot(_.startsWith(prefixErrorPath))
    val newInitialErrorPaths = initialErrorPathsNotStartingWithPrefix ++ errorPaths.map(prefixErrorPath ++ _)
    InitialErrorPaths.set(newInitialErrorPaths)
    ScalaAMReporter.setCurrentErrorPaths(errorPaths)
    errorPaths
  }

}