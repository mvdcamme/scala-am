import backend._
import backend.tree._
import backend.solvers._

object InitialErrorPaths {
  private var initialErrorPaths: Option[List[Path]] = None
  def get: Option[List[Path]] = initialErrorPaths
  def set(paths: List[Path]): Unit = {
    initialErrorPaths = Some(paths)
    Reporter.replaceWhitelistedPaths(paths)
  }
}

object ScalaAMConcolicSolver {

  private var latestInputs: Map[String, Int] = Map()

  private def resetInputs(): Unit = {
    latestInputs = Map()
  }
  def getInputs: Map[String, Int] = latestInputs

  def getInput(inputName: String): Option[Int] = {
    latestInputs.get(inputName)
  }

  private def handleAnalysisResult[Abs: IsSchemeLattice]
  (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult): List[Path] = {
    if (ConcolicRunTimeFlags.checkAnalysis) {
      result match {
        case outputGraph: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, errorPathDetector.aam.State] =>
          val errorPaths = errorPathDetector.detectErrors(outputGraph.hasGraph.graph)
          Logger.log(s"### Concolic got error paths $errorPaths", Logger.U)
          errorPaths
        case _ =>
          Logger.log(s"### Concolic did not get expected graph, got $result instead", Logger.U)
          Nil
      }
    } else {
      Nil
    }
  }

  def solve(): Boolean = {
    resetInputs()
    Reporter.addExploredPath(ScalaAMReporter.getCurrentReport)
    val result = ConcolicSolver.solve
    result match {
      case NewInput(input) =>
        latestInputs = input
        true
      case SymbolicTreeFullyExplored => false
    }
  }

  def handleInitialAnalysisResult[Abs: IsSchemeLattice]
  (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult): List[Path] = {
    val errorPaths = handleAnalysisResult[Abs](errorPathDetector)(result)
    InitialErrorPaths.set(errorPaths)
    errorPaths
  }


  def handleRunTimeAnalysisResult[Abs: IsSchemeLattice]
  (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult, prefixErrorPath: Path): List[Path] = {
    val errorPaths = handleAnalysisResult[Abs](errorPathDetector)(result)
    val initialErrorPathsNotStartingWithPrefix = InitialErrorPaths.get.get.filterNot(_.startsWith(prefixErrorPath))
    val newInitialErrorPaths = initialErrorPathsNotStartingWithPrefix ++ errorPaths.map(prefixErrorPath ++ _)
    InitialErrorPaths.set(newInitialErrorPaths)
    ScalaAMReporter.setCurrentErrorPaths(errorPaths)
    errorPaths
  }

}