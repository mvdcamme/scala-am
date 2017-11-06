import backend._
import backend.solvers._

object ScalaAMConcolicSolver {

  private var latestInputs: Map[String, Int] = Map()

  private def resetInputs(): Unit = {
    latestInputs = Map()
  }
  def getInputs: Map[String, Int] = latestInputs

  private var initialErrorPaths: Option[List[ThenElsePath]] = None
  def getInitialErrorPaths: Option[List[ThenElsePath]] = initialErrorPaths

  /**
    * To be called when someone negated the given node to create a new, unexplored path, but the resulting path
    * was considered unsatisfiable.
    * @param node
    */
  private def nodeWasTried(node: BranchSymbolicNode): Unit = {
    assert(node.thenBranchTaken != node.elseBranchTaken, "Should not happen: one of both branches should be True")
    if (node.thenBranchTaken) {
      assert(node.elseBranch.isEmpty)
      node.elseBranchTaken = true
    } else {
      assert(node.thenBranch.isEmpty)
      node.thenBranchTaken = true
    }
  }

  private def negateAllSuccessors(node: BranchSymbolicNode): Unit = {
    node.elseBranchTaken = true
    node.thenBranchTaken = true
    if (node.elseBranch.isDefined) {
      negateAllSuccessors(node.elseBranch.get)
    }
    if (node.thenBranch.isDefined) {
      negateAllSuccessors(node.thenBranch.get)
    }
  }

  /**
    * Makes symbolic nodes which have already been discovered, though not necessarily explored yet, and along which
    * no errors were found during static anaysis uneligble for concolic testing.
    * @param node
    * @param errorPaths
    */
  private def negateNodesNotFollowingErrorPath(node: BranchSymbolicNode, errorPaths: List[ThenElsePath]): Unit = {
    val nonEmptyPaths = errorPaths.filter(_.nonEmpty)
    val startsWithThen = nonEmptyPaths.filter(_.head == ThenBranchTaken)
    val startsWithElse = nonEmptyPaths.filter(_.head == ElseBranchTaken)

    if (startsWithThen.isEmpty) {
      // No errors located along the then-branch of node b
      node.thenBranchTaken = true
      if (node.thenBranch.isDefined) {
        negateAllSuccessors(node.thenBranch.get)
      }
    } else if (node.thenBranch.isDefined) {
      val tailStartsWithThen = startsWithThen.map(_.tail)
      negateNodesNotFollowingErrorPath(node.thenBranch.get, tailStartsWithThen)
    }

    if (startsWithElse.isEmpty) {
      // No errors located along the else-branch of node b
      node.elseBranchTaken = true
      if (node.elseBranch.isDefined) {
        negateAllSuccessors(node.elseBranch.get)
      }
    } else if (node.elseBranch.isDefined) {
      val tailStartsWithElse = startsWithElse.map(_.tail)
      negateNodesNotFollowingErrorPath(node.elseBranch.get, tailStartsWithElse)
    }
  }

  def getInput(inputName: String): Option[Int] = {
    latestInputs.get(inputName)
  }

  private def handleAnalysisResult[Abs: IsSchemeLattice]
  (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult, startNode: Option[BranchSymbolicNode]): List[ThenElsePath] = {
    if (ConcolicRunTimeFlags.checkAnalysis) {
      result match {
        case outputGraph: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, errorPathDetector.aam.State] =>
          val errorPaths = errorPathDetector.detectErrors(outputGraph.hasGraph.graph)
          Logger.log(s"### Concolic got error paths $errorPaths", Logger.U)
          startNode match {
            case Some(node) =>
              negateNodesNotFollowingErrorPath(node, errorPaths)
            case None =>
            // Do nothing
          }
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
    (result: StaticAnalysisResult, startNode: Option[BranchSymbolicNode]): List[ThenElsePath] = {
    val errorPaths = handleAnalysisResult[Abs](errorPathDetector)(result, startNode)
    initialErrorPaths = Some(errorPaths)
    errorPaths
  }


  def handleRunTimeAnalysisResult[Abs: IsSchemeLattice]
  (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult, startNode: Option[BranchSymbolicNode], prefixErrorPath: ThenElsePath): List[ThenElsePath] = {
    val errorPaths = handleAnalysisResult[Abs](errorPathDetector)(result, startNode)
    val initialErrorPathsNotStartingWithPrefix = initialErrorPaths.get.filterNot(_.startsWith(prefixErrorPath))
    val newInitialErrorPaths = initialErrorPathsNotStartingWithPrefix ++ errorPaths.map(prefixErrorPath ++ _)
    initialErrorPaths = Some(newInitialErrorPaths)
    ScalaAMReporter.setCurrentErrorPaths(errorPaths)
    errorPaths
  }

}