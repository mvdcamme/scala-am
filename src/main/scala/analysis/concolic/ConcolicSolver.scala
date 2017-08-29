import SymbolicTreeHelper.TreePath

object ConcolicSolver {

  private var latestInputs: Map[String, Int] = Map()

  private def resetInputs(): Unit = {
    latestInputs = Map()
  }

  def getInputs: Map[String, Int] = latestInputs

  private def doOneSolveIteration(constraints: List[ConcolicConstraint]): Boolean = {
    resetInputs()
    val solutions = Z3.solve(constraints)
    solutions match {
      case Satisfiable(solution) =>
        latestInputs = solution.toMap[String, Int]
        true
      case Unsatisfiable =>
        false
    }
  }

  @scala.annotation.tailrec
  final def solve: Boolean = {
    // optIncompletelyExploredPath refers to a path (if there is one) that ends in a BranchNode with at least one branch
    // that has not yet been explored.
    val optIncompletelyExploredPath: Option[TreePath] = Reporter.findUnexploredNode
    optIncompletelyExploredPath match {
      case Some(incompletelyExploredPath) =>
        val unexploredPath: TreePath = negatePath(incompletelyExploredPath)
        println(s"Unexplored path would be ${unexploredPath.seen}")
        val wasSuccessful = doOneSolveIteration(unexploredPath.seen)
        if (wasSuccessful) {
          true
        } else {
          nodeWasTried(incompletelyExploredPath.last._1.asInstanceOf[BranchSymbolicNode])
          solve
        }
      case None =>
        false
    }
  }

  def getInput(inputName: String): Option[Int] = {
    latestInputs.get(inputName)
  }

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

  def negatePath(path: TreePath): TreePath = {
    val lastNode = path.last._1.asInstanceOf[BranchSymbolicNode] // last node of the path should always be a BranchSymbolicNode
    if (!lastNode.thenBranchTaken) {
      // Explore then-branch: don't have to do anything:
      path
    } else {
      // Explore else-branch
      val negatedBranchConstraint = lastNode.branch.negate
      val negatedBranchNode = lastNode.copy(branch = negatedBranchConstraint)
      path.init :+ negatedBranchNode
    }
  }

  def handleAnalysisResult[Abs: IsSchemeLattice]
    (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult): Unit = result match {
    case outputGraph: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, errorPathDetector.aam.State] =>
      val errorPaths = errorPathDetector.detectErrors(outputGraph.output.graph)
      Logger.log(s"### Concolic got error paths $errorPaths", Logger.U)
    case result =>
      Logger.log(s"### Concolic did not get expected graph, got $result instead", Logger.U)
  }

}
