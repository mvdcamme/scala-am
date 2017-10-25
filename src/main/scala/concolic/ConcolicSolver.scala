import SymbolicTreeHelper.TreePath

object ConcolicSolver {

  type Path = List[SemanticsFilterAnnotation]

  private var latestInputs: Map[String, Int] = Map()

  private def resetInputs(): Unit = {
    latestInputs = Map()
  }
  def getInputs: Map[String, Int] = latestInputs

  private var initialErrorPaths: Option[List[Path]] = None
  def getInitialErrorPaths: Option[List[Path]] = initialErrorPaths

  private def doOneSolveIteration(constraints: List[BranchConstraint]): Boolean = {
    resetInputs()
    val solutions = Z3.solve(constraints)
    solutions match {
      case Satisfiable(solution) =>
        latestInputs = solution.toMap[String, Int]
        Logger.log(s"latestInputs are $latestInputs", Logger.U)
        true
      case Unsatisfiable =>
        false
      case SomeZ3Error =>
        throw new Exception(s"Concolic testing failed for some reason")
    }
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
  private def negateNodesNotFollowingErrorPath(node: BranchSymbolicNode, errorPaths: List[Path]): Unit = {
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

  @scala.annotation.tailrec
  final def solve: Boolean = {
    // optIncompletelyExploredPath refers to a path (if there is one) that ends in a BranchNode with at least one branch
    // that has not yet been explored.
    val optIncompletelyExploredPath: Option[TreePath] = Reporter.findUnexploredNode
    optIncompletelyExploredPath match {
      case Some(incompletelyExploredPath) =>
        val unexploredPath: TreePath = negatePath(incompletelyExploredPath)
        Logger.log(s"Unexplored path would be ${unexploredPath.seen}", Logger.U)
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

  def negatePath(path: TreePath): TreePath = {
    val init = path.init
    val lastNode = path.last._1
    if (! lastNode.thenBranchTaken) {
      init :+ lastNode
    } else {
      init.addNegatedNode(lastNode)
    }
  }

  private def handleAnalysisResult[Abs: IsSchemeLattice]
    (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult, startNode: Option[BranchSymbolicNode]): List[Path] = {
    if (ConcolicRunTimeFlags.checkAnalysis) {
      result match {
        case outputGraph: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, errorPathDetector.aam.State] =>
          val errorPaths = errorPathDetector.detectErrors(outputGraph.output.graph)
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

  def handleInitialAnalysisResult[Abs: IsSchemeLattice]
    (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult, startNode: Option[BranchSymbolicNode]): List[Path] = {
    val errorPaths = handleAnalysisResult[Abs](errorPathDetector)(result, startNode)
    initialErrorPaths = Some(errorPaths)
    errorPaths
  }


  def handleRunTimeAnalysisResult[Abs: IsSchemeLattice]
  (errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T])
    (result: StaticAnalysisResult, startNode: Option[BranchSymbolicNode], prefixErrorPath: Path): List[Path] = {
    val errorPaths = handleAnalysisResult[Abs](errorPathDetector)(result, startNode)
    val initialErrorPathsNotStartingWithPrefix = initialErrorPaths.get.filterNot(_.startsWith(prefixErrorPath))
    val newInitialErrorPaths = initialErrorPathsNotStartingWithPrefix ++ errorPaths.map(prefixErrorPath ++ _)
    initialErrorPaths = Some(newInitialErrorPaths)
    Reporter.setCurrentErrorPaths(errorPaths)
    errorPaths
  }

}
