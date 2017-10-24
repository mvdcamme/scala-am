import SymbolicTreeHelper.TreePath

object ConcolicSolver {

  type ErrorPath = List[SemanticsFilterAnnotation]

  private var latestInputs: Map[String, Int] = Map()

  private def resetInputs(): Unit = {
    latestInputs = Map()
  }
  def getInputs: Map[String, Int] = latestInputs

  private var initialErrorPaths: Option[List[ErrorPath]] = None
  def getInitialErrorPaths: Option[List[ErrorPath]] = initialErrorPaths

  private def doOneSolveIteration(constraints: List[ConcolicConstraint]): Boolean = {
    resetInputs()
    val solutions = Z3.solve(constraints.flatMap({
      case b: BranchConstraint =>
        List(b)
      case _ =>
        Nil
    }))
    solutions match {
      case Satisfiable(solution) =>
        latestInputs = solution.toMap[String, Int]
        println(s"latestInputs are $latestInputs")
        true
      case Unsatisfiable =>
        false
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

  private def negateAllSuccessors(node: SymbolicNode): Unit = node match {
    case b: BranchSymbolicNode =>
      b.elseBranchTaken = true
      b.thenBranchTaken = true
      if (b.elseBranch.isDefined) {
        negateAllSuccessors(b.elseBranch.get)
      }
      if (b.thenBranch.isDefined) {
        negateAllSuccessors(b.thenBranch.get)
      }
  }

  /**
    * Makes symbolic nodes which have already been discovered, though not necessarily explored yet, and along which
    * no errors were found during static anaysis uneligble for concolic testing.
    * @param node
    * @param errorPaths
    */
  private def negateNodesNotFollowingErrorPath(node: SymbolicNode, errorPaths: List[ErrorPath]): Unit = node match {
    case b: BranchSymbolicNode =>
      val nonEmptyPaths = errorPaths.filter(_.nonEmpty)
      val startsWithThen = nonEmptyPaths.filter(_.head == ThenBranchTaken)
      val startsWithElse = nonEmptyPaths.filter(_.head == ElseBranchTaken)

      if (startsWithThen.isEmpty) {
        // No errors located along the then-branch of node b
        b.thenBranchTaken = true
        if (b.thenBranch.isDefined) {
          negateAllSuccessors(b.thenBranch.get)
        }
      } else if (b.thenBranch.isDefined) {
        val tailStartsWithThen = startsWithThen.map(_.tail)
        negateNodesNotFollowingErrorPath(b.thenBranch.get, tailStartsWithThen)
      }

      if (startsWithElse.isEmpty) {
        // No errors located along the else-branch of node b
        b.elseBranchTaken = true
        if (b.elseBranch.isDefined) {
          negateAllSuccessors(b.elseBranch.get)
        }
      } else if (b.elseBranch.isDefined) {
        val tailStartsWithElse = startsWithElse.map(_.tail)
        negateNodesNotFollowingErrorPath(b.elseBranch.get, tailStartsWithElse)
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

  def negatePath(path: TreePath): TreePath = {
    val lastNode = path.last._1.asInstanceOf[BranchSymbolicNode] // last node of the path should always be a BranchSymbolicNode
    if (!lastNode.thenBranchTaken) {
      // Explore then-branch: don't have to do anything
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
    (result: StaticAnalysisResult, startNode: Option[SymbolicNode], isInitial: Boolean): List[ErrorPath] = {
    if (ConcolicRunTimeFlags.checkAnalysis) {
      result match {
        case outputGraph: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, errorPathDetector.aam.State] =>
          val errorPaths = errorPathDetector.detectErrors(outputGraph.output.graph)
          Logger.log(s"### Concolic got error paths $errorPaths", Logger.U)
          if (isInitial) initialErrorPaths = Some(errorPaths)

          startNode match {
            case Some(node) =>
              negateNodesNotFollowingErrorPath(node, errorPaths)
            case None =>
            // Do nothing
          }
          errorPaths
        case result =>
          Logger.log(s"### Concolic did not get expected graph, got $result instead", Logger.U)
          Nil
      }
    } else {
      Nil
    }
  }

}
