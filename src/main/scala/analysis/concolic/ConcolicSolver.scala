import SymbolicTreeHelper.TreePath

trait WrappedSymbolicNode
case class WrappedBranchNode(node: BranchSymbolicNode)
case class WrappedStatementNode(node: StatementSymbolicNode, var thenPossible: Boolean, var elsePossible: Boolean)

object ConcolicSolver {

  type ErrorPath = List[SemanticsFilterAnnotation]

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
    case s: StatementSymbolicNode => s.followUp match {
      case Some(followUp) =>
        negateAllSuccessors(followUp)
      case None =>
    }
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

//  private def wrapTree(node: SymbolicNode): WrappedSymbolicNode = node match {
//    case s: StatementSymbolicNode =>
//
//  }

  /**
    * Makes symbolic nodes which have already been discovered, though not necessarily explored yet, and along which
    * no errors were found during static anaysis uneligble for concolic testing.
    * @param node
    * @param errorPaths
    */
  private def negateNodesNotFollowingErrorPath(node: SymbolicNode, errorPaths: List[ErrorPath]): Unit = node match {
    case s: StatementSymbolicNode => s.followUp match {
      case Some(followUp) =>
        // Continue with follow-up node
        negateNodesNotFollowingErrorPath(followUp, errorPaths)
      case None =>
        // Do nothing
    }
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
        negateNodesNotFollowingErrorPath(b.thenBranch.get, startsWithThen)
      }

      if (startsWithElse.isEmpty) {
        // No errors located along the else-branch of node b
        b.elseBranchTaken = true
        if (b.elseBranch.isDefined) {
          negateAllSuccessors(b.elseBranch.get)
        }
      } else if (b.elseBranch.isDefined) {
        negateNodesNotFollowingErrorPath(b.elseBranch.get, startsWithElse)
      }
  }


//    errorPath.headOption match {
//    case Some(head) => path match {
//      case s: StatementSymbolicNode => s.followUp match {
//        // Don't look at the error path, just continue with the follow-up of s, if there is one.
//        case Some(followUp) =>
//          negateNodesNotFollowingErrorPath(followUp, errorPath)
//        case None =>
//          // Do nothing
//      }
//      case b: BranchSymbolicNode => head match {
//        case ThenBranchTaken =>
//          // Error is located along then-branch, so do not consider the else-branch.
//          b.elseBranchTaken = true
//          if (b.elseBranch.isDefined) {
//            negateAllSuccessors(b.elseBranch.get)
//          }
//        case ElseBranchTaken =>
//
//      }
//    }
//    case None =>
//      // Do nothing
//  }
//
//  private def followsErrorPath(path: TreePath, errorPaths: List[ErrorPath]): Boolean = {
//    def loopPath(path: List[SymbolicNode], errorPath: List[ErrorPath]): Boolean = path.headOption match {
//      case Some(head) => head match {
//        case b: BranchSymbolicNode =>
//
//        case _: StatementSymbolicNode =>
//          loopPath(path.tail, errorPath)
//      }
//    }
//    loopPath(path.original, errorPaths)
//  }

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
    (result: StaticAnalysisResult): List[ErrorPath] = result match {
    case outputGraph: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, errorPathDetector.aam.State] =>
      val errorPaths = errorPathDetector.detectErrors(outputGraph.output.graph)
      negateNodesNotFollowingErrorPath(Reporter.getRoot.get, errorPaths)
      Logger.log(s"### Concolic got error paths $errorPaths", Logger.U)
      errorPaths
    case result =>
      Logger.log(s"### Concolic did not get expected graph, got $result instead", Logger.U)
      Nil
  }

}
