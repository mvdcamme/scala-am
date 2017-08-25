import scala.collection.mutable.{Map => MMap}

object ConcolicSolver {

  private var latestInputs: Map[String, Int] = Map()

  private def resetInputs(): Unit = {
    latestInputs = Map()
  }

  def getInputs: Map[String, Int] = latestInputs

  private def doOneSolveIteration(path: List[SymbolicNode]): Boolean = {
    resetInputs()
    val constraints = path.map(_.constraint)
    val solutions = Z3.solve(constraints)
    solutions match {
      case Satisfiable(solution) =>
        latestInputs = solution.toMap[String, Int]
        true
      case Unsatisfiable =>
        false
    }
  }

  def solve: Boolean = {
    // optIncompletelyExploredPath refers to a path (if there is one) that ends in a BranchNode with at least one branch
    // that has not yet been explored.
    val optIncompletelyExploredPath = Reporter.findUnexploredNode
    optIncompletelyExploredPath match {
      case Some(incompletelyExploredPath) =>
        Reporter.printTree()
        println(s"Reporter recorded path ${Reporter.getReport}")
        val unexploredPath = ConcolicSolver.negatePath(incompletelyExploredPath)
        println(s"Unexplored path would be ${unexploredPath.map(_.constraint)}")
        val wasSuccessful = doOneSolveIteration(unexploredPath)
        if (wasSuccessful) {
          true
        } else {
          solve
        }
      case None =>
        false
    }
  }

  def getInput(inputName: String): Option[Int] = {
    latestInputs.get(inputName)
  }

  def negatePath(path: List[SymbolicNode]): List[SymbolicNode] = {
    val lastNode = path.last.asInstanceOf[BranchSymbolicNode] // last node of the path should always be a BranchSymbolicNode
    if (!lastNode.thenBranchTaken) {
      // Explore then-branch: don't have to do anything:
      path
    } else {
      val lastNode = path.last.asInstanceOf[BranchSymbolicNode] // last node of the path should always be a BranchSymbolicNode
      // Explore else-branch
      val negatedBranchConstraint = lastNode.branch.negate
      val negatedBranchNode = lastNode.copy(branch = negatedBranchConstraint)
      path.init :+ negatedBranchNode
    }
  }

}
