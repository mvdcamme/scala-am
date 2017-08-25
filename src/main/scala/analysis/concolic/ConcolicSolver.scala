import scala.collection.mutable.{ Map => MMap }

import com.microsoft.z3._

object ConcolicSolver {

  private var latestInputs: Map[String, Int] = Map()

  private def resetInputs(): Unit = {
    latestInputs = Map()
  }

  def getInputs: Map[String, Int] = latestInputs

  private def doOneSolveIteration(path: List[SymbolicNode]): Unit = {
    resetInputs()
    val constraints = path.map(_.constraint)
    val solutions = Z3.solve(constraints)
    solutions match {
      case Satisfiable(solution) =>
        latestInputs = solution.toMap[String, Int]
    }
  }
  
  def solve(path: List[SymbolicNode]): Unit = {
    doOneSolveIteration(path)
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
