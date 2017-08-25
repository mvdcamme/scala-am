import scala.collection.mutable.{ Map => MMap }

import com.microsoft.z3._

object ConcolicSolver {

  private var latestInputs: Map[String, Int] = Map()

  private def resetInputs(): Unit = {
    latestInputs = Map()
  }

  def getInputs: Map[String, Int] = latestInputs

  def solve(path: List[SymbolicNode]): Unit = {
    resetInputs()
    val constraints = path.map(_.constraint)
    val solutions = Z3.solve(constraints)
    latestInputs = solutions.get.toMap[String, Int]
  }

  def getInput(inputName: String): Option[Int] = {
    latestInputs.get(inputName)
  }

  def negatePath(path: List[SymbolicNode]): List[SymbolicNode] = {
    val lastNode = path.last.asInstanceOf[BranchSymbolicNode] // last node of the path should always be a BranchSymbolicNode
    if (!lastNode.thenBranchTaken) {
      // Explore then-branch
      val negatedBranchConstraint = lastNode.branch.negate
      val negatedBranchNode = lastNode.copy(branch = negatedBranchConstraint)
      path.init :+ negatedBranchNode
    } else {
      // Explore else-branch
      val negatedBranchConstraint = lastNode.branch.negate
      val negatedBranchNode = lastNode.copy(branch = negatedBranchConstraint)
      path.init :+ negatedBranchNode
    }
  }

}
