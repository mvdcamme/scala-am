import scala.collection.mutable.{ Map => MMap }

import com.microsoft.z3._

object ConcolicSolver {

  private var latestInputs: Map[String, Int] = Map()

  def solve(path: List[SymbolicNode]): Unit = {
    val constraints = path.map(_.constraint)
    val solutions = Z3.solve(constraints)
    latestInputs = solutions.get.toMap[String, Int]
  }

  def getInput(inputName: String): Option[Int] = {
    latestInputs.get(inputName)
  }

}
