import backend._
import backend.expression.ConcolicInput
import backend.solvers.{ConcolicSolver, ConcolicSolverResult}
import backend.tree.SymbolicNodeViewer
import backend.tree.search_strategy._

trait HasInputs {
  def getInputs: List[(ConcolicInput, Int)]
}

abstract class ScalaAMSolver extends HasInputs {

  protected var latestInputs: List[(ConcolicInput, Int)] = Nil

  protected def resetInputs(): Unit = {
    latestInputs = Nil
  }
  def getInputs: List[(ConcolicInput, Int)] = latestInputs

  protected def convertInputs(inputs: Map[ConcolicInput, Int]): List[(ConcolicInput, Int)] = {
    inputs.toList.sortBy(_._1.id)
  }

  protected def solveViaBackend[SymbolicNodeUsed : SymbolicNodeViewer](root: SymbolicNodeUsed, searchStrategy: SearchStrategy[SymbolicNodeUsed]): ConcolicSolverResult = {
    val backendSolver = new ConcolicSolver[SymbolicNodeUsed]
    backendSolver.solve(root, searchStrategy)
  }

  def solve(pathConstraint: PathConstraintWithMatchers): Boolean
}