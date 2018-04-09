import backend.PathConstraintWith
import backend.expression.ConcolicInput
import backend.solvers._
import backend.tree._
import backend.tree.search_strategy._

trait HasInputs {
  def getInputs: List[(ConcolicInput, Int)]
}

abstract class ScalaAMSolver[PCElement, NodeExtraInfo] extends HasInputs {

  protected var latestInputs: List[(ConcolicInput, Int)] = Nil

  protected def resetInputs(): Unit = {
    latestInputs = Nil
  }
  def getInputs: List[(ConcolicInput, Int)] = latestInputs
  def getRoot: SymbolicNode[NodeExtraInfo]
  def deleteSymbolicTree(): Unit
  def clearBackendReporter(): Unit

  protected def convertInputs(inputs: Map[ConcolicInput, Int]): List[(ConcolicInput, Int)] = {
    inputs.toList.sortBy(_._1.id)
  }

  protected def solveViaBackend(root: SymbolicNode[NodeExtraInfo], searchStrategy: SearchStrategy[NodeExtraInfo]): ConcolicSolverResult = {
    val backendSolver = new ConcolicSolver[NodeExtraInfo]
    backendSolver.solve(root, searchStrategy)
  }

  def solve(pathConstraint: PathConstraintWith[PCElement]): Boolean
}