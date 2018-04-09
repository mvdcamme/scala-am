import backend._
import backend.solvers._
import backend.tree.SymbolicNode
import backend.tree.search_strategy.BreadthFirstSearch

import abstract_state_heuristic._

class AbstractStateSolver[State] extends ScalaAMSolver[AbstractStatePCElement[State], State] {

  val reporter: Reporter[State, PathConstraintWithState[State]] = new AbstractStateReporter[State]

  def getRoot: SymbolicNode[State] = reporter.getRoot.get
  def deleteSymbolicTree(): Unit = reporter.deleteSymbolicTree()
  def clearBackendReporter(): Unit = reporter.clear()

  def solve(pathConstraint: PathConstraintWithState[State]): Boolean = {
    reporter.writeSymbolicTree("tree.dot")
    resetInputs()
    reporter.addExploredPath(pathConstraint)
    val result = solveViaBackend(reporter.getRoot.get, new AbstractStateSearch[State])
    result match {
      case NewInput(inputs) =>
        latestInputs = convertInputs(inputs)
        true
      case SymbolicTreeFullyExplored => false
    }
  }

}
