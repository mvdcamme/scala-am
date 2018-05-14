import backend._
import backend.solvers._
import backend.tree.SymbolicNode
import backend.tree.search_strategy.BreadthFirstSearch

import abstract_state_heuristic._

class AbstractStateSolver[State] extends ScalaAMSolver[AbstractStatePCElement[State], State] {

  val reporter: Reporter[State, PathConstraintWithState[State]] = new AbstractStateReporter[State]

  def getRoot: Option[SymbolicNode[State]] = reporter.getRoot
  def deleteSymbolicTree(): Unit = reporter.deleteSymbolicTree()
  def clearBackendReporter(): Unit = reporter.clear()

  def solve(pathConstraint: PathConstraintWithState[State], shouldMerge: Boolean): Boolean = {
    resetInputs()
    if (shouldMerge) {
      reporter.mergePath(pathConstraint)
    } else {
      reporter.addExploredPath(pathConstraint)
    }
    reporter.writeSymbolicTree("tree.dot")
    val result = solveViaBackend(reporter.getRoot.get, new AbstractStateSearch[State])
    result match {
      case NewInput(inputs) =>
        latestInputs = convertInputs(inputs)
        true
      case SymbolicTreeFullyExplored => false
    }
  }

}
