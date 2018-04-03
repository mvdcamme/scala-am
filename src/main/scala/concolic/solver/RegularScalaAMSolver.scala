import backend._
import backend.solvers._
import backend.tree.SymbolicNode
import backend.tree.search_strategy.BreadthFirstSearch

class RegularScalaAMSolver extends ScalaAMSolver {

  val backendReporter: Reporter[SymbolicNode, PathConstraint] = Reporter

  def solve(pathConstraint: PathConstraintWithMatchers): Boolean = {
    backendReporter.writeSymbolicTree("tree.dot")
    resetInputs()
    val castedBackendReporter = backendReporter.asInstanceOf[Reporter[SymbolicNode, PathConstraint]]
    castedBackendReporter.addExploredPath(pathConstraint)
    val result = solveViaBackend(castedBackendReporter.getRoot.get, new BreadthFirstSearch[SymbolicNode])
    result match {
      case NewInput(inputs) =>
        latestInputs = convertInputs(inputs)
        true
      case SymbolicTreeFullyExplored => false
    }
  }
}