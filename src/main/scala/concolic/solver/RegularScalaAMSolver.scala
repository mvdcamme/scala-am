import backend._
import backend.solvers._
import backend.tree.SymbolicNode
import backend.tree.search_strategy.BreadthFirstSearch

class RegularScalaAMSolver extends ScalaAMSolver[RegularPCElement, Unit] {

  val backendReporter: Reporter[Unit, PathConstraint] = Reporter

  def getRoot: SymbolicNode[Unit] = backendReporter.getRoot.get
  def deleteSymbolicTree(): Unit = backendReporter.deleteSymbolicTree()
  def clearBackendReporter(): Unit = backendReporter.clear()

  def solve(pathConstraint: PathConstraint): Boolean = {
    backendReporter.writeSymbolicTree("tree.dot")
    resetInputs()
    backendReporter.addExploredPath(pathConstraint)
    val result = solveViaBackend(backendReporter.getRoot.get, new BreadthFirstSearch[Unit])
    result match {
      case NewInput(inputs) =>
        latestInputs = convertInputs(inputs)
        true
      case SymbolicTreeFullyExplored => false
    }
  }
}