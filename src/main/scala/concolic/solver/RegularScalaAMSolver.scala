import backend._
import backend.expression.ConcolicInput
import backend.solvers._
import backend.tree.SymbolicNode
import backend.tree.search_strategy.BreadthFirstSearch

class RegularScalaAMSolver extends ScalaAMSolver[RegularPCElement, Unit] {

  val backendReporter: Reporter[Unit, PathConstraint] = Reporter

  def getRoot: Option[SymbolicNode[Unit]] = backendReporter.getRoot
  def deleteSymbolicTree(): Unit = backendReporter.deleteSymbolicTree()
  def clearBackendReporter(): Unit = backendReporter.clear()

  def solve(pathConstraint: PathConstraint, shouldMerge: Boolean): Boolean = {
    resetInputs()
    if (shouldMerge) {
      backendReporter.mergePath(pathConstraint)
    } else {
      backendReporter.addExploredPath(pathConstraint)
    }
    backendReporter.writeSymbolicTree("tree.dot")
    val result = solveViaBackend(backendReporter.getRoot.get, new BreadthFirstSearch[Unit])
    result match {
      case NewInput(inputs) =>
        latestInputs = convertInputs(inputs)
        true
      case SymbolicTreeFullyExplored => false
    }
  }
}

class MockRegularScalaAMSolver extends RegularScalaAMSolver {

  private val common: List[(ConcolicInput, Int)] = List((ConcolicInput(0), 0), (ConcolicInput(1), 0), (ConcolicInput(2), 0))
  private var mockInputs: List[List[(ConcolicInput, Int)]] = List(common ++ List((ConcolicInput(3), 1), (ConcolicInput(4), 2)),
                                                                  common ++ List((ConcolicInput(3), 2), (ConcolicInput(4), 1)),
    List((ConcolicInput(0), 1)))

  override def solve(pathConstraint: PathConstraint, shouldMerge: Boolean): Boolean = mockInputs match {
    case head :: rest =>
      latestInputs = head
      mockInputs = rest
      true
    case Nil => false
  }
}