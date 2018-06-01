import backend._
import backend.path_filtering.PartialRegexMatcher
import backend.solvers._
import backend.tree._
import backend.tree.search_strategy.BreadthFirstSearch
import backend.expression.ConcolicInput

class PartialMatcherSolver extends ScalaAMSolver[PartialMatcherPCElement, PartialRegexMatcher] {

  val backendReporter: Reporter[PartialRegexMatcher, PathConstraintWithMatchers] = new PartialMatcherReporter

  def getRoot: Option[SymbolicNode[PartialRegexMatcher]] = backendReporter.getRoot
  def deleteSymbolicTree(): Unit = backendReporter.deleteSymbolicTree()
  def clearBackendReporter(): Unit = backendReporter.clear()

  /**
    * If no [[PartialRegexMatcher]] was defined for the first constraint, adds the given [[PartialRegexMatcher]]
    * to this constraint.
    * @param report
    * @param toPatchWith
    * @return A copy of the given report, where the [[PartialRegexMatcher]] of the first constraint was replaced by
    *         the given [[PartialRegexMatcher]], if the first constraint didn't have a [[PartialRegexMatcher]] yet.
    */
  private def patchInitialPartialMatcher(report: PathConstraintWithMatchers, toPatchWith: PartialRegexMatcher): PathConstraintWithMatchers = report match {
    case Nil => Nil
    case (constraint, constraintTrue, None) :: rest =>
      /* If no PartialMatcher was included when adding this constraint, add it now. */
      (constraint, constraintTrue, Some(toPatchWith)) :: rest
    case list => list
  }

  def solve(pathConstraint: PathConstraintWithMatchers, shouldMerge: Boolean): Boolean = {
    resetInputs()
    val initialPartialMatcher = PartialMatcherStore.getInitial.get
    val patchedPathConstraint = patchInitialPartialMatcher(pathConstraint, initialPartialMatcher)
    if (shouldMerge) {
      backendReporter.mergePath(patchedPathConstraint)
    } else {
      backendReporter.addExploredPath(patchedPathConstraint)
    }
    backendReporter.writeSymbolicTree("tree.dot")
    val result = solveViaBackend(backendReporter.getRoot.get, new BreadthFirstSearch[PartialRegexMatcher])
    result match {
      case NewInput(inputs) =>
        latestInputs = convertInputs(inputs)
        true
      case SymbolicTreeFullyExplored => false
    }
  }

}

class MockPartialMatcherSolver extends PartialMatcherSolver {

  private var mockInputs: List[List[(ConcolicInput, Int)]] = List(List((ConcolicInput(0), 2), (ConcolicInput(1), 1), (ConcolicInput(2), 0)), List((ConcolicInput(0), 0), (ConcolicInput(1), 1), (ConcolicInput(2), 2)))

  override def solve(pathConstraint: PathConstraintWithMatchers, shouldMerge: Boolean): Boolean = mockInputs match {
    case head :: rest =>
      latestInputs = head
      mockInputs = rest
      true
    case Nil => false
  }
}