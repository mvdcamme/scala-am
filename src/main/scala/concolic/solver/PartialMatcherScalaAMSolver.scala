import backend._
import backend.path_filtering.PartialRegexMatcher
import backend.solvers._
import backend.tree._
import backend.tree.search_strategy.BreadthFirstSearch

class PartialMatcherSolver extends ScalaAMSolver {

  val backendReporter: Reporter[PMSymbolicNode, PathConstraintWithMatchers] = new PartialMatcherReporter

  def getRoot: SymbolicNode = backendReporter.symbolicNodeViewer.asSymbolicNode(backendReporter.getRoot.get)
  def deleteSymbolicTree(): Unit = backendReporter.deleteSymbolicTree()

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

  def solve(pathConstraint: PathConstraintWithMatchers): Boolean = {
    backendReporter.writeSymbolicTree("tree.dot")
    resetInputs()
    val initialPartialMatcher = PartialMatcherStore.getInitial.get
    val patchedPathConstraint = patchInitialPartialMatcher(pathConstraint, initialPartialMatcher)
    backendReporter.addExploredPath(patchedPathConstraint)
    val result = solveViaBackend(backendReporter.getRoot.get, new MostErrorsReachableSearch)
    result match {
      case NewInput(inputs) =>
        latestInputs = convertInputs(inputs)
        true
      case SymbolicTreeFullyExplored => false
    }
  }

}