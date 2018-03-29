import backend._
import backend.expression.ConcolicInput
import backend.path_filtering.PartialRegexMatcher
import backend.solvers._
import backend.tree._

class ScalaAMConcolicSolver(val useAnalysis: Boolean) {

  var backendReporter: Reporter[_, _] = if (useAnalysis) {
    null /* TODO Refactor this for the love of god */
  } else {
    Reporter
  }

  private var latestInputs: List[(ConcolicInput, Int)] = Nil

  private def resetInputs(): Unit = {
    latestInputs = Nil
  }
  def getInputs: List[(ConcolicInput, Int)] = latestInputs

  private def convertInputs(inputs: Map[ConcolicInput, Int]) = {
    inputs.toList.sortBy(_._1.id)
  }

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

  var count = 0

  def solve[SymbolicNodeUsed : SymbolicNodeViewer](root: SymbolicNodeUsed): ConcolicSolverResult = {
    val backendSolver = new ConcolicSolver[SymbolicNodeUsed]
    backendSolver.solve(root)
  }

  def solve(pathConstraint: PathConstraintWithMatchers): Boolean = {
    count += 1
    resetInputs()
    val result: ConcolicSolverResult = if (useAnalysis) {
      val initialPartialMatcher = PartialMatcherStore.getInitial.get
      val reporter: Reporter[PMSymbolicNode, PathConstraintWithMatchers] = if (backendReporter == null) {
        new PartialMatcherReporter(initialPartialMatcher)
      } else {
        backendReporter
      }.asInstanceOf[Reporter[PMSymbolicNode, PathConstraintWithMatchers]]
      backendReporter = reporter
      reporter.writeSymbolicTree("tree.dot")
      try {
        reporter.addExploredPath(pathConstraint)
        solve(reporter.getRoot.get)
      } catch {
        case exception: java.lang.AssertionError =>
          println("CAUGHT")
          throw exception
      }
    } else {
      val castedBackendReporter = backendReporter.asInstanceOf[Reporter[SymbolicNode, PathConstraint]]
      castedBackendReporter.addExploredPath(pathConstraintWithMatchersToPathConstraint(pathConstraint))
      solve(castedBackendReporter.getRoot.get)
    }
    result match {
      case NewInput(inputs) =>
        latestInputs = convertInputs(inputs)
        true
      case SymbolicTreeFullyExplored => false
    }
  }

}