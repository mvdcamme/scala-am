import backend._
import backend.expression.ConcolicInput
import backend.path_filtering.PartialRegexMatcher
import backend.solvers._

class ScalaAMConcolicSolver {

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

  private sealed trait Solver {
    def solve: ConcolicSolverResult
  }
  private class ActualSolver extends Solver {
    def solve: ConcolicSolverResult = ConcolicSolver.solve
  }
  private class MockupSolver extends Solver {
    private var mockupResults: List[NewInput] = List(
      NewInput(Map(ConcolicInput(0) -> 1)),
      NewInput(Map(ConcolicInput(0) -> 0, ConcolicInput(1) -> 1)),
      NewInput(Map(ConcolicInput(0) -> 2)),
      NewInput(Map(ConcolicInput(0) -> 0, ConcolicInput(1) -> 0, ConcolicInput(2) -> 1)))
    def solve: ConcolicSolverResult = {
      ConcolicSolver.solve
      mockupResults.headOption match {
        case Some(result) =>
          mockupResults = mockupResults.tail
          result
        case None => SymbolicTreeFullyExplored
      }
    }
  }

  private val solver: Solver = new MockupSolver

  def solve(reporter: ScalaAMReporter): Boolean = {
    count += 1
    resetInputs()
    val report = reporter.pathStorage.getCurrentReport
    PartialMatcherStore.getInitial match {
      case Some(initialPartialMatcher) =>
        Reporter.getRoot
        Reporter.writeSymbolicTree("tree.dot")
        try {
          Reporter.addExploredPathWithPartialMatcher(report, initialPartialMatcher)
        } catch {
          case exception: java.lang.AssertionError =>
            println("CAUGHT")
            throw exception
        }

      case None => Reporter.addExploredPath(report)
    }
//    val result = ConcolicSolver.solve
    val result = solver.solve
    result match {
      case NewInput(inputs) =>
        latestInputs = convertInputs(inputs)
        true
      case SymbolicTreeFullyExplored => false
    }
  }

}