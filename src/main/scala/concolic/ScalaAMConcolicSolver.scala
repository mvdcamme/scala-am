import backend._
import backend.expression.ConcolicInput
import backend.path_filtering.PartialRegexMatcher
import backend.solvers._
import backend.tree._
import backend.tree.search_strategy.{BreadthFirstSearch, SearchStrategy, TreePath}
import dk.brics.automaton.State
import backend._
import backend.path_filtering.PartialRegexMatcher
import backend.tree.BranchSymbolicNode

class MostErrorsReachableSearch extends SearchStrategy[PMSymbolicNode] {

  import scala.language.implicitConversions
  implicit private def pmNodeToNode(pmNode: PMBranchSymbolicNode): BranchSymbolicNode = pmNode.toSymbolicNode

  def countReachableErrors(partialRegexMatcher: PartialRegexMatcher): Int = {
    @scala.annotation.tailrec
    def loop(todo: Set[State], visited: Set[State], currentCount: Int): Int = todo.headOption match {
      case None => currentCount
      case Some(head) if visited.contains(head) => loop(todo.tail, visited, currentCount)
      case Some(head) =>
        val destStates = scala.collection.JavaConverters.asScalaSet(head.getTransitions).map(_.getDest)
        val updatedCount = if (head.isAccept) currentCount + 1 else currentCount
        loop(todo.tail ++ destStates, visited + head, updatedCount)
    }
    loop(Set(partialRegexMatcher.lastState), Set(), 0)
  }

  def findAllUnexploredNodes(b: PMBranchSymbolicNode, treePath: TreePath): Set[(TreePath, Int)] = {
    if (! (b.thenBranchTaken && b.elseBranchTaken)) {
      Set((treePath, countReachableErrors(b.pm)))
    } else {
      (b.thenBranch, b.elseBranch) match {
        case (thenBranch: PMBranchSymbolicNode, elseBranch: PMBranchSymbolicNode) =>
          findAllUnexploredNodes(thenBranch, treePath.addThenBranch(thenBranch)) ++ findAllUnexploredNodes(elseBranch, treePath.addElseBranch(elseBranch))
        case (thenBranch: PMBranchSymbolicNode, _) => findAllUnexploredNodes(thenBranch, treePath.addThenBranch(thenBranch))
        case (_, elseBranch: PMBranchSymbolicNode) => findAllUnexploredNodes(elseBranch, treePath.addThenBranch(elseBranch))
        case (_, _) => Set()
      }
    }
  }

  def findFirstUnexploredNode(symbolicNode: PMSymbolicNode): Option[TreePath] = symbolicNode match {
    case PMRegularLeafNode | PMSafeNode(_) | PMUnexploredNode | PMUnsatisfiableNode => None
    case b: PMBranchSymbolicNode =>
      val allResults = findAllUnexploredNodes(b, TreePath(Nil, Nil))
      Logger.log(s"All results are $allResults", Logger.E)
      allResults.filter(_._1.length > 0).toList.sortWith((t1, t2) => t1._2 > t2._2).headOption.map(_._1)
  }

}

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

  def solve[SymbolicNodeUsed : SymbolicNodeViewer](root: SymbolicNodeUsed, searchStrategy: SearchStrategy[SymbolicNodeUsed]): ConcolicSolverResult = {
    val backendSolver = new ConcolicSolver[SymbolicNodeUsed]
    backendSolver.solve(root, searchStrategy)
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
        solve(reporter.getRoot.get, new MostErrorsReachableSearch)
      } catch {
        case exception: java.lang.AssertionError =>
          println("CAUGHT")
          throw exception
      }
    } else {
      val castedBackendReporter = backendReporter.asInstanceOf[Reporter[SymbolicNode, PathConstraint]]
      castedBackendReporter.addExploredPath(pathConstraintWithMatchersToPathConstraint(pathConstraint))
      solve(castedBackendReporter.getRoot.get, new BreadthFirstSearch[SymbolicNode])
    }
    result match {
      case NewInput(inputs) =>
        latestInputs = convertInputs(inputs)
        true
      case SymbolicTreeFullyExplored => false
    }
  }

}