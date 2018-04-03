import backend.expression.ConcolicInput
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
      val initTree: TreePath = TreePath.init(b) match {
        case Left(left) => left
        case Right(right) => right
      }
      val allResults = findAllUnexploredNodes(b, initTree)
      Logger.log(s"All results are $allResults", Logger.E)
      allResults.filter(_._1.length > 0).toList.sortWith((t1, t2) => t1._2 > t2._2).headOption.map(_._1)
  }
}

trait HasInputs {
  def getInputs: List[(ConcolicInput, Int)]
}

abstract class ScalaAMConcolicSolver extends HasInputs {

  protected var latestInputs: List[(ConcolicInput, Int)] = Nil

  protected def resetInputs(): Unit = {
    latestInputs = Nil
  }
  def getInputs: List[(ConcolicInput, Int)] = latestInputs

  protected def convertInputs(inputs: Map[ConcolicInput, Int]): List[(ConcolicInput, Int)] = {
    inputs.toList.sortBy(_._1.id)
  }

  def solve(pathConstraint: PathConstraintWithMatchers): Boolean
}

class RegularConcolicSolver extends ScalaAMConcolicSolver {

  val backendReporter: Reporter[SymbolicNode, PathConstraint] = Reporter

  def solve(pathConstraint: PathConstraintWithMatchers): Boolean = {
    backendReporter.writeSymbolicTree("tree.dot")
    resetInputs()
    val castedBackendReporter = backendReporter.asInstanceOf[Reporter[SymbolicNode, PathConstraint]]
    castedBackendReporter.addExploredPath(pathConstraint)
    val result = new ConcolicSolver[SymbolicNode].solve(castedBackendReporter.getRoot.get, new BreadthFirstSearch[SymbolicNode])
    result match {
      case NewInput(inputs) =>
        latestInputs = convertInputs(inputs)
        true
      case SymbolicTreeFullyExplored => false
    }
  }

}

class PartialMatcherConcolicSolver extends ScalaAMConcolicSolver {

  val backendReporter: Reporter[PMSymbolicNode, PathConstraintWithMatchers] = new PartialMatcherReporter

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
    val result = new ConcolicSolver[PMSymbolicNode].solve(backendReporter.getRoot.get, new MostErrorsReachableSearch)
    result match {
      case NewInput(inputs) =>
        latestInputs = convertInputs(inputs)
        true
      case SymbolicTreeFullyExplored => false
    }
  }

}