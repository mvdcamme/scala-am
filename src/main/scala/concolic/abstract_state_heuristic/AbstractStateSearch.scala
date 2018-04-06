import scala.collection.immutable.Queue

import backend.tree.BranchConstraint
import backend.tree.search_strategy.{BreadthFirstSearch, SearchStrategy}

import abstract_state_heuristic._

/**
  *
  * @param original The original expressions recorded by the Reporter
  * @param seen     Nodes in which the else-branch were taken are negated to represent the path actually taken during execution
  */
case class TreePath[State](original: List[AbstractStateBranchSymbolicNode[State]], seen: List[BranchConstraint]) {
  def length: Int = original.length
  def last: (AbstractStateBranchSymbolicNode[State], BranchConstraint) = {
    (original.last, seen.last)
  }

  def init: TreePath[State] = {
    TreePath(original.init, seen.init)
  }

  // Same as addStatement or addThenBranch basically
  def :+(node: AbstractStateBranchSymbolicNode[State]): TreePath[State] = {
    TreePath(original :+ node, seen :+ node.branch)
  }

  def addNegatedNode(node: AbstractStateBranchSymbolicNode[State]): TreePath[State] = {
    TreePath(original :+ node, seen :+ node.branch.negate)
  }

  def addThenBranch(node: AbstractStateBranchSymbolicNode[State]): TreePath[State] = {
    TreePath(original :+ node, seen :+ node.branch)
  }

  def addElseBranch(node: AbstractStateBranchSymbolicNode[State]): TreePath[State] = {
    TreePath(original :+ node, seen.init :+ seen.last.negate :+ node.branch)
  }

  def toBackendTreePath: backend.tree.search_strategy.TreePath = backend.tree.search_strategy.TreePath(original.map(_.toSymbolicNode), seen)
}

object TreePath {
  def init[State](b: AbstractStateBranchSymbolicNode[State]): Either[TreePath[State], TreePath[State]] = {
    if (!b.thenBranchTaken || !b.elseBranchTaken) {
      // This node has not been fully explored yet, so return this node as an unexplored path
      Left(TreePath(List(b), List(if (b.elseBranchTaken) b.branch.negate else b.branch)))
    } else if (b.thenBranchTaken) {
      Right(TreePath(List(b), List(b.branch)))
    } else {
      Right(TreePath(List(b), List(b.branch.negate)))
    }
  }
}

class AbstractStateSearch[State] extends SearchStrategy[AbstractStateSymbolicNode[State]] {

  private def countAbstractStates(symbolicNode: AbstractStateSymbolicNode[State]): Map[State, Int] = {
    def loop(currentNode: AbstractStateSymbolicNode[State], acc: Map[State, Int]): Map[State, Int] = currentNode match {
      case AbstractStateRegularLeafNode() | AbstractStateSafeNode(_) | AbstractStateUnexploredNode() | AbstractStateUnsatisfiableNode() => acc
      case AbstractStateBranchSymbolicNode(_, trueBranch, elseBranch, state) =>
        val updatedAcc = acc.updated(state, acc.getOrElse(state, 0) + 1)
        val updatedAcc2 = loop(trueBranch, updatedAcc)
        loop(elseBranch, updatedAcc2)
    }
    loop(symbolicNode, Map())
  }

  @scala.annotation.tailrec
  private def loop(queue: Queue[TreePath[State]], countedAbstractStates: Map[State, Int]): Option[TreePath[State]] = queue.headOption match {
    case None => None
    case Some(path) =>
      val tailQueue = queue.tail
      val latestNode = path.last
      latestNode._1 match {
        case b: AbstractStateBranchSymbolicNode[State] =>
          if ((!b.thenBranchTaken || !b.elseBranchTaken) && countedAbstractStates.getOrElse(b.state, 0) == 1) {
            Some(path)
          } else {
            // Both branches have already been explored, so continue looking through both branches to find an unexplored node
            // Although both branches have been explored, it could be that they don't actually have any successors, e.g., because the branch ends

            // Only add child-branches if they are a BranchSymbolicNode
            val newQueue: Queue[TreePath[State]] = (b.thenBranch, b.elseBranch) match {
              // Negate branches
              case (thenBranch: AbstractStateBranchSymbolicNode[State], elseBranch: AbstractStateBranchSymbolicNode[State]) =>
                tailQueue :+ path.addThenBranch(thenBranch) :+ path.addElseBranch(elseBranch)
              case (thenBranch: AbstractStateBranchSymbolicNode[State], _) =>
                tailQueue :+ path.addThenBranch(thenBranch)
              case (_, elseBranch: AbstractStateBranchSymbolicNode[State]) =>
                tailQueue :+ path.addElseBranch(elseBranch)
              case (_, _) =>
                tailQueue
            }
            loop(newQueue, countedAbstractStates)
          }
      }
  }

  def findFirstUnexploredNode(symbolicNode: AbstractStateSymbolicNode[State]): Option[backend.tree.search_strategy.TreePath] = {
    val countedAbstractStates = countAbstractStates(symbolicNode)
    if (countedAbstractStates.exists(_._2 == 1)) {

      symbolicNode match {
        case b: AbstractStateBranchSymbolicNode[State] =>
          val initialTreePath = TreePath.init[State](b)
          initialTreePath match {
            case Left(left) =>
              // Very first node in the tree is unexplored, so just return that one
              Some(left.toBackendTreePath)
            case Right(right) =>
              val queue: Queue[TreePath[State]] = Queue(right)
              val result = loop(queue, countedAbstractStates)
              result.map(_.toBackendTreePath)
          }
        case AbstractStateRegularLeafNode() | AbstractStateSafeNode(_) | AbstractStateUnexploredNode() | AbstractStateUnsatisfiableNode() => None
      }

    } else {
      Logger.log("No unique abstract state reachable, so switching to BFS", Logger.E)
      new BreadthFirstSearch[AbstractStateSymbolicNode[State]].findFirstUnexploredNode(symbolicNode)
    }
  }

}
