import scala.collection.immutable.Queue

import backend.tree._
import backend.tree.search_strategy.{BreadthFirstSearch, SearchStrategy}

import abstract_state_heuristic._

/**
  *
  * @param original The original expressions recorded by the Reporter
  * @param seen     Nodes in which the else-branch were taken are negated to represent the path actually taken during execution
  */
case class TreePath[State](original: List[BranchSymbolicNode[State]], seen: List[BranchConstraint]) {
  def length: Int = original.length
  def last: (BranchSymbolicNode[State], BranchConstraint) = {
    (original.last, seen.last)
  }

  def init: TreePath[State] = {
    TreePath(original.init, seen.init)
  }

  // Same as addStatement or addThenBranch basically
  def :+(node: BranchSymbolicNode[State]): TreePath[State] = {
    TreePath(original :+ node, seen :+ node.branch)
  }

  def addNegatedNode(node: BranchSymbolicNode[State]): TreePath[State] = {
    TreePath(original :+ node, seen :+ node.branch.negate)
  }

  def addThenBranch(node: BranchSymbolicNode[State]): TreePath[State] = {
    TreePath(original :+ node, seen :+ node.branch)
  }

  def addElseBranch(node: BranchSymbolicNode[State]): TreePath[State] = {
    TreePath(original :+ node, seen.init :+ seen.last.negate :+ node.branch)
  }

  def toBackendTreePath: backend.tree.search_strategy.TreePath[State] = backend.tree.search_strategy.TreePath(original, seen)
}

object TreePath {
  def init[State](b: BranchSymbolicNode[State]): Either[TreePath[State], TreePath[State]] = {
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

class AbstractStateSearch[State] extends SearchStrategy[State] {

  private def countAbstractStates(symbolicNode: SymbolicNode[State]): Map[State, Int] = {
    def loop(currentNode: SymbolicNode[State], acc: Map[State, Int]): Map[State, Int] = currentNode match {
      case RegularLeafNode() | SafeNode(_) | UnexploredNode() | UnsatisfiableNode() => acc
      case BranchSymbolicNode(_, trueBranch, elseBranch, state) =>
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
        case b: BranchSymbolicNode[State] =>
          if ((!b.thenBranchTaken || !b.elseBranchTaken) && countedAbstractStates.getOrElse(b.extraInfo, 0) == 1) {
            Some(path)
          } else {
            // Both branches have already been explored, so continue looking through both branches to find an unexplored node
            // Although both branches have been explored, it could be that they don't actually have any successors, e.g., because the branch ends

            // Only add child-branches if they are a BranchSymbolicNode
            val newQueue: Queue[TreePath[State]] = (b.thenBranch, b.elseBranch) match {
              // Negate branches
              case (thenBranch: BranchSymbolicNode[State], elseBranch: BranchSymbolicNode[State]) =>
                tailQueue :+ path.addThenBranch(thenBranch) :+ path.addElseBranch(elseBranch)
              case (thenBranch: BranchSymbolicNode[State], _) =>
                tailQueue :+ path.addThenBranch(thenBranch)
              case (_, elseBranch: BranchSymbolicNode[State]) =>
                tailQueue :+ path.addElseBranch(elseBranch)
              case (_, _) =>
                tailQueue
            }
            loop(newQueue, countedAbstractStates)
          }
      }
  }

  def findFirstUnexploredNode(symbolicNode: SymbolicNode[State]): Option[backend.tree.search_strategy.TreePath[State]] = {
    val countedAbstractStates = countAbstractStates(symbolicNode)
    val nrOfUniqueAbstractStates = countedAbstractStates.count(_._2 == 1)
    Logger.E(s"Tree has $nrOfUniqueAbstractStates unique abstract states")
    if (nrOfUniqueAbstractStates >= 1) {

      symbolicNode match {
        case b: BranchSymbolicNode[State] =>
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
        case RegularLeafNode() | SafeNode(_) | UnexploredNode() | UnsatisfiableNode() => None
      }

    } else {
      Logger.E("No unique abstract state reachable, so switching to BFS")
      new BreadthFirstSearch[State].findFirstUnexploredNode(symbolicNode)
    }
  }

}
