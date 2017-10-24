import scala.collection.immutable.Queue

object SymbolicTreeHelper {

  /**
    *
    * @param original The original expressions recorded by the Reporter
    * @param seen     Nodes in which the else-branch were taken are negated to represent the path actually taken during execution
    */
  case class TreePath(original: List[BranchSymbolicNode], seen: List[BranchConstraint]) {
    def length: Int = original.length

    def last: (BranchSymbolicNode, BranchConstraint) = {
      (original.last, seen.last)
    }

    def init: TreePath = {
      TreePath(original.init, seen.init)
    }

    // Same as addStatement or addThenBranch basically
    def :+(node: BranchSymbolicNode): TreePath = {
      TreePath(original :+ node, seen :+ node.branch)
    }

    def addNegatedNode(node: BranchSymbolicNode): TreePath = {
      TreePath(original :+ node, seen :+ node.branch.negate)
    }

    def addThenBranch(node: BranchSymbolicNode): TreePath = {
      TreePath(original :+ node, seen :+ node.branch)
    }

    def addElseBranch(node: BranchSymbolicNode): TreePath = {
      TreePath(original :+ node, seen.init :+ seen.last.negate :+ node.branch)
    }
  }

  object TreePath {
    def init(b: BranchSymbolicNode): Either[TreePath, TreePath] = {
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

  def findFirstUnexploredNode(symbolicNode: BranchSymbolicNode): Option[TreePath] = {
    def loop(queue: Queue[TreePath]): Option[TreePath] = {
      queue.headOption match {
        case Some(path) =>
          val tailQueue = queue.tail
          val latestNode = path.last
          latestNode._1 match {
            case b: BranchSymbolicNode =>
              if (!b.thenBranchTaken || !b.elseBranchTaken) {
                assert(b.thenBranchTaken != b.elseBranchTaken, "Should not happen: one of both branches should be True")
                Some(path)
              } else {
                // Both branches have already been explored, so continue looking through both branches to find an unexplored node
                // Although both branches have been explored, it could be that they don't actually have any successors, e.g., because the branch ends
                val newQueue: Queue[TreePath] = (b.thenBranch, b.elseBranch) match {
                  // Negate branches
                  case (Some(thenBranch), Some(elseBranch)) =>
                    tailQueue :+ path.addThenBranch(thenBranch) :+ path.addElseBranch(elseBranch)
                  case (Some(thenBranch), None) =>
                    tailQueue :+ path.addThenBranch(thenBranch)
                  case (None, Some(elseBranch)) =>
                    tailQueue :+ path.addElseBranch(elseBranch)
                  case (None, None) =>
                    tailQueue
                }
                loop(newQueue)
              }
          }
        case None =>
          // Queue is empty: no unexplored nodes found
          None
      }
    }

    val initialTreePath = TreePath.init(symbolicNode)
    initialTreePath match {
      case Left(left) =>
        // Very first node in the tree is unexplored, so just return that one
        Some(left)
      case Right(right) =>
        val queue: Queue[TreePath] = Queue(initialTreePath.right.get)
        loop(queue)
    }
  }

}
