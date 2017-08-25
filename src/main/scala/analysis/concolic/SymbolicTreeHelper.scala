import scala.collection.immutable.Queue

object SymbolicTreeHelper {

  def findFirstUnexploredNode(symbolicNode: SymbolicNode): Option[List[SymbolicNode]] = {
    def loop(queue: Queue[List[SymbolicNode]]): Option[List[SymbolicNode]] = {
      queue.headOption match {
        case Some(path) =>
          val tailQueue = queue.tail
          val latestNode = path.last
          latestNode match {
            case s: StatementSymbolicNode => s.followUp match {
              case Some(followUp) =>
                val newPath = path :+ followUp
                loop(tailQueue :+ newPath)
              case None =>
                // Leaf node: pop queue and continue
                loop(tailQueue)
            }
            case b: BranchSymbolicNode =>
              if (!b.thenBranchTaken || !b.elseBranchTaken) {
                assert(b.thenBranchTaken != b.elseBranchTaken, "Should not happen: one of both branches should be True")
                Some(path)
              } else {
                // Both branches have already been explored, so continue looking through both branches to find an unexplored node
                // Although both branches have been explored, it could be that they don't actually have any successors, e.g., because the branch ends
                val newQueue: Queue[List[SymbolicNode]] = (b.thenBranch, b.elseBranch) match {
                  case (Some(thenBranch), Some(elseBranch)) =>
                    tailQueue :+ (path :+ thenBranch) :+ (path :+ elseBranch)
                  case (Some(thenBranch), None) =>
                    tailQueue :+ (path :+ thenBranch)
                  case (None, Some(elseBranch)) =>
                    tailQueue :+ (path :+ elseBranch)
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
    val queue: Queue[List[SymbolicNode]] = Queue(List(symbolicNode))
    loop(queue)
  }

}
