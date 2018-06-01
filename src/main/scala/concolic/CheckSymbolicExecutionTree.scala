import backend.expression._
import backend.tree.SymbolicNode
import backend.tree._

object CheckSymbolicExecutionTree {

  private val lastId: Int = 9
  private var totalNumberOfMerges: Int = 56
  private def checkNrOfMerges(): Boolean = {
    totalNumberOfMerges -= 1
    totalNumberOfMerges >= 0
  }

  private def isFinalConstraint(constraint: BranchConstraint): Either[String, Boolean] = constraint.exp match {
    case RelationalConcolicExpression(left, IntEqual, right) => left match {
      case ConcolicInput(id) => Right(id == lastId)
      case _ => Left(s"Left-expression in a RelationalCOncolicExpression should always be a ConcolicInput, but is $left")
    }
    case exp => Left(s"Execution tree should only contain RelationalConcolicExpression-constraints with an IntEqual operator, but got $exp")
  }
  private def isFinalConstraint(node: SymbolicNode[_]): Either[String, Boolean] = node match {
    case bsn: BranchSymbolicNode[_] => isFinalConstraint(bsn.branch)
    case _ => Left(s"Should be a BranchSymbolicNode, but is $node")
  }

  private def isLeafOrSafeNode(node: SymbolicNode[_]): Boolean = node match {
    case RegularLeafNode() | SafeNode(_) => true
    case _ => false
  }

  private def walkNonFinalNode(node: SymbolicNode[_]): Option[String] = node match {
    case RegularLeafNode() | SafeNode(_) | UnexploredNode() | UnsatisfiableNode() => Some(s"Shouldn't get a $node here")
    case MergedNode() => if (checkNrOfMerges()) None else Some("Too many MergedNodes encountered")
    case bsn: BranchSymbolicNode[_] => walkTree(bsn)
  }

  private def walkTree(node: BranchSymbolicNode[_]): Option[String] = node match {
    case BranchSymbolicNode(branch, thenBranch, elseBranch, _) =>
      isFinalConstraint(branch) match {
        case Left(str) => Some(str)
        case Right(isFinal) => if (isFinal) {
          /*
           * If the current constraint indicates a final check (i.e., something of the form i9 = k),
           * the left and right nodes should either be safe or regular nodes, or branch nodes of the form i9 = k + 1.
           */
          val walkedLeft = if (isLeafOrSafeNode(thenBranch)) None else { thenBranch match {
            case bsn: BranchSymbolicNode[_] => walkTree(bsn)
            case _ => Some(s"thenBranch of a final constraint should either be a SafeNode, a LeafNode or another BranchSymbolicNode, but is $thenBranch instead")
          } }
          val walkedRight = if (isLeafOrSafeNode(elseBranch)) None else { elseBranch match {
            case bsn: BranchSymbolicNode[_] => walkTree(bsn)
            case _ => Some(s"elseBranch of a final constraint should either be a SafeNode, a LeafNode or another BranchSymbolicNode, but is $elseBranch instead")
          } }
          /* Append the two values. */
          walkedLeft.flatMap(_ => walkedRight)
        } else {
          val walkedLeft = walkNonFinalNode(thenBranch)
          val walkedRight = walkNonFinalNode(elseBranch)
          walkedLeft.flatMap(_ => walkedRight)
        }
      }
  }

  def check(root: SymbolicNode[_]): Option[String] = root match {
    case bsn: BranchSymbolicNode[_] => walkTree(bsn)
    case _ => Some(s"Root should be a BranchSymbolicNode, but is a $root")
  }

}
