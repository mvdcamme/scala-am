import backend.PathConstraintWith
import backend.tree._

package object abstract_state_heuristic {

  type AbstractStatePCElement[State] = (Constraint, Boolean, Option[State])
  type PathConstraintWithState[State] = PathConstraintWith[AbstractStatePCElement[State]]

  sealed trait AbstractStateSymbolicNode[State] {
    def toSymbolicNode: SymbolicNode
  }
  case class AbstractStateBranchSymbolicNode[State](branch: BranchConstraint,
                                                    var thenBranch: AbstractStateSymbolicNode[State],
                                                    var elseBranch: AbstractStateSymbolicNode[State],
                                                    state: State) extends AbstractStateSymbolicNode[State] {
    def toSymbolicNode: BranchSymbolicNode = BranchSymbolicNode(branch, thenBranch.toSymbolicNode, elseBranch.toSymbolicNode)
    def thenBranchTaken: Boolean = thenBranch match {
      case AbstractStateUnexploredNode() => false
      case _ => true
    }
    def elseBranchTaken: Boolean = elseBranch match {
      case AbstractStateUnexploredNode() => false
      case _ => true
    }
    def setElseBranch(child: AbstractStateSymbolicNode[State]): Unit = {
      elseBranch = child
    }
    def setThenBranch(child: AbstractStateSymbolicNode[State]): Unit = {
      thenBranch = child
    }
  }
  case class AbstractStateRegularLeafNode[State]() extends AbstractStateSymbolicNode[State] {
    def toSymbolicNode: SymbolicNode = RegularLeafNode
  }
  case class AbstractStateSafeNode[State](node: AbstractStateSymbolicNode[State]) extends AbstractStateSymbolicNode[State] {
    def toSymbolicNode: SafeNode = SafeNode(node.toSymbolicNode)
  }
  case class AbstractStateUnexploredNode[State]() extends AbstractStateSymbolicNode[State] {
    def toSymbolicNode: SymbolicNode = UnexploredNode
  }
  case class AbstractStateUnsatisfiableNode[State]() extends AbstractStateSymbolicNode[State] {
    def toSymbolicNode: SymbolicNode = UnsatisfiableNode
  }

  implicit def abstractStateSymbolicNodeSymbolicNodeViewer[State]: SymbolicNodeViewer[AbstractStateSymbolicNode[State]] = new SymbolicNodeViewer[AbstractStateSymbolicNode[State]] {
    override def asSymbolicNode(abstractStateNode: AbstractStateSymbolicNode[State]): SymbolicNode = abstractStateNode.toSymbolicNode
  }

}
