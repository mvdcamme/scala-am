import backend.tree._

object CompareSymbolicTrees {

  private def countNodes(node: SymbolicNode): Int = node match {
    case EmptyNode => 1
    case IllegalizedNode(_) => 1
    case BranchSymbolicNode(_, _, _, thenBranch, elseBranch) => 1 + countNodes(thenBranch) + countNodes(elseBranch)
  }

  /**
    * Counts how many nodes in the second tree are hidden under illegalized nodes in the first tree.
    * @param node1
    * @param node2
    * @return
    */
  def compareTrees(node1: SymbolicNode, node2: SymbolicNode): Int = (node1, node2) match {
    case (BranchSymbolicNode(_, _, _, thenBranch1, elseBranch1), BranchSymbolicNode(_, _, _, thenBranch2, elseBranch2)) =>
      compareTrees(thenBranch1, thenBranch2) + compareTrees(elseBranch1, elseBranch2)
    case (_: BranchSymbolicNode, EmptyNode) => 0
    case (_: BranchSymbolicNode, IllegalizedNode(_)) => 0
    case (EmptyNode, _: BranchSymbolicNode) => 0
    case (EmptyNode, EmptyNode) => 0
    case (EmptyNode, IllegalizedNode(_)) => 0
    case (IllegalizedNode(_), bsn: BranchSymbolicNode) => countNodes(bsn)
    case (IllegalizedNode(_), EmptyNode) => 1
    case (IllegalizedNode(_), IllegalizedNode(_)) => 0
  }

}
