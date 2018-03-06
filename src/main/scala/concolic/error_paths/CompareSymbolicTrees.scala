import backend.tree._

object CompareSymbolicTrees {

  private def countPaths(node: SymbolicNode): Int = node match {
    case EmptyNode => 1
    case IllegalizedNode(_) => 0
    case BranchSymbolicNode(_, _, _, thenBranch, elseBranch) => countPaths(thenBranch) + countPaths(elseBranch)
  }

  /**
    * Counts how many paths in the second tree are hidden under illegalized nodes in the first tree.
    * @param node1
    * @param node2
    * @return
    */
  def countUniqueIllegalizedPaths(node1: SymbolicNode, node2: SymbolicNode): Int = (node1, node2) match {
    case (BranchSymbolicNode(_, _, _, thenBranch1, elseBranch1), BranchSymbolicNode(_, _, _, thenBranch2, elseBranch2)) =>
      countUniqueIllegalizedPaths(thenBranch1, thenBranch2) + countUniqueIllegalizedPaths(elseBranch1, elseBranch2)
    case (_: BranchSymbolicNode, EmptyNode) => 0
    case (_: BranchSymbolicNode, IllegalizedNode(_)) => 0
    case (EmptyNode, _: BranchSymbolicNode) => 0
    case (EmptyNode, EmptyNode) => 0
    case (EmptyNode, IllegalizedNode(_)) => 0
    case (IllegalizedNode(_), bsn: BranchSymbolicNode) => countPaths(bsn)
    case (IllegalizedNode(_), EmptyNode) => 1
    case (IllegalizedNode(_), IllegalizedNode(_)) => 0
  }

  /**
    * Counts how many unique paths there are in the second tree that are not hidden under illegalized nodes in the first tree.
    * @param node1
    * @param node2
    * @return
    */
  def countUniqueNonIllegalizedPaths(node1: SymbolicNode, node2: SymbolicNode): Int = (node1, node2) match {
    case (BranchSymbolicNode(_, _, _, thenBranch1, elseBranch1), BranchSymbolicNode(_, _, _, thenBranch2, elseBranch2)) =>
      countUniqueNonIllegalizedPaths(thenBranch1, thenBranch2) + countUniqueNonIllegalizedPaths(elseBranch1, elseBranch2)
    case (_: BranchSymbolicNode, EmptyNode) => 0
    case (_: BranchSymbolicNode, IllegalizedNode(_)) => 0
    case (EmptyNode, bsn: BranchSymbolicNode) => countPaths(bsn)
    case (EmptyNode, EmptyNode) => 0
    case (EmptyNode, IllegalizedNode(_)) => 0
    case (IllegalizedNode(_), _: BranchSymbolicNode) => 0
    case (IllegalizedNode(_), EmptyNode) => 0
    case (IllegalizedNode(_), IllegalizedNode(_)) => 0
  }

}
