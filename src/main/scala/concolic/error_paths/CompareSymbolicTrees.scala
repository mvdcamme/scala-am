import backend.tree._

object CompareSymbolicTrees {

  private def countPaths(node: SymbolicNode): Int = node match {
    case EmptyNode => 1
    case IllegalizedNode(_) => 0
    case BranchSymbolicNode(_, thenBranchTaken, elseBranchTaken, thenBranch, elseBranch) =>
      (if (thenBranchTaken) countPaths(thenBranch) else 0) + (if (elseBranchTaken) countPaths(elseBranch) else 0)
  }

  /**
    * Counts how many paths in the second tree are hidden under illegalized nodes in the first tree.
    * @param node1
    * @param node2
    * @return
    */
  def countUniqueIllegalizedPaths(node1: SymbolicNode, node2: SymbolicNode): Int = (node1, node2) match {
    case (BranchSymbolicNode(_, thenBranchTaken1, elseBranchTaken1, thenBranch1, elseBranch1),
          BranchSymbolicNode(_, thenBranchTaken2, elseBranchTaken2, thenBranch2, elseBranch2)) =>
      /*
       * If the then/else-branch of node1 was taken, but the corresponding branch of node2 was not taken, 0 nodes
       * should be counted because there are no nodes in the corresponding branch of node2 that are hidden under
       * illegalized nodes in node1.
       * If the then/else-branch of node1 was not taken, but was proven to be illegal, and the corresponding branch
       * of node2 was taken, count the paths in that branch of node2.
       */
      (if (thenBranchTaken1 && thenBranchTaken2) countUniqueIllegalizedPaths(thenBranch1, thenBranch2)
       else if (thenBranchTaken2 && thenBranch1.isInstanceOf[IllegalizedNode]) countPaths(thenBranch2) else 0) +
      (if (elseBranchTaken1 && elseBranchTaken2) countUniqueIllegalizedPaths(elseBranch1, elseBranch2)
       else if (elseBranchTaken2 && elseBranch1.isInstanceOf[IllegalizedNode]) countPaths(elseBranch2) else 0)
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
    case (BranchSymbolicNode(_, thenBranchTaken1, elseBranchTaken1, thenBranch1, elseBranch1),
          BranchSymbolicNode(_, thenBranchTaken2, elseBranchTaken2, thenBranch2, elseBranch2)) =>
      /*
       * If the then/else-branch of node2 was taken, but the corresponding branch of node1 was not taken, the paths
       * in that branch of node2 definitely don't appear in node1 and should hence be counted.
       */
      (if (thenBranchTaken1 && thenBranchTaken2) countUniqueNonIllegalizedPaths(thenBranch1, thenBranch2)
       else if (thenBranchTaken2 && ! thenBranch1.isInstanceOf[IllegalizedNode]) countPaths(thenBranch2) else 0) +
      (if (elseBranchTaken1 && elseBranchTaken2) countUniqueNonIllegalizedPaths(elseBranch1, elseBranch2)
       else if (elseBranchTaken2 && ! elseBranch1.isInstanceOf[IllegalizedNode]) countPaths(elseBranch2) else 0)
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
