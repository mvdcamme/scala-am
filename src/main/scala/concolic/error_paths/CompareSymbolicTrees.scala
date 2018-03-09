import backend.tree._

object CompareSymbolicTrees {

  def isLeafNode(node: SymbolicNode): Boolean = node match {
    case RegularLeafNode | UnsatisfiableNode | UnexploredNode => true
    case _: BranchSymbolicNode | _: SafeNode => false
  }

  private def countPaths(node: SymbolicNode): Int = node match {
    case _ if isLeafNode(node) => 1
    case SafeNode(_) => 0
    case b@BranchSymbolicNode(_, thenBranch, elseBranch) =>
      (if (b.thenBranchTaken) countPaths(thenBranch) else 0) + (if (b.elseBranchTaken) countPaths(elseBranch) else 0)
  }

  /**
    * Counts how many paths in the second tree are hidden under illegalized nodes in the first tree.
    * @param node1
    * @param node2
    * @return
    */
  def countUniqueSafePaths(node1: SymbolicNode, node2: SymbolicNode): Int = (node1, node2) match {
    case (b1@BranchSymbolicNode(_, thenBranch1, elseBranch1),
          b2@BranchSymbolicNode(_, thenBranch2, elseBranch2)) =>
      /*
       * If the then/else-branch of node1 was taken, but the corresponding branch of node2 was not taken, 0 nodes
       * should be counted because there are no nodes in the corresponding branch of node2 that are hidden under
       * safe nodes in node1.
       * If the then/else-branch of node1 was not taken, but was proven to be safe, and the corresponding branch
       * of node2 was taken, count the paths in that branch of node2.
       */
      (if (b1.thenBranchTaken && b2.thenBranchTaken) countUniqueSafePaths(thenBranch1, thenBranch2)
       else if (b2.thenBranchTaken && thenBranch1.isInstanceOf[SafeNode]) countPaths(thenBranch2) else 0) +
      (if (b1.elseBranchTaken && b2.elseBranchTaken) countUniqueSafePaths(elseBranch1, elseBranch2)
       else if (b2.elseBranchTaken && elseBranch1.isInstanceOf[SafeNode]) countPaths(elseBranch2) else 0)
    case (_: BranchSymbolicNode, _: SafeNode) => 0
    case (_: BranchSymbolicNode, _) if isLeafNode(node2) => 0
    case (SafeNode(_), bsn: BranchSymbolicNode) => countPaths(bsn)
    case (SafeNode(_), SafeNode(_)) => 0
    case (SafeNode(_), _) if isLeafNode(node2) => 1
    case _ if isLeafNode(node1)  => 0
  }

  /**
    * Counts how many unique paths there are in the second tree that are not hidden under illegalized nodes in the first tree.
    * @param node1
    * @param node2
    * @return
    */
  def countUniqueNonSafePaths(node1: SymbolicNode, node2: SymbolicNode): Int = (node1, node2) match {
    case (b1@BranchSymbolicNode(_, thenBranch1, elseBranch1),
          b2@BranchSymbolicNode(_, thenBranch2, elseBranch2)) =>
      /*
       * If the then/else-branch of node2 was taken, but the corresponding branch of node1 was not taken, the paths
       * in that branch of node2 definitely don't appear in node1 and should hence be counted.
       */
      (if (b1.thenBranchTaken && b2.thenBranchTaken) countUniqueNonSafePaths(thenBranch1, thenBranch2)
       else if (b2.thenBranchTaken && ! thenBranch1.isInstanceOf[SafeNode]) countPaths(thenBranch2) else 0) +
      (if (b1.elseBranchTaken && b2.elseBranchTaken) countUniqueNonSafePaths(elseBranch1, elseBranch2)
       else if (b2.elseBranchTaken && ! elseBranch1.isInstanceOf[SafeNode]) countPaths(elseBranch2) else 0)
    case (_: BranchSymbolicNode, _: SafeNode) => 0
    case (_: BranchSymbolicNode, _) if isLeafNode(node2) => 0
    case (SafeNode(_), _: BranchSymbolicNode) => 0
    case (SafeNode(_), SafeNode(_)) => 0
    case (SafeNode(_), _) if isLeafNode(node2) => 0
    case (_, bsn: BranchSymbolicNode) if isLeafNode(node1) => countPaths(bsn)
    case (_, SafeNode(_)) if isLeafNode(node1) => 0
    case _ if isLeafNode(node1) && isLeafNode(node2) => 0
  }

}
