case class BranchSymbolicNode(branch: BranchConstraint,
                              var thenBranchTaken: Boolean,
                              var elseBranchTaken: Boolean,
                              var thenBranch: Option[BranchSymbolicNode],
                              var elseBranch: Option[BranchSymbolicNode]) {
  /**
    * Combines two SymbolicNodes, giving preference to the object receiving this method call.
    * E.g., if two SymbolicNodes have similar properties, properties of the receiving object are copied
    * @param that
    * @return
    */
  def combine(that: BranchSymbolicNode): BranchSymbolicNode = {
    //      val tbNode: BranchSymbolicNode = if (this.thenBranchTaken) {
    //        this.copy(thenBranchTaken = this.thenBranchTaken, thenBranch = this.thenBranch)
    //      } else if
    val t1: BranchSymbolicNode = if (this.thenBranchTaken) {
      // This node has already taken the then-branch, no need to combine it with the other for the then-branch
      this
    } else if (that.thenBranchTaken) {
      // This node has not yet taken the then-branch but the other has, transfer then-branch-properties to new node
      this.copy(thenBranchTaken = that.thenBranchTaken, thenBranch = that.thenBranch)
    } else {
      // Neither this or the other node has taken the then-branch
      this
    }
    val t2: BranchSymbolicNode = if (this.elseBranchTaken) {
      t1
    } else if (that.elseBranchTaken) {
      t1.copy(elseBranchTaken = that.elseBranchTaken, elseBranch = that.elseBranch)
    } else {
      t1
    }
    t2
  }

  def deepCopy: BranchSymbolicNode =
    BranchSymbolicNode(branch, thenBranchTaken, elseBranchTaken, thenBranch.map(_.deepCopy), elseBranch.map(_.deepCopy))
}