trait SymbolicNode {
  def constraint: ConcolicConstraint

  /**
    * Combines two SymbolicNodes, giving preference to the object receiving this method call.
    * E.g., if two SymbolicNodes have similar properties, properties of the receiving object are copied
    * @param that
    * @return
    */
  def combine(that: SymbolicNode): SymbolicNode
}

case class BranchSymbolicNode(branch: BranchConstraint,
                              var thenBranchTaken: Boolean,
                              var elseBranchTaken: Boolean,
                              var thenBranch: Option[SymbolicNode],
                              var elseBranch: Option[SymbolicNode])
  extends SymbolicNode {
  def constraint: ConcolicConstraint = branch

  def combine(that: SymbolicNode): SymbolicNode = that match {
    case that: BranchSymbolicNode =>
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
    case _: StatementSymbolicNode =>
      assert(false, "Should not happen")
      ???
  }
}
case class StatementSymbolicNode(statement: StatementConstraint,
                                 var followUp: Option[SymbolicNode])
  extends SymbolicNode {
  def constraint: ConcolicConstraint = statement

  def combine(that: SymbolicNode): SymbolicNode = that match {
    case _: StatementSymbolicNode =>
      this
    case _: BranchSymbolicNode =>
      assert(false, "Should not happen")
      ???
  }
}