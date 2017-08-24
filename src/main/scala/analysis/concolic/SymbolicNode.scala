trait SymbolicNode

case class BranchSymbolicNode(branch: BranchConstraint,
                              var thenBranchTaken: Boolean,
                              var elseBranchTaken: Boolean,
                              var thenBranch: Option[SymbolicNode],
                              var elseBranch: Option[SymbolicNode])
  extends SymbolicNode
case class StatementSymbolicNode(statement: StatementConstraint,
                                 var followUp: Option[SymbolicNode])
  extends SymbolicNode