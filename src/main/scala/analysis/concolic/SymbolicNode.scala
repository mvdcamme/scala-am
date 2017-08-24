trait SymbolicNode

case class BranchSymbolicNode(branch: BranchConstraint,
                              thenBranchTaken: Boolean,
                              thenBranch: Option[SymbolicNode],
                              elseBranch: Option[SymbolicNode])
case class StatementSymbolicNode(statement: StatementConstraint, followUp: Option[SymbolicNode])