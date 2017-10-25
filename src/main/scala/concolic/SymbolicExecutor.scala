case object NoSymbolicNode extends SymbolicNode
case class ExpressionSymbolicNode(exp: ConcolicExpression) extends SymbolicNode

object SymbolicExecutor {

  type ErrorPath = List[SemanticError]


  private type SymbolicExecutionResult = (SymbolicNode, List[ErrorPath])

  def symbolicallyExecute(exp: SchemeExp, errorsPaths: List[ErrorPath]): SymbolicExecutionResult = exp match {
    case SchemeValue(ValueInteger(i), _) =>
      (ExpressionSymbolicNode(ConcolicInt(i)), errorsPaths)
    case SchemeValue(_, _) =>
      (NoSymbolicNode, errorsPaths)
  }

}
