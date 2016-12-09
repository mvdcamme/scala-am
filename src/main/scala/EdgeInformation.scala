trait EdgeInformation

case object NoEdgeInformation extends EdgeInformation {
  override def toString = "void"
}
case object TODOEdgeInformation extends EdgeInformation

/*
 * Control-flow split due to if: took else or then.
 */
case object ElseBranchTaken extends EdgeInformation {
  override def toString = "Else"
}
case object ThenBranchTaken extends EdgeInformation {
  override def toString = "Then"
}

/*
 * Control-flow split due to operator-value: check operator's AST.
 */
case class OperatorTaken[Exp : Expression](body: List[Exp]) extends EdgeInformation {
  override def toString = body.toString()
}

/*
 * Control-flow split due to spurious return: check continuation frame used.
 */
case class FrameFollowed(frame: Frame) extends EdgeInformation {
  override def toString = frame.toString
}

/*
 * State not explored further because it was already subsumed by another state in the graph.
 */
case object StateSubsumed extends EdgeInformation {
  override def toString = "Subsumed by"
}

/*
 * State leads to the evaluation of the given expression.
 */
case class EvaluatingExpression[Exp : Expression](exp: Exp) extends EdgeInformation {
  override def toString = s"Evaluate $exp"
}

/*
 * State leads to a continuation state that reached the given value.
 */
case class ReachedValue[Abs : JoinLattice](v: Abs) extends EdgeInformation {
  override def toString = s"Reached $v"
}

/*
 * State leads to a continuation state that reached the given concrete value.
 */
case class ReachedConcreteValue(v: ConcreteConcreteLattice.L) extends EdgeInformation {
  override def toString = s"Reached concrete $v"
}