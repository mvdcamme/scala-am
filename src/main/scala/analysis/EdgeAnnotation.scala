trait EdgeAnnotation

case object NoEdgeAnnotation$ extends EdgeAnnotation {
  override def toString = "void"
}
case object TODOEdgeAnnotation extends EdgeAnnotation

/*
 * Control-flow split due to if: took else-branch or then-branch.
 */
case object ElseBranchTaken extends EdgeAnnotation {
  override def toString = "Else"
}

case object ThenBranchTaken extends EdgeAnnotation {
  override def toString = "Then"
}

/*
 * Control-flow split due to spurious return: check continuation frame used.
 */
case class FrameFollowed[Abs : JoinLattice](frame: SchemeFrame[Abs, HybridAddress.A, HybridTimestamp.T]) extends EdgeAnnotation {
  override def toString = s"Followed $frame"
}

/*
 * Pushed a frame into the continuation store.
 */
case class FramePushed[Abs : JoinLattice](frame: SchemeFrame[Abs, HybridAddress.A, HybridTimestamp.T]) extends EdgeAnnotation {
  override def toString = s"Pushed $frame"
}

/*
 * Current continuation address changed to a.
 */
case class NextKontAddressNow(a: KontAddr) extends EdgeAnnotation {
  override def toString = s"next = $a"
}

/*
 * State not explored further because it was already subsumed by another state in the graph.
 */
case object StateSubsumed extends EdgeAnnotation {
  override def toString = "Subsumed by"
}

/*
 * State leads to the evaluation of the given expression.
 */
case class EvaluatingExpression[Exp : Expression](exp: Exp) extends EdgeAnnotation {
  override def toString = s"Evaluate $exp"
}

/*
 * State leads to a continuation state that reached the given value.
 */
case class ReachedValue[Abs : JoinLattice](v: Abs) extends EdgeAnnotation {
  override def toString = s"Reached $v"
}

/*
 * State leads to a continuation state that reached the given concrete value.
 */
case class ReachedConcreteValue(v: ConcreteConcreteLattice.L) extends EdgeAnnotation {
  override def toString = s"Reached concrete $v"
}