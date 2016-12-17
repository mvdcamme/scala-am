trait EdgeAnnotation

case object NoEdgeAnnotation$ extends EdgeAnnotation {
  override def toString = "void"
}
case object TODOEdgeAnnotation extends EdgeAnnotation

/*
 * Control-flow split due to spurious return: check continuation frame used.
 */
case class FrameFollowed[Abs : JoinLattice](frame: SchemeFrame[Abs, HybridAddress.A, HybridTimestamp.T]) extends EdgeAnnotation {
  override def toString = s"Followed $frame"
}

/*
 * Popped a continuation address.
 */
case class KontAddrPopped(oldA: KontAddr, newA: KontAddr) extends EdgeAnnotation {
  override def toString = s"Popped = $oldA, next = $newA"
}

/*
 * Pushed a continuation address.
 */
case class KontAddrPushed(a: KontAddr) extends EdgeAnnotation {
  override def toString = s"Pushed = $a"
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