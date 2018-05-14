trait FilterAnnotation

trait MachineFilterAnnotation extends FilterAnnotation
trait SemanticsFilterAnnotation extends FilterAnnotation

case object ElseBranchFilter extends SemanticsFilterAnnotation
case object ThenBranchFilter extends SemanticsFilterAnnotation


/**
  *
  * @param fExp
  * @param fValue
  * @param t: the timestamp of the originating state (i.e., the caller side)
  */
abstract class FunCallMark[Exp: Expression, Abs: JoinLattice, Time: Timestamp](
    val fExp: Exp,
    val fValue: Abs,
    val t: Time)
  extends MachineFilterAnnotation {
}

case class ClosureCallMark[Exp: Expression, Abs: JoinLattice, Time: Timestamp](
    override val fExp: Exp,
    closureValue: Abs,
    lambda: Exp,
    override val t: Time)
    extends FunCallMark[Exp, Abs, Time](fExp, closureValue, t)

case class PrimCallMark[Exp: Expression, Abs: JoinLattice, Addr: Address, Time: Timestamp](
    override val fExp: Exp,
    primValue: Abs,
    override val t: Time)
    extends FunCallMark[Exp, Abs, Time](fExp, primValue, t)

/*
 * Control-flow split due to spurious return: check continuation frame used.
 */
case class FrameFollowed[Abs : JoinLattice, Addr: Address, Time: Timestamp](frame: ConvertableSchemeFrame[Abs, Addr, Time])
  extends MachineFilterAnnotation {
  override def toString = s"Followed $frame"
}

/*
 * Popped a continuation address.
 */
case class KontAddrPopped(oldA: KontAddr, newA: KontAddr)
  extends MachineFilterAnnotation {
  override def toString = s"Popped = $oldA, next = $newA"
}

/*
 * State not explored further because it was already subsumed by another state in the graph.
 */
//TODO should also take into control subsumption into account
case object StateSubsumed
  extends MachineFilterAnnotation {
  override def toString = "Subsumed by"
}

/*
 * State leads to the evaluation of the given expression.
 */
case class EvaluatingExpression[Exp : Expression](exp: Exp)
  extends MachineFilterAnnotation {
  override def toString = s"Evaluate $exp"
}