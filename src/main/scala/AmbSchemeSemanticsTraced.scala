/*

class AmbSchemeSemanticsTraced[Abs : JoinLattice, Addr : Address, Time : Timestamp]
  (override val absSem: SchemeSemantics[Abs, Addr, Time])
  extends SchemeSemanticsTraced[Abs, Addr, Time](absSem) {

  case class FrameAmbT(exps: List[SchemeExp]) extends SchemeFrameT
  case class FrameUndoAction(action: Action[SchemeExp, Abs, Addr]) extends SchemeFrameT

  val actionPopFailKont = ActionPopFailKontT[SchemeExp, Abs, Addr]()

  override def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[InterpreterStep[SchemeExp, Abs, Addr]] = e match {
    case SchemeAmb(Nil) =>
      Set(interpreterStep(List(actionPopFailKont)))
    case SchemeAmb(exp :: rest) =>
      Set(interpreterStep(List(ActionPushFailKontT(FrameAmbT(rest)), ActionEvalT(exp))))
    case _ => super.stepEval(e, ρ, σ, t)
  }

  override def stepKont(v: Abs, frame: Frame, σ: Store[Addr, Abs], t: Time): Set[InterpreterStep[SchemeExp, Abs, Addr]] = frame match {
    case FrameAmbT(Nil) =>
      Set(interpreterStep(List(actionPopFailKont)))
    case FrameAmbT(exp :: rest) =>
      Set(interpreterStep(List(ActionPushFailKontT(FrameAmbT(rest)), ActionEvalT(exp))))
    case UndoActionFrame(action) =>
      val actions: List[Action[SchemeExp, Abs, Addr]] = List(action.asInstanceOf[Action[SchemeExp, Abs, Addr]],
                                                             ActionPopFailKontT())
      Set(interpreterStep(actions))
    case _ => super.stepKont(v, frame, σ, t)
  }

}

*/