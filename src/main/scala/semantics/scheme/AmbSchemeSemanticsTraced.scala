class AmbSchemeSemanticsTraced[Abs : IsSchemeLattice, Addr : Address, Time : Timestamp]
  (override val absSem: SchemeSemantics[Abs, Addr, ZeroCFA.T], primitives: SchemePrimitives[Addr, Abs])
  extends SchemeSemanticsTraced[Abs, Addr, Time](absSem, primitives) {

  case class FrameAmbT(exps: List[SchemeExp]) extends SchemeFrameT

  //TODO override convertFrame

  val actionPopFailKont = ActionPopFailKontT[SchemeExp, Abs, Addr]()

  override def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[InterpreterStep[SchemeExp, Abs, Addr]] = e match {
    case SchemeAmb(Nil, _) =>
      Set(interpreterStep(List(actionPopFailKont)))
    case SchemeAmb(exp :: rest, _) =>
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