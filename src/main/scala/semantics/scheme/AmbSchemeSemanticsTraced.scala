class AmbSchemeSemanticsTraced[Abs: IsSchemeLattice, Addr: Address,
Time: Timestamp](primitives: SchemePrimitives[Addr, Abs])
    extends SchemeSemanticsTraced[Abs, Addr, Time](primitives) {

  case class FrameAmbT(exps: List[SchemeExp]) extends SchemeFrameT

  val actionPopFailKont = ActionPopFailKontT[SchemeExp, Abs, Addr]()

  override def convertAbsInFrame[OtherAbs: IsConvertableLattice](frame: SchemeFrame[Abs, Addr, Time],
                                                                 convertValue: (Abs) => OtherAbs,
                                                                 convertEnv: (Environment[Addr]) => Environment[Addr],
                                                                 abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time])
  : SchemeFrame[OtherAbs, Addr, Time] = ???

  override def stepEval(e: SchemeExp,
                        ρ: Environment[Addr],
                        σ: Store[Addr, Abs],
                        t: Time): Set[InterpreterStep[SchemeExp, Abs, Addr]] =
    e match {
      case SchemeAmb(Nil, _) =>
        Set(interpreterStep(List(actionPopFailKont)))
      case SchemeAmb(exp :: rest, _) =>
        Set(
          interpreterStep(
            List(ActionPushFailKontT(FrameAmbT(rest)), ActionEvalT(exp))))
      case _ => super.stepEval(e, ρ, σ, t)
    }

  override def stepKont(v: Abs,
                        frame: Frame,
                        σ: Store[Addr, Abs],
                        t: Time): Set[InterpreterStep[SchemeExp, Abs, Addr]] =
    frame match {
      case FrameAmbT(Nil) =>
        Set(interpreterStep(List(actionPopFailKont)))
      case FrameAmbT(exp :: rest) =>
        Set(
          interpreterStep(
            List(ActionPushFailKontT(FrameAmbT(rest)), ActionEvalT(exp))))
      case UndoActionFrame(action) =>
        val actions: List[ActionTrace[SchemeExp, Abs, Addr]] = List(
          action.asInstanceOf[ActionTrace[SchemeExp, Abs, Addr]],
          ActionPopFailKontT())
        Set(interpreterStep(actions))
      case _ => super.stepKont(v, frame, σ, t)
    }

}
