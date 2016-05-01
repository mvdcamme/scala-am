class AmbSchemeSemanticsTraced[Abs : AbstractValue, Addr : Address, Time : Timestamp]
  extends SchemeSemanticsTraced[Abs, Addr, Time] {

  case class FrameAmb(exps: List[SchemeExp]) extends SchemeFrame
  case class FrameUndoAction(action: Action[SchemeExp, Abs, Addr]) extends SchemeFrame

  val actionPopFailKont = ActionPopFailKontTraced[SchemeExp, Abs, Addr]()

  override def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[Step[SchemeExp, Abs, Addr]] = e match {
    case SchemeAmb(Nil) =>
      Set(interpreterReturn(List(actionPopFailKont)))
    case SchemeAmb(exp :: rest) =>
      Set(interpreterReturn(List(ActionPushFailKontTraced(FrameAmb(rest)), ActionEvalTraced(exp))))
    case _ => super.stepEval(e, ρ, σ, t)
  }

  override def stepKont(v: Abs, frame: Frame, σ: Store[Addr, Abs], t: Time) : Set[Step[SchemeExp, Abs, Addr]] = frame match {
    case FrameAmb(Nil) =>
      Set(interpreterReturn(List(actionPopFailKont)))
    case FrameAmb(exp :: rest) =>
      Set(interpreterReturn(List(ActionPushFailKontTraced(FrameAmb(rest)))))
    case UndoActionFrame(action) => {
      var actions : List[Action[SchemeExp, Abs, Addr]] = List(ActionPopFailKontTraced())
      action match {
        case ActionSinglePopKontTraced() =>
          actions = action.asInstanceOf[Action[SchemeExp, Abs, Addr]] :: actions
        case ActionSinglePushKontTraced(frame) =>
          actions = action.asInstanceOf[Action[SchemeExp, Abs, Addr]] :: actions
        case ActionSingleSaveValTraced(value) =>
          actions = action.asInstanceOf[Action[SchemeExp, Abs, Addr]] :: actions
        case ActionSingleRestoreEnvTraced() =>
          actions = action.asInstanceOf[Action[SchemeExp, Abs, Addr]] :: actions
        case ActionSingleRestoreValTraced() =>
          actions = action.asInstanceOf[Action[SchemeExp, Abs, Addr]] :: actions
        case ActionSingleSaveSpecificEnvTraced(ρ) =>
          actions = action.asInstanceOf[Action[SchemeExp, Abs, Addr]] :: actions
      }
      Set(interpreterReturn(actions))
    }
    case _ => super.stepKont(v, frame, σ, t)
  }

}