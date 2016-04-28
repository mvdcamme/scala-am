

trait FailureStackElement[Exp, Abs, Addr]
case class FailureFrame[Exp : Expression, Abs : AbstractValue, Addr : Address](frame: Frame)
  extends FailureStackElement[Exp, Abs, Addr]
case class UndoAction[Exp : Expression, Abs : AbstractValue, Addr : Address](action: Action[Exp, Abs, Addr])
  extends FailureStackElement[Exp, Abs, Addr]

case class ActionPopFailKontTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends Action[Exp, Abs, Addr]
case class ActionPushFailKontTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (failureStackElement: FailureStackElement[Exp, Abs, Addr])
  extends Action[Exp, Abs, Addr]
case class ActionPushSpecificValTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (value: Abs) extends Action[Exp, Abs, Addr]
case class ActionSaveSpecificEnvTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (ρ: Environment[Addr])extends Action[Exp, Abs, Addr]
case class ActionRestoreValTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends Action[Exp, Abs, Addr]