//case class FailureFrame[Exp : Expression, Abs : AbstractValue, Addr : Address](frame: Frame)
//  extends Frame {
  //  def subsumes(that: Frame) = frame match {
  //    case FailureFrame(thatFrame) =>
      //      frame.subsumes(thatFrame)
  //    case _ => false
      //  }
  //}
case class UndoActionFrame[Exp : Expression, Abs : AbstractValue, Addr : Address](action: Action[Exp, Abs, Addr])
  extends Frame {
    def subsumes(that: Frame) = that.equals(this)
  }

case class ActionPopFailKontTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends Action[Exp, Abs, Addr]
case class ActionPushFailKontTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (failureFrame: Frame) extends Action[Exp, Abs, Addr]
case class ActionPushKStackKontTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (frame: Frame) extends Action[Exp, Abs, Addr]
case class ActionPushSpecificValTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]
(value: Abs) extends Action[Exp, Abs, Addr]
case class ActionSaveSpecificEnvTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (ρ: Environment[Addr]) extends Action[Exp, Abs, Addr]
case class ActionRestoreValTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends Action[Exp, Abs, Addr]
