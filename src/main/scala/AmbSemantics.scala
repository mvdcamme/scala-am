//case class FailureFrame[Exp : Expression, Abs : AbstractValue, Addr : Address](frame: Frame)
//  extends Frame {
  //  def subsumes(that: Frame) = frame match {
  //    case FailureFrame(thatFrame) =>
      //      frame.subsumes(thatFrame)
  //    case _ => false
      //  }
  //}
case class UndoActionFrame[Exp : Expression, Abs : AbstractValue, Addr : Address](action: ActionSingleTraced[Exp, Abs, Addr])
  extends Frame {
    def subsumes(that: Frame) = that.equals(this)
  }

case class ActionPopFailKontTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends Action[Exp, Abs, Addr]
case class ActionPushFailKontTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (failureFrame: Frame) extends Action[Exp, Abs, Addr]


/*
 * Single actions
 */

abstract class ActionSingleTraced[Exp : Expression, Abs : AbstractValue, Addr : Address] extends Action[Exp, Abs, Addr]

case class ActionSinglePopKontTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends ActionSingleTraced[Exp, Abs, Addr]
case class ActionSinglePushKontTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (frame: Frame) extends ActionSingleTraced[Exp, Abs, Addr]
case class ActionSingleSaveSpecificEnvTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (ρ: Environment[Addr]) extends ActionSingleTraced[Exp, Abs, Addr]
case class ActionSingleSaveValTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (value: Abs) extends ActionSingleTraced[Exp, Abs, Addr]
case class ActionSingleRestoreEnvTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends ActionSingleTraced[Exp, Abs, Addr]
case class ActionSingleRestoreValTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends ActionSingleTraced[Exp, Abs, Addr]
