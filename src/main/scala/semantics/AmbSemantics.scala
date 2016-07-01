/*
 * Restart points
 */

case class RestartStoppedInBacktrack[Exp : Expression, Abs : JoinLattice, Addr : Address]()
  extends RestartPoint[Exp, Abs, Addr]

/*
 * Frames
 */

case class UndoActionFrame[Exp : Expression, Abs : JoinLattice, Addr : Address]
  (action: ActionSingleT[Exp, Abs, Addr])
  extends Frame {
    def subsumes(that: Frame) = that.equals(this)
  }

/*
 * Normal actions
 */

case class ActionPopFailKontT[Exp : Expression, Abs : JoinLattice, Addr : Address]()
  extends ActionT[Exp, Abs, Addr]
case class ActionPushFailKontT[Exp : Expression, Abs : JoinLattice, Addr : Address]
  (failureFrame: Frame)
  extends ActionT[Exp, Abs, Addr]

/*
 * Single actions
 */

abstract class ActionSingleT[Exp : Expression, Abs : JoinLattice, Addr : Address]
  extends ActionT[Exp, Abs, Addr]

case class ActionSinglePopKontT[Exp : Expression, Abs : JoinLattice, Addr : Address]()
  extends ActionSingleT[Exp, Abs, Addr]
case class ActionSinglePushKontT[Exp : Expression, Abs : JoinLattice, Addr : Address]
  (frame: Frame)
  extends ActionSingleT[Exp, Abs, Addr]
case class ActionSingleSaveSpecificEnvT[Exp : Expression, Abs : JoinLattice, Addr : Address]
  (ρToSave: Environment[Addr], ρToReplace: Environment[Addr])
  extends ActionSingleT[Exp, Abs, Addr]
case class ActionSingleSaveValT[Exp : Expression, Abs : JoinLattice, Addr : Address]
  (value: Abs)
  extends ActionSingleT[Exp, Abs, Addr]
case class ActionSingleRestoreEnvT[Exp : Expression, Abs : JoinLattice, Addr : Address]()
  extends ActionSingleT[Exp, Abs, Addr]
case class ActionSingleRestoreValT[Exp : Expression, Abs : JoinLattice, Addr : Address]()
  extends ActionSingleT[Exp, Abs, Addr]
