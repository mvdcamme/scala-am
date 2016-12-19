trait StateChangeEdge[Exp, Abs, Addr, Time]

trait ApplyStateChangeEdge[Exp, Abs, Addr, Time] {

  type State

  def applyStateChangeEdge(stateChangeEdge: StateChangeEdge[Exp, Abs, Addr, Time]): State

}

/*********************************************************************************************************************
 *                                                     AAM edges                                                     *
 *********************************************************************************************************************/

case class ControlValueReached[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
  (v: Abs)
  extends StateChangeEdge[Exp, Abs, Addr, Time]

case class ControlExpEvaluated[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
  (e: Exp, env: Environment[Addr])
  extends StateChangeEdge[Exp, Abs, Addr, Time]

case class TimeTick[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
  ()
  extends StateChangeEdge[Exp, Abs, Addr, Time]

case class TimeTickExp[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
  (e: Exp)
  extends StateChangeEdge[Exp, Abs, Addr, Time]

case class KontAddrChanged[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp, KontAddr : KontAddress]
  (a: KontAddr)
  extends StateChangeEdge[Exp, Abs, Addr, Time]

case class KontStoreFramePush[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp, KontAddr : KontAddress]
  (pushAddress: KontAddr, kont: Kont[KontAddr])
  extends StateChangeEdge[Exp, Abs, Addr, Time]

case class ControlErrorReached[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
  (error: SemanticError)
  extends StateChangeEdge[Exp, Abs, Addr, Time]

case class StoreExtend[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
  (a: Addr, v: Abs)
  extends StateChangeEdge[Exp, Abs, Addr, Time]

case class StoreUpdate[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
  (a: Addr, v: Abs)
  extends StateChangeEdge[Exp, Abs, Addr, Time]