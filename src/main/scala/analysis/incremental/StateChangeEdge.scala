trait StateChangeEdge[+State <: StateTrait[_, _, _, _]]

trait ActionReplayApplier[Exp, Abs, Addr, Time, State <: StateTrait[Exp, Abs, Addr, Time]] {

  def applyActionReplay(state: State, action: ActionReplay[Exp, Abs, Addr])
                       (implicit sabs: IsSchemeLattice[Abs]): Set[(State, List[EdgeFilterAnnotation])]

  def subsumes(s1: State, s2: State): Option[StateSubsumed[Abs, Addr]]
  def statesEqual(s1: State, s2: State): Boolean

  def halted(state: State): Boolean

  def evaluatedFalse(state: State)
                    (implicit sabs: IsSchemeLattice[Abs]): Boolean
  def evaluatedTrue(state: State)
                   (implicit sabs: IsSchemeLattice[Abs]): Boolean

  def getKonts(state: State): Set[Kont[KontAddr]]

  case class JoinedInfo(finalValue: Abs, store: Store[Addr, Abs], kstore: KontStore[KontAddr])
                       (implicit abs: JoinLattice[Abs]) {
    def join(other: JoinedInfo): JoinedInfo = {
      val joinedFinalValue = abs.join(finalValue, other.finalValue)
      val joinedStore = store.join(other.store)
      val joinedKStore = kstore.join(other.kstore)
      JoinedInfo(joinedFinalValue, joinedStore, joinedKStore)
    }
  }
  def joinStates(states: Set[State]): JoinedInfo

}

abstract class StoreChangeSemantics[Abs : JoinLattice, Addr : Address]
  ()
  extends StateChangeEdge[StateTrait[_, Abs, Addr, _]] {
  implicit def convert[State <: StateTrait[_, Abs, Addr, _]]: StateChangeEdge[State]
}

case class StoreExtendSemantics[Abs : JoinLattice, Addr : Address]
  (a: Addr, value: Abs)
  extends StoreChangeSemantics[Abs, Addr] {
  override implicit def convert[State <: StateTrait[_, Abs, Addr, _]] = StoreExtend[Abs, Addr, State](a, value)
}

case class StoreUpdateSemantics[Abs : JoinLattice, Addr : Address]
  (a: Addr, value: Abs)
  extends StoreChangeSemantics[Abs, Addr] {
  override implicit def convert[State <: StateTrait[_, Abs, Addr, _]] = StoreUpdate[Abs, Addr, State](a, value)
}






case class ControlErrorReached[State <: StateTrait[_, _, _, _]]
(error: SemanticError)
  extends StateChangeEdge[State]

case class ControlExpEvaluated[Exp : Expression, Addr : Address, State <: StateTrait[Exp, _, Addr, _]]
  (e: Exp, env: Environment[Addr])
  extends StateChangeEdge[State]

case class ControlValueReached[Abs : JoinLattice, State <: StateTrait[_, Abs, _, _]]
(v: Abs)
  extends StateChangeEdge[State]

case class KontAddrChanged[KontAddr : KontAddress, State <: StateTrait[_, _, _, _]]
  (a: KontAddr)
  extends StateChangeEdge[State]

case class KontStoreFramePush[KontAddr : KontAddress, State <: StateTrait[_, _, _, _]]
  (pushAddress: KontAddr, kont: Kont[KontAddr])
  extends StateChangeEdge[State]

case class  StoreExtend[Abs : JoinLattice, Addr : Address, State <: StateTrait[_, Abs, Addr, _]]
  (a: Addr, v: Abs)
  extends StateChangeEdge[State]

case class StoreUpdate[Abs : JoinLattice, Addr : Address, State <: StateTrait[_, Abs, Addr, _]]
  (a: Addr, v: Abs)
  extends StateChangeEdge[State]

case class TimeTick[State <: StateTrait[_, _, _, _]]
()
  extends StateChangeEdge[State]

case class TimeTickExp[Exp : Expression, State <: StateTrait[Exp, _, _, _]]
(e: Exp)
  extends StateChangeEdge[State]