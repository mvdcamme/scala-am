class UsesGraph[Exp : Expression,
                AbstL : IsSchemeLattice,
                Addr : Address,
                State <: StateTrait[Exp, AbstL, Addr, _]] {

  type EdgeAnnotation = (List[EdgeFilterAnnotation], List[ActionReplay[Exp, AbstL, Addr]])
  type Edge = (EdgeAnnotation, State)
  type AbstractGraph = Graph[State, EdgeAnnotation]

  type AbstractFrame = SchemeFrame[AbstL, HybridAddress.A, HybridTimestamp.T]
  type ConcreteFrame = SchemeFrame[ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]

}
