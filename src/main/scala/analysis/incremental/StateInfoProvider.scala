trait StateInfoProvider[Exp, Abs, Addr, Time, State <: StateTrait[Exp, Abs, Addr, Time]] {

  def halted(state: State): Boolean
  def store(state: State): Store[Addr, Abs]

  def deltaStoreEmpty(state1: State, state2: State): Boolean
  def deltaKStore(state1: State, state2: State): Option[Iterable[(KontAddr, KontAddr)]]

}