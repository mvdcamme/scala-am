trait StateInfoProvider[State] {

  def halted(state: State): Boolean

  def deltaStoreEmpty(state1: State, state2: State): Boolean
  def deltaKStore(state1: State, state2: State): Option[Iterable[(KontAddr, KontAddr)]]

}