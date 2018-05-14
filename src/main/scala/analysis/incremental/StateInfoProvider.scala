trait StateInfoProvider[Exp, Abs, Addr, Time, State] {

  def evalExp(state: State): Option[Exp]
  def valueReached(state: State): Option[Abs]

  def halted(state: State): Boolean
  def subsumes(state1: State, state2: State): Boolean
//  def store(state: State): Store[Addr, Abs]

//  def deltaStoreEmpty(state1: State, state2: State): Boolean
//  def deltaKStore(state1: State, state2: State): Option[Iterable[(KontAddr, KontAddr)]]

}