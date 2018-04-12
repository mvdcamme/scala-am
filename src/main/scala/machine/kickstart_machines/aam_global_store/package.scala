import scalaz.Scalaz._

package object aam_global_store {

  type FullState[Exp, Abs, Addr, Time] = (KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time], GlobalStore[Addr, Abs], KontStore[KontAddr])

  type Tup[Exp, Abs, Addr, Time] = (EdgeAnnotation[Exp, Abs, Addr], KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time])
  type G[Exp, Abs, Addr, Time] = Graph[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time], EdgeAnnotation[Exp, Abs, Addr], KickstartAAMGlobalStoreState.Context[Exp, Abs, Addr, Time]]

  case class GlobalStore[Addr: Address, Abs: JoinLattice](store: DeltaStore[Addr, Abs], delta: Map[Addr, Abs]) {
    def includeDelta(d: Option[Map[Addr, Abs]]): GlobalStore[Addr, Abs] = d match {
      case Some(d) => this.copy(delta = delta |+| d)
      case None => throw new Exception("AAMGlobalStore should be used with a store that supports delta!")
    }
    def isUnchanged = delta.isEmpty
    def commit = if (isUnchanged) { this } else { this.copy(store = store.addDelta(delta), delta = Map[Addr, Abs]()) }
  }

  implicit def stateWithKey[Exp: Expression, Abs: JoinLattice, Addr: Address, Time: Timestamp] = new WithKey[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]] {
    type K = KontAddr
    def key(st: KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]) = st.a
  }


}
