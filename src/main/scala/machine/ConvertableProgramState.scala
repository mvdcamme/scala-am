import ConcreteConcreteLattice.{ L => ConcreteValue }

trait ConvertableProgramState[Exp, Addr, Time] {

  type Conversion[AbstL] = (ConvertedControl[Exp, AbstL, Addr], Store[Addr, AbstL], KontStore[KontAddr], KontAddr, HybridTimestamp.T)
  def addressesReachable: Set[Addr]

  def convertState[AbstL: IsConvertableLattice](
      concSem: ConvertableSemantics[Exp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
      abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
      initialKontAddress: KontAddr,
      mapKontAddress: (KontAddr, Option[Environment[HybridAddress.A]]) => KontAddr): Conversion[AbstL]
}