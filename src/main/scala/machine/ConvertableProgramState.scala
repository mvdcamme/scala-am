import ConcreteConcreteLattice.{L => ConcreteValue}
import backend.PathConstraint

trait ConvertableProgramState[Exp, Addr, Time] {

  type Conversion[AbstL] = (ConvertedControl[Exp, AbstL, Addr], Store[Addr, AbstL], KontStore[KontAddr], KontAddr, HybridTimestamp.T)

  def store: Store[Addr, ConcreteValue]
  def optEnvs: Option[(Environment[Addr], concolic.SymbolicEnvironment)]

  def addressesReachable: Set[Addr]

  def convertState[AbstL: IsConvertableLattice](
      concSem: ConvertableSemantics[Exp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
      abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
      initialKontAddress: KontAddr, mapKontAddress: (KontAddr, Option[Environment[HybridAddress.A]]) => KontAddr,
      pathConstraint: PathConstraint): Conversion[AbstL]
}