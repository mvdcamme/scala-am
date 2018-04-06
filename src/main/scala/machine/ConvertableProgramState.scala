import ConcreteConcreteLattice.{L => ConcreteValue}
import backend.PathConstraint

trait ConvertableProgramState[Exp, Addr, Time] {

  type Conversion[AbstL] = (ConvertedControl[Exp, AbstL, Addr], Store[Addr, AbstL], KontStore[KontAddr], KontAddr, HybridTimestamp.T)

  def store: Store[Addr, ConcreteValue]

  def addressesReachable: Set[Addr]

  def convertState[AbstL: IsConvertableLattice](
      abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
      pathConstraint: PathConstraint): Conversion[AbstL]
}