import scala.annotation.tailrec
import scala.collection.immutable.Stack

import ConcreteConcreteLattice.{ L => ConcreteValue }

trait ConvertableProgramState[Exp, Addr, Time] {

  def addressesReachable: Set[Addr]

  def convertState[AbstL: IsConvertableLattice](
      concSem: ConvertableSemantics[Exp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
      abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
      initialKontAddress: KontAddr,
      mapKontAddress: (KontAddr, Option[Environment[HybridAddress.A]]) => KontAddr): (ConvertedControl[Exp, AbstL, Addr], Store[Addr, AbstL], KontStore[KontAddr], KontAddr, HybridTimestamp.T)
}