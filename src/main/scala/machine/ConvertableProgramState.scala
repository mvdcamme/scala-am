import scala.annotation.tailrec
import scala.collection.immutable.Stack

import ConcreteConcreteLattice.ConcreteValue

trait ConvertableProgramState[Exp, Addr, Time] {

  def convertState[AbstL: IsConvertableLattice](
      concSem: ConvertableSemantics[Exp,
                                    ConcreteValue,
                                    HybridAddress.A,
                                    HybridTimestamp.T],
      abstSem: BaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
      initialKontAddress: KontAddr,
      mapKontAddress: (KontAddr, Option[Environment[HybridAddress.A]]) => KontAddr)
    : (ConvertedControl[Exp, AbstL, Addr], Store[Addr, AbstL], KontStore[KontAddr], KontAddr, HybridTimestamp.T)
}