import ConcreteConcreteLattice.{ L => ConcreteValue }

class ShouldCheckAddress[Addr] {
  private var addressesChecked: Set[Addr] = Set()
  def addCheckedAddress(a: Addr): Unit = {
    addressesChecked += a
  }
  def hasCheckedAddress(a: Addr): Boolean = {
    addressesChecked.contains(a)
  }
}

trait ConcolicMachineStateComparer {
  def equalModuloTimestamp(state1: ConcolicMachineState, state2: ConcolicMachineState): Boolean
}

abstract class ConcolicMachineStateComparerBasic extends ConcolicMachineStateComparer {
  protected def isRelevantAddress(address: HybridAddress.A): Boolean

  def equalModuloTimestamp(state1: ConcolicMachineState, state2: ConcolicMachineState): Boolean = {
    type Frame = SchemeConcolicFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]
    if (state1.store.keys.size == state2.store.keys.size) {
      val kstack1: List[SchemeConcolicFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]] = state1.kstoreToContStack
      val kstack2: List[SchemeConcolicFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]] = state2.kstoreToContStack
      val addressChecker = new ShouldCheckAddress[HybridAddress.A]
      val solution = for {
        timeMapping <- implicitly[CompareTimestampsWithMapping[HybridTimestamp.T]].compareWithMapping(state1.t, state2.t, Mapping.initial)
        aMapping <- KontAddr.equalModuloTimestamp(state1.a, state2.a, timeMapping)
        controlMapping <- state1.control.equalModuloTimestamp(state2.control, addressChecker, state1.store, state2.store, aMapping)
        kstackMapping <- kstack1.zip(kstack2).foldLeft[Option[Mapping[HybridTimestamp.T]]](Some(controlMapping))((maybeAcc, tuple) => {
          maybeAcc.flatMap(acc => tuple._1.equalModuloTimestamp(tuple._2, addressChecker, state1.store, state2.store, acc))
        })
      } yield {
        kstackMapping
      }
      solution.isDefined
    } else {
      false
    }
  }
}

object ConcolicMachineStateComparerBasic extends ConcolicMachineStateComparerBasic {
  protected def isRelevantAddress(address: HybridAddress.A): Boolean = true
}

class ConcolicMachineStateComparerWithAnalysis[N](val addrConverter: AddressConverter[HybridAddress.A]) {

  private val readDependenciesComputer = new ComputeFutureReadDependencies[N, HybridAddress.A]

  class DoIt(val initialStateGraph: Graph[N, EdgeAnnotation[_, _, HybridAddress.A], _], val nodes: Set[N]) extends ConcolicMachineStateComparerBasic {

    {
      assert(nodes.nonEmpty)
    }

    protected def isRelevantAddress(address: HybridAddress.A): Boolean = {
      val futureReadDependencies = readDependenciesComputer.computeFutureReadDependencies(nodes, initialStateGraph)
      val convertedAddress = addrConverter.convertAddress(address)
      futureReadDependencies.contains(convertedAddress)
    }
  }
}