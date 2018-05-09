import backend.PathConstraint
import backend.expression.ConcolicExpression

import concolic.SymbolicEnvironment
import ConcreteConcreteLattice.{L => ConcreteValue}

trait ConcolicControl extends Control[SchemeExp, ConcreteValue, HybridAddress.A] {
  override def subsumes(that: Control[SchemeExp, ConcreteValue, HybridAddress.A]): Boolean = false
}

case class ConcolicControlEval(exp: SchemeExp, env: Environment[HybridAddress.A], symEnv: SymbolicEnvironment) extends ConcolicControl
case class ConcolicControlKont(v: ConcreteValue, concolicValue: Option[ConcolicExpression]) extends ConcolicControl
case class ConcolicControlError(error: SemanticError) extends ConcolicControl

case class ConcolicMachineState(control: ConcolicControl, store: Store[HybridAddress.A, ConcreteValue], kstore: KontStore[KontAddr], a: KontAddr, t: HybridTimestamp.T)
  extends ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T]
    with StateTrait[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T] {

  def isErrorState: Boolean = control match {
    case _: ConcolicControlError => true
    case _ => false
  }
  def isUserErrorState: Boolean = control match {
    case ConcolicControlError(UserError(_, _)) => true
    case _ => false
  }

  def addressesReachable: Set[HybridAddress.A] = {
    store.toSet.map(_._1)
  }

  def halted: Boolean = control match {
    case ConcolicControlError(_) => true
    case ConcolicControlKont(_, _) => a == HaltKontAddress
    case _ => false
  }

  def graphNodeColor: Color = control match {
    case ConcolicControlEval(_, _, _) => Colors.Green
    case ConcolicControlKont(_, _) => Colors.Pink
    case ConcolicControlError(_) => Colors.Red
  }

  private def convertValue[AbstL: IsConvertableLattice](abstPrims: SchemePrimitives[HybridAddress.A, AbstL])
    (value: ConcreteValue, makeValuePrecise: Boolean): AbstL = {
    val addrConverter = new DefaultHybridAddressConverter[SchemeExp]
    ConcreteConcreteLattice.convert[SchemeExp, AbstL, HybridAddress.A](value, addrConverter, convertEnv, abstPrims, makeValuePrecise)
  }

  /**
    * Converts all addresses in the environment.
    * @param env The environment for which all addresses must be converted.
    * @return A new environment with all addresses converted.
    */
  private def convertEnv(env: Environment[HybridAddress.A]): Environment[HybridAddress.A] = {
    val addressConverter = new DefaultHybridAddressConverter[SchemeExp]()
    env.map { (address) =>
      addressConverter.convertAddress(address)
    }
  }

  private def convertStoreWithExactSymVariables[AbstL: IsConvertableLattice](
    store: Store[HybridAddress.A, ConcreteValue],
    convertValue: (ConcreteValue, Boolean) => AbstL,
    preciseVariablesAddresses: Set[HybridAddress.A]): Store[HybridAddress.A, AbstL] = {

    var valuesConvertedStore = Store.empty[HybridAddress.A, AbstL]
    def addToNewStore(tuple: (HybridAddress.A, ConcreteValue)): Boolean = {
      val (address: HybridAddress.A, value: ConcreteValue) = tuple
      val convertedAddress = new DefaultHybridAddressConverter[SchemeExp]().convertAddress(address)
      val convertedValue = convertValue(value, preciseVariablesAddresses.contains(address))
      valuesConvertedStore = valuesConvertedStore.extend(convertedAddress, convertedValue)
      true
    }
    store.forall(addToNewStore)
    valuesConvertedStore
  }

  private def garbageCollectKStore(kontStore: KontStore[KontAddr], initialAddress: KontAddr): KontStore[KontAddr] = {
    @scala.annotation.tailrec
    def loop(address: KontAddr, gcedKStore: KontStore[KontAddr]): KontStore[KontAddr] = {
      if (address == HaltKontAddress) {
        gcedKStore
      } else {
        val konts = kontStore.lookup(address)
        assert(konts.size == 1) // Concolic machine uses concrete execution, so there should only be one Kont
        val next = konts.head.next
        val frame = konts.head.frame
        val extendedGcedKStore = gcedKStore.extend(address, Kont(frame, next))
        loop(next, extendedGcedKStore)
      }
    }
    loop(initialAddress, TimestampedKontStore(Map(), 0))
  }

  private def convertKStore[AbstL: IsConvertableLattice](
    kontStore: KontStore[KontAddr], initialAddress: KontAddr, convertValue: ConcreteValue => AbstL,
    abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T]): KontStore[KontAddr] = {
    @scala.annotation.tailrec
    def loop(address: KontAddr, convertedKStore: KontStore[KontAddr]): KontStore[KontAddr] = {
      if (address == HaltKontAddress) {
        convertedKStore
      } else {
        val convertedAddress = convertKontAddr(address)
        val konts = kontStore.lookup(address)
        assert(konts.size == 1) // Concolic machine uses concrete execution, so there should only be one Kont
        val next = konts.head.next
        val convertedNext = convertKontAddr(next)
        val frame = konts.head.frame
        val castedFrame = frame.asInstanceOf[ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]]
        val convertedFrame = castedFrame.convert[AbstL](convertValue, convertEnv, abstSem)
        val newConvertedKStore = convertedKStore.extend(convertedAddress, Kont(convertedFrame, convertedNext))
        loop(next, newConvertedKStore)
      }
    }
    loop(initialAddress, TimestampedKontStore(Map(), 0))
  }

  private var reached = Set[HybridAddress.A]()

  private def reachesValue(sto: Store[HybridAddress.A, ConcreteValue], pathConstraint: PathConstraint)
    (value: ConcreteValue): Reached[HybridAddress.A] =
    ConcreteConcreteLattice.latticeInfoProvider.reaches[HybridAddress.A](value, reachesEnvironment(sto, pathConstraint), reachesAddress(sto, pathConstraint))

  private def reachesAddress(sto: Store[HybridAddress.A, ConcreteValue], pathConstraint: PathConstraint)
    (address: HybridAddress.A): Reached[HybridAddress.A] = {
    if (! reached.contains(address)) {
      reached = reached + address
      val lookedUp = sto.lookup(address).map(reachesValue(sto, pathConstraint)).getOrElse(Reached.empty[HybridAddress.A])
      Reached(Set(address) ++ lookedUp.addressesReachable, lookedUp.preciseAddresses)
    } else {
      Reached.empty[HybridAddress.A]
    }
  }

  private def reachesEnvironment(sto: Store[HybridAddress.A, ConcreteValue], pathConstraint: PathConstraint)
    (env: Environment[HybridAddress.A], symEnv: SymbolicEnvironment): Reached[HybridAddress.A] = {

    val exactSymVariables = ExactSymbolicVariablesFinder.findExactSymbolicVariables(env, symEnv, pathConstraint)
    Logger.log(s"exactSymVariables are $exactSymVariables", Logger.I)
    def shouldMakeValPrecise(variable: String): Boolean = {
      /*
       * If a variable is not part of the symbolic environment, the variable's value was not computed via
       * operations supported by the concolic tester (e.g., because the value is a string).
       * We therefore *assume* [TODO This is not actually correct: counterexample:
       * (let ((a 0) (i (random))) (if (< i 0) (set! a 1) (set! a 2)))
       *    -> a is exact but should not be made precise? Although, given the path constraint, a is of course constant]
       * for the moment that this value is independent of the value of the input variables,
       * and can therefore be precisely abstracted.
       *
       *
       */
      exactSymVariables.contains(variable) || ! concolic.containsVariable(variable, symEnv)
    }

    var preciseAddresses = exactSymVariables.map(env.lookup(_).get)

    var reached: Set[HybridAddress.A] = Set()
    env.forall((tuple) => {
      val reachedResult = reachesAddress(sto, pathConstraint)(tuple._2)
      reached = (reached + tuple._2) ++ reachedResult.addressesReachable
      preciseAddresses ++= reachedResult.preciseAddresses
      true
    })
    Reached(reached, preciseAddresses)
  }

  private def reachesKontAddr[KAddr <: KontAddr](sto: Store[HybridAddress.A, ConcreteValue], kstore: KontStore[KAddr],
    ka: KAddr, pathConstraint: PathConstraint): Reached[HybridAddress.A] = {
    kstore.lookup(ka).foldLeft[Reached[HybridAddress.A]](Reached.empty[HybridAddress.A])((acc, kont) => {
      val castedFrame = kont.frame.asInstanceOf[ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]]
      acc ++ castedFrame.reaches(reachesValue(sto, pathConstraint), reachesEnvironment(sto, pathConstraint), reachesAddress(sto, pathConstraint)) ++ reachesKontAddr(sto, kstore, kont.next, pathConstraint)
    })
  }

  private def reachesControl[KAddr <: KontAddr](sto: Store[HybridAddress.A, ConcreteValue],
    kstore: KontStore[KAddr], control: ConcolicControl,
    pathConstraint: PathConstraint): Reached[HybridAddress.A] =
    control match {
      case ConcolicControlEval(_, env, symEnv) =>
        reachesEnvironment(sto, pathConstraint)(env, symEnv)
      case ConcolicControlKont(value, _) =>
        reachesValue(sto, pathConstraint)(value)
      case ConcolicControlError(_) => Reached.empty[HybridAddress.A]
    }

  private def reachesStoreAddresses[KAddr <: KontAddr](sto: Store[HybridAddress.A, ConcreteValue],
    control: ConcolicControl, kstore: KontStore[KAddr],
    ka: KAddr, pathConstraint: PathConstraint): Reached[HybridAddress.A] = {
    reachesControl[KAddr](sto, kstore, control, pathConstraint) ++ reachesKontAddr[KAddr](sto, kstore, ka, pathConstraint)
  }

  private def garbageCollectStore[KAddr <: KontAddr]
  (store: Store[HybridAddress.A, ConcreteValue], control: ConcolicControl, kstore: KontStore[KAddr],
    ka: KAddr, pathConstraint: PathConstraint): (Store[HybridAddress.A, ConcreteValue], Set[HybridAddress.A]) = {
    reached = Set()
    val result: Reached[HybridAddress.A] = reachesStoreAddresses[KAddr](store, control, kstore, ka, pathConstraint)
    val gcedStore = store.gc(result.addressesReachable)
    (gcedStore, result.preciseAddresses)
  }

  private def convertKontAddr(ka: KontAddr): KontAddr = {
    val kontAddrConverter = new DefaultKontAddrConverter[SchemeExp]
    kontAddrConverter.convertKontAddr(ka)
  }

  private def converstateWithExactSymVariables[AbstL: IsConvertableLattice](
    abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
    pathConstraint: PathConstraint): Conversion[AbstL] = {

    val convertValueFun = convertValue[AbstL](abstSem.primitives) _

    val convertedControl: ConvertedControl[SchemeExp, AbstL, HybridAddress.A] = control match {
      case ConcolicControlEval(exp, env, _) =>
        ConvertedControlEval[SchemeExp, AbstL, HybridAddress.A](exp, convertEnv(env))
      case ConcolicControlKont(v, _) =>
        ConvertedControlKont[SchemeExp, AbstL, HybridAddress.A](convertValueFun(v, false))
    }

    val (gcedStore, preciseVariablesAddresses) = garbageCollectStore(store, control, kstore, a, pathConstraint)
    val convertedStore = convertStoreWithExactSymVariables(gcedStore, convertValueFun, preciseVariablesAddresses)

    val convertedKStore = convertKStore[AbstL](kstore, a, convertValueFun(_, false), abstSem)
    val convertedA = convertKontAddr(a)
    val newT = DefaultHybridTimestampConverter.convertTimestamp(t)
    (convertedControl, convertedStore, convertedKStore, convertedA, newT)
  }

  def convertState[AbstL: IsConvertableLattice](
    abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
    pathConstraint: PathConstraint): Conversion[AbstL] = {
    converstateWithExactSymVariables(abstSem, pathConstraint)
  }

  def garbageCollectState: ConcolicMachineState = {
    val (gcedStore, _) = garbageCollectStore(store, control, kstore, a, Nil)
    val gcedKStore = garbageCollectKStore(kstore, a)
    ConcolicMachineState(control, gcedStore, gcedKStore, a, t)
  }

  def compareStatesModuloConcreteAddressesId(state1: ConcolicMachineState, state2: ConcolicMachineState): Boolean = {
    ???
  }
}

object ConcolicMachineState {
  def inject(exp: SchemeExp, env: Environment[HybridAddress.A], sto: Store[HybridAddress.A, ConcreteValue]): ConcolicMachineState = {
    ConcolicMachineState(ConcolicControlEval(exp, env, concolic.initialSymEnv), sto, TimestampedKontStore[KontAddr](Map(), 0), HaltKontAddress, Timestamp[HybridTimestamp.T].initial(""))
  }
}