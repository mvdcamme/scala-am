import scala.annotation.tailrec
import scala.collection.immutable.Stack

trait ActionReturn[Exp, Abs, Addr, Time, +State] {
  def getState: State =
    throw new Exception(
      s"Unexpected result: $this does not have a resulting state")
}
case class ActionStep[Exp: Expression,
                      Addr: Address,
                      Time: Timestamp,
                      State <: ConcreteTracingProgramState[Exp, Addr, Time]](
    newState: State,
    action: ActionT[Exp, ConcreteConcreteLattice.L, Addr])
    extends ActionReturn[Exp, ConcreteConcreteLattice.L, Addr, Time, State] {
  override def getState: State = newState
}
case class GuardFailed[Exp: Expression,
                       Abs: JoinLattice,
                       Addr: Address,
                       Time: Timestamp,
                       State](rp: RestartPoint[Exp, Abs, Addr],
                              guardID: Integer)
    extends ActionReturn[Exp, Abs, Addr, Time, State]
case class TraceEnded[Exp: Expression, Abs: JoinLattice, Addr: Address,
Time: Timestamp, State](rp: RestartPoint[Exp, Abs, Addr])
    extends ActionReturn[Exp, Abs, Addr, Time, State]

case class IncorrectStackSizeException() extends Exception
case class VariableNotFoundException(variable: String)
    extends Exception(variable)
case class NotAPrimitiveException(message: String) extends Exception(message)

trait ConcretableTracingProgramState[Exp] {

  def concretableState: ProgramState[Exp]

  /**
    * Checks if the current state is a final state. It is the case if it
    * reached the end of the computation, or an error
    */
  def halted: Boolean = concretableState.control match {
    case TracingControlEval(_) => false
    case TracingControlKont(a) => a == HaltKontAddress
    case TracingControlError(_) => true
  }

  /**
    * Returns the set of final values that can be reached
    */
  def finalValues = concretableState.control match {
    case TracingControlKont(_) =>
      Set[ConcreteConcreteLattice.L](concretableState.v)
    case _ => Set[ConcreteConcreteLattice.L]()
  }

  def graphNodeColor = concretableState.control match {
    case TracingControlEval(_) => Colors.Green
    case TracingControlKont(_) => Colors.Pink
    case TracingControlError(_) => Colors.Red
  }

  def concreteSubsumes(that: ConcretableTracingProgramState[Exp]): Boolean =
    concretableState.control.subsumes(that.concretableState.control) &&
      concretableState.ρ.subsumes(that.concretableState.ρ) &&
      concretableState.σ.subsumes(that.concretableState.σ) &&
      concretableState.a == that.concretableState.a &&
      concretableState.kstore.subsumes(that.concretableState.kstore) &&
      concretableState.t == that.concretableState.t

  def control: TracingControl[Exp, ConcreteConcreteLattice.L, HybridAddress.A] =
    concretableState.control
  def ρ: Environment[HybridAddress.A] = concretableState.ρ
  def σ: Store[HybridAddress.A, ConcreteConcreteLattice.L] = concretableState.σ
  def kstore: KontStore[KontAddr] = concretableState.kstore
  def a: KontAddr = concretableState.a
  def t: HybridTimestamp.T = concretableState.t
  def v: ConcreteConcreteLattice.L = concretableState.v
  def vStack: List[Storable[ConcreteConcreteLattice.L, HybridAddress.A]] =
    concretableState.vStack

}

trait ConcreteTracingProgramState[Exp, Addr, Time]
    extends TracingProgramState[Exp, ConcreteConcreteLattice.L, Addr, Time] {

  type ConcreteValue = ConcreteConcreteLattice.L

  def control: TracingControl[Exp, ConcreteValue, Addr]
  def ρ: Environment[Addr]
  def σ: Store[Addr, ConcreteValue]
  def kstore: KontStore[KontAddr]
  def a: KontAddr
  def t: Time
  def v: ConcreteValue
  def vStack: List[Storable[ConcreteValue, Addr]]

  def step(sem: SemanticsTraced[Exp, ConcreteValue, Addr, Time])
    : Option[InterpreterStep[Exp, ConcreteValue, Addr]]
  def applyAction(sem: SemanticsTraced[Exp, ConcreteValue, Addr, Time],
                  action: ActionT[Exp, ConcreteValue, Addr])
    : ActionReturn[Exp,
                   ConcreteValue,
                   Addr,
                   Time,
                   ConcreteTracingProgramState[Exp, Addr, Time]]
  def restart(sem: SemanticsTraced[Exp, ConcreteValue, Addr, Time],
              restartPoint: RestartPoint[Exp, ConcreteValue, Addr])
    : ConcreteTracingProgramState[Exp, Addr, Time]

  def runHeader(
      sem: SemanticsTraced[Exp, ConcreteValue, HybridAddress.A, Time],
      assertions: List[ActionT[Exp, ConcreteValue, Addr]])
    : Option[ConcreteTracingProgramState[Exp, Addr, Time]]

  def convertState[AbstL: IsConvertableLattice](
      free: Free[Exp, AbstL, HybridAddress.A, HybridTimestamp.T],
      concSem: SemanticsTraced[Exp,
                               ConcreteValue,
                               HybridAddress.A,
                               HybridTimestamp.T],
      abstSem: BaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T])
    : (ConvertedControl[Exp, AbstL, Addr],
       Environment[Addr],
       Store[Addr, AbstL],
       KontStore[FreeKontAddr],
       FreeKontAddr,
       HybridTimestamp.T)

  def generateTraceInformation(action: ActionT[Exp, ConcreteValue, Addr])
    : CombinedInfos[ConcreteValue, HybridAddress.A]
}

/**
  * A machine state is made of a control component, a value store, a
  * continuation store, and an address representing where the current
  * continuation lives.
  */
case class ProgramState[Exp: Expression](
    override val control: TracingControl[Exp,
                                         ConcreteConcreteLattice.L,
                                         HybridAddress.A],
    override val ρ: Environment[HybridAddress.A],
    override val σ: Store[HybridAddress.A, ConcreteConcreteLattice.L],
    override val kstore: KontStore[KontAddr],
    override val a: KontAddr,
    override val t: HybridTimestamp.T,
    override val v: ConcreteConcreteLattice.L,
    override val vStack: List[
      Storable[ConcreteConcreteLattice.L, HybridAddress.A]])(
    implicit sabs: IsSchemeLattice[ConcreteConcreteLattice.L],
    latInfoProv: LatticeInfoProvider[ConcreteConcreteLattice.L])
    extends ConcreteTracingProgramState[Exp,
                                        HybridAddress.A,
                                        HybridTimestamp.T]
    with ConcretableTracingProgramState[Exp] {

  def abs = implicitly[JoinLattice[ConcreteConcreteLattice.L]]
  def addr = implicitly[Address[HybridAddress.A]]
  def time = implicitly[Timestamp[HybridTimestamp.T]]

  def popStack[A](stack: List[A]): (A, List[A]) = stack match {
    case head :: tail =>
      (head, tail)
    case Nil =>
      throw new IncorrectStackSizeException()
  }

  def popStackItems[A](stack: List[A], n: Integer): (List[A], List[A]) =
    if (n <= stack.length) {
      stack.splitAt(n)
    } else {
      throw new IncorrectStackSizeException()
    }

  /**
    * Computes the set of states that follow the current state
    */
  def step(
      sem: SemanticsTraced[Exp,
                           ConcreteValue,
                           HybridAddress.A,
                           HybridTimestamp.T])
    : Option[InterpreterStep[Exp, ConcreteValue, HybridAddress.A]] = {

    /**
      * Verifies that the set of InterpreterSteps, each of which leads to a successor of the current state, is a
      * singleton, as there should only one possible successor of the current state during concrete execution of
      * a program. Also extracts this InterpreterStep from the set and returns it.
      * @param results The set of InterpreterSteps to be checked. Should be a singleton.
      * @return The (only) InterpreterStep included in the given, singleton set. If the given set was not a singleton,
      *         an AssertionError is thrown.
      */
    def assertResults(
        results: Set[InterpreterStep[Exp, ConcreteValue, HybridAddress.A]])
      : InterpreterStep[Exp, ConcreteValue, HybridAddress.A] = {
      assert(results.size == 1)
      results.head
    }

    val result = control match {
      /* In a eval state, call the semantic's evaluation method */
      case TracingControlEval(e) =>
        Some(assertResults(sem.stepEval(e, ρ, σ, t)))
      /* In a continuation state, call the semantic's continuation method */
      case TracingControlKont(ka) =>
        val konts = kstore.lookup(ka)
        /* During concrete interpretation, a continuation address should always point to just one frame. */
        assert(konts.size == 1)
        val kont = konts.head
        val results = sem.stepKont(v, kont.frame, σ, t)
        assert(results.size == 1)
        val result = results.head
        val removeKontAction =
          ActionRemoveKontT[Exp, ConcreteValue, HybridAddress.A](ka, kont)
        Some(result.copy(trace = removeKontAction :: result.trace))
      /* In an error state, the state is not able to make a step */
      case TracingControlError(_) => None
    }
    result match {
      case Some(result) =>
        Some(result)
      case None => None
    }
  }

  def restart(sem: SemanticsTraced[Exp,
                                   ConcreteValue,
                                   HybridAddress.A,
                                   HybridTimestamp.T],
              restartPoint: RestartPoint[Exp, ConcreteValue, HybridAddress.A])
    : ProgramState[Exp] = restartPoint match {
    case RestartFromControl(newControlExp) =>
      val newT = time.tick(t)
      ProgramState(
        TracingControlEval[Exp, ConcreteValue, HybridAddress.A](newControlExp),
        ρ,
        σ,
        kstore,
        a,
        newT,
        v,
        vStack)
    case RestartGuardDifferentClosure(action) =>
      handleClosureRestart(sem, action)
    case RestartTraceEnded() => this
    case RestartSpecializedPrimitive(originalPrim, n, fExp, argsExps) =>
      val primAppliedState = applyPrimitive(originalPrim, n, fExp, argsExps)
      primAppliedState.applyAction(sem, ActionPopKontT()).getState
  }

  def runHeader(sem: SemanticsTraced[Exp,
                                     ConcreteValue,
                                     HybridAddress.A,
                                     HybridTimestamp.T],
                assertions: List[ActionT[Exp, ConcreteValue, HybridAddress.A]])
    : Option[ProgramState[Exp]] = {
    assertions.foldLeft(Some(this): Option[ProgramState[Exp]])({
      (someProgramState, action) =>
        someProgramState.fold(None: Option[ProgramState[Exp]])(programState =>
          programState.applyAction(sem, action) match {
            case ActionStep(newState, _) => Some(newState)
            case GuardFailed(_, _) => None
            case TraceEnded(_) =>
              /* Should not happen */
              None
        })
    })
  }

  def doActionStepInTraced(
      sem: SemanticsTraced[Exp,
                           ConcreteValue,
                           HybridAddress.A,
                           HybridTimestamp.T],
      action: ActionT[Exp, ConcreteValue, HybridAddress.A])
    : ProgramState[Exp] = action match {
    case ActionStepInT(fexp, bodyHead, _, argsv, n, frame, _, _) =>
      val (vals, poppedVStack) = popStackItems(vStack, n)
      val clo = vals.last.getVal
      val updatedEnvAndStores = sem
        .bindClosureArgs(clo, argsv.zip(vals.init.reverse.map(_.getVal)), σ, t)
        .head
      updatedEnvAndStores match {
        case Right((ρ2, σ2, e)) =>
          val next = NormalKontAddress(e, t) // Hack to get infinite number of addresses in concrete mode
          val newVStack = //machine.StoreEnv[HybridValue, HybridAddress.A](ρ2) ::
            StoreEnv[ConcreteValue, HybridAddress.A](ρ) ::
              poppedVStack
          val newT = time.tick(t, fexp)
          ProgramState[Exp](
            TracingControlEval[Exp, ConcreteValue, HybridAddress.A](e),
            ρ2,
            σ2,
            kstore.extend(next, Kont(frame, a)),
            next,
            newT,
            v,
            newVStack)
        case Left(expectedNrOfArgs) =>
          val newT = time.tick(t)
          ProgramState[Exp](
            TracingControlError(
              ArityError(fexp.toString, expectedNrOfArgs, n - 1)),
            ρ,
            σ,
            kstore,
            a,
            newT,
            v,
            poppedVStack)
      }
  }

  def handleClosureRestart(
      sem: SemanticsTraced[Exp,
                           ConcreteValue,
                           HybridAddress.A,
                           HybridTimestamp.T],
      action: ActionT[Exp, ConcreteValue, HybridAddress.A])
    : ProgramState[Exp] =
    doActionStepInTraced(sem, action)

  def applyPrimitive(primitive: Primitive[HybridAddress.A, ConcreteValue],
                     n: Integer,
                     fExp: Exp,
                     argsExps: List[Exp]): ProgramState[Exp] = {
    val (vals, newVStack) = popStackItems(vStack, n)
    val operands: List[ConcreteValue] = vals.take(n - 1).map(_.getVal)
    val result = primitive.call(fExp, argsExps.zip(operands.reverse), σ, t)
    result.value match {
      case Some((res, σ2, effects)) =>
        val newT = time.tick(t)
        ProgramState(control, ρ, σ2, kstore, a, newT, res, newVStack)
      case None =>
        throw new Exception(result.errors.head.toString)
    }
  }

  def restoreEnv(): ProgramState[Exp] = {
    try {
      val (newρ, newVStack) = popStack(vStack)
      val newT = time.tick(t)
      ProgramState(control, newρ.getEnv, σ, kstore, a, newT, v, newVStack)
    } catch {
      case e: java.lang.IndexOutOfBoundsException =>
        throw new IncorrectStackSizeException()
    }
  }

  protected def saveEnv(): ProgramState[Exp] = {
    val newT = time.tick(t)
    ProgramState(control,
                 ρ,
                 σ,
                 kstore,
                 a,
                 newT,
                 v,
                 StoreEnv[ConcreteValue, HybridAddress.A](ρ) :: vStack)
  }

  def applyAction(sem: SemanticsTraced[Exp,
                                       ConcreteValue,
                                       HybridAddress.A,
                                       HybridTimestamp.T],
                  action: ActionT[Exp, ConcreteValue, HybridAddress.A])
    : ActionReturn[Exp,
                   ConcreteValue,
                   HybridAddress.A,
                   HybridTimestamp.T,
                   ProgramState[Exp]] = {

    ActionLogger.logAction[Exp, ConcreteValue, HybridAddress.A](action)

    val newT = time.tick(t)

    def handleGuard(guard: ActionGuardT[Exp, ConcreteValue, HybridAddress.A],
                    guardCheckFunction: ConcreteValue => Boolean)
      : ActionReturn[Exp,
                     ConcreteValue,
                     HybridAddress.A,
                     HybridTimestamp.T,
                     ProgramState[Exp]] = {
      if (guardCheckFunction(v)) {
        ActionStep(this, guard)
      } else {
        GuardFailed(guard.rp, guard.id)
      }
    }

    def handleClosureGuard(
        guard: ActionGuardSameClosure[Exp, ConcreteValue, HybridAddress.A],
        currentClosure: ConcreteValue): ActionReturn[Exp,
                                                     ConcreteValue,
                                                     HybridAddress.A,
                                                     HybridTimestamp.T,
                                                     ProgramState[Exp]] = {
      (guard.recordedClosure, currentClosure) match {
        case (ConcreteConcreteLattice.lattice
                .Element(ConcreteConcreteLattice.lattice.Closure(lam1, env1)),
              ConcreteConcreteLattice.lattice.Element(
              ConcreteConcreteLattice.lattice.Closure(lam2, env2))) =>
          if (lam1 == lam2) {
            ActionStep(this, guard)
          } else {
            Logger.log(
              s"Closure guard failed: recorded closure $lam1 does not match current closure $lam2",
              Logger.D)
            GuardFailed(guard.rp, guard.id)
          }
        case _ =>
          throw new Exception(
            s"Mixing concrete values with abstract values: ${guard.recordedClosure} and $currentClosure")
      }
    }

    action match {
      case ActionAllocVarsT(variables) =>
        val addresses = variables.map(varName => addr.variable(varName, v, t))
        val (ρ1, σ1) = variables
          .zip(addresses)
          .foldLeft((ρ, σ))({
            case ((ρ2, σ2), (currV, currA)) =>
              (ρ2.extend(currV, currA), σ2.extend(currA, abs.bottom))
          })
        ActionStep(ProgramState(control, ρ1, σ1, kstore, a, newT, v, vStack),
                   action)
      case ActionCreateClosureT(λ) =>
        val newClosure = sabs.inject[Exp, HybridAddress.A]((λ, ρ))
        ActionStep(
          ProgramState(control, ρ, σ, kstore, a, newT, newClosure, vStack),
          action)
      case ActionEndClosureCallT() =>
        ActionStep(this, action)
      case ActionEndPrimCallT() =>
        ActionStep(this, action)
      /* When an error is reached, we go to an error state */
      case ActionErrorT(err) =>
        ActionStep(ProgramState(TracingControlError(err),
                                ρ,
                                σ,
                                kstore,
                                a,
                                newT,
                                v,
                                vStack),
                   action)
      /* When a value needs to be evaluated, we go to an eval state */
      case ActionEvalT(e, _, _) =>
        ActionStep(ProgramState(TracingControlEval(e),
                                ρ,
                                σ,
                                kstore,
                                a,
                                newT,
                                v,
                                vStack),
                   action)
      /* When a continuation needs to be pushed, push it in the continuation store */
      case ActionEvalPushT(e, frame, _, _) =>
        val next = NormalKontAddress(e, t) // Hack to get infinite number of addresses in concrete mode
        ActionStep(ProgramState(TracingControlEval(e),
                                ρ,
                                σ,
                                kstore.extend(next, Kont(frame, a)),
                                next,
                                newT,
                                v,
                                vStack),
                   action)
      case ActionExtendEnvT(varName) =>
        val value = vStack.head.getVal
        val va = addr.variable(varName, value, t)
        val ρ1 = ρ.extend(varName, va)
        val σ1 = σ.extend(va, value)
        val newVStack = vStack.tail
        ActionStep(
          ProgramState(control, ρ1, σ1, kstore, a, newT, v, newVStack),
          action)
      case ActionExtendStoreT(addr, lit) =>
        val σ1 = σ.extend(addr, lit)
        ActionStep(ProgramState(control, ρ, σ1, kstore, a, newT, lit, vStack),
                   action)
      case ActionLookupVariableT(varName, _, _) =>
        val newV = σ.lookup(ρ.lookup(varName).get).get
        ActionStep(ProgramState(control, ρ, σ, kstore, a, newT, newV, vStack),
                   action)
      case ActionLookupVariablePushT(varName, _, _) =>
        ρ.lookup(varName) match {
          case Some(address) =>
            σ.lookup(address) match {
              case Some(value) =>
                ActionStep(
                  ProgramState(
                    control,
                    ρ,
                    σ,
                    kstore,
                    a,
                    newT,
                    value,
                    StoreVal[ConcreteValue, HybridAddress.A](value) :: vStack),
                  action)
              case None =>
                throw new Exception(
                  s"Could not find address $address in store")
            }
          case None =>
            throw new Exception(
              s"Could not find variable $varName in environment")
        }
      case ActionPopKontT() =>
        val next = if (a == HaltKontAddress) { HaltKontAddress } else {
          val kset = kstore.lookup(a)
          assert(kset.size == 1)
          kset.head.next
        }
        ActionStep(ProgramState(TracingControlKont(a),
                                ρ,
                                σ,
                                kstore,
                                next,
                                newT,
                                v,
                                vStack),
                   action)
      case ActionPrimCallT(n: Integer, fExp, argsExps) =>
        val (vals, _) = popStackItems(vStack, n)
        val operator = vals.last.getVal
        val primitivesSet =
          sabs.getPrimitives[HybridAddress.A, ConcreteValue](operator)
        assert(primitivesSet.size == 1)
        ActionStep(applyPrimitive(primitivesSet.head, n, fExp, argsExps),
                   action)
      case ActionPushValT() =>
        ActionStep(
          ProgramState(control,
                       ρ,
                       σ,
                       kstore,
                       a,
                       newT,
                       v,
                       StoreVal[ConcreteValue, HybridAddress.A](v) :: vStack),
          action)
      case ActionReachedValueT(lit, _, _) =>
        ActionStep(ProgramState(control, ρ, σ, kstore, a, newT, lit, vStack),
                   action)
      case ActionReachedValuePushT(lit, _, _) =>
        ActionStep(ProgramState(
                     control,
                     ρ,
                     σ,
                     kstore,
                     a,
                     newT,
                     lit,
                     StoreVal[ConcreteValue, HybridAddress.A](lit) :: vStack),
                   action)
      case ActionRemoveKontT(a, k) =>
        ActionStep(copy(kstore = kstore.remove(a, k)), action)
      case ActionRestoreEnvT() =>
        ActionStep(restoreEnv(), action)
      case ActionRestoreSaveEnvT() =>
        ActionStep(restoreEnv().saveEnv(), action)
      case ActionSaveEnvT() =>
        ActionStep(saveEnv(), action)
      case ActionSetVarT(variable) =>
        ρ.lookup(variable) match {
          case Some(address) =>
            ActionStep(ProgramState(control,
                                    ρ,
                                    σ.update(address, v),
                                    kstore,
                                    a,
                                    newT,
                                    v,
                                    vStack),
                       action)
          case None =>
            throw new VariableNotFoundException(variable)
        }
      case ActionGuardSpecializedPrimitive(expectedType, n, rp, guardID) =>
        val operands = popStackItems(vStack, n - 1)._1.map(_.getVal)
        val currentOperandsTypes = latInfoProv.simpleTypes(operands)
        if (currentOperandsTypes == expectedType) {
          ActionStep(this, action)
        } else {
          GuardFailed(rp, guardID)
        }
      case ActionSpecializePrimitive(expectedType, prim, n, fExp, argsExps) =>
        ActionStep(applyPrimitive(prim, n, fExp, argsExps), action)
      case ActionStartFunCallT() =>
        ActionStep(this, action)
      /* When a function is stepped in, we also go to an eval state */
      case ActionStepInT(fexp, e, args, argsv, n, frame, _, _) =>
        ActionStep(doActionStepInTraced(sem, action), action)
      case ActionPutRegister(varName, registerIndex) =>
        val value = σ.lookup(ρ.lookup(varName).get).get
        RegisterStore.setRegister(registerIndex, value)
        ActionStep(this, action)
      case ActionLookupRegister(registerIndex) =>
        val value = RegisterStore.getRegister(registerIndex)
        ActionStep(ProgramState(control, ρ, σ, kstore, a, newT, value, vStack),
                   action)
      case ActionLookupRegisterPush(registerIndex) =>
        val value = RegisterStore.getRegister(registerIndex)
        ActionStep(
          ProgramState(
            control,
            ρ,
            σ,
            kstore,
            a,
            newT,
            value,
            StoreVal[ConcreteValue, HybridAddress.A](value) :: vStack),
          action)
      case action: ActionEndTrace[Exp, ConcreteValue, HybridAddress.A] =>
        TraceEnded(action.restartPoint)
      case action: ActionGuardFalseT[Exp, ConcreteValue, HybridAddress.A] =>
        handleGuard(action, sabs.isFalse)
      case action: ActionGuardTrueT[Exp, ConcreteValue, HybridAddress.A] =>
        handleGuard(action, sabs.isTrue)
      case action: ActionGuardSameClosure[Exp,
                                          ConcreteValue,
                                          HybridAddress.A] =>
        val n = action.rp.action.n
        try {
          handleClosureGuard(action, vStack(n - 1).getVal)
        } catch {
          case e: java.lang.IndexOutOfBoundsException =>
            throw new IncorrectStackSizeException
        }
    }

  }

  /**
    * Builds the state with the initial environment and stores
    */
  def this(sem: SemanticsTraced[Exp,
                                ConcreteConcreteLattice.L,
                                HybridAddress.A,
                                HybridTimestamp.T],
           exp: Exp)(
      implicit sabs: IsSchemeLattice[ConcreteConcreteLattice.L],
      latInfoProv: LatticeInfoProvider[ConcreteConcreteLattice.L]) =
    this(TracingControlEval(exp),
         Environment.initial[HybridAddress.A](sem.initialEnv),
         Store.initial[HybridAddress.A, ConcreteConcreteLattice.L](
           sem.initialStore),
         KontStore.empty[KontAddr],
         HaltKontAddress,
         HybridTimestamp.isTimestamp.initial(""),
         sabs.inject(false),
         Nil)

  override def toString = control match {
    case TracingControlKont(_) => s"ko($v)"
    case _ => control.toString()
  }

  private def convertValue[AbstL: IsConvertableLattice](
      concPrims: Primitives[HybridAddress.A, ConcreteValue],
      abstPrims: SchemePrimitives[HybridAddress.A, AbstL]
  )(value: ConcreteValue): AbstL =
    ConcreteConcreteLattice.convert[Exp, AbstL, HybridAddress.A](
      value,
      new DefaultHybridAddressConverter[Exp],
      convertEnv,
      concPrims,
      abstPrims)

  /**
    * Converts all addresses in the environment.
    * @param env The environment for which all addresses must be converted.
    * @return A new environment with all addresses converted.
    */
  private def convertEnv(
      env: Environment[HybridAddress.A]): Environment[HybridAddress.A] = {
    val addressConverter = new DefaultHybridAddressConverter()
    env.map { (address) => addressConverter.convertAddress(address) }
  }

  /**
    * Converts all addresses in the store.
    * @param σ The store for which all addresses must be converted.
    * @return A new store with all addresses converted.
    */
  /* Currently, we don't actually convert addresses in the environment or store, so at the moment,
   * this function is just the identity function. */
  private def convertSto[AbstL: IsConvertableLattice](
      σ: Store[HybridAddress.A, AbstL]): Store[HybridAddress.A, AbstL] = σ

  /**
    * Maps a KontAddr to a FreeKontAddr.
    * @param address The KontAddr to be converted.
    * @param env The environment to used to allocate the FreeKontAddr.
    * @return The converted FreeKontAddr.
    */
  private def mapKontAddress(address: KontAddr,
                             env: Environment[HybridAddress.A]): FreeKontAddr =
    address match {
      case address: NormalKontAddress[Exp, HybridTimestamp.T] =>
        FreeNormalKontAddress(address.exp, env)
      case HaltKontAddress => FreeHaltKontAddress
    }

  private def convertKStoreToFreeFrames(
      concSem: SemanticsTraced[Exp,
                               ConcreteValue,
                               HybridAddress.A,
                               HybridTimestamp.T],
      abstSem: BaseSchemeSemantics[ConcreteValue,
                                   HybridAddress.A,
                                   HybridTimestamp.T],
      kontStore: KontStore[KontAddr],
      ρ: Environment[HybridAddress.A],
      a: KontAddr,
      vStack: List[Storable[ConcreteValue, HybridAddress.A]])
    : (FreeKontAddr, KontStore[FreeKontAddr]) = {

    @tailrec
    def loop(a: KontAddr,
             vStack: List[Storable[ConcreteValue, HybridAddress.A]],
             ρ: Environment[HybridAddress.A],
             stack: List[(KontAddr,
                          Option[Frame],
                          List[Storable[ConcreteValue, HybridAddress.A]],
                          Environment[HybridAddress.A])])
      : (FreeKontAddr, KontStore[FreeKontAddr]) = a match {
      case HaltKontAddress =>
        stack.foldLeft[(FreeKontAddr, KontStore[FreeKontAddr])](
          (FreeHaltKontAddress, KontStore.empty[FreeKontAddr]))({
          case ((actualNext, extendedKontStore),
                (a, someNewSemFrame, newVStack, newρ)) =>
            someNewSemFrame match {
              case Some(newSemFrame) =>
                val convertedA = mapKontAddress(a, newρ)
                (convertedA,
                 extendedKontStore.extend(convertedA,
                                          Kont(newSemFrame, actualNext)))
              case None =>
                (actualNext, extendedKontStore)
            }

        })
      case _ =>
        val Kont(frame, next) = kontStore.lookup(a).head
        val (someNewSemFrame, newVStack, newρ) =
          concSem.convertToAbsSemanticsFrame(frame, ρ, vStack, abstSem)
        loop(next,
             newVStack,
             newρ,
             (a, someNewSemFrame, newVStack, newρ) :: stack)
    }
    loop(a, vStack, ρ, Nil)
  }

  private def convertKStoreAbsValuesInFrames[AbstL: IsConvertableLattice](
      kontStore: KontStore[FreeKontAddr],
      convertValue: ConcreteValue => AbstL,
      concBaseSem: BaseSchemeSemantics[ConcreteValue,
                                       HybridAddress.A,
                                       HybridTimestamp.T],
      abstSem: BaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T])
    : KontStore[FreeKontAddr] = {
    kontStore.map(
      (kontAddr) =>
        kontAddr, //TODO used the identity function, should use the DefaultKontAddrConverter
      (frame: Frame) =>
        concBaseSem.convertAbsInFrame[AbstL](
          frame.asInstanceOf[SchemeFrame[ConcreteValue,
                                         HybridAddress.A,
                                         HybridTimestamp.T]],
          convertValue,
          convertEnv,
          abstSem))
  }

  private def convertStore[AbstL: IsConvertableLattice](
      store: Store[HybridAddress.A, ConcreteValue],
      convertValue: ConcreteValue => AbstL): Store[HybridAddress.A, AbstL] = {
    var valuesConvertedStore = Store.empty[HybridAddress.A, AbstL]
    def addToNewStore(tuple: (HybridAddress.A, ConcreteValue)): Boolean = {
      val convertedAddress =
        new DefaultHybridAddressConverter().convertAddress(tuple._1)
      val convertedValue = convertValue(tuple._2)
      valuesConvertedStore =
        valuesConvertedStore.extend(convertedAddress, convertedValue)
      true
    }
    store.forall(addToNewStore)
    valuesConvertedStore
  }

  var reached = Set[HybridAddress.A]()

  private def reachesValue(concBaseSem: BaseSchemeSemantics[ConcreteValue,
                                                            HybridAddress.A,
                                                            HybridTimestamp.T],
                           env: Environment[HybridAddress.A],
                           sto: Store[HybridAddress.A, ConcreteValue])(
      value: ConcreteValue): Set[HybridAddress.A] =
    latInfoProv.reaches[HybridAddress.A](value,
                                         reachesEnvironment(concBaseSem, sto),
                                         reachesAddress(concBaseSem, env, sto))

  private def reachesAddress(
      concBaseSem: BaseSchemeSemantics[ConcreteValue,
                                       HybridAddress.A,
                                       HybridTimestamp.T],
      env: Environment[HybridAddress.A],
      sto: Store[HybridAddress.A, ConcreteValue])(
      address: HybridAddress.A): Set[HybridAddress.A] = {
    if (! reached.contains(address)) {
      reached = reached + address
      Set(address) ++ sto
        .lookup(address)
        .foldLeft[Set[HybridAddress.A]](Set())((_, value) =>
        reachesValue(concBaseSem, env, sto)(value))
    } else {
      Set()
    }
  }

  private def reachesEnvironment(
      concBaseSem: BaseSchemeSemantics[ConcreteValue,
                                       HybridAddress.A,
                                       HybridTimestamp.T],
      sto: Store[HybridAddress.A, ConcreteValue])(
      env: Environment[HybridAddress.A]): Set[HybridAddress.A] = {
    var reached: Set[HybridAddress.A] = Set()
    env.forall((tuple) => {
      reached = (reached + tuple._2) ++ reachesAddress(concBaseSem, env, sto)(
          tuple._2)
      true
    })
    reached
  }

  private def reachesKontAddr(
      concBaseSem: BaseSchemeSemantics[ConcreteValue,
                                       HybridAddress.A,
                                       HybridTimestamp.T],
      env: Environment[HybridAddress.A],
      sto: Store[HybridAddress.A, ConcreteValue],
      kstore: KontStore[FreeKontAddr])(
      ka: FreeKontAddr): Set[HybridAddress.A] = {
    kstore
      .lookup(ka)
      .foldLeft[Set[HybridAddress.A]](Set())(
        (acc, kont) =>
          acc ++ concBaseSem.frameReaches(
            kont.frame.asInstanceOf[SchemeFrame[ConcreteValue,
                                                HybridAddress.A,
                                                HybridTimestamp.T]],
            reachesValue(concBaseSem, env, sto),
            reachesEnvironment(concBaseSem, sto),
            reachesAddress(concBaseSem, env, sto)) ++ reachesKontAddr(concBaseSem, env, sto, kstore)(kont.next))
  }

  private def reachesControl(
      concBaseSem: BaseSchemeSemantics[ConcreteValue,
                                       HybridAddress.A,
                                       HybridTimestamp.T],
      env: Environment[HybridAddress.A],
      sto: Store[HybridAddress.A, ConcreteValue],
      kstore: KontStore[FreeKontAddr])(
      control: ConvertedControl[Exp, ConcreteValue, HybridAddress.A])
    : Set[HybridAddress.A] =
    control match {
      case ConvertedControlEval(_, env) =>
        reachesEnvironment(concBaseSem, sto)(env)
      case ConvertedControlKont(value) =>
        reachesValue(concBaseSem, env, sto)(value)
      case ConvertedControlError(_) => Set()
    }

  private def reachesStoreAddresses(
      concBaseSem: BaseSchemeSemantics[ConcreteValue,
                                       HybridAddress.A,
                                       HybridTimestamp.T],
      sto: Store[HybridAddress.A, ConcreteValue])(
      control: ConvertedControl[Exp, ConcreteValue, HybridAddress.A],
      env: Environment[HybridAddress.A],
      kstore: KontStore[FreeKontAddr],
      value: ConcreteValue,
      ka: FreeKontAddr): Set[HybridAddress.A] = {
    reachesControl(concBaseSem, env, sto, kstore)(control) ++
      reachesEnvironment(concBaseSem, sto)(env) ++ reachesValue(
      concBaseSem,
      env,
      sto)(value) ++ reachesKontAddr(concBaseSem, env, sto, kstore)(ka)
  }

  def convertState[AbstL: IsConvertableLattice](
      free: Free[Exp, AbstL, HybridAddress.A, HybridTimestamp.T],
      concSem: SemanticsTraced[Exp,
                               ConcreteValue,
                               HybridAddress.A,
                               HybridTimestamp.T],
      abstSem: BaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T])
    : (ConvertedControl[Exp, AbstL, HybridAddress.A],
       Environment[HybridAddress.A],
       Store[HybridAddress.A, AbstL],
       KontStore[FreeKontAddr],
       FreeKontAddr,
       HybridTimestamp.T) = {

    val concBaseSem =
      new SchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T](
        new SchemePrimitives[HybridAddress.A, ConcreteValue])

    val convertValueFun =
      convertValue[AbstL](concSem.primitives, abstSem.primitives) _

    val convertedρ = convertEnv(ρ)
    val startKontAddress = control match {
      case TracingControlEval(_) | TracingControlError(_) => a
      case TracingControlKont(ka) => ka
    }
    val (convertedA, mappedKStore) = convertKStoreToFreeFrames(
      concSem,
      concBaseSem,
      kstore,
      convertedρ,
      startKontAddress,
      vStack)
    val convertedKStore = convertKStoreAbsValuesInFrames[AbstL](
      mappedKStore,
      convertValueFun,
      concBaseSem,
      abstSem)

    val mappedControl: ConvertedControl[Exp, ConcreteValue, HybridAddress.A] =
      control match {
        case TracingControlEval(exp) =>
          ConvertedControlEval[Exp, ConcreteValue, HybridAddress.A](exp,
                                                                    convertedρ)
        case TracingControlKont(ka) =>
          ConvertedControlKont[Exp, ConcreteValue, HybridAddress.A](v)
      }

    val convertedControl = mappedControl match {
      case c: ConvertedControlEval[Exp, ConcreteValue, HybridAddress.A] =>
        ConvertedControlEval[Exp, AbstL, HybridAddress.A](c.e, c.ρ)
      case c: ConvertedControlKont[Exp, ConcreteValue, HybridAddress.A] =>
        ConvertedControlKont[Exp, AbstL, HybridAddress.A](convertValueFun(c.v))
    }

    reached = Set[HybridAddress.A]()
    val storeAddressReachable = reachesStoreAddresses(concBaseSem, σ)(
      mappedControl,
      ρ,
      mappedKStore,
      v,
      convertedA)
    val GCedStore = σ.gc(storeAddressReachable)
    Logger.log(s"Size of original store ${σ.toSet.size}; size of gc-ed store: ${GCedStore.toSet.size}", Logger.U)
    val convertedStore = convertStore(GCedStore, convertValueFun)
    val newT = DefaultHybridTimestampConverter.convertTimestamp(t)
    (convertedControl,
     convertedρ,
     convertedStore,
     convertedKStore,
     convertedA,
     newT)
  }

  def generateTraceInformation(
      action: ActionT[Exp, ConcreteValue, HybridAddress.A])
    : CombinedInfos[ConcreteValue, HybridAddress.A] = action match {
    case ActionAllocVarsT(variables) =>
      val addresses = variables.map((variable) => ρ.lookup(variable).get)
      TraceInfos.single(AddressesAllocated(addresses))
    case ActionExtendEnvT(variable) =>
      TraceInfos.single(AddressesAllocated(List(ρ.lookup(variable).get)))
    case ActionLookupVariableT(variable, _, _) =>
      val a = ρ.lookup(variable).get
      TraceInfos.single(VariableLookedUp(variable, a, σ.lookup(a).get))
    case ActionLookupVariablePushT(variable, _, _) =>
      val a = ρ.lookup(variable).get
      TraceInfos.single(VariableLookedUp(variable, a, σ.lookup(a).get))
    case ActionPrimCallT(_, _, _) =>
      TraceInfos.single(PrimitiveAppliedInfo(v, vStack))
    case ActionSetVarT(variable) =>
      TraceInfos.single(AddressesReassigned(List(ρ.lookup(variable).get)))
    case ActionStepInT(_, _, args, _, _, _, _, _) =>
      val addresses = args.map((arg) => ρ.lookup(arg).get)
      TraceInfos.single(AddressesAllocated(addresses))
    case _ =>
      TraceInfos.nil
  }

  def concretableState = this

  def subsumes(
      that: TracingProgramState[Exp,
                                ConcreteValue,
                                HybridAddress.A,
                                HybridTimestamp.T]): Boolean = that match {
    case that: ProgramState[Exp] =>
      concreteSubsumes(that)
    case _ => false
  }
}
