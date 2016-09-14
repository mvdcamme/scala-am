import scala.annotation.tailrec
import scala.collection.immutable.Stack

trait ActionReturn[Exp, Abs, Addr, Time, +State] {
  def getState: State =
    throw new Exception(s"Unexpected result: $this does not have a resulting state")
}
case class ActionStep[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp, State <: ConcreteTracingProgramState[Exp, Abs, Addr, Time]]
  (newState: State, action: ActionT[Exp, Abs, Addr])
  extends ActionReturn[Exp, Abs, Addr, Time, State] {
  override def getState: State = newState
}
case class GuardFailed[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp, State]
  (rp: RestartPoint[Exp, Abs, Addr], guardID: Integer)
  extends ActionReturn[Exp, Abs, Addr, Time, State]
case class TraceEnded[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp, State]
  (rp: RestartPoint[Exp, Abs, Addr])
  extends ActionReturn[Exp, Abs, Addr, Time, State]

case class IncorrectStackSizeException() extends Exception
case class VariableNotFoundException(variable: String) extends Exception(variable)
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
    case TracingControlKont(_) => Set[HybridLattice.L](concretableState.v)
    case _ => Set[HybridLattice.L]()
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

  def control: TracingControl[Exp, HybridLattice.L, HybridAddress.A] = concretableState.control
  def ρ: Environment[HybridAddress.A] = concretableState.ρ
  def σ: Store[HybridAddress.A, HybridLattice.L] = concretableState.σ
  def kstore: KontStore[KontAddr] = concretableState.kstore
  def a: KontAddr = concretableState.a
  def t: HybridTimestamp.T = concretableState.t
  def v: HybridLattice.L = concretableState.v
  def vStack: List[Storable[HybridLattice.L, HybridAddress.A]] = concretableState.vStack

}

trait ConcreteTracingProgramState[Exp, Abs, Addr, Time] extends TracingProgramState[Exp, Abs, Addr, Time] {
  type HybridValue = HybridLattice.L

  def control: TracingControl[Exp, Abs, Addr]
  def ρ: Environment[Addr]
  def σ: Store[Addr, Abs]
  def kstore: KontStore[KontAddr]
  def a: KontAddr
  def t: Time
  def v: Abs
  def vStack: List[Storable[Abs, Addr]]

  def step(sem: SemanticsTraced[Exp, Abs, Addr, Time]): Option[(InterpreterStep[Exp, Abs, Addr], KontStore[KontAddr])]
  def applyAction(sem: SemanticsTraced[Exp, Abs, Addr, Time],
                  action: ActionT[Exp, Abs, Addr]):
    ActionReturn[Exp, Abs, Addr, Time, ConcreteTracingProgramState[Exp, Abs, Addr, Time]]
  def restart(sem: SemanticsTraced[Exp, Abs, Addr, Time],
              restartPoint: RestartPoint[Exp, Abs, Addr]): ConcreteTracingProgramState[Exp, Abs, Addr, Time]

  def runHeader(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, Time],
                assertions: List[ActionT[Exp, Abs, Addr]]): Option[ConcreteTracingProgramState[Exp, Abs, Addr, Time]]

  def convertState(aam: AAM[Exp, Abs, Addr, Time])
                  (sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T]):
    (ConvertedControl[Exp, Abs, Addr], Environment[Addr], Store[Addr, Abs], KontStore[KontAddr], KontAddr, HybridTimestamp.T)

  def generateTraceInformation(action: ActionT[Exp, Abs, Addr]): CombinedInfos[HybridValue, HybridAddress.A]

  def setKStore(kontStore: KontStore[KontAddr]): ConcreteTracingProgramState[Exp, Abs, Addr, Time]
}

/**
  * A machine state is made of a control component, a value store, a
  * continuation store, and an address representing where the current
  * continuation lives.
  */
case class ProgramState[Exp : Expression]
  (override val control: TracingControl[Exp, HybridLattice.L, HybridAddress.A],
   override val ρ: Environment[HybridAddress.A],
   override val σ: Store[HybridAddress.A, HybridLattice.L],
   override val kstore: KontStore[KontAddr],
   override val a: KontAddr,
   override val t: HybridTimestamp.T,
   override val v: HybridLattice.L,
   override val vStack: List[Storable[HybridLattice.L, HybridAddress.A]])
  extends ConcreteTracingProgramState[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T]
  with ConcretableTracingProgramState[Exp] {

  def sabs = implicitly[IsSchemeLattice[HybridValue]]
  def abs = implicitly[JoinLattice[HybridLattice.L]]
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
  def step(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T])
          :Option[(InterpreterStep[Exp, HybridValue, HybridAddress.A], KontStore[KontAddr])] = {
    val result = control match {
      /* In a eval state, call the semantic's evaluation method */
      case TracingControlEval(e) => Some((sem.stepEval(e, ρ, σ, t), kstore))
      /* In a continuation state, call the semantic's continuation method */
      case TracingControlKont(ka) =>
        val kont =  kstore.lookup(ka).head
        Some((sem.stepKont(v, kont.frame, σ, t), kstore.remove(ka, kont)))
      /* In an error state, the state is not able to make a step */
      case TracingControlError(_) => None
    }
    result match {
      case Some((set, kstore)) =>
        assert(set.size == 1)
        Some((set.head, kstore))
      case None => None
    }
  }

  def restart(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T],
              restartPoint: RestartPoint[Exp, HybridValue, HybridAddress.A])
             :ProgramState[Exp] = restartPoint match {
    case RestartFromControl(newControlExp) =>
      val newT = time.tick(t)
      ProgramState(TracingControlEval[Exp, HybridLattice.L, HybridAddress.A](newControlExp), ρ, σ, kstore, a, newT, v, vStack)
    case RestartGuardDifferentClosure(action) =>
      handleClosureRestart(sem, action)
    case RestartTraceEnded() => this
    case RestartSpecializedPrimitive(originalPrim, n, fExp, argsExps) =>
      val primAppliedState = applyPrimitive(originalPrim, n, fExp, argsExps)
      primAppliedState.applyAction(sem, ActionPopKontT()).getState
  }

  def runHeader(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T],
                assertions: List[ActionT[Exp, HybridValue, HybridAddress.A]])
               :Option[ProgramState[Exp]] = {
    assertions.foldLeft(Some(this): Option[ProgramState[Exp]])({ (someProgramState, action) =>
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

  def doActionStepInTraced(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T],
    action: ActionT[Exp, HybridValue, HybridAddress.A]): ProgramState[Exp] = action match {
    case ActionStepInT(fexp, bodyHead, _, argsv, n, frame, _, _) =>
      val (vals, poppedVStack) = popStackItems(vStack, n)
      val clo = vals.last.getVal
      val updatedEnvAndStores = sem.bindClosureArgs(clo, argsv.zip(vals.init.reverse.map(_.getVal)), σ, t).head
      updatedEnvAndStores match {
        case Right((ρ2, σ2, e)) =>
          val next = NormalKontAddress(e, t) // Hack to get infinite number of addresses in concrete mode
          val newVStack = //machine.StoreEnv[HybridValue, HybridAddress.A](ρ2) ::
                          StoreEnv[HybridValue, HybridAddress.A](ρ) ::
                          poppedVStack
          val newT = time.tick(t, fexp)
          ProgramState[Exp](TracingControlEval[Exp, HybridValue, HybridAddress.A](e), ρ2, σ2, kstore.extend(next, Kont(frame, a)), next, newT, v, newVStack)
        case Left(expectedNrOfArgs) =>
          val newT = time.tick(t)
          ProgramState[Exp](TracingControlError(ArityError(fexp.toString, expectedNrOfArgs, n - 1)), ρ, σ, kstore, a, newT, v, poppedVStack)
      }
  }

  def handleClosureRestart(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T],
                           action: ActionT[Exp, HybridValue, HybridAddress.A]): ProgramState[Exp] =
    doActionStepInTraced(sem, action)

  def applyPrimitive(primitive: Primitive[HybridAddress.A, HybridValue], n: Integer, fExp: Exp, argsExps: List[Exp]): ProgramState[Exp] = {
    val (vals, newVStack) = popStackItems(vStack, n)
    val operands: List[HybridValue] = vals.take(n - 1).map(_.getVal)
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
      case e : java.lang.IndexOutOfBoundsException =>
        throw new IncorrectStackSizeException()
    }
  }

  protected def saveEnv(): ProgramState[Exp] = {
    val newT = time.tick(t)
    ProgramState(control, ρ, σ, kstore, a, newT, v, StoreEnv[HybridValue, HybridAddress.A](ρ) :: vStack)
  }

  def applyAction(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T],
                  action: ActionT[Exp, HybridValue, HybridAddress.A])
                 :ActionReturn[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T, ProgramState[Exp]] = {

    ActionLogger.logAction[Exp, HybridValue, HybridAddress.A](action)

    val newT = time.tick(t)

    def handleGuard(guard: ActionGuardT[Exp, HybridValue, HybridAddress.A],
                    guardCheckFunction: HybridValue => Boolean)
                   :ActionReturn[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T, ProgramState[Exp]] = {
      if (guardCheckFunction(v)) {
        ActionStep(this, guard)
      } else {
        GuardFailed(guard.rp, guard.id)
      }
    }

    def handleClosureGuard(guard: ActionGuardSameClosure[Exp, HybridValue, HybridAddress.A],
                           currentClosure: HybridValue)
                          :ActionReturn[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T, ProgramState[Exp]] = {
      (guard.recordedClosure, currentClosure) match {
        case (HybridLattice.Concrete(HybridLattice.concreteLattice.lattice.Element(HybridLattice.concreteLattice.lattice.Closure(lam1, env1))),
              HybridLattice.Concrete(HybridLattice.concreteLattice.lattice.Element(HybridLattice.concreteLattice.lattice.Closure(lam2, env2)))) =>
          if (lam1 == lam2) {
            ActionStep(this, guard)
          } else {
            Logger.log(s"Closure guard failed: recorded closure $lam1 does not match current closure $lam2", Logger.D)
            GuardFailed(guard.rp, guard.id)
          }
        case (HybridLattice.Abstract(_), HybridLattice.Abstract(_)) =>
          ActionStep(this, guard)
        case _ =>
          throw new Exception(s"Mixing concrete values with abstract values: ${guard.recordedClosure} and $currentClosure")
      }
    }

    action match {
      case ActionAllocVarsT(variables) =>
        val addresses = variables.map(varName => addr.variable(varName, v, t))
        val (ρ1, σ1) = variables.zip(addresses).foldLeft((ρ, σ))({ case ((ρ2, σ2), (currV, currA)) => (ρ2.extend(currV, currA), σ2.extend(currA, abs.bottom)) })
        ActionStep(ProgramState(control, ρ1, σ1, kstore, a, newT, v, vStack), action)
      case ActionCreateClosureT(λ) =>
        val newClosure = sabs.inject[Exp, HybridAddress.A]((λ, ρ))
        ActionStep(ProgramState(control, ρ, σ, kstore, a, newT, newClosure, vStack), action)
      case ActionEndClosureCallT() =>
        ActionStep(this, action)
      case ActionEndPrimCallT() =>
        ActionStep(this, action)
      /* When an error is reached, we go to an error state */
      case ActionErrorT(err) =>
        ActionStep(ProgramState(TracingControlError(err), ρ, σ, kstore, a, newT, v, vStack), action)
      /* When a value needs to be evaluated, we go to an eval state */
      case ActionEvalT(e, _, _) =>
        ActionStep(ProgramState(TracingControlEval(e), ρ, σ, kstore, a, newT, v, vStack), action)
      /* When a continuation needs to be pushed, push it in the continuation store */
      case ActionEvalPushT(e, frame, _, _) =>
        val next = NormalKontAddress(e, t) // Hack to get infinite number of addresses in concrete mode
        ActionStep(ProgramState(TracingControlEval(e), ρ, σ, kstore.extend(next, Kont(frame, a)), next, newT, v, vStack), action)
      case ActionExtendEnvT(varName) =>
        val value = vStack.head.getVal
        val va = addr.variable(varName, value, t)
        val ρ1 = ρ.extend(varName, va)
        val σ1 = σ.extend(va, value)
        val newVStack = vStack.tail
        ActionStep(ProgramState(control, ρ1, σ1, kstore, a, newT, v, newVStack), action)
      case ActionExtendStoreT(addr, lit) =>
        val σ1 = σ.extend(addr, lit)
        ActionStep(ProgramState(control, ρ, σ1, kstore, a, newT, lit, vStack), action)
      case ActionLookupVariableT(varName, _, _) =>
        val newV = σ.lookup(ρ.lookup(varName).get).get
        ActionStep(ProgramState(control, ρ, σ, kstore, a, newT, newV, vStack), action)
      case ActionLookupVariablePushT(varName, _, _) => ρ.lookup(varName) match {
        case Some(address) =>
          σ.lookup(address) match {
            case Some(value) =>
              ActionStep(ProgramState(control, ρ, σ, kstore, a, newT, value, StoreVal[HybridValue, HybridAddress.A](value) :: vStack), action)
            case None =>
              throw new Exception(s"Could not find address $address in store")
          }
        case None =>
          throw new Exception(s"Could not find variable $varName in environment")
      }
      case ActionPopKontT() =>
        val next = if (a == HaltKontAddress) { HaltKontAddress } else {
          val kset = kstore.lookup(a)
          assert(kset.size == 1)
          kset.head.next}
        ActionStep(ProgramState(TracingControlKont(a), ρ, σ, kstore, next, newT, v, vStack), action)
      case ActionPrimCallT(n: Integer, fExp, argsExps) =>
        val (vals, _) = popStackItems(vStack, n)
        val operator = vals.last.getVal
        val primitivesSet = sabs.getPrimitives[HybridAddress.A, HybridValue](operator)
        assert(primitivesSet.size == 1)
        ActionStep(applyPrimitive(primitivesSet.head, n, fExp, argsExps), action)
      case ActionPushValT() =>
        ActionStep(ProgramState(control, ρ, σ, kstore, a, newT, v, StoreVal[HybridValue, HybridAddress.A](v) :: vStack), action)
      case ActionReachedValueT(lit, _, _) =>
        ActionStep(ProgramState(control, ρ, σ, kstore, a, newT, lit, vStack), action)
      case ActionReachedValuePushT(lit, _, _) =>
        ActionStep(ProgramState(control, ρ, σ, kstore, a, newT, lit, StoreVal[HybridValue, HybridAddress.A](lit) :: vStack), action)
      case ActionRestoreEnvT() =>
        ActionStep(restoreEnv(), action)
      case ActionRestoreSaveEnvT() =>
        ActionStep(restoreEnv().saveEnv(), action)
      case ActionSaveEnvT() =>
        ActionStep(saveEnv(), action)
      case ActionSetVarT(variable) =>
        ρ.lookup(variable) match {
          case Some(address) =>
            ActionStep(ProgramState(control, ρ, σ.update(address, v), kstore, a, newT, v, vStack), action)
          case None =>
            throw new VariableNotFoundException(variable)
        }
      case ActionGuardSpecializedPrimitive(expectedType, n, rp, guardID) =>
        val operands = popStackItems(vStack, n - 1)._1.map(_.getVal)
        val currentOperandsTypes = HybridLattice.LatticeConverter.getValuesTypes(operands)
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
        ActionStep(ProgramState(control, ρ, σ, kstore, a, newT, value, vStack), action)
      case ActionLookupRegisterPush(registerIndex) =>
        val value = RegisterStore.getRegister(registerIndex)
        ActionStep(ProgramState(control, ρ, σ, kstore, a, newT, value, StoreVal[HybridValue, HybridAddress.A](value) :: vStack), action)
      case action : ActionEndTrace[Exp, HybridValue, HybridAddress.A] =>
        TraceEnded(action.restartPoint)
      case action : ActionGuardFalseT[Exp, HybridValue, HybridAddress.A] =>
        handleGuard(action, sabs.isFalse)
      case action : ActionGuardTrueT[Exp, HybridValue, HybridAddress.A] =>
        handleGuard(action, sabs.isTrue)
      case action : ActionGuardSameClosure[Exp, HybridValue, HybridAddress.A] =>
        val n = action.rp.action.n
        try {
          handleClosureGuard(action, vStack(n - 1).getVal)
        } catch {
          case e : java.lang.IndexOutOfBoundsException =>
            throw new IncorrectStackSizeException
        }
    }

  }

  /**
    * Builds the state with the initial environment and stores
    */
  def this(sem: SemanticsTraced[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T],
           sabs: IsSchemeLattice[HybridLattice.L],
           exp: Exp) =
    this(TracingControlEval(exp),
         Environment.initial[HybridAddress.A](sem.initialEnv),
         Store.initial[HybridAddress.A, HybridLattice.L](sem.initialStore),
         KontStore.empty[KontAddr],
         HaltKontAddress,
         HybridTimestamp.isTimestamp.initial(""),
         sabs.inject(false),
         Nil)

  override def toString = control match {
    case TracingControlKont(_) => s"ko($v)"
    case _ => control.toString()
  }

  private def convertValue(σ: Store[HybridAddress.A, HybridLattice.L])(value: HybridValue): HybridValue =
    HybridLattice.convert[Exp, HybridAddress.A](value)

  /**
    * Maps one KontAddr to another.
    * @param address The KontAddr to be mapped.
    * @return The mapped KontAddr.
    */
  private def mapKontAddress(address: KontAddr): KontAddr = address match {
    case address: NormalKontAddress[Exp, HybridTimestamp.T] =>
      NormalKontAddress(address.exp, HybridTimestamp.convertTime(address.time))
    case HaltKontAddress => HaltKontAddress
  }

  def convertKStore(aam: AAM[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T],
                    sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T],
                    kontStore: KontStore[KontAddr],
                    ρ: Environment[HybridAddress.A],
                    a: KontAddr,
                    vStack: List[Storable[HybridValue, HybridAddress.A]])
                   :(KontAddr, KontStore[KontAddr]) = {

    @tailrec
    def loop(newKontStore: KontStore[KontAddr],
             a: KontAddr,
             vStack: List[Storable[HybridValue, HybridAddress.A]],
             ρ: Environment[HybridAddress.A],
             stack: List[(KontAddr, Option[Frame], List[Storable[HybridValue, HybridAddress.A]], Environment[HybridAddress.A])]): (KontAddr, KontStore[KontAddr]) = a match {
      case HaltKontAddress =>
        stack.foldLeft[(KontAddr, KontStore[KontAddr])] ((HaltKontAddress, newKontStore)) ({
          case ((actualNext, extendedKontStore), (a, someNewSemFrame, newVStack, newρ)) => someNewSemFrame match {
            case Some(newSemFrame) =>
              (a, extendedKontStore.extend(a, Kont(newSemFrame, actualNext)))
            case None =>
              (actualNext, extendedKontStore)
          }

        })
      case _ =>
        val Kont(frame, next) = kontStore.lookup(a).head
        val (someNewSemFrame, newVStack, newρ) = sem.convertToAbsSemanticsFrame(frame, ρ, vStack, HybridLattice.convert[Exp, HybridAddress.A](_))
        loop(newKontStore, next, newVStack, newρ, (a, someNewSemFrame, newVStack, newρ) :: stack)
    }
    loop(KontStore.empty, a, vStack, ρ, Nil)
  }

  def convertState(aam: AAM[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T])
                  (sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T]):
  (ConvertedControl[Exp, HybridValue, HybridAddress.A], Environment[HybridAddress.A],
   Store[HybridAddress.A, HybridValue], KontStore[KontAddr], KontAddr, HybridTimestamp.T) = {
    val newT = HybridTimestamp.convertTime(t)
    var newσ = Store.empty[HybridAddress.A, HybridLattice.L]
    def addToNewStore(tuple: (HybridAddress.A, HybridValue)): Boolean = {
      val newValue = convertValue(σ)(tuple._2)
      newσ = newσ.extend(tuple._1, newValue)
      true
    }
    σ.forall(addToNewStore)
    //val newA: KontAddr = a
    val newV = convertValue(σ)(v)
    val newVStack = vStack.map({
      case StoreVal(v) => StoreVal[HybridValue, HybridAddress.A](convertValue(σ)(v))
      case StoreEnv(ρ) => StoreEnv[HybridValue, HybridAddress.A](ρ)
    })
    val startKontAddress = control match {
      case TracingControlEval(_) | TracingControlError(_) => a
      case TracingControlKont(ka) => ka
    }
    val (newA, convertedKontStore) = convertKStore(aam, sem, kstore, ρ, startKontAddress, newVStack)
    val mappedConvertedKontStore = convertedKontStore.map(mapKontAddress)
    val convertedA = mapKontAddress(newA)
    val newControl = control match {
      case TracingControlEval(exp) =>
        ConvertedControlEval[Exp, HybridValue, HybridAddress.A](exp, ρ)
      case TracingControlKont(ka) =>
        ConvertedControlKont[Exp, HybridValue, HybridAddress.A](newV)
    }
    (newControl, ρ, newσ, mappedConvertedKontStore, convertedA, newT)
  }

  def generateTraceInformation(action: ActionT[Exp, HybridValue, HybridAddress.A]): CombinedInfos[HybridValue, HybridAddress.A] = action match {
    case ActionAllocVarsT(variables) =>
      val addresses = variables.map( (variable) => ρ.lookup(variable).get)
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
      val addresses = args.map( (arg) => ρ.lookup(arg).get)
      TraceInfos.single(AddressesAllocated(addresses))
    case _ =>
      TraceInfos.nil
  }

  def concretableState = this

  def subsumes(that: TracingProgramState[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T]): Boolean = that match {
    case that: ProgramState[Exp] =>
      concreteSubsumes(that)
    case _ => false
  }

  def setKStore(newKstore: KontStore[KontAddr]): ProgramState[Exp] =
    ProgramState(control, ρ, σ, newKstore, a, t, v, vStack)
}