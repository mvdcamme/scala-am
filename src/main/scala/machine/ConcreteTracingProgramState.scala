import scala.collection.immutable.Stack

trait ActionReturn[Exp, Abs, Addr, Time, +State] {
  def getState: State =
    throw new Exception(s"Unexpected result: $this does not have a resulting state")
}
case class ActionStep[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp, State <: ConcreteTracingProgramState[Exp, Abs, Addr, Time]]
  (newState: State, action: Action[Exp, Abs, Addr])
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

trait ConcretableTracingProgramState[Exp, Time] {

  def concretableState: ProgramState[Exp, Time]

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

  def concreteSubsumes(that: ConcretableTracingProgramState[Exp, Time]): Boolean =
    concretableState.control.subsumes(that.concretableState.control) &&
    concretableState.ρ.subsumes(that.concretableState.ρ) &&
    concretableState.σ.subsumes(that.concretableState.σ) &&
    concretableState.a == that.concretableState.a &&
    concretableState.kstore.subsumes(that.concretableState.kstore) &&
    concretableState.t == that.concretableState.t

}

trait ConcreteTracingProgramState[Exp, Abs, Addr, Time] extends TracingProgramState[Exp, Abs, Addr, Time] {
  type HybridValue = HybridLattice.L

  def step(sem: SemanticsTraced[Exp, Abs, Addr, Time]): Option[InterpreterStep[Exp, Abs, Addr]]
  def applyAction(sem: SemanticsTraced[Exp, Abs, Addr, Time],
                  action: Action[Exp, Abs, Addr]):
    ActionReturn[Exp, Abs, Addr, Time, ConcreteTracingProgramState[Exp, Abs, Addr, Time]]
  def restart(sem: SemanticsTraced[Exp, Abs, Addr, Time],
              restartPoint: RestartPoint[Exp, Abs, Addr]): ConcreteTracingProgramState[Exp, Abs, Addr, Time]

  def runHeader(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, Time],
                assertions: List[Action[Exp, Abs, Addr]]): Option[ConcreteTracingProgramState[Exp, Abs, Addr, Time]]

//  TODO
//  def convertState(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, Time]):
//    (ConvertedControl[Exp, Abs, Addr], Store[Addr, Abs], KontStore[KontAddr], KontAddr, Time)

  def generateTraceInformation(action: Action[Exp, Abs, Addr]): Option[TraceInformation[HybridValue]]
}

/**
  * A machine state is made of a control component, a value store, a
  * continuation store, and an address representing where the current
  * continuation lives.
  */
case class ProgramState[Exp : Expression, Time : Timestamp]
  (control: TracingControl[Exp, HybridLattice.L, HybridAddress.A],
   ρ: Environment[HybridAddress.A],
   σ: Store[HybridAddress.A, HybridLattice.L],
   kstore: KontStore[KontAddr],
   a: KontAddr,
   t: Time,
   v: HybridLattice.L,
   vStack: List[Storable[HybridLattice.L, HybridAddress.A]]) extends ConcreteTracingProgramState[Exp, HybridLattice.L, HybridAddress.A, Time]
                           with ConcretableTracingProgramState[Exp, Time] {

  def sabs = implicitly[IsSchemeLattice[HybridValue]]
  def abs = implicitly[JoinLattice[HybridLattice.L]]
  def addr = implicitly[Address[HybridAddress.A]]
  def time = implicitly[Timestamp[Time]]

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
  def step(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, Time]): Option[InterpreterStep[Exp, HybridValue, HybridAddress.A]] = {
    val result = control match {
      /* In a eval state, call the semantic's evaluation method */
      case TracingControlEval(e) => Some(sem.stepEval(e, ρ, σ, t))
      /* In a continuation state, call the semantic's continuation method */
      case TracingControlKont(ka) => Some(sem.stepKont(v, kstore.lookup(ka).head.frame, σ, t))
      /* In an error state, the state is not able to make a step */
      case TracingControlError(_) => None
    }
    result match {
      case Some(set) =>
        assert(set.size == 1)
        Some(set.head)
      case None => None
    }
  }

  def restart(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, Time],
              restartPoint: RestartPoint[Exp, HybridValue, HybridAddress.A]): ProgramState[Exp, Time] = restartPoint match {
    case RestartFromControl(newControlExp) =>
      ProgramState(TracingControlEval[Exp, HybridLattice.L, HybridAddress.A](newControlExp), ρ, σ, kstore, a, t, v, vStack)
    case RestartGuardDifferentClosure(action) =>
      handleClosureRestart(sem, action)
    case RestartGuardDifferentPrimitive(action) =>
      handlePrimitiveRestart(action)
    case RestartTraceEnded() => this
    case RestartSpecializedPrimitive(originalPrim, n, fExp, argsExps) =>
      val primAppliedState = applyPrimitive(originalPrim, n, fExp, argsExps)
      primAppliedState.applyAction(sem, ActionPopKontT()).getState
  }

  def runHeader(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, Time],
                assertions: List[Action[Exp, HybridValue, HybridAddress.A]]): Option[ProgramState[Exp, Time]] = {
    assertions.foldLeft(Some(this): Option[ProgramState[Exp, Time]])({ (someProgramState, action) =>
      someProgramState.fold(None: Option[ProgramState[Exp, Time]])(programState =>
      programState.applyAction(sem, action) match {
        case ActionStep(newState, _) => Some(newState)
        case GuardFailed(_, _) => None
        case TraceEnded(_) =>
          /* Should not happen */
          None
      })
      })
  }

  def doActionStepInTraced(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, Time],
    action: Action[Exp, HybridValue, HybridAddress.A]): ProgramState[Exp, Time] = action match {
    case ActionStepInT(fexp, bodyHead, _, argsv, n, frame, _, _) =>
      val (vals, poppedVStack) = popStackItems(vStack, n)
      val clo = vals.last.getVal
      val updatedEnvAndStores = sem.bindClosureArgs(clo, argsv.zip(vals.init.reverse.map(_.getVal)), σ, t).head
      updatedEnvAndStores match {
        case Right((ρ2, σ2, e)) =>
          val next = NormalKontAddress(e, t) // Hack to get infinite number of addresses in concrete mode
          val newVStack = //StoreEnv[HybridValue, HybridAddress.A](ρ2) ::
                          StoreEnv[HybridValue, HybridAddress.A](ρ) ::
                          poppedVStack
          ProgramState[Exp, Time](TracingControlEval[Exp, HybridValue, HybridAddress.A](e), ρ2, σ2, kstore.extend(next, Kont(frame, a)), next, time.tick(t, fexp), v, newVStack)
        case Left(expectedNrOfArgs) =>
          ProgramState[Exp, Time](TracingControlError(ArityError(fexp.toString, expectedNrOfArgs, n - 1)), ρ, σ, kstore, a, t, v, poppedVStack)
      }
  }

  def handleClosureRestart(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, Time],
                           action: Action[Exp, HybridValue, HybridAddress.A]): ProgramState[Exp, Time] =
    doActionStepInTraced(sem, action)

  def applyPrimitive(primitive: Primitive[HybridAddress.A, HybridValue], n: Integer, fExp: Exp, argsExps: List[Exp]): ProgramState[Exp, Time] = {
    val (vals, newVStack) = popStackItems(vStack, n)
    val operands: List[HybridValue] = vals.take(n - 1).map(_.getVal)
    val result = primitive.call(fExp, argsExps.zip(operands.reverse), σ, t)
    result.value match {
      case Some((res, σ2, effects)) =>
        ProgramState(control, ρ, σ2, kstore, a, t, res, newVStack)
      case None =>
        throw new Exception(result.errors.head.toString)
    }
  }

  def handlePrimitiveRestart(action: Action[Exp, HybridValue, HybridAddress.A]): ProgramState[Exp, Time] = {
    action match {
      case ActionPrimCallT(n: Integer, fExp, argsExps) =>
        val (vals, newVStack) = popStackItems(vStack, n)
        val operator: HybridValue = vals.last.getVal
        val operands: List[HybridValue] = vals.take(n - 1).map(_.getVal)
        val primitive: Set[Primitive[HybridAddress.A, HybridValue]] = sabs.getPrimitives[HybridAddress.A, HybridValue](operator)
        val result = primitive.headOption match {
          case Some(p) => p.call(fExp, argsExps.zip(operands.reverse), σ, t)
          case None => throw new NotAPrimitiveException(s"Operator $fExp not a primitive: $operator")
        }
        result.value match {
          case Some((v, newσ, effects)) =>
            val primAppliedState = ProgramState(control, ρ, σ, kstore, a, t, v, newVStack)
            val next = if (primAppliedState.a == HaltKontAddress) { HaltKontAddress } else { primAppliedState.kstore.lookup(primAppliedState.a).head.next }
            ProgramState(TracingControlKont(primAppliedState.a), primAppliedState.ρ, primAppliedState.σ, primAppliedState.kstore, next, primAppliedState.t, primAppliedState.v, primAppliedState.vStack)
          case None => throw new Exception(result.errors.head.toString)
        }
    }
  }

  def restoreEnv(): ProgramState[Exp, Time] = {
    try {
      val (newρ, newVStack) = popStack(vStack)
      ProgramState(control, newρ.getEnv, σ, kstore, a, t, v, newVStack)
    } catch {
      case e : java.lang.IndexOutOfBoundsException =>
        throw new IncorrectStackSizeException()
    }
  }

  protected def saveEnv(): ProgramState[Exp, Time] =
    ProgramState(control, ρ, σ, kstore, a, t, v, StoreEnv[HybridValue, HybridAddress.A](ρ) :: vStack)

  def applyAction(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, Time],
                  action: Action[Exp, HybridValue, HybridAddress.A]): ActionReturn[Exp, HybridValue, HybridAddress.A, Time, ProgramState[Exp, Time]] = {

    ActionLogger.logAction[Exp, HybridValue, HybridAddress.A](action)

    def handleGuard(guard: ActionGuardT[Exp, HybridValue, HybridAddress.A],
                    guardCheckFunction: HybridValue => Boolean): ActionReturn[Exp, HybridValue, HybridAddress.A, Time, ProgramState[Exp, Time]] = {
      if (guardCheckFunction(v)) {
        ActionStep(this, guard)
      } else {
        GuardFailed(guard.rp, guard.id)
      }
    }

    def handleClosureGuard(guard: ActionGuardSameClosure[Exp, HybridValue, HybridAddress.A], currentClosure: HybridValue): ActionReturn[Exp, HybridValue, HybridAddress.A, Time, ProgramState[Exp, Time]] = {
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

    def handlePrimitiveGuard(guard: ActionGuardSamePrimitive[Exp, HybridValue, HybridAddress.A], currentPrimitive: HybridValue): ActionReturn[Exp, HybridValue, HybridAddress.A, Time, ProgramState[Exp, Time]] = {
      if (guard.recordedPrimitive == currentPrimitive) {
        ActionStep(this, guard)
      } else {
        Logger.log(s"Primitive guard failed: recorded primitive ${guard.recordedPrimitive} does not match current primitive $currentPrimitive", Logger.D)
        GuardFailed(guard.rp, guard.id)
      }
    }

    action match {
      case ActionAllocVarsT(variables) =>
        val addresses = variables.map(varName => addr.variable(varName, v, t))
        val (ρ1, σ1) = variables.zip(addresses).foldLeft((ρ, σ))({ case ((ρ2, σ2), (currV, currA)) => (ρ2.extend(currV, currA), σ2.extend(currA, abs.bottom)) })
        ActionStep(ProgramState(control, ρ1, σ1, kstore, a, t, v, vStack), action)
      case ActionCreateClosureT(λ) =>
        val newClosure = sabs.inject[Exp, HybridAddress.A]((λ, ρ))
        ActionStep(ProgramState(control, ρ, σ, kstore, a, t, newClosure, vStack), action)
      case ActionEndClosureCallT() =>
        ActionStep(this, action)
      case ActionEndPrimCallT() =>
        ActionStep(this, action)
      /* When an error is reached, we go to an error state */
      case ActionErrorT(err) =>
        ActionStep(ProgramState(TracingControlError(err), ρ, σ, kstore, a, t, v, vStack), action)
      /* When a value needs to be evaluated, we go to an eval state */
      case ActionEvalT(e, _, _) =>
        ActionStep(ProgramState(TracingControlEval(e), ρ, σ, kstore, a, t, v, vStack), action)
      /* When a continuation needs to be pushed, push it in the continuation store */
      case ActionEvalPushT(e, frame, _, _) =>
        val next = NormalKontAddress(e, t) // Hack to get infinite number of addresses in concrete mode
        ActionStep(ProgramState(TracingControlEval(e), ρ, σ, kstore.extend(next, Kont(frame, a)), next, t, v, vStack), action)
      case ActionExtendEnvT(varName: String) =>
        val value = vStack.head.getVal
        val va = addr.variable(varName, value, t)
        val ρ1 = ρ.extend(varName, va)
        val σ1 = σ.extend(va, value)
        val newVStack = vStack.tail
        ActionStep(ProgramState(control, ρ1, σ1, kstore, a, t, v, newVStack), action)
      case ActionExtendStoreT(addr, lit) =>
        val σ1 = σ.extend(addr, lit)
        ActionStep(ProgramState(control, ρ, σ1, kstore, a, t, lit, vStack), action)
      case ActionLookupVariableT(varName, _, _) =>
        val newV = σ.lookup(ρ.lookup(varName).get).get
        ActionStep(ProgramState(control, ρ, σ, kstore, a, t, newV, vStack), action)
      case ActionLookupVariablePushT(varName, _, _) => ρ.lookup(varName) match {
        case Some(address) =>
          val newV = σ.lookup(address).get
          ActionStep(ProgramState(control, ρ, σ, kstore, a, t, newV, StoreVal[HybridValue, HybridAddress.A](newV) :: vStack), action)
        case None =>
          throw new Exception(s"Could not find variable $varName in environment")
      }
      case ActionPopKontT() =>
        val next = if (a == HaltKontAddress) { HaltKontAddress } else { kstore.lookup(a).head.next }
        ActionStep(ProgramState(TracingControlKont(a), ρ, σ, kstore, next, t, v, vStack), action)
      case ActionPrimCallT(n: Integer, fExp, argsExps) =>
        val (vals, _) = popStackItems(vStack, n)
        val operator = vals.last.getVal
        val primitivesSet = sabs.getPrimitives[HybridAddress.A, HybridValue](operator)
        assert(primitivesSet.size == 1) //TODO
        ActionStep(applyPrimitive(primitivesSet.head, n, fExp, argsExps), action)
      case ActionPushValT() =>
        ActionStep(ProgramState(control, ρ, σ, kstore, a, t, v, StoreVal[HybridValue, HybridAddress.A](v) :: vStack), action)
      case ActionReachedValueT(lit, _, _) =>
        ActionStep(ProgramState(control, ρ, σ, kstore, a, t, lit, vStack), action)
      case ActionReachedValuePushT(lit, _, _) =>
        ActionStep(ProgramState(control, ρ, σ, kstore, a, t, lit, StoreVal[HybridValue, HybridAddress.A](lit) :: vStack), action)
      case ActionRestoreEnvT() =>
        ActionStep(restoreEnv(), action)
      case ActionRestoreSaveEnvT() =>
        ActionStep(restoreEnv().saveEnv(), action)
      case ActionSaveEnvT() =>
        ActionStep(saveEnv(), action)
      case ActionSetVarT(variable) =>
        ρ.lookup(variable) match {
          case Some(address) =>
            ActionStep(ProgramState(control, ρ, σ.update(address, v), kstore, a, t, v, vStack), action)
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
        ActionStep(ProgramState(control, ρ, σ, kstore, a, t, value, vStack), action)
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
      case action : ActionGuardSamePrimitive[Exp, HybridValue, HybridAddress.A] =>
        val n = action.rp.action.n
        if (vStack.length > n - 1) {
          handlePrimitiveGuard(action, vStack(n - 1).getVal)
        } else {
          throw new IncorrectStackSizeException()
        }
      case ActionGuardAssertFreeVariable(variableName, expectedValue, rp, guardID) =>
        ρ.lookup(variableName) match {
          case Some(address) =>
            val currentValue = σ.lookup(address).get
            if (currentValue == expectedValue) {
              ActionStep(this, action)
            } else {
              Logger.log(s"Variable $variableName with current value $currentValue does not match its expected value $expectedValue", Logger.V)
              GuardFailed(rp, guardID)
            }
          case None =>
            /* Variable could not be found => automatic guard failure as the value is completely unknown */
            GuardFailed(rp, guardID)
        }
    }

  }

  /**
    * Builds the state with the initial environment and stores
    */
  def this(sem: SemanticsTraced[Exp, HybridLattice.L, HybridAddress.A, Time],
           sabs: IsSchemeLattice[HybridLattice.L],
           exp: Exp,
           time: Timestamp[Time]) =
    this(TracingControlEval(exp),
         Environment.initial[HybridAddress.A](sem.initialEnv),
         Store.initial[HybridAddress.A, HybridLattice.L](sem.initialStore),
         KontStore.empty[KontAddr],
         HaltKontAddress,
         time.initial(""),
         sabs.inject(false),
         Nil)

  override def toString = control match {
    case TracingControlKont(_) => s"ko($v)"
    case _ => control.toString()
  }

//  TODO
//  val valueConverter: AbstractConcreteToAbstractType = new AbstractConcreteToAbstractType
//
//  def convertValue(σ: Store[HybridAddress.A, HybridLattice.type])(value: HybridValue): HybridValue = value match {
//    case HybridLattice.Concrete(v) => HybridLattice.Abstract(valueConverter.convert[Exp](v, σ))
//    case HybridLattice.Abstract(v) => value
//  }
//
//  def convertEnvironment(env: Environment[HybridAddress.A]): Environment[HybridAddress.A] =
//    new Environment[HybridAddress.A](env.content.map { tuple => (tuple._1, HybridAddress.A.convertAddress(tuple._2))})
//
//  def convertControl(control: TracingControl[Exp, HybridValue, HybridAddress.A], σ: Store[HybridAddress.A, HybridLattice.type]): TracingControl[Exp, HybridValue, HybridAddress.A] = control match {
//    case TracingControlEval(exp) => TracingControlEval(exp)
//    case TracingControlKont(ka) => TracingControlKont(convertKontAddress(ka))
//    case TracingControlError(error) => TracingControlError(error)
//  }
//
//  def convertKontAddress(address: KontAddr): KontAddr = address match {
//    case address : NormalKontAddress[Exp, HybridAddress.A] =>
//      NormalKontAddress(address.exp, HybridAddress.convertAddress(address.addr))
//    case HaltKontAddress => HaltKontAddress
//  }
//
//  def convertKStore(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, Time],
//                    kontStore: KontStore[KontAddr],
//                    ρ: Environment[HybridAddress.A],
//                    a: KontAddr,
//                    vStack: List[Storable[HybridValue, HybridAddress.A]]): KontStore[KontAddr] = {
//    def loop(newKontStore: KontStore[KontAddr], a: KontAddr, vStack: List[Storable[HybridValue, HybridAddress.A]], ρ: Environment[HybridAddress.A]): KontStore[KontAddr] = a match {
//      case HaltKontAddress => newKontStore
//      case _ =>
//        val Kont(frame, next) = kontStore.lookup(a).head
//        val (convertedFrame, newVStack, newρ) = sem.convertToAbsSemanticsFrame(frame, ρ, vStack)
//        val extendedNewKontStore = newKontStore.extend(a, Kont(convertedFrame, next))
//        loop(extendedNewKontStore, next, newVStack, newρ)
//    }
//    loop(new KontStore[KontAddr](), a, vStack, ρ)
//  }
//
//  def convertState(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, Time]):
//  (ConvertedControl[Exp, HybridValue, HybridAddress.A], Store[HybridAddress.A, HybridValue], KontStore[KontAddr], KontAddr, Time) = {
//    val newρ = convertEnvironment(ρ)
//    var newσ = Store.empty[HybridAddress.A, HybridLattice.type]
//    def addToNewStore(tuple: (HybridAddress.A, HybridValue)): Boolean = {
//      val newAddress = HybridAddress.convertAddress(tuple._1)
//      val newValue = convertValue(σ)(tuple._2)
//      newσ = newσ.extend(newAddress, newValue)
//      true
//    }
//    σ.forall(addToNewStore)
//    val newA = convertKontAddress(a)
//    val newV = convertValue(σ)(v)
//    val newVStack = vStack.map({
//      case StoreVal(v) => StoreVal[HybridValue, HybridAddress.A](convertValue(σ)(v))
//      case StoreEnv(ρ) => StoreEnv[HybridValue, HybridAddress.A](convertEnvironment(ρ))
//    })
//    val startKontAddress = control match {
//      case TracingControlEval(_) | TracingControlError(_) => a
//      case TracingControlKont(ka) => ka
//    }
//    val convertedKontStore = convertKStore(sem, kstore, ρ, startKontAddress, newVStack)
//    val absSem = sem.absSem
//    val newKStore = convertedKontStore.map(convertKontAddress, absSem.convertFrame(HybridAddress.convertAddress, convertValue(σ)))
//    val newControl = control match {
//      case TracingControlEval(exp) =>
//        ConvertedControlEval[Exp, HybridValue, HybridAddress.A](exp, newρ)
//      case TracingControlKont(ka) =>
//        ConvertedControlKont[Exp, HybridValue, HybridAddress.A](newV)
//    }
//    (newControl, newσ, newKStore, newA, t)
//  }



  def generateTraceInformation(action: Action[Exp, HybridValue, HybridAddress.A]): Option[TraceInformation[HybridValue]] = action match {
    case ActionPrimCallT(_, _, _) =>
      Some(PrimitiveAppliedInfo(v, vStack))
    case _ =>
      None
  }

  def concretableState = this

  def subsumes(that: TracingProgramState[Exp, HybridValue, HybridAddress.A, Time]): Boolean = that match {
    case that: ProgramState[Exp, Time] =>
      concreteSubsumes(that)
    case _ => false
  }
}