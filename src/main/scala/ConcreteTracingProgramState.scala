import scala.collection.immutable.Stack

trait InstructionStep[Exp, Abs, Addr, Time, +State] {
  def getState: State =
    throw new Exception(s"Unexpected result: $this does not have a resulting state")
}
case class NormalInstructionStep[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp, State <: ConcreteTracingProgramState[Exp, Abs, Addr, Time]]
 (newState: State, action: Action[Exp, Abs, Addr]) extends InstructionStep[Exp, Abs, Addr, Time, State] {
  override def getState: State = newState
}
case class GuardFailed[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp, State]
(rp: RestartPoint[Exp, Abs, Addr]) extends InstructionStep[Exp, Abs, Addr, Time, State]
case class TraceEnded[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp, State]
(rp: RestartPoint[Exp, Abs, Addr]) extends InstructionStep[Exp, Abs, Addr, Time, State]

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
    case TracingControlKont(HaltKontAddress) => true
    case TracingControlKont(_) => concretableState.abs.isError(concretableState.v)
    case TracingControlError(_) => true
  }

  /**
    * Returns the set of final values that can be reached
    */
  def finalValues = concretableState.control match {
    case TracingControlKont(_) => Set[HybridLattice.Hybrid](concretableState.v)
    case _ => Set[HybridLattice.Hybrid]()
  }

  def graphNodeColor = concretableState.control match {
    case TracingControlEval(_) => "#DDFFDD"
    case TracingControlKont(_) => "#FFDDDD"
    case TracingControlError(_) => "#FF0000"
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
  type HybridValue = HybridLattice.Hybrid

  def step(sem: SemanticsTraced[Exp, Abs, Addr, Time]): Option[Step[Exp, Abs, Addr]]
  def applyAction(sem: SemanticsTraced[Exp, Abs, Addr, Time],
                  action: Action[Exp, Abs, Addr]):
    InstructionStep[Exp, Abs, Addr, Time, ConcreteTracingProgramState[Exp, Abs, Addr, Time]]
  def restart(sem: SemanticsTraced[Exp, Abs, Addr, Time],
              restartPoint: RestartPoint[Exp, Abs, Addr]): ConcreteTracingProgramState[Exp, Abs, Addr, Time]

  def runAssertions(assertions: List[Action[Exp, Abs, Addr]]): Boolean

  def convertState(oldSem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
                   newSem: Semantics[Exp, HybridValue, HybridAddress, Time]):
    (ConvertedControl[Exp, Abs, Addr], Store[Addr, Abs], KontStore[KontAddr], KontAddr, Time)

  def generateTraceInformation(action: Action[Exp, Abs, Addr]): Option[TraceInformation[HybridValue]]
}

/**
  * A machine state is made of a control component, a value store, a
  * continuation store, and an address representing where the current
  * continuation lives.
  */
case class ProgramState[Exp : Expression, Time : Timestamp]
  (control: TracingControl[Exp, HybridLattice.Hybrid, HybridAddress],
   ρ: Environment[HybridAddress],
   σ: Store[HybridAddress, HybridLattice.Hybrid],
   kstore: KontStore[KontAddr],
   a: KontAddr,
   t: Time,
   v: HybridLattice.Hybrid,
   vStack: List[Storable[HybridLattice.Hybrid, HybridAddress]]) extends ConcreteTracingProgramState[Exp, HybridLattice.Hybrid, HybridAddress, Time]
                           with ConcretableTracingProgramState[Exp, Time] {

  def abs = implicitly[AbstractValue[HybridValue]]
  def addr = implicitly[Address[HybridAddress]]
  def time = implicitly[Timestamp[Time]]

  case class RestartSpecializedPrimCall(originalPrim: HybridValue, n: Integer, fExp: Exp, argsExps: List[Exp]) extends RestartPoint[Exp, HybridValue, HybridAddress]

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
  def step(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time]): Option[Step[Exp, HybridValue, HybridAddress]] = {
    val result = control match {
      /* In a eval state, call the semantic's evaluation method */
      case TracingControlEval(e) => Some(sem.stepEval(e, ρ, σ, t))
      /* In a continuation state, if the value reached is not an error, call the
       * semantic's continuation method */
      case TracingControlKont(_) if abs.isError(v) => None
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

  def restart(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
              restartPoint: RestartPoint[Exp, HybridValue, HybridAddress]): ProgramState[Exp, Time] = restartPoint match {
    case RestartFromControl(newControlExp) =>
      ProgramState(TracingControlEval(newControlExp), ρ, σ, kstore, a, t, v, vStack)
    case RestartGuardDifferentClosure(action) =>
      handleClosureRestart(sem, action)
    case RestartGuardDifferentPrimitive(action) =>
      handlePrimitiveRestart(action)
    case RestartTraceEnded() => this
    case RestartSpecializedPrimCall(originalPrim, n, fExp, argsExps) =>
      val primAppliedState = applyPrimitive(originalPrim, n, fExp, argsExps)
      primAppliedState.applyAction(sem, ActionPopKontT()).getState
  }

  def runAssertions(assertions: List[Action[Exp, HybridValue, HybridAddress]]): Boolean = {
    assertions.foldLeft(true)({ (assertionsValid, assertion) =>
      if (!assertionsValid) {
        assertionsValid
      } else {
        assertion match {
          case ActionGuardAssertFreeVariable(variableName, expectedValue, _) =>
            ρ.lookup(variableName) match {
              case Some(address) =>
                val currentValue = σ.lookup(address)
                if (currentValue == expectedValue) {
                  true
                } else {
                  Logger.log(s"Variable $variableName with current value $currentValue does not match its expected value $expectedValue", Logger.V)
                  false
                }
              case None => false
            }
        }
      }
    })
  }

  def doActionStepInTraced(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
                           action: Action[Exp, HybridValue, HybridAddress]): Set[ProgramState[Exp, Time]] = action match {
    case ActionStepInT(fexp, _, _, argsv, n, frame, _, _) =>
      val (vals, newVStack) = popStackItems(vStack, n)
      val clo = vals.last.getVal
        val updatedEnvAndStores = sem.bindClosureArgs(clo, argsv.zip(vals.init.reverse.map(_.getVal)), σ, t) //(ρ2, σ2, e)
        updatedEnvAndStores.map({ (tuple) => tuple match {
          case Some((ρ2, σ2, e)) =>
            val next = NormalKontAddress(e, addr.variable("__kont__", t)) // Hack to get infinite number of addresses in concrete mode
            ProgramState[Exp, Time](TracingControlEval[Exp, HybridValue, HybridAddress](e), ρ2, σ2, kstore.extend(next, Kont(frame, a)), next, time.tick(t, fexp), v, StoreEnv[HybridValue, HybridAddress](ρ) :: newVStack)
          case None =>
            ProgramState(TracingControlError(s"Arity error when calling $fexp. got ${n - 1})"), ρ, σ, kstore, a, t, v, newVStack)
        }
        })
  }

  def handleClosureRestart(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
                           action: Action[Exp, HybridValue, HybridAddress]): ProgramState[Exp, Time] =
    doActionStepInTraced(sem, action).head

  def applyPrimitive(operator: HybridValue, n: Integer, fExp: Exp, argsExps: List[Exp]): ProgramState[Exp, Time] = {
    val (vals, newVStack) = popStackItems(vStack, n)
    val operands: List[HybridValue] = vals.take(n - 1).map(_.getVal)
    val primitive: Option[Primitive[HybridAddress, HybridValue]] = abs.getPrimitive[HybridAddress, HybridValue](operator)
    val result = primitive match {
      case Some(p) => p.call(fExp, argsExps.zip(operands.reverse), σ, t)
      case None => throw new NotAPrimitiveException(s"Operator $fExp not a primitive: $operator")
    }
    result match {
      case Left(error) =>
        throw new Exception(error)
      case Right((res, σ2)) =>
        ProgramState(control, ρ, σ2, kstore, a, t, res, newVStack)
    }
  }

  def handlePrimitiveRestart(action: Action[Exp, HybridValue, HybridAddress]): ProgramState[Exp, Time] = {
    action match {
      case ActionPrimCallT(n: Integer, fExp, argsExps) =>
        val (vals, newVStack) = popStackItems(vStack, n)
        val operator: HybridValue = vals.last.getVal
        val operands: List[HybridValue] = vals.take(n - 1).map(_.getVal)
        val primitive: Option[Primitive[HybridAddress, HybridValue]] = abs.getPrimitive[HybridAddress, HybridValue](operator)
        val result = primitive match {
          case Some(p) => p.call(fExp, argsExps.zip(operands.reverse), σ, t)
          case None => throw new NotAPrimitiveException(s"Operator $fExp not a primitive: $operator")
        }
        result match {
          case Left(error) => throw new Exception(error)
          case Right((v, newσ)) =>
            val primAppliedState = ProgramState(control, ρ, σ, kstore, a, t, v, newVStack)
            val next = if (primAppliedState.a == HaltKontAddress) { HaltKontAddress } else { primAppliedState.kstore.lookup(primAppliedState.a).head.next }
            ProgramState(TracingControlKont(primAppliedState.a), primAppliedState.ρ, primAppliedState.σ, primAppliedState.kstore, next, primAppliedState.t, primAppliedState.v, primAppliedState.vStack)
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
    ProgramState(control, ρ, σ, kstore, a, t, v, StoreEnv[HybridValue, HybridAddress](ρ) :: vStack)

  def applyAction(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
                  action: Action[Exp, HybridValue, HybridAddress]): InstructionStep[Exp, HybridValue, HybridAddress, Time, ProgramState[Exp, Time]] = {

    ActionLogger.logAction[Exp, HybridValue, HybridAddress](action)

    def handleGuard(guard: ActionGuardT[Exp, HybridValue, HybridAddress],
                    guardCheckFunction: HybridValue => Boolean): InstructionStep[Exp, HybridValue, HybridAddress, Time, ProgramState[Exp, Time]] = {
      if (guardCheckFunction(v)) {
        NormalInstructionStep(this, guard)
      } else {
        GuardFailed(guard.rp)
      }
    }

    def handleClosureGuard(guard: ActionGuardSameClosure[Exp, HybridValue, HybridAddress], currentClosure: HybridValue): InstructionStep[Exp, HybridValue, HybridAddress, Time, ProgramState[Exp, Time]] = {
      (guard.recordedClosure, currentClosure) match {
        case (HybridLattice.Left(AbstractConcrete.AbstractClosure(lam1, env1)), HybridLattice.Left(AbstractConcrete.AbstractClosure(lam2, env2))) =>
          if (lam1 == lam2) {
            NormalInstructionStep(this, guard)
          } else {
            Logger.log(s"Closure guard failed: recorded closure $lam1 does not match current closure $lam2", Logger.D)
            GuardFailed(guard.rp)
          }
        case (HybridLattice.Right(_), HybridLattice.Right(_)) =>
          NormalInstructionStep(this, guard)
        case _ =>
          throw new Exception(s"Mixing concrete values with abstract values: ${guard.recordedClosure} and $currentClosure")
      }
    }

    def handlePrimitiveGuard(guard: ActionGuardSamePrimitive[Exp, HybridValue, HybridAddress], currentPrimitive: HybridValue): InstructionStep[Exp, HybridValue, HybridAddress, Time, ProgramState[Exp, Time]] = {
      if (guard.recordedPrimitive == currentPrimitive) {
        NormalInstructionStep(this, guard)
      } else {
        Logger.log(s"Primitive guard failed: recorded primitive ${guard.recordedPrimitive} does not match current primitive $currentPrimitive", Logger.D)
        GuardFailed(guard.rp)
      }
    }

    action match {
      case ActionAllocVarsT(variables) =>
        val addresses = variables.map(v => addr.variable(v, t))
        val (ρ1, σ1) = variables.zip(addresses).foldLeft((ρ, σ))({ case ((ρ2, σ2), (currV, currA)) => (ρ2.extend(currV, currA), σ2.extend(currA, abs.bottom)) })
        NormalInstructionStep(ProgramState(control, ρ1, σ1, kstore, a, t, v, vStack), action)
      case ActionCreateClosureT(λ) =>
        val newClosure = abs.inject[Exp, HybridAddress]((λ, ρ))
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, newClosure, vStack), action)
      case ActionEndClosureCallT() =>
        NormalInstructionStep(this, action)
      case ActionEndPrimCallT() =>
        NormalInstructionStep(this, action)
      /* When an error is reached, we go to an error state */
      case ActionErrorT(err) =>
        NormalInstructionStep(ProgramState(TracingControlError(err), ρ, σ, kstore, a, t, v, vStack), action)
      /* When a value needs to be evaluated, we go to an eval state */
      case ActionEvalT(e, _, _) =>
        NormalInstructionStep(ProgramState(TracingControlEval(e), ρ, σ, kstore, a, t, v, vStack), action)
      /* When a continuation needs to be pushed, push it in the continuation store */
      case ActionEvalPushT(e, frame, _, _) =>
        val next = NormalKontAddress(e, addr.variable("__kont__", t)) // Hack to get infinite number of addresses in concrete mode
        NormalInstructionStep(ProgramState(TracingControlEval(e), ρ, σ, kstore.extend(next, Kont(frame, a)), next, t, v, vStack), action)
      case ActionExtendEnvT(varName: String) =>
        val va = addr.variable(varName, t)
        val ρ1 = ρ.extend(varName, va)
        val value = vStack.head.getVal
        val σ1 = σ.extend(va, value)
        val newVStack = vStack.tail
        NormalInstructionStep(ProgramState(control, ρ1, σ1, kstore, a, t, v, newVStack), action)
      case ActionExtendStoreT(addr, lit) =>
        val σ1 = σ.extend(addr, lit)
        NormalInstructionStep(ProgramState(control, ρ, σ1, kstore, a, t, lit, vStack), action)
      case ActionLookupVariableT(varName, _, _) =>
        val newV = σ.lookup(ρ.lookup(varName).get)
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, newV, vStack), action)
      case ActionLookupVariablePushT(varName, _, _) =>
        val newV = σ.lookup(ρ.lookup(varName).get)
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, newV, StoreVal[HybridValue, HybridAddress](newV) :: vStack), action)
      case ActionPopKontT() =>
        val next = if (a == HaltKontAddress) { HaltKontAddress } else { kstore.lookup(a).head.next }
        NormalInstructionStep(ProgramState(TracingControlKont(a), ρ, σ, kstore, next, t, v, vStack), action)
      case ActionPrimCallT(n: Integer, fExp, argsExps) =>
        val (vals, _) = popStackItems(vStack, n)
        val operator = vals.last.getVal
        NormalInstructionStep(applyPrimitive(operator, n, fExp, argsExps), action)
      case ActionPushValT() =>
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, v, StoreVal[HybridValue, HybridAddress](v) :: vStack), action)
      case ActionReachedValueT(lit, _, _) =>
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, lit, vStack), action)
      case ActionReachedValuePushT(lit, _, _) =>
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, lit, StoreVal[HybridValue, HybridAddress](lit) :: vStack), action)
      case ActionRestoreEnvT() =>
        NormalInstructionStep(restoreEnv(), action)
      case ActionRestoreSaveEnvT() =>
        NormalInstructionStep(restoreEnv().saveEnv(), action)
      case ActionSaveEnvT() =>
        NormalInstructionStep(saveEnv(), action)
      case ActionSetVarT(variable) =>
        ρ.lookup(variable) match {
          case Some(address) =>
            NormalInstructionStep(ProgramState(control, ρ, σ.update(address, v), kstore, a, t, v, vStack), action)
          case None =>
            throw new VariableNotFoundException(variable)
        }
      case ActionSpecializePrimitive(expectedType, prim, originalPrim, n, fExp, argsExps) =>
        val operands = popStackItems(vStack, n - 1)._1.map(_.getVal)
        val currentOperandsTypes = HybridLattice.checkValuesTypes(operands)
        if (currentOperandsTypes == expectedType) {
          NormalInstructionStep(applyPrimitive(prim, n, fExp, argsExps), action)
        } else {
          GuardFailed(RestartSpecializedPrimCall(originalPrim, n, fExp, argsExps))
        }
      case ActionStartFunCallT() =>
        NormalInstructionStep(this, action)
      /* When a function is stepped in, we also go to an eval state */
      case ActionStepInT(fexp, e, args, argsv, n, frame, _, _) =>
        NormalInstructionStep(doActionStepInTraced(sem, action).head, action)
      case action : ActionEndTrace[Exp, HybridValue, HybridAddress] =>
        TraceEnded(action.restartPoint)
      case action : ActionGuardFalseT[Exp, HybridValue, HybridAddress] =>
        handleGuard(action, abs.isFalse)
      case action : ActionGuardTrueT[Exp, HybridValue, HybridAddress] =>
        handleGuard(action, abs.isTrue)
      case action : ActionGuardSameClosure[Exp, HybridValue, HybridAddress] =>
        val n = action.rp.action.n
        try {
          handleClosureGuard(action, vStack(n - 1).getVal)
        } catch {
          case e : java.lang.IndexOutOfBoundsException =>
            throw new IncorrectStackSizeException
        }
      case action : ActionGuardSamePrimitive[Exp, HybridValue, HybridAddress] =>
        val n = action.rp.action.n
        if (vStack.length > n - 1) {
          handlePrimitiveGuard(action, vStack(n - 1).getVal)
        } else {
          throw new IncorrectStackSizeException()
        }
    }

  }

  /**
    * Builds the state with the initial environment and stores
    */
  def this(exp: Exp, primitives: Primitives[HybridAddress, HybridLattice.Hybrid], abs: AbstractValue[HybridLattice.Hybrid], time: Timestamp[Time]) =
    this(TracingControlEval(exp), Environment.empty[HybridAddress]().extend(primitives.forEnv),
         Store.initial(primitives.forStore, true),
         new KontStore[KontAddr](), HaltKontAddress, time.initial, abs.inject(false), Nil)

  override def toString = control match {
    case TracingControlKont(_) => s"ko($v)"
    case _ => control.toString()
  }

  val valueConverter: AbstractConcreteToAbstractType = new AbstractConcreteToAbstractType

  def convertValue(σ: Store[HybridAddress, HybridLattice.Hybrid])(value: HybridValue): HybridValue = value match {
    case HybridLattice.Left(v) => HybridLattice.Right(valueConverter.convert[Exp](v, σ))
    case HybridLattice.Right(v) => value
    case HybridLattice.Prim(p) => value
  }

  def convertEnvironment(env: Environment[HybridAddress]): Environment[HybridAddress] =
    new Environment[HybridAddress](env.content.map { tuple => (tuple._1, HybridAddress.convertAddress(tuple._2))})

  def convertControl(control: TracingControl[Exp, HybridValue, HybridAddress], σ: Store[HybridAddress, HybridLattice.Hybrid]): TracingControl[Exp, HybridValue, HybridAddress] = control match {
    case TracingControlEval(exp) => TracingControlEval(exp)
    case TracingControlKont(ka) => TracingControlKont(convertKontAddress(ka))
    case TracingControlError(string) => TracingControlError(string)
  }

  def convertKontAddress(address: KontAddr): KontAddr = address match {
    case address : NormalKontAddress[Exp, HybridAddress] =>
      NormalKontAddress(address.exp, HybridAddress.convertAddress(address.addr))
    case HaltKontAddress => HaltKontAddress
  }

  def convertState(oldSem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
                   newSem: Semantics[Exp, HybridValue, HybridAddress, Time]):
  (ConvertedControl[Exp, HybridValue, HybridAddress], Store[HybridAddress, HybridValue], KontStore[KontAddr], KontAddr, Time) = {
    val newρ = convertEnvironment(ρ)
    var newσ = Store.empty[HybridAddress, HybridLattice.Hybrid]
    def addToNewStore(tuple: (HybridAddress, HybridValue)): Boolean = {
      val newAddress = HybridAddress.convertAddress(tuple._1)
      val newValue = convertValue(σ)(tuple._2)
      newσ = newσ.extend(newAddress, newValue)
      true
    }
    σ.forall(addToNewStore)
    val newA = convertKontAddress(a)
    val newV = convertValue(σ)(v)
    val newVStack = vStack.map({
      case StoreVal(v) => StoreVal[HybridValue, HybridAddress](convertValue(σ)(v))
      case StoreEnv(ρ) => StoreEnv[HybridValue, HybridAddress](convertEnvironment(ρ))
    })
    val startKontAddress = control match {
      case TracingControlEval(_) | TracingControlError(_) => a
      case TracingControlKont(ka) => ka
    }
    val convertedKontStore = oldSem.newConvertKStore(newSem, kstore, ρ, startKontAddress, newVStack)
    val newKStore = convertedKontStore.map(convertKontAddress, newSem.convertFrame(HybridAddress.convertAddress, convertValue(σ)))
    val newControl = control match {
      case TracingControlEval(exp) =>
        ConvertedControlEval[Exp, HybridValue, HybridAddress](exp, newρ)
      case TracingControlKont(ka) =>
        ConvertedControlKont[Exp, HybridValue, HybridAddress](newV)
    }
    (newControl, newσ, newKStore, newA, t)
  }



  def generateTraceInformation(action: Action[Exp, HybridValue, HybridAddress]): Option[TraceInformation[HybridValue]] = action match {
    case ActionPrimCallT(_, _, _) =>
      Some(PrimitiveAppliedInfo(v, vStack))
    case _ =>
      None
  }

  def concretableState = this

  def subsumes(that: TracingProgramState[Exp, HybridValue, HybridAddress, Time]): Boolean = that match {
    case that: ProgramState[Exp, Time] =>
      concreteSubsumes(that)
    case _ => false
  }
}