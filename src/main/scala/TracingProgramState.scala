trait InstructionStep[Exp, Abs, Addr] {
  def getState: TracingProgramState[Exp, Abs, Addr] =
    throw new Exception(s"Unexpected result: $this does not have a resulting state")
}
case class NormalInstructionStep[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (newState : TracingProgramState[Exp, Abs, Addr], action : Action[Exp, Abs, Addr]) extends InstructionStep[Exp, Abs, Addr] {
  override def getState: TracingProgramState[Exp, Abs, Addr] = newState
}
case class GuardFailed[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (rp : RestartPoint[Exp, Abs, Addr]) extends InstructionStep[Exp, Abs, Addr]
case class TraceEnded[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (rp : RestartPoint[Exp, Abs, Addr]) extends InstructionStep[Exp, Abs, Addr]

case class IncorrectStackSizeException() extends Exception
case class VariableNotFoundException(variable : String) extends Exception(variable)
case class NotAPrimitiveException(message : String) extends Exception(message)

trait TracingProgramState[Exp, Abs, Addr] {
  type HybridValue = HybridLattice.Hybrid

  def step(): InstructionStep[Exp, Abs, Addr]
  def applyAction(action: Action[Exp, Abs, Addr]): InstructionStep[Exp, Abs, Addr]
  def restart(restartPoint: RestartPoint[Exp, HybridValue, HybridAddress]): TracingProgramState[Exp, Abs, Addr]

  def halted: Boolean

  def convertState(): (TracingProgramState[Exp, Abs, Addr], TracingProgramState[Exp, Abs, Addr])

  def generateTraceInformation(action: Action[Exp, Abs, Addr]): Option[TraceInformation[HybridValue]]

  /**
    * Checks whether a states subsumes another, i.e., if it is "bigger". This
    * is used to perform subsumption checking when exploring the state space,
    * in order to avoid exploring states for which another state that subsumes
    * them has already been explored.
    */
  def subsumes(that: TracingProgramState[Exp, Abs, Addr]): Boolean
}

/**
  * A machine state is made of a control component, a value store, a
  * continuation store, and an address representing where the current
  * continuation lives.
  */
case class ProgramState[Exp : Expression, Time : Timestamp]
  (control: TracingControl[Exp, HybridLattice.Hybrid, HybridAddress],
   ρ : Environment[HybridAddress],
   σ: Store[HybridAddress, HybridLattice.Hybrid],
   kstore: KontStore[KontAddr],
   a: KontAddr,
   t: Time,
   v : HybridLattice.Hybrid,
   vStack : List[Storable]) extends TracingProgramState[Exp, HybridLattice.Hybrid, HybridAddress] {

  val abs = implicitly[AbstractValue[HybridValue]]
  val addr = implicitly[Address[HybridAddress]]
  val time = implicitly[Timestamp[Time]]
  val sem = implicitly[SemanticsTraced[Exp, HybridValue, HybridAddress, Time]]

  /** The primitives are defined in AbstractValue.scala and are available through the Primitives class */
  val primitives = new Primitives[HybridAddress, HybridValue]()

  case class RestartSpecializedPrimCall(originalPrim: HybridValue, n: Integer, fExp: Exp, argsExps: List[Exp]) extends RestartPoint[Exp, HybridValue, HybridAddress]

  def popStack[A](stack : List[A]) : (A, List[A]) = (stack.head, stack.tail)
  def popStackItems[A](stack : List[A], n : Integer) : (List[A], List[A]) = stack.splitAt(n)

  def restart(restartPoint: RestartPoint[Exp, HybridValue, HybridAddress]): TracingProgramState[Exp, HybridValue, HybridAddress] = restartPoint match {
    case RestartGuardFailed(newControlExp) =>
      ProgramState(TracingControlEval(newControlExp), ρ, σ, kstore, a, t, v, vStack)
    case RestartGuardDifferentClosure(action) =>
      handleClosureRestart(action)
    case RestartGuardDifferentPrimitive(action) =>
      handlePrimitiveRestart(action)
    case RestartTraceEnded() => this
    case RestartSpecializedPrimCall(originalPrim, n, fExp, argsExps) =>
      val primAppliedState = applyPrimitive(originalPrim, n, fExp, argsExps)
      primAppliedState.applyAction(ActionPopKontTraced()).getState
  }

  /**
    * Computes the set of states that follow the current state
    */
  def step(): Option[sem.InterpreterReturn] = control match {
    /* In a eval state, call the semantic's evaluation method */
    case TracingControlEval(e) => Some(sem.stepEval(e, ρ, σ, t))
    /* In a continuation state, if the value reached is not an error, call the
     * semantic's continuation method */
    case TracingControlKont(_) if abs.isError(v) => None
    case TracingControlKont(ka) => Some(sem.stepKont(v, kstore.lookup(ka).head.frame, σ, t))
    /* In an error state, the state is not able to make a step */
    case TracingControlError(_) => None
  }

  def doActionStepInTraced(action : Action[Exp, HybridValue, HybridAddress]) : ProgramState[Exp, Time] = action match {
    case ActionStepInTraced(fexp, _, _, argsv, n, frame, _, _) =>
      val (vals, newVStack) = popStackItems(vStack, n)
      val clo = vals.last.getVal
      try {
        val (ρ2, σ2, e) = sem.bindClosureArgs(clo, argsv.zip(vals.init.reverse.map(_.getVal)), σ, t).head
        val next = NormalKontAddress(e, addr.variable("__kont__", t)) // Hack to get infinite number of addresses in concrete mode
        ProgramState(TracingControlEval[Exp, HybridValue, HybridAddress](e), ρ2, σ2, kstore.extend(next, Kont(frame, a)), next, time.tick(t, fexp), v, StoreEnv(ρ2) :: newVStack)
      } catch {
        case e: sem.InvalidArityException =>
          ProgramState(TracingControlError(s"Arity error when calling $fexp. got ${n - 1})"), ρ, σ, kstore, a, t, v, newVStack)
      }
  }

  def handleClosureRestart(action : Action[Exp, HybridValue, HybridAddress]) : ProgramState[Exp, Time] =
    doActionStepInTraced(action)

  def applyPrimitive(operator: HybridValue, n : Integer, fExp : Exp, argsExps : List[Exp]) : ProgramState[Exp, Time] = {
    val (vals, newVStack) = popStackItems(vStack, n)
    val operands : List[HybridValue] = vals.take(n - 1).map(_.getVal)
    val primitive : Option[Primitive[HybridAddress, HybridValue]] = abs.getPrimitive[HybridAddress, HybridValue](operator)
    val result = primitive match {
      case Some(p) => p.call(fExp, argsExps.zip(operands.reverse), σ, t)
      case None => throw new NotAPrimitiveException(s"Operator $fExp not a primitive: $operator")
    }
    result match {
      case Left(error) =>
        throw new Exception(error)
      case Right((res, _)) =>
        ProgramState(control, ρ, σ, kstore, a, t, res, newVStack)
    }
  }

  def handlePrimitiveRestart(action : Action[Exp, HybridValue, HybridAddress]) : ProgramState[Exp, Time] = {
    action match {
      case ActionPrimCallTraced(n : Integer, fExp, argsExps) =>
        val (vals, newVStack) = popStackItems(vStack, n)
        val operator : HybridValue = vals.last.getVal
        val operands : List[HybridValue] = vals.take(n - 1).map(_.getVal)
        val primitive : Option[Primitive[HybridAddress, HybridValue]] = abs.getPrimitive[HybridAddress, HybridValue](operator)
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

  def applyAction(action: Action[Exp, HybridValue, HybridAddress]): InstructionStep[Exp, HybridValue, HybridAddress] = {

    def handleGuard(guard: ActionGuardTraced[Exp, HybridValue, HybridAddress],
                    guardCheckFunction: HybridValue => Boolean): InstructionStep[Exp, HybridValue, HybridAddress] = {
      if (guardCheckFunction(v)) {
        NormalInstructionStep(this, guard)
      } else {
        GuardFailed(guard.rp)
      }
    }

    def handleClosureGuard(guard : ActionGuardSameClosure[Exp, HybridValue, HybridAddress], currentClosure : HybridValue): InstructionStep[Exp, HybridValue, HybridAddress] = {
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
          throw new Exception("Mixing concrete values with abstract values")
      }
    }

    def handlePrimitiveGuard(guard : ActionGuardSamePrimitive[Exp, HybridValue, HybridAddress], currentPrimitive : HybridValue): InstructionStep[Exp, HybridValue, HybridAddress] = {
      if (guard.recordedPrimitive == currentPrimitive) {
        NormalInstructionStep(this, guard)
      } else {
        Logger.log(s"Primitive guard failed: recorded primitive ${guard.recordedPrimitive} does not match current primitive $currentPrimitive", Logger.D)
        GuardFailed(guard.rp)
      }
    }

    action match {
      case ActionAllocVarsTraced(variables) =>
        val addresses = variables.map(v => addr.variable(v, t))
        val (ρ1, σ1) = variables.zip(addresses).foldLeft((ρ, σ))({ case ((ρ2, σ2), (currV, currA)) => (ρ2.extend(currV, currA), σ2.extend(currA, abs.bottom)) })
        NormalInstructionStep(ProgramState(control, ρ1, σ1, kstore, a, t, v, vStack), action)
      case ActionCreateClosureTraced(λ) =>
        val newClosure = abs.inject[Exp, HybridAddress]((λ, ρ))
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, newClosure, vStack), action)
      case ActionDefineVarsTraced(variables) =>
        val addresses = variables.map(v => addr.variable(v, t))
        val (vals, newVStack) = popStackItems(vStack, variables.length)
        val (ρ1, σ1) = vals.zip(variables.zip(addresses)).foldLeft((ρ, σ))({ case ((ρ2, σ2), (value, (currV, currA))) => (ρ2.extend(currV, currA), σ2.extend(currA, value.getVal)) })
        NormalInstructionStep(ProgramState(control, ρ1, σ1, kstore, a, t, v, newVStack), action)
      case ActionDropValsTraced(n) =>
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, v, vStack.drop(n)), action)
      case ActionEndClosureCallTraced() =>
        NormalInstructionStep(this, action)
      case ActionEndPrimCallTraced() =>
        NormalInstructionStep(this, action)
      /* When an error is reached, we go to an error state */
      case ActionErrorTraced(err) =>
        NormalInstructionStep(ProgramState(TracingControlError(err), ρ, σ, kstore, a, t, v, vStack), action)
      /* When a value needs to be evaluated, we go to an eval state */
      case ActionEvalTraced(e, _, _) =>
        NormalInstructionStep(ProgramState(TracingControlEval(e), ρ, σ, kstore, a, t, v, vStack), action)
      case ActionExtendEnvTraced(varName : String) =>
        val va = addr.variable(varName, t)
        val ρ1 = ρ.extend(varName, va)
        val σ1 = σ.extend(va, v)
        NormalInstructionStep(ProgramState(control, ρ1, σ1, kstore, a, t, v, vStack), action)
      case ActionLookupVariableTraced(varName, _, _) =>
        val newV = σ.lookup(ρ.lookup(varName).get)
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, newV, vStack), action)
      case ActionLookupVariablePushTraced(varName, _, _) =>
        val newV = σ.lookup(ρ.lookup(varName).get)
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, newV, StoreVal(newV) :: vStack), action)
      case ActionPopKontTraced() =>
        val next = if (a == HaltKontAddress) { HaltKontAddress } else { kstore.lookup(a).head.next }
        NormalInstructionStep(ProgramState(TracingControlKont(a), ρ, σ, kstore, next, t, v, vStack), action)
      case ActionPrimCallTraced(n : Integer, fExp, argsExps) =>
        val (vals, _) = popStackItems(vStack, n)
        val operator = vals.last.getVal
        NormalInstructionStep(applyPrimitive(operator, n, fExp, argsExps), action)
      /* When a continuation needs to be pushed, push it in the continuation store */
      case ActionPushTraced(e, frame, _, _) =>
        val next = NormalKontAddress(e, addr.variable("__kont__", t)) // Hack to get infinite number of addresses in concrete mode
        NormalInstructionStep(ProgramState(TracingControlEval(e), ρ, σ, kstore.extend(next, Kont(frame, a)), next, t, v, vStack), action)
      case ActionPushValTraced() =>
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, v, StoreVal(v) :: vStack), action)
      case ActionReachedValueTraced(lit, _, _) =>
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, lit, vStack), action)
      case ActionReachedValuePushTraced(lit, _, _) =>
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, lit, StoreVal(lit) :: vStack), action)
      case ActionRestoreEnvTraced() =>
        try {
          val (newρ, newVStack) = popStack(vStack)
          NormalInstructionStep(ProgramState(control, newρ.getEnv, σ, kstore, a, t, v, newVStack), action)
        } catch {
          case e : java.lang.IndexOutOfBoundsException =>
            throw new IncorrectStackSizeException()
        }
      case ActionSaveEnvTraced() =>
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, v, StoreEnv(ρ) :: vStack), action)
      case ActionSetVarTraced(variable) =>
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
      case ActionStartFunCallTraced() =>
        NormalInstructionStep(this, action)
      /* When a function is stepped in, we also go to an eval state */
      case ActionStepInTraced(fexp, e, args, argsv, n, frame, _, _) =>
        NormalInstructionStep(doActionStepInTraced(action), action)
      case action : ActionEndTrace[Exp, HybridValue, HybridAddress] =>
        TraceEnded(action.restartPoint)
      case action : ActionGuardFalseTraced[Exp, HybridValue, HybridAddress] =>
        handleGuard(action, abs.isFalse)
      case action : ActionGuardTrueTraced[Exp, HybridValue, HybridAddress] =>
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
        handlePrimitiveGuard(action, vStack(n - 1).getVal)
    }

  }

  /**
    * Builds the state with the initial environment and stores
    */
  def this(exp: Exp) = this(TracingControlEval(exp), Environment.empty[HybridAddress]().extend(primitives.forEnv),
    Store.initial(primitives.forStore, true),
    new KontStore[KontAddr](), HaltKontAddress, time.initial, abs.inject(false), Nil)

  override def toString = control match {
    case TracingControlKont(_) => s"ko($v)"
    case _ => control.toString()
  }

  /**
    * Checks if the current state is a final state. It is the case if it
    * reached the end of the computation, or an error
    */
  def halted: Boolean = control match {
    case TracingControlEval(_) => false
    case TracingControlKont(HaltKontAddress) => true
    case TracingControlKont(_) => abs.isError(v)
    case TracingControlError(_) => true
  }

  val valueConverter : AbstractConcreteToAbstractType = new AbstractConcreteToAbstractType

  def convertValue(σ : Store[HybridAddress, HybridLattice.Hybrid])(value : HybridValue) : HybridValue = value match {
    case HybridLattice.Left(v) => HybridLattice.Right(valueConverter.convert[Exp](v, σ))
    case HybridLattice.Right(v) => value
    case HybridLattice.Prim(p) => value
  }

  def convertEnvironment(env : Environment[HybridAddress]) : Environment[HybridAddress] =
    new Environment[HybridAddress](env.content.map { tuple => (tuple._1, HybridAddress.convertAddress(tuple._2))})

  def convertControl(control : TracingControl[Exp, HybridValue, HybridAddress], σ : Store[HybridAddress, HybridLattice.Hybrid]): TracingControl[Exp, HybridValue, HybridAddress] = control match {
    case TracingControlEval(exp) => TracingControlEval(exp)
    case TracingControlKont(ka) => TracingControlKont(convertKontAddress(ka))
    case TracingControlError(string) => TracingControlError(string)
  }

  def convertKontAddress(address : KontAddr) : KontAddr = address match {
    case address : NormalKontAddress[Exp, HybridAddress] =>
      NormalKontAddress(address.exp, HybridAddress.convertAddress(address.addr))
    case HaltKontAddress => HaltKontAddress
  }

  def convertState(): (ProgramState[Exp, Time], ProgramState[Exp, Time]) = {
    val newControl = convertControl(control, σ)
    val newρ = convertEnvironment(ρ)
    var newσ = Store.empty[HybridAddress, HybridLattice.Hybrid]
    val newKStore = kstore.map(convertKontAddress, sem.convertFrame(HybridAddress.convertAddress, convertValue(σ)))
    val newA = convertKontAddress(a)
    val newV = convertValue(σ)(v)
    val newVStack = vStack.map({
      case StoreVal(v) => StoreVal(convertValue(σ)(v))
      case StoreEnv(ρ) => StoreEnv(convertEnvironment(ρ))
    })
    def addToNewStore(tuple: (HybridAddress, HybridValue)): Boolean = {
      val newAddress = HybridAddress.convertAddress(tuple._1)
      val newValue = convertValue(σ)(tuple._2)
      newσ = newσ.extend(newAddress, newValue)
      true
    }
    σ.forall(addToNewStore)
    (this, ProgramState(newControl, newρ, newσ, newKStore, newA, t, newV, newVStack))
  }

  def generateTraceInformation(action : Action[Exp, HybridValue, HybridAddress]): Option[TraceInformation[HybridValue]] = action match {
    case ActionPrimCallTraced(_, _, _) =>
      Some(PrimitiveAppliedInfo(v, vStack))
    case _ =>
      None
  }

  def subsumes(that: TracingProgramState[Exp, HybridValue, HybridAddress]): Boolean = that match {
    case that: ProgramState[Exp, Time] =>
      control.subsumes(that.control) && ρ.subsumes(that.ρ) && σ.subsumes(that.σ) &&
        a == that.a && kstore.subsumes(that.kstore) && t == that.t
    case _ => false
  }
}