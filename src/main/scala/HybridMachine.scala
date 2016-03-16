/**
 * Implementation of a CESK machine following the AAM approach (Van Horn, David,
 * and Matthew Might. "Abstracting abstract machines." ACM Sigplan
 * Notices. Vol. 45. No. 9. ACM, 2010).
 *
 * A difference with the paper is that we separate the continuation store
 * (KontStore) from the value store (Store). That simplifies the implementation
 * of both stores, and the only change it induces is that we are not able to
 * support first-class continuation as easily (we don't support them at all, but
 * they could be added).
 *
 * Also, in the paper, a CESK state is made of 4 components: Control,
 * Environment, Store, and Kontinuation. Here, we include the environment in the
 * control component, and we distinguish "eval" states from "continuation"
 * states. An eval state has an attached environment, as an expression needs to
 * be evaluated within this environment, whereas a continuation state only
 * contains the value reached.
 */

class HybridMachine[Exp : Expression, Time : Timestamp](override val sem : SemanticsTraced[Exp, HybridLattice.Hybrid, HybridAddress, Time])
    extends EvalKontMachineTraced[Exp, HybridLattice.Hybrid, HybridAddress, Time](sem) {


  var ACTIONS_EXECUTED : sem.Trace = List()
  
  type HybridValue = HybridLattice.Hybrid

  type TraceInstruction = sem.TraceInstruction
  type TraceInstructionStates = (TraceInstruction, Option[ProgramState])
  type TraceWithStates = List[TraceInstructionStates]
  type TraceWithoutStates = sem.Trace
  type AssertedTrace = (TraceWithoutStates, TraceWithStates)
  
  def name = "HybridMachine"

  val TRACING_THRESHOLD = 0

  /** The primitives are defined in AbstractValue.scala and are available through the Primitives class */
  val primitives = new Primitives[HybridAddress, HybridValue]()

  val tracerContext : TracerContext[Exp, HybridValue, HybridAddress, Time] =
    new TracerContext[Exp, HybridValue, HybridAddress, Time](sem, new TraceOptimizer[Exp, HybridValue, HybridAddress, Time](sem, this))

  /**
   * The store used for continuations is a KontStore (defined in
   * Kontinuation.scala). It is parameterized by continuation addresses, that
   * are element of the KontAddress typeclass.
   */
  trait KontAddr
  case class NormalKontAddress(exp: Exp, addr: HybridAddress) extends KontAddr {
    override def toString = s"NormalKontAddress($exp)"
  }
  object HaltKontAddress extends KontAddr {
    override def toString = "HaltKontAddress"
  }

  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }
  /**
    * Or it can be a continuation component, where a value has been reached and a
    * continuation should be popped from the stack to continue the evaluation
    */
  case class ControlKont(ka : KontAddr) extends Control {
    override def toString = s"ko($ka)"
    override def toString(store: Store[HybridAddress, HybridValue]) = s"ko($ka)"
    def subsumes(that: Control) = that match {
      case ControlKont(ka2) => ka == ka2
      case _ => false
    }
  }

  object Converter {

    val valueConverter : AbstractConcreteToAbstractType = new AbstractConcreteToAbstractType

    def convertValue(σ : Store[HybridAddress, HybridLattice.Hybrid])(value : HybridValue) : HybridValue = value match {
      case HybridLattice.Left(v) => HybridLattice.Right(valueConverter.convert[Exp](v, σ))
      case HybridLattice.Right(v) => value
      case HybridLattice.Prim(p) => value
    }

    def convertEnvironment(env : Environment[HybridAddress]) : Environment[HybridAddress] =
      new Environment[HybridAddress](env.content.map { tuple => (tuple._1, HybridAddress.convertAddress(tuple._2))})

    def convertControl(control : Control, σ : Store[HybridAddress, HybridLattice.Hybrid]) : Control = control match {
      case ControlEval(exp) => ControlEval(exp)
      case ControlKont(ka) => ControlKont(convertKontAddress(ka))
      case ControlError(string) => ControlError(string)
    }

    def convertKontAddress(address : KontAddr) : KontAddr = address match {
      case NormalKontAddress(exp, addr) => NormalKontAddress(exp, HybridAddress.convertAddress(addr))
      case HaltKontAddress => HaltKontAddress
    }

    def convertState(s : ProgramState) : (ProgramState, ProgramState) = s match {
      case ProgramState(control, ρ, σ, kstore, a, t, v, vStack) =>
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
        (s, ProgramState(newControl, newρ, newσ, newKStore, newA, t, newV, newVStack))
    }
  }

  private def getOperandType(operand : HybridValue) : AbstractType = operand match {
    case HybridLattice.Left(AbstractConcrete.AbstractFloat(_)) => AbstractType.AbstractFloat
    case HybridLattice.Left(AbstractConcrete.AbstractInt(_)) => AbstractType.AbstractInt
    case _ => AbstractType.AbstractTop
  }

  def checkValuesTypes(operands : List[HybridValue]) : AbstractType = {
    operands.foldLeft(AbstractType.AbstractBottom : AbstractType)({ (operandsTypes, operand) =>
      if (operandsTypes == AbstractType.AbstractBottom) {
        getOperandType(operand)
      } else if (operandsTypes == getOperandType(operand)) {
        operandsTypes
      } else {
        AbstractType.AbstractTop
      }
    })
  }

  case class RestartSpecializedPrimCall(originalPrim : HybridValue, n : Integer, fExp : Exp, argsExps : List[Exp]) extends RestartPoint[Exp, HybridValue, HybridAddress]

  def popStack[A](stack : List[A]) : (A, List[A]) = (stack.head, stack.tail)
  def popStackItems[A](stack : List[A], n : Integer) : (List[A], List[A]) = stack.splitAt(n)

  def replaceTc(state: ExecutionState, tc : tracerContext.TracerContext) =
    new ExecutionState(state.ep, state.ps, tc, state.tn)

  trait InstructionStep
  case class NormalInstructionStep(newState : ProgramState, instruction : sem.TraceInstruction) extends InstructionStep
  case class GuardFailed(rp : RestartPoint[Exp, HybridValue, HybridAddress]) extends InstructionStep
  case class TraceEnded(rp : RestartPoint[Exp, HybridValue, HybridAddress]) extends InstructionStep

  def restart(restartPoint: RestartPoint[Exp, HybridValue, HybridAddress], state : ProgramState) : ProgramState = restartPoint match {
    case RestartGuardFailed(newControlExp) =>
      ProgramState(ControlEval(newControlExp), state.ρ, state.σ, state.kstore,
        state.a, state.t, state.v, state.vStack)
    case RestartGuardDifferentClosure(action) =>
      handleClosureRestart(state, action)
    case RestartGuardDifferentPrimitive(action) =>
      handlePrimitiveRestart(state, action)
    case RestartTraceEnded() => state
    case RestartSpecializedPrimCall(originalPrim, n, fExp, argsExps) =>
      val primAppliedState = applyPrimitive(state, originalPrim, n, fExp, argsExps)
      applyTrace(primAppliedState, List(ActionPopKontTraced()))
  }

  def doActionStepInTraced(state : ProgramState, action : Action[Exp, HybridValue, HybridAddress]) : ProgramState = state match {
    case ProgramState(control, ρ, σ, kstore, a, t, v, vStack) =>
      action match {
        case ActionStepInTraced(fexp, e, args, argsv, n, frame, _, _) =>
          val next = NormalKontAddress(e, addr.variable("__kont__", t)) // Hack to get infinite number of addresses in concrete mode
          val (vals, newVStack) = popStackItems(vStack, n)
          val clo = vals.last.getVal
          try {
            val (ρ2, σ2) = sem.bindClosureArgs(clo, argsv.zip(vals.init.reverse.map(_.getVal)), σ, t).head
            ProgramState(ControlEval(e), ρ2, σ2, kstore.extend(next, Kont(frame, a)), next, time.tick(t, fexp), v, StoreEnv(ρ2) :: newVStack)
          } catch {
            case e: sem.InvalidArityException =>
              ProgramState(ControlError(s"Arity error when calling $fexp. (${args.length} arguments expected, got ${n - 1})"), ρ, σ, kstore, a, t, v, newVStack)
          }
      }
  }

  def handleClosureRestart(state : ProgramState, action : Action[Exp, HybridValue, HybridAddress]) : ProgramState =
    doActionStepInTraced(state, action)

  def applyPrimitive(state : ProgramState, operator: HybridValue, n : Integer, fExp : Exp, argsExps : List[Exp]) : ProgramState = {
    val (vals, newVStack) = popStackItems(state.vStack, n)
    val operands : List[HybridValue] = vals.take(n - 1).map(_.getVal)
    val primitive : Option[Primitive[HybridAddress, HybridValue]] = abs.getPrimitive[HybridAddress, HybridValue](operator)
    val result = primitive match {
      case Some(p) => p.call(fExp, argsExps.zip(operands.reverse), state.σ, state.t)
      case None => throw new NotAPrimitiveException(s"Operator $fExp not a primitive: $operator")
    }
    result match {
      case Left(error) =>
        throw new Exception(error)
      case Right((res, _)) =>
        state.copy(v = res, vStack = newVStack)
    }
  }

  def handlePrimitiveRestart(state : ProgramState, action : Action[Exp, HybridValue, HybridAddress]) : ProgramState = {
    action match {
      case ActionPrimCallTraced(n : Integer, fExp, argsExps) =>
        val (vals, newVStack) = popStackItems(state.vStack, n)
        val operator : HybridValue = vals.last.getVal
        val operands : List[HybridValue] = vals.take(n - 1).map(_.getVal)
        val primitive : Option[Primitive[HybridAddress, HybridValue]] = abs.getPrimitive[HybridAddress, HybridValue](operator)
        val result = primitive match {
          case Some(p) => p.call(fExp, argsExps.zip(operands.reverse), state.σ, state.t)
          case None => throw new NotAPrimitiveException(s"Operator $fExp not a primitive: $operator")
        }
        result match {
          case Left(error) => throw new Exception(error)
          case Right((v, newσ)) =>
            val primAppliedState = ProgramState(state.control, state.ρ, state.σ, state.kstore, state.a, state.t, v, newVStack)
            val next = if (primAppliedState.a == HaltKontAddress) { HaltKontAddress } else { primAppliedState.kstore.lookup(primAppliedState.a).head.next }
            ProgramState(ControlKont(primAppliedState.a), primAppliedState.ρ, primAppliedState.σ, primAppliedState.kstore, next, primAppliedState.t, primAppliedState.v, primAppliedState.vStack)
        }
    }
  }

  case class IncorrectStackSizeException() extends Exception
  case class IncorrectStorableException(message : String) extends Exception(message)
  case class VariableNotFoundException(variable : String) extends Exception(variable)
  case class NotAPrimitiveException(message : String) extends Exception(message)

  def applyAction(state : ProgramState, action : Action[Exp, HybridValue, HybridAddress]) : InstructionStep = {

    val control = state.control
    val ρ = state.ρ
    val σ = state.σ
    val kstore = state.kstore
    val a = state.a
    val t = state.t
    val v = state.v
    val vStack = state.vStack

    def handleGuard(guard: ActionGuardTraced[Exp, HybridValue, HybridAddress],
                    guardCheckFunction : HybridValue => Boolean) : InstructionStep = {
      if (guardCheckFunction(v)) {
        NormalInstructionStep(state, guard)
      } else {
        GuardFailed(guard.rp)
      }
    }

    def handleClosureGuard(guard : ActionGuardSameClosure[Exp, HybridValue, HybridAddress], currentClosure : HybridValue) : InstructionStep = {
      (guard.recordedClosure, currentClosure) match {
        case (HybridLattice.Left(AbstractConcrete.AbstractClosure(lam1, env1)), HybridLattice.Left(AbstractConcrete.AbstractClosure(lam2, env2))) =>
          if (lam1 == lam2) {
            NormalInstructionStep(state, guard)
          } else {
            println(s"Closure guard failed: recorded closure $lam1 does not match current closure $lam2")
            GuardFailed(guard.rp)
          }
        case (HybridLattice.Right(_), HybridLattice.Right(_)) =>
          NormalInstructionStep(state, guard)
        case _ =>
          throw new Exception("Mixing concrete values with abstract values")
      }
    }

    def handlePrimitiveGuard(guard : ActionGuardSamePrimitive[Exp, HybridValue, HybridAddress], currentPrimitive : HybridValue) = {
      if (guard.recordedPrimitive == currentPrimitive) {
        NormalInstructionStep(state, guard)
      } else {
        println(s"Primitive guard failed: recorded primitive ${guard.recordedPrimitive} does not match current primitive $currentPrimitive")
        GuardFailed(guard.rp)
      }
    }

    if (TracerFlags.PRINT_ACTIONS_EXECUTED) {
      ACTIONS_EXECUTED = ACTIONS_EXECUTED :+ action
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
        NormalInstructionStep(state, action)
      case ActionEndPrimCallTraced() =>
        NormalInstructionStep(state, action)
      /* When an error is reached, we go to an error state */
      case ActionErrorTraced(err) =>
        NormalInstructionStep(ProgramState(ControlError(err), ρ, σ, kstore, a, t, v, vStack), action)
      /* When a value needs to be evaluated, we go to an eval state */
      case ActionEvalTraced(e, _, _) =>
        NormalInstructionStep(ProgramState(ControlEval(e), ρ, σ, kstore, a, t, v, vStack), action)
      case ActionExtendEnvTraced(varName : String) =>
        val va = addr.variable(varName, t)
        val ρ1 = ρ.extend(name, va)
        val σ1 = σ.extend(va, v)
        NormalInstructionStep(ProgramState(control, ρ1, σ1, kstore, a, t, v, vStack), action)
      case ActionLookupVariableTraced(varName, _, _) =>
        val newV = σ.lookup(ρ.lookup(varName).get)
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, newV, vStack), action)
      case ActionPopKontTraced() =>
        val next = if (a == HaltKontAddress) { HaltKontAddress } else { kstore.lookup(a).head.next }
        NormalInstructionStep(ProgramState(ControlKont(a), ρ, σ, kstore, next, t, v, vStack), action)
      case ActionPrimCallTraced(n : Integer, fExp, argsExps) =>
        val (vals, _) = popStackItems(vStack, n)
        val operator = vals.last.getVal
        NormalInstructionStep(applyPrimitive(state, operator, n, fExp, argsExps), action)
      /* When a continuation needs to be pushed, push it in the continuation store */
      case ActionPushTraced(e, frame, _, _) =>
        val next = NormalKontAddress(e, addr.variable("__kont__", t)) // Hack to get infinite number of addresses in concrete mode
        NormalInstructionStep(ProgramState(ControlEval(e), ρ, σ, kstore.extend(next, Kont(frame, a)), next, t, v, vStack), action)
      case ActionPushValTraced() =>
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, v, StoreVal(v) :: vStack), action)
      case ActionReachedValueTraced(lit, _, _) =>
        NormalInstructionStep(ProgramState(control, ρ, σ, kstore, a, t, lit, vStack), action)
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
        val currentOperandsTypes = checkValuesTypes(operands)
        if (currentOperandsTypes == expectedType) {
          NormalInstructionStep(applyPrimitive(state, prim, n, fExp, argsExps), action)
        } else {
          GuardFailed(RestartSpecializedPrimCall(originalPrim, n, fExp, argsExps))
        }
      case ActionStartFunCallTraced() =>
        NormalInstructionStep(state, action)
      /* When a function is stepped in, we also go to an eval state */
      case ActionStepInTraced(fexp, e, args, argsv, n, frame, _, _) =>
        NormalInstructionStep(doActionStepInTraced(state, action), action)
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

  def applyTraceIntermediateResults(state : ProgramState, trace : sem.Trace) : List[ProgramState] = {
    trace.scanLeft(state)((currentState, action) => applyAction(currentState, action) match {
      case NormalInstructionStep(updatedState, _) => updatedState
      case _ => throw new Exception(s"Unexpected result while applying action $action")
    })
  }

  def applyTraceAndGetStates(state : ProgramState, trace : sem.Trace) : (ProgramState, TraceWithStates) = {
    val intermediateStates = applyTraceIntermediateResults(state, trace)
    val resultingState = intermediateStates.last
    (resultingState, trace.zip(intermediateStates.tail.map({ s => if (TracerFlags.APPLY_DETAILED_OPTIMIZATIONS) Some(s) else None})))
  }

  def applyTrace(state : ProgramState, trace : sem.Trace) : ProgramState = {
    applyTraceIntermediateResults(state, trace).last
  }

  class Storable(combo : Either[HybridValue, Environment[HybridAddress]]) {

    def getVal : HybridValue = combo match {
      case Left(value) =>
        value
      case Right(env) =>
        throw new IncorrectStorableException(s"Environment $env is not a Hybridvalue")
    }

    def getEnv : Environment[HybridAddress] = combo match {
      case Left(value) =>
        throw new IncorrectStorableException(s"Hybridvalue $value is not an environment")
      case Right(env) =>
        env
    }
  }

  case class StoreVal(value : HybridValue) extends Storable(Left(value))
  case class StoreEnv(env : Environment[HybridAddress]) extends Storable(Right(env))

  /**
   * A machine state is made of a control component, a value store, a
   * continuation store, and an address representing where the current
   * continuation lives.
   */
  case class ProgramState(control: Control, ρ : Environment[HybridAddress], σ: Store[HybridAddress, HybridValue], kstore: KontStore[KontAddr],
                          a: KontAddr, t: Time, v : HybridValue, vStack : List[Storable]) {

    /**
      * Builds the state with the initial environment and stores
      */
    def this(exp: Exp) = this(ControlEval(exp), Environment.empty[HybridAddress]().extend(primitives.forEnv),
      Store.initial(primitives.forStore, true),
      new KontStore[KontAddr](), HaltKontAddress, time.initial, abs.inject(false), Nil)

    override def toString = control match {
      case ControlKont(_) => s"ko($v)"
      case _ => control.toString()
    }

    /**
      * Checks whether a states subsumes another, i.e., if it is "bigger". This
      * is used to perform subsumption checking when exploring the state space,
      * in order to avoid exploring states for which another state that subsumes
      * them has already been explored.
      *
      * The tracer context is ignored in this check, because it only stores "meta" information not relevant to the actual program state.
      */
    def subsumes(that: ProgramState): Boolean = control.subsumes(that.control) && ρ.subsumes(that.ρ) && σ.subsumes(that.σ) &&
                                                a == that.a && kstore.subsumes(that.kstore) && t == that.t

    private def applyActionAbstract(state : ProgramState, action : Action[Exp, HybridValue, HybridAddress]) : Set[ProgramState] = {

      val control = state.control
      val ρ = state.ρ
      val σ = state.σ
      val kstore = state.kstore
      val a = state.a
      val t = state.t
      val v = state.v
      val vStack = state.vStack

      try {
        action match {
          case ActionPopKontTraced() =>
            val nextsSet = if (a == HaltKontAddress) { Set(HaltKontAddress) } else { kstore.lookup(a).map(_.next) }
            nextsSet.map(ProgramState(ControlKont(a), ρ, σ, kstore, _, t, v, vStack))
          case _ =>
            val result = applyAction(state, action)
            result match {
              case NormalInstructionStep(newState, _) => Set(newState)
              case GuardFailed(_) => Set(state) /* Guard failures (though they might happen) are not relevant here, so we ignore them */
              case _ => throw new Exception(s"Encountered an unexpected result while performing abstract interpretation: $result")
            }
        }
      } catch {
        case _ : IncorrectStackSizeException |
             _ : IncorrectStorableException |
             _ : VariableNotFoundException |
             _ : NotAPrimitiveException =>
          Set[ProgramState]()
      }

    }

    private def applyTraceAbstract(state : ProgramState, trace : sem.Trace) : Set[(ProgramState, sem.Trace)] = {
      val newStates = trace.foldLeft(Set(state))({ (currentStates, action) =>
       currentStates.flatMap(applyActionAbstract(_, action))
      })
      newStates.map({ (newState) => (newState, trace) })
    }

    private def integrate(a: KontAddr, interpreterReturns: Set[sem.InterpreterReturn]): Set[(ProgramState, sem.Trace)] = {
      interpreterReturns.flatMap({itpRet => itpRet match {
        case sem.InterpreterReturn(trace, _) =>
          applyTraceAbstract(this, trace)
      }})
    }

    def stepAbstract() : Set[(ProgramState, sem.Trace)] = {
      control match {
        /* In a eval state, call the semantic's evaluation method */
        case ControlEval(e) => integrate(a, sem.stepEval(e, ρ, σ, t))
        /* In a continuation state, if the value reached is not an error, call the
         * semantic's continuation method */
        case ControlKont(_) if abs.isError(v) => Set()
        case ControlKont(ka) => kstore.lookup(ka).flatMap({
          case Kont(frame, next) => integrate(next, sem.stepKont(v, frame, σ, t))
        })
        /* In an error state, the state is not able to make a step */
        case ControlError(_) => Set()
      }
    }

    /**
      * Computes the set of states that follow the current state
      */
    def doInterpreterStep() : Set[sem.InterpreterReturn] = control match {
      /* In a eval state, call the semantic's evaluation method */
      case ControlEval(e) => sem.stepEval(e, ρ, σ, t)
      /* In a continuation state, if the value reached is not an error, call the
       * semantic's continuation method */
      case ControlKont(_) if abs.isError(v) => Set()
      case ControlKont(ka) => sem.stepKont(v, kstore.lookup(ka).head.frame, σ, t)
      /* In an error state, the state is not able to make a step */
      case ControlError(_) => Set()
    }

    /**
      * Checks if the current state is a final state. It is the case if it
      * reached the end of the computation, or an error
      */
    def halted: Boolean = control match {
      case ControlEval(_) => false
      case ControlKont(HaltKontAddress) => true
      case ControlKont(_) => abs.isError(v)
      case ControlError(_) => true
    }
  }

  /*
   * Enumeration of possible execution phases
   */
  object ExecutionPhase extends Enumeration {
    type ExecutionPhase = Value
    val NI = Value("NormalInterpretation")
    val TR = Value("TraceRecording")
    val TE = Value("TraceExecution")
  }

  val NI = ExecutionPhase.NI
  val TE = ExecutionPhase.TE
  val TR = ExecutionPhase.TR

  case class ExecutionState(ep: ExecutionPhase.Value, ps : ProgramState, tc : tracerContext.TracerContext, tn : Option[tracerContext.TraceNode]) {

    type InterpreterReturn = SemanticsTraced[Exp, HybridLattice.Hybrid, HybridAddress, Time]#InterpreterReturn

    def runAssertions(assertions : TraceWithoutStates, state : ProgramState) : Boolean =
      assertions.foldLeft(true)({ (assertionsValid, assertion) =>
        if (! assertionsValid) {
          assertionsValid
        } else {
          assertion match {
            case ActionGuardAssertFreeVariable(variableName, expectedValue, _) =>
              val ρ = state.ρ
              val σ = state.σ
              ρ.lookup(variableName) match {
                case Some(address) =>
                  val currentValue = σ.lookup(address)
                  if (currentValue == expectedValue) {
                    true
                  } else {
                    println(s"Variable $variableName with current value $currentValue does not match its expected value $expectedValue")
                    false
                  }
                case None => false
              }
          }
        }
      })

    def checkTraceAssertions(state : ProgramState, tc : tracerContext.TracerContext, label : tracerContext.Label) : Boolean = {
      val traceNode = tracerContext.getTrace(tc, label)
      val assertions = traceNode.trace._1
      runAssertions(assertions, state)
    }

    def startExecutingTrace(state : ProgramState, tc : tracerContext.TracerContext, label : tracerContext.Label): ExecutionState = {
      println(s"Trace with label $label already exists; EXECUTING TRACE")
      val traceNode = tracerContext.getTrace(tc, label)
      val assertions = traceNode.trace._1
      ExecutionState(TE, state, tc, Some(traceNode))
    }

    def doTraceExecutingStep() : Set[ExecutionState] = {
      val (traceHead, updatedTraceNode) = tracerContext.stepTrace(tn.get, tc)
      val instructionStep = applyAction(ps, traceHead)
      instructionStep match {
        case NormalInstructionStep(newPs, _) =>
          Set(ExecutionState(ep, newPs, tc, Some(updatedTraceNode)))
        case GuardFailed(rp) =>
          println(s"Guard $traceHead failed")
          val psRestarted = restart(rp, ps)
          Set(ExecutionState(NI, psRestarted, tc, None))
        case TraceEnded(rp) =>
          println("Non-looping trace finished executing")
          val psRestarted = restart(rp, ps)
          Set(ExecutionState(NI, psRestarted, tc, None))
      }
    }

    type TracingSignal = SemanticsTraced[Exp, HybridValue, HybridAddress, Time]#TracingSignal

    def continueWithProgramState(state : ProgramState, trace : sem.Trace) : ExecutionState = {
      val updatedPs = applyTrace(state, trace)
      ExecutionState(ep, updatedPs, tc, tn)
    }

    def continueWithProgramStateTracing(state : ProgramState, trace : sem.Trace) : ExecutionState = {
      val (newState, traceWithStates) = applyTraceAndGetStates(ps, trace)
      val traceAppendedTc = tracerContext.appendTrace(tc, traceWithStates)
      ExecutionState(ep, newState, traceAppendedTc, tn)
    }

    def canStartLoopEncounteredRegular(newState : ProgramState, trace : sem.Trace, label : sem.Label) : ExecutionState = {
      val newTc = tracerContext.incLabelCounter(tc, label)
      val labelCounter = tracerContext.getLabelCounter(newTc, label)
      if (tracerContext.traceExists(newTc, label)) {
        if (checkTraceAssertions(newState, newTc, label)) {
          startExecutingTrace(newState, newTc, label)
        } else {
          ExecutionState(NI, newState, newTc, tn)
        }
      } else if (TracerFlags.DO_TRACING && labelCounter >= TRACING_THRESHOLD) {
        println(s"Started tracing $label")
        val someBoundVariables = trace.find(_.isInstanceOf[ActionStepInTraced[Exp, HybridValue, HybridAddress]]).flatMap({
          case ActionStepInTraced(_, _, args, _, _, _, _, _) => Some(args)
          case _ => None /* Should not happen */
        })
        val tcTRStarted = tracerContext.startTracingLabel(newTc, label, someBoundVariables.getOrElse(List[String]()))
        ExecutionState(TR, newState, tcTRStarted, tn)
      } else {
        ExecutionState(NI, newState, newTc, tn)
      }
    }

    def canStartLoopEncounteredTracing(state : ProgramState, trace : sem.Trace, label : sem.Label) : ExecutionState = {
      val (newState, traceWithStates) = applyTraceAndGetStates(ps, trace)
      val traceAppendedTc = tracerContext.appendTrace(tc, traceWithStates)
      if (tracerContext.isTracingLabel(traceAppendedTc, label)) {
        println(s"Stopped tracing $label; LOOP DETECTED")
        val tcTRStopped = tracerContext.stopTracing(traceAppendedTc, true, None)
        startExecutingTrace(newState, tcTRStopped, label)
      } else {
        ExecutionState(ep, newState, traceAppendedTc, tn)
      }
    }

    def canEndLoopEncounteredTracing(state : ProgramState, trace : sem.Trace, restartPoint: RestartPoint[Exp, HybridValue, HybridAddress], label : sem.Label) : ExecutionState = {
      val (newState, traceWithStates) = applyTraceAndGetStates(ps, trace)
      if (tracerContext.isTracingLabel(tc, label)) {
        println(s"Stopped tracing $label; NO LOOP DETECTED")
        val traceEndedInstruction = sem.endTraceInstruction(RestartTraceEnded())
        val tcTRStopped = tracerContext.stopTracing(tc, false, Some(traceEndedInstruction))
        ExecutionState(NI, newState, tcTRStopped, tn)
      } else {
        val traceAppendedTc = tracerContext.appendTrace(tc, traceWithStates)
        ExecutionState(TR, newState, traceAppendedTc, tn)
      }
    }

    def handleSignalRegular(state : ProgramState, trace : sem.Trace, signal : TracingSignal) : ExecutionState = signal match {
      case sem.TracingSignalEnd(_, _) => continueWithProgramState(state, trace)
      case sem.TracingSignalStart(label) => canStartLoopEncounteredRegular(applyTrace(state, trace), trace, label)
    }

    def handleSignalTracing(state : ProgramState, trace : sem.Trace, signal : TracingSignal) : ExecutionState = signal match {
      case sem.TracingSignalEnd(label, restartPoint) => canEndLoopEncounteredTracing(state, trace, restartPoint, label)
      case sem.TracingSignalStart(label) => canStartLoopEncounteredTracing(state, trace, label)
    }

    def handleResponseRegular(responses : Set[sem.InterpreterReturn]) : Set[ExecutionState] = {
      responses.map({
        case sem.InterpreterReturn(trace, sem.TracingSignalFalse()) => continueWithProgramState(ps, trace)
        case sem.InterpreterReturn(trace, signal) => handleSignalRegular(ps, trace, signal)
      })
    }

    def handleResponseTracing(responses : Set[sem.InterpreterReturn]) : Set[ExecutionState] = {
      responses.map({
        case sem.InterpreterReturn(trace, sem.TracingSignalFalse()) => continueWithProgramStateTracing(ps, trace)
        case sem.InterpreterReturn(trace, signal) => handleSignalTracing(ps, trace, signal)
      })
    }

    def stepConcrete() : Set[ExecutionState] = {
      ep match {
        case NI => handleResponseRegular(ps.doInterpreterStep())
        case TE => doTraceExecutingStep()
        case TR => handleResponseTracing(ps.doInterpreterStep())
      }
    }

  }

  case class AAMOutput[Annotation](halted: Set[ProgramState], count: Int, t: Double, graph: Option[Graph[ProgramState, Annotation]])
      extends Output[HybridValue] {

    /**
     * Returns the list of final values that can be reached
     */
    def finalValues = halted.flatMap(st => st.control match {
      case ControlKont(_) => Set[HybridValue](st.v)
      case _ => Set[HybridValue]()
    })

    /**
     * Checks if a halted state contains a value that subsumes @param v
     */
    def containsFinalValue(v: HybridValue) = finalValues.exists(v2 => abs.subsumes(v2, v))

    /**
     * Returns the number of visited states
     */
    def numberOfStates = count

    /**
     * Returns the time taken to evaluate the expression
     */
    def time = t

    /**
     * Outputs the graph in a dot file
     */
    def toDotFile(path: String) = graph match {
      case Some(g) => g.toDotFile(path, _.toString.take(40),
        (s) => if (halted.contains(s)) { "#FFFFDD" } else { s.control match {
          case ControlEval(_) => "#DDFFDD"
          case ControlKont(_) => "#FFDDDD"
          case ControlError(_) => "#FF0000"
        }}, _.toString.take(20))
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  var numberOfTracesRecorded = 0

  /**
    * Explores the state graph generated by State's step function.
    * @param todo is the set of states that needs to be visited
    * @param visited is the set of states already visited, they won't be visited again
    * @param halted is the set of final states reached
    * @param graph is the graph in its current form
    * @return the final states as well as the computed graph
    */
  @scala.annotation.tailrec
  private def loop(todo: Set[ExecutionState], visited: Set[ExecutionState],
                   halted: Set[ExecutionState], startingTime: Long, graph: Option[Graph[ProgramState, String]]): AAMOutput[String] = {
    todo.headOption match {
      case Some(s) =>
        if (visited.contains(s)) {
          /* If we already visited the state, or if it is subsumed by another already
           * visited state, we ignore it. The subsumption part reduces the
           * number of visited states but leads to non-determinism due to the
           * non-determinism of Scala's headOption (it seems so at least). */
          loop(todo.tail, visited, halted, startingTime, graph)
        } else if (s.ps.halted) {
          /* If the state is a final state, add it to the list of final states and
           * continue exploring the graph */
          loop(todo.tail, visited + s, halted + s, startingTime, graph)
        } else {
          /* Otherwise, compute the successors of this state, update the graph, and push
           * the new successors on the todo list */
          val succs = s.stepConcrete()
          assert(succs.size == 1)
          val succHead = succs.head
          val newGraph = graph.map(_.addEdges(succs.map(s2 => (s.ps, "", s2.ps))))
          val newExecutionPhase = succHead.ep
          if (TracerFlags.SWITCH_ABSTRACT && s.ep == TR && newExecutionPhase != TR) {
            numberOfTracesRecorded += 1
            val abstractOutput = switchToAbstract(todo.tail ++ succs, visited + s, halted, startingTime)
            abstractOutput.toDotFile(s"abstract_$numberOfTracesRecorded.dot")
            switchToConcrete()
            val traceOptimizedTc = tracerContext.applyStaticAnalysisOptimization(s.tc.traceInfo.get.label, succHead.tc, abstractOutput)
            val tcUpdatedSuccHead = succHead.copy(tc = traceOptimizedTc)
            loop(todo.tail ++ Set(tcUpdatedSuccHead), visited + s, halted, startingTime, newGraph)
          } else {
            loop(todo.tail ++ succs, visited + s, halted, startingTime, newGraph)
          }
        }
      case None =>

        if (TracerFlags.PRINT_ACTIONS_EXECUTED) {
          println("####### actions executed #######")
          for (ac <- ACTIONS_EXECUTED) {
            println(ac)
          }
          println("####### actions executed #######")
        }
        AAMOutput[String](halted.map(_.ps), visited.size,
        (System.nanoTime - startingTime) / Math.pow(10, 9), graph)
    }
  }

  private def switchToAbstract(todo: Set[ExecutionState], visited: Set[ExecutionState], halted: Set[ExecutionState],
                               startingTime: Long) : AAMOutput[sem.Trace] = {
    println("HybridMachine switching to abstract")
    def mapF(states : Set[(ProgramState, ProgramState)], annotation : String)(graph : Graph[ProgramState, String]) = {
      graph.addEdges(states.map(tuple => (tuple._1, annotation, tuple._2)))
    }
    HybridLattice.switchToAbstract
    HybridAddress.switchToAbstract
    val convTodo = todo.map((es) => Converter.convertState(es.ps))
    val convVisited = visited.map((es) => Converter.convertState(es.ps))
    val convHalted = halted.map((es) => Converter.convertState(es.ps))
    val newTodo = convTodo.map(_._2)
    val newVisited = convVisited.map(_._2)
    val newHalted = convHalted.map(_._2)
    val newGraph = new Graph[ProgramState, sem.Trace]()
    loopAbstract(newTodo, newVisited, newHalted, startingTime, newGraph)
  }

  private def switchToConcrete() : Unit = {
    println("HybridMachine switching to concrete")
    HybridLattice.switchToConcrete
    HybridAddress.switchToConcrete
  }

  @scala.annotation.tailrec
  private def loopAbstract(todo: Set[ProgramState], visited: Set[ProgramState],
                           halted: Set[ProgramState], startingTime: Long, graph: Graph[ProgramState, sem.Trace]): AAMOutput[sem.Trace] = {
    todo.headOption match {
      case Some(s) =>
        if (visited.contains(s) || visited.exists(s2 => s2.subsumes(s))) {
          /* If we already visited the state, or if it is subsumed by another already
           * visited state, we ignore it. The subsumption part reduces the
           * number of visited states but leads to non-determinism due to the
           * non-determinism of Scala's headOption (it seems so at least). */
          loopAbstract(todo.tail, visited, halted, startingTime, graph)
        } else if (s.halted) {
          /* If the state is a final state, add it to the list of final states and
           * continue exploring the graph */
          loopAbstract(todo.tail, visited + s, halted + s, startingTime, graph)
        } else {
          /* Otherwise, compute the successors of this state, update the graph, and push
           * the new successors on the todo list */
          val succs = s.stepAbstract()
          val newGraph = graph.addEdges(succs.map(s2 => (s, s2._2, s2._1)))
          loopAbstract(todo.tail ++ succs.map(_._1), visited + s, halted, startingTime, newGraph)
        }
      case None => AAMOutput[sem.Trace](halted, visited.size,
        (System.nanoTime - startingTime) / Math.pow(10, 9), Some(graph))
    }
  }

  def injectExecutionState(exp : Exp) : ExecutionState =
    new ExecutionState(NI, new ProgramState(exp), tracerContext.newTracerContext, None)

  /**
   * Performs the evaluation of an expression, possibly writing the output graph
   * in a file, and returns the set of final states reached
   */
  def eval(exp: Exp, graph: Boolean): Output[HybridValue] = {
    loop(Set(injectExecutionState(exp)), Set(), Set(), System.nanoTime,
      if (graph) { Some(new Graph[ProgramState, String]()) } else { None })
  }
}
