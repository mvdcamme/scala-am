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


  val PRINT_ACTIONS_EXECUTED = false
  var ACTIONS_EXECUTED : sem.Trace = List()
  
  type HybridValue = HybridLattice.Hybrid
  
  def name = "HybridMachine"

  val SWITCH_ABSTRACT = false
  val DO_TRACING = true

  val TRACING_THRESHOLD = 0

  val tracerContext : TracerContext[Exp, HybridValue, HybridAddress, Time] =
    new TracerContext[Exp, HybridValue, HybridAddress, Time](sem, new TraceOptimizer[Exp, HybridValue, HybridAddress, Time](sem))

  /** The primitives are defined in AbstractValue.scala and are available through the Primitives class */
  val primitives = new Primitives[HybridAddress, HybridValue]()

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
    override def toString() = s"ko(${ka})"
    override def toString(store: Store[HybridAddress, HybridValue]) = s"ko(${ka})"
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
          case Left(v) => Left(convertValue(σ)(v))
          case Right(ρ) => Right(convertEnvironment(ρ))
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

  def popStack[A](stack : List[A]) : (A, List[A]) = (stack.head, stack.tail)
  def popStackItems[A](stack : List[A], n : Integer) : (List[A], List[A]) = stack.splitAt(n)

  def replaceTc(state: ExecutionState, tc : tracerContext.TracerContext) =
    new ExecutionState(state.ep, state.ps, tc, state.tn)

  def applyAction(state : ProgramState, action : Action[Exp, HybridValue, HybridAddress]) : ProgramState = {

    val control = state.control
    val ρ = state.ρ
    val σ = state.σ
    val kstore = state.kstore
    val a = state.a
    val t = state.t
    val v = state.v
    val vStack = state.vStack

    def restart(restartPoint: sem.RestartPoint, state : ProgramState) : ProgramState = restartPoint match {
      case sem.RestartGuardFailed(newControlExp) =>
        ProgramState(ControlEval(newControlExp), state.ρ, state.σ, state.kstore,
              state.a, state.t, state.v, state.vStack)
      case sem.RestartTraceEnded() => state
    }

    def handleGuard(guard: ActionGuardTraced[Exp, HybridValue, HybridAddress, sem.RestartPoint],
                    guardCheckFunction : HybridValue => Boolean) : ProgramState = {
      if (guardCheckFunction(v)) {
        replaceTc(state, newTc)
      } else {
        println(s"Guard $guard failed")
        val tcStopped = tracerContext.stopExecuting(tc)
        val stateTEStopped = replaceTc(state, tcStopped)
        restart(guard.restartPoint, stateTEStopped)
      }
    }

    def doActionStepInTraced(state : ProgramState, action : Action[Exp, HybridValue, HybridAddress]) : ProgramState = state match {
      case ProgramState(control, ρ, σ, kstore, a, t, tc, v, vStack) =>
        action match {
          case ActionStepInTraced(fexp, e, args, argsv, n, frame, _, _) =>
            val next = NormalKontAddress(e, addr.variable("__kont__", t)) // Hack to get infinite number of addresses in concrete mode
          val (vals, newVStack) = popStackItems(vStack, n)
            val clo = vals.last.left.get
            try {
              val (ρ2, σ2) = sem.bindClosureArgs(clo, argsv.zip(vals.init.reverse.map(_.left.get)), σ, t).head
              ProgramState(ControlEval(e), ρ2, σ2, kstore.extend(next, Kont(frame, a)), next, time.tick(t, fexp), tc, v, Right(ρ) :: newVStack)
            } catch {
              case e: sem.InvalidArityException =>
                ProgramState(ControlError(s"Arity error when calling $fexp. (${args.length} arguments expected, got ${n - 1})"), ρ, σ, kstore, a, t, tc, v, newVStack)
            }
        }
    }

    def handleClosureRestart(state : ProgramState, guard : ActionGuardSameClosure[Exp, HybridValue, HybridAddress, sem.RestartPoint]) : ProgramState =
      doActionStepInTraced(state, guard.action)

    def handlePrimitiveRestart(state : ProgramState, guard : ActionGuardSamePrimitive[Exp, HybridValue, HybridAddress, sem.RestartPoint]) : ProgramState = {
      val action = guard.action
      action match {
        case ActionPrimCallTraced(n : Integer, fExp : Exp, argsExps : List[Exp]) =>
          val (vals, newVStack) = popStackItems(vStack, n)
          val operator : HybridValue = vals.last.left.get
          val operands : List[HybridValue] = vals.take(n - 1).map(_.left.get)
          val primitive : Option[Primitive[HybridAddress, HybridValue]] = abs.getPrimitive[HybridAddress, HybridValue](operator)
          val result = primitive match {
            case Some(primitive) => primitive.call(fExp, argsExps.zip(operands.reverse), σ, t)
            case None => throw new Exception(s"Operator $fExp not a primitive: $operator")
          }
          result match {
            case Left(error) => throw new Exception(error)
            case Right((v, newσ)) =>
              val primAppliedState = ProgramState(control, ρ, σ, kstore, a, t, newTc, v, newVStack)
              val next = if (primAppliedState.a == HaltKontAddress) { HaltKontAddress } else { primAppliedState.kstore.lookup(primAppliedState.a).head.next }
              ProgramState(ControlKont(primAppliedState.a), primAppliedState.ρ, primAppliedState.σ, primAppliedState.kstore, next, primAppliedState.t, primAppliedState.tc, primAppliedState.v, primAppliedState.vStack)
          }
      }
    }

    def handleClosureGuard(guard : ActionGuardSameClosure[Exp, HybridValue, HybridAddress, sem.RestartPoint], currentClosure : HybridValue) = {
      (guard.recordedClosure, currentClosure) match {
        case (HybridLattice.Left(AbstractConcrete.AbstractClosure(lam1, env1)), HybridLattice.Left(AbstractConcrete.AbstractClosure(lam2, env2))) =>
          if (lam1 == lam2) {
            state
          } else {
            println(s"Closure guard failed: recorded closure ${lam1} does not match current closure ${lam2}")
            val tcStopped = tracerContext.stopExecuting(tc)
            val stateTEStopped = replaceTc(state, tcStopped)
            handleClosureRestart(stateTEStopped, guard)
          }
        case (HybridLattice.Right(_), HybridLattice.Right(_)) =>
          state
        case _ =>
          throw new Exception("Mixing concrete values with abstract values")
      }
    }

    def handlePrimitiveGuard(guard : ActionGuardSamePrimitive[Exp, HybridValue, HybridAddress, sem.RestartPoint], currentPrimitive : HybridValue) = {
      if (guard.recordedPrimitive == currentPrimitive) {
        state
      } else {
        println(s"Primitive guard failed: recorded primitive ${guard.recordedPrimitive} does not match current primitive $currentPrimitive")
        val tcStopped = tracerContext.stopExecuting(tc)
        val stateTEStopped = replaceTc(state, tcStopped)
        handlePrimitiveRestart(stateTEStopped, guard)
      }
    }

    if (PRINT_ACTIONS_EXECUTED) {
      ACTIONS_EXECUTED = ACTIONS_EXECUTED :+ action
    }

    action match {
      case ActionAllocVarsTraced(variables) =>
        val addresses = variables.map(v => addr.variable(v, t))
        val (ρ1, σ1) = variables.zip(addresses).foldLeft((ρ, σ))({ case ((ρ, σ), (v, a)) => (ρ.extend(v, a), σ.extend(a, abs.bottom)) })
        ProgramState(control, ρ1, σ1, kstore, a, t, newTc, v, vStack)
      case ActionCreateClosureTraced(λ) =>
        val newClosure = abs.inject[Exp, HybridAddress]((λ, ρ))
        ProgramState(control, ρ, σ, kstore, a, t, newTc, newClosure, vStack)
      case ActionDefineVarsTraced(variables) =>
        val addresses = variables.map(v => addr.variable(v, t))
        val (vals, newVStack) = popStackItems(vStack, variables.length)
        val (ρ1, σ1) = vals.zip(variables.zip(addresses)).foldLeft((ρ, σ))({ case ((ρ, σ), (value, (v, a))) => (ρ.extend(v, a), σ.extend(a, value.left.get)) })
        ProgramState(control, ρ1, σ1, kstore, a, t, newTc, v, newVStack)
      /* When an error is reached, we go to an error state */
      case ActionError(err) => ProgramState(ControlError(err), ρ, σ, kstore, a, t, newTc, v, vStack)
      /* When a value needs to be evaluated, we go to an eval state */
      case ActionEvalTraced(e, _, _) => ProgramState(ControlEval(e), ρ, σ, kstore, a, t, newTc, v, vStack)
      case ActionExtendEnvTraced(varName : String) =>
        val va = addr.variable(varName, t)
        val ρ1 = ρ.extend(name, va)
        val σ1 = σ.extend(va, v)
        ProgramState(control, ρ1, σ1, kstore, a, t, tc, v, vStack)
      case ActionLiteralTraced(v) => ProgramState(control, ρ, σ, kstore, a, t, newTc, v, vStack)
      case ActionLookupVariableTraced(varName, _, _) =>
        val newV = σ.lookup(ρ.lookup(varName).get)
        ProgramState(control, ρ, σ, kstore, a, t, newTc, newV, vStack)
      case ActionPopKontTraced() =>
        val next = if (a == HaltKontAddress) { HaltKontAddress } else { kstore.lookup(a).head.next }
        ProgramState(ControlKont(a), ρ, σ, kstore, next, t, newTc, v, vStack)
      case ActionPrimCallTraced(n : Integer, fExp : Exp, argsExps : List[Exp]) =>
        val (vals, newVStack) = popStackItems(vStack, n)
        val operator : HybridValue = vals.last.left.get
        val operands : List[HybridValue] = vals.take(n - 1).map(_.left.get)
        val primitive : Option[Primitive[HybridAddress, HybridValue]] = abs.getPrimitive[HybridAddress, HybridValue](operator)
        val result = primitive match {
          case Some(primitive) => primitive.call(fExp, argsExps.zip(operands.reverse), σ, t)
          case None => throw new Exception(s"Operator $fExp not a primitive: $operator")
        }
        result match {
          case Left(error) => throw new Exception(error)
          case Right((v, newσ)) => ProgramState(control, ρ, σ, kstore, a, t, newTc, v, newVStack)
        }
      /* When a continuation needs to be pushed, push it in the continuation store */
      /*
      Replace frame to be pushed by fnction that takes a store and returns a new frame
       */
      case ActionPushTraced(e, frame, _, _) =>
        val next = NormalKontAddress(e, addr.variable("__kont__", t)) // Hack to get infinite number of addresses in concrete mode
        ProgramState(ControlEval(e), ρ, σ, kstore.extend(next, Kont(frame, a)), next, t, newTc, v, vStack)
      case ActionPushValTraced() => ProgramState(control, ρ, σ, kstore, a, t, newTc, v, Left(v) :: vStack)
      case ActionReachedValueTraced(v, _, _) => ProgramState(control, ρ, σ, kstore, a, t, newTc, v, vStack)
      case ActionRestoreEnvTraced() =>
        val (newρ, newVStack) = popStack(vStack)
        ProgramState(control, newρ.right.get, σ, kstore, a, t, newTc, v, newVStack)
      case ActionSaveEnvTraced() =>
        ProgramState(control, ρ, σ, kstore, a, t, newTc, v, Right(ρ) :: vStack)
      case ActionSetVarTraced(variable) => ProgramState(control, ρ, σ.update(ρ.lookup(variable).get, v), kstore, a, t, newTc, v, vStack)
      /* When a function is stepped in, we also go to an eval state */
      case ActionStepInTraced(fexp, e, args, argsv, n, frame, _, _) =>
        doActionStepInTraced(state, action)
      case action : ActionEndTrace[Exp, HybridValue, HybridAddress, sem.RestartPoint] =>
        val tcStopped = tracerContext.stopExecuting(newTc)
        val stateTEStopped = replaceTc(state, tcStopped)
        restart(action.restartPoint, stateTEStopped)
      case action : ActionGuardFalseTraced[Exp, HybridValue, HybridAddress, sem.RestartPoint] =>
        handleGuard(action, abs.isFalse)
      case action : ActionGuardTrueTraced[Exp, HybridValue, HybridAddress, sem.RestartPoint] =>
        handleGuard(action, abs.isTrue)
      case action : ActionGuardSameClosure[Exp, HybridValue, HybridAddress, sem.RestartPoint] =>
        val n = action.action.n
        handleClosureGuard(action, vStack(n - 1).left.get)
      case action : ActionGuardSamePrimitive[Exp, HybridValue, HybridAddress, sem.RestartPoint] =>
        val n = action.action.n
        handlePrimitiveGuard(action, vStack(n - 1).left.get)
    }
  }

  def applyTrace(state : ProgramState, trace : sem.Trace) : ProgramState = {
    trace.foldLeft(state)(applyAction)
  }

  type Storable = Either[HybridValue, Environment[HybridAddress]]

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
      new KontStore[KontAddr](), HaltKontAddress, time.initial, tracerContext.newTracerContext, abs.inject(false), Nil)

    override def toString() = control match {
      case ControlKont(_) => s"ko(${v})"
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
    def subsumes(that: ProgramState): Boolean = control.subsumes(that.control) && ρ.subsumes(that.ρ) && σ.subsumes(that.σ) && a == that.a && kstore.subsumes(that.kstore) && t == that.t

    private def integrate(a: KontAddr, interpreterReturns: Set[sem.InterpreterReturn]): Set[ProgramState] = {
      interpreterReturns.map({itpRet => itpRet match {
        case sem.InterpreterReturn(trace, _) => applyTrace(this, trace)
      }})
    }

    def stepAbstract() : Set[ProgramState] = {
      control match {
        /* In a eval state, call the semantic's evaluation method */
        case ControlEval(e) => integrate(a, sem.stepEval(e, ρ, σ, t))
        /* In a continuation state, if the value reached is not an error, call the
         * semantic's continuation method */
        case ControlKont(_) if abs.isError(v) => Set()
        case ControlKont(ka) => integrate(a, sem.stepKont(v, kstore.lookup(ka).head.frame, σ, t))
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

  case class ExecutionState(ep: ExecutionPhase.Value, ps : ProgramState, tc : tracerContext.TracerContext, tn : tracerContext.TraceNode) {

    type InterpreterReturn = SemanticsTraced[Exp, HybridLattice.Hybrid, HybridAddress, Time]#InterpreterReturn

    def startExecutingTrace(state : ProgramState, label : sem.Label): ExecutionState = {
      println(s"Trace with label $label already exists; EXECUTING TRACE")
      val tc = state.tc
      val traceNode = tracerContext.getTrace(tc, label)
      val tcTEStarted = new tracerContext.TracerContext(tc.label, tc.labelCounters, tc.traceNodes,
                                                        tc.trace, TE, Some(traceNode))
      replaceTc(state, tcTEStarted)
    }

    def doTraceExecutingStep() : Set[ProgramState] = {
      val (traceHead, newTc) = tracerContext.stepTrace(tc)
      val newState = applyAction(this, traceHead)
      val newNewTc = new tracerContext.TracerContext(newState.tc.label, newState.tc.labelCounters, newState.tc.traceNodes,
                                                     newState.tc.trace, newState.tc.executionPhase, newTc.traceExecuting)
      Set(replaceTc(newState, newNewTc))
    }

    type TracingSignal = SemanticsTraced[Exp, HybridValue, HybridAddress, Time]#TracingSignal
    type RestartPoint = SemanticsTraced[Exp, HybridValue, HybridAddress, Time]#RestartPoint

    def continueWithProgramState(state : ProgramState, trace : sem.Trace) =
      applyTrace(state, trace)

    def continueWithProgramStateTracing(state : ProgramState, trace : sem.Trace) : ProgramState = {
      val traceAppendedTc = tracerContext.appendTrace(state.tc, trace)
      val newState = applyTrace(state, trace)
      replaceTc(newState, traceAppendedTc)
    }

    def canStartLoopEncounteredRegular(newState : ProgramState, trace : sem.Trace, label : sem.Label) : ProgramState = {
      val newTc = tracerContext.incLabelCounter(newState.tc, label)
      val tcReplacedNewState = replaceTc(newState, newTc)
      val labelCounter = tracerContext.getLabelCounter(newTc, label)
      if (tracerContext.traceExists(newTc, label)) {
        startExecutingTrace(tcReplacedNewState, label)
      } else if (DO_TRACING && labelCounter >= TRACING_THRESHOLD) {
        println(s"Started tracing $label")
        val tcTRStarted = tracerContext.startTracingLabel(newTc, label)
        replaceTc(tcReplacedNewState, tcTRStarted)
      } else {
        tcReplacedNewState
      }
    }

    def canStartLoopEncounteredTracing(newState : ProgramState, trace : sem.Trace, label : sem.Label) : ProgramState = {
      val newStateTc = newState.tc
      val traceAppendedTc = tracerContext.appendTrace(newStateTc, trace)
      if (tracerContext.isTracingLabel(traceAppendedTc, label)) {
        println(s"Stopped tracing $label; LOOP DETECTED")
        val tcTRStopped = tracerContext.stopTracing(traceAppendedTc, true, None)
        val stateTRStopped = replaceTc(newState, tcTRStopped)
        startExecutingTrace(stateTRStopped, label)
      } else {
        replaceTc(newState, traceAppendedTc)
      }
    }

    def canEndLoopEncounteredTracing(newState : ProgramState, trace : sem.Trace, restartPoint: RestartPoint, label : sem.Label) : ProgramState = {
      val newStateTc = newState.tc
      if (tracerContext.isTracingLabel(newStateTc, label)) {
        println(s"Stopped tracing $label; NO LOOP DETECTED")
        val tcTRStopped = tracerContext.stopTracing(newStateTc, false, Some(tracerContext.semantics.RestartTraceEnded()))
        replaceTc(newState, tcTRStopped)
      } else {
        val traceAppendedTc = tracerContext.appendTrace(newStateTc, trace)
        replaceTc(newState, traceAppendedTc)
      }
    }

    def handleSignalRegular(state : ProgramState, trace : sem.Trace, signal : TracingSignal) : ProgramState = signal match {
      case sem.TracingSignalEnd(_, _) => continueWithProgramState(state, trace)
      case sem.TracingSignalStart(label) => canStartLoopEncounteredRegular(applyTrace(state, trace), trace, label)
    }

    def handleSignalTracing(newState : ProgramState, trace : sem.Trace, signal : TracingSignal) : ProgramState = signal match {
      case sem.TracingSignalEnd(label, restartPoint) => canEndLoopEncounteredTracing(newState, trace, restartPoint, label)
      case sem.TracingSignalStart(label) => canStartLoopEncounteredTracing(newState, trace, label)
    }

    def handleResponseRegular(responses : Set[sem.InterpreterReturn]) : Set[ProgramState] = {
      responses.map({
        case sem.InterpreterReturn(trace, sem.TracingSignalFalse()) => continueWithProgramState(this, trace)
        case sem.InterpreterReturn(trace, signal) => handleSignalRegular(this, trace, signal)
      })
    }

    def handleResponseTracing(responses : Set[sem.InterpreterReturn]) : Set[ProgramState] = {
      responses.map({
        case sem.InterpreterReturn(trace, sem.TracingSignalFalse()) => continueWithProgramStateTracing(this, trace)
        case sem.InterpreterReturn(trace, signal) => handleSignalTracing(applyTrace(this, trace), trace, signal)
      })
    }

    def stepConcrete() : Set[ProgramState] = {
      val executionPhase = tc.executionPhase
      executionPhase match {
        case tracerContext.NI => handleResponseRegular(doInterpreterStep())
        case tracerContext.TE => doTraceExecutingStep()
        case tracerContext.TR => handleResponseTracing(doInterpreterStep())
      }
    }

  }

  case class AAMOutput(halted: Set[ProgramState], count: Int, t: Double, graph: Option[Graph[ProgramState, String]])
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
  private def loop(todo: Set[ProgramState], visited: Set[ProgramState],
                   halted: Set[ProgramState], startingTime: Long, graph: Option[Graph[ProgramState, String]]): AAMOutput = {
    todo.headOption match {
      case Some(s) =>
        val previousExecutionPhase = s.tc.executionPhase
        if (visited.contains(s)) {
          /* If we already visited the state, or if it is subsumed by another already
           * visited state, we ignore it. The subsumption part reduces the
           * number of visited states but leads to non-determinism due to the
           * non-determinism of Scala's headOption (it seems so at least). */
          loop(todo.tail, visited, halted, startingTime, graph)
        } else if (s.halted) {
          /* If the state is a final state, add it to the list of final states and
           * continue exploring the graph */
          loop(todo.tail, visited + s, halted + s, startingTime, graph)
        } else {
          /* Otherwise, compute the successors of this state, update the graph, and push
           * the new successors on the todo list */
          val succs = s.stepConcrete()
          val newExecutionPhase = succs.head.tc.executionPhase
          if (SWITCH_ABSTRACT && previousExecutionPhase == tracerContext.TR && newExecutionPhase != tracerContext.TR) {
            numberOfTracesRecorded += 1
            val abstractOutput = switchToAbstract(todo, visited, halted, startingTime, graph)
            abstractOutput.toDotFile(s"abstract_$numberOfTracesRecorded.dot")
            switchToConcrete()
          }
          val newGraph = graph.map(_.addEdges(succs.map(s2 => (s, "", s2))))
          loop(todo.tail ++ succs, visited + s, halted, startingTime, newGraph)
        }
      case None =>

        if (PRINT_ACTIONS_EXECUTED) {
          println("####### actions executed #######")
          for (ac <- ACTIONS_EXECUTED) {
            println(ac)
          }
          println("####### actions executed #######")
        }
        AAMOutput(halted, visited.size,
        (System.nanoTime - startingTime) / Math.pow(10, 9), graph)
    }
  }

  private def switchToAbstract(todo: Set[ProgramState], visited: Set[ProgramState], halted: Set[ProgramState],
                               startingTime: Long, graph: Option[Graph[ProgramState, String]]) : AAMOutput = {
    println("HybridMachine switching to abstract")
    def mapF(states : Set[(ProgramState, ProgramState)], annotation : String)(graph : Graph[ProgramState, String]) = {
      graph.addEdges(states.map(tuple => (tuple._1, annotation, tuple._2)))
    }
    HybridLattice.switchToAbstract
    HybridAddress.switchToAbstract
    val convTodo = todo.map(Converter.convertState)
    val convVisited = visited.map(Converter.convertState)
    val convHalted = halted.map(Converter.convertState)
    val newTodo = convTodo.map(_._2)
    val newVisited = convVisited.map(_._2)
    val newHalted = convHalted.map(_._2)
    val newGraph = graph.map(mapF(convTodo, "Converted todo")_ compose mapF(convVisited, "Converted visited")_ compose mapF(convHalted, "Converted halted")_)
    loopAbstract(newTodo, newVisited, newHalted, startingTime, newGraph)
  }

  private def switchToConcrete() : Unit = {
    println("HybridMachine switching to concrete")
    HybridLattice.switchToConcrete
    HybridAddress.switchToConcrete
  }

  @scala.annotation.tailrec
  private def loopAbstract(todo: Set[ProgramState], visited: Set[ProgramState],
                           halted: Set[ProgramState], startingTime: Long, graph: Option[Graph[ProgramState, String]]): AAMOutput = {
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
          val newGraph = graph.map(_.addEdges(succs.map(s2 => (s, "", s2))))
          loopAbstract(todo.tail ++ succs, visited + s, halted, startingTime, newGraph)
        }
      case None => AAMOutput(halted, visited.size,
        (System.nanoTime - startingTime) / Math.pow(10, 9), graph)
    }
  }

  /**
   * Performs the evaluation of an expression, possibly writing the output graph
   * in a file, and returns the set of final states reached
   */
  def eval(exp: Exp, graph: Boolean): Output[HybridValue] = {
    loop(Set(new ProgramState(exp)), Set(), Set(), System.nanoTime,
      if (graph) { Some(new Graph[ProgramState, String]()) } else { None })
  }
}
