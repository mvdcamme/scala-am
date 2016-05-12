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

class HybridMachine[Exp : Expression, Time : Timestamp]
  (override val sem: SemanticsTraced[Exp, HybridLattice.Hybrid, HybridAddress, Time],
   val tracingFlags: TracingFlags,
   injectProgramState: (Exp, Primitives[HybridAddress, HybridLattice.Hybrid], AbstractValue[HybridLattice.Hybrid], Timestamp[Time]) =>
                       ConcreteTracingProgramState[Exp, HybridLattice.Hybrid, HybridAddress, Time])
    extends EvalKontMachineTraced[Exp, HybridLattice.Hybrid, HybridAddress, Time](sem) {

  type HybridValue = HybridLattice.Hybrid

  type TraceInstruction = Action[Exp, HybridValue, HybridAddress]
  type TraceWithoutStates = List[TraceInstruction]
  type TraceInstructionInfo = (TraceInstruction, Option[TraceInformation[HybridValue]])
  type TraceWithInfos = List[TraceInstructionInfo]

  /** The primitives are defined in AbstractValue.scala and are available through the Primitives class */
  implicit val primitives = new Primitives[HybridAddress, HybridValue]()

  type PS = ConcreteTracingProgramState[Exp, HybridValue, HybridAddress, Time]

  case class TraceFull(startProgramState: PS, assertions: TraceWithoutStates, trace: TraceWithInfos)

  def name = "HybridMachine"

  val tracerContext: TracerContext[Exp, HybridValue, HybridAddress, Time] =
    new TracerContext[Exp, HybridValue, HybridAddress, Time](sem, new TraceOptimizer[Exp, HybridValue, HybridAddress, Time](sem, this), this)

  def applyTraceIntermediateResults(state: PS, trace: TraceWithoutStates): List[PS] = {
    trace.scanLeft(state)((currentState, action) => currentState.applyAction(sem, action) match {
      case NormalInstructionStep(updatedState, _) => updatedState
      case result => throw new Exception(s"Unexpected result while applying action $action; got result $result")
    })
  }

  def applyTraceAndGetStates(state: PS, trace: TraceWithoutStates): (PS, TraceWithInfos) = {
    val intermediateStates = applyTraceIntermediateResults(state, trace)
    val resultingState = intermediateStates.last
    val traceStatesZipped = trace.zip(intermediateStates.tail)
    val traceSomeTraceInfoZipped: TraceWithInfos = traceStatesZipped.map({ (instructionState) =>
      (instructionState._1, instructionState._2.generateTraceInformation(instructionState._1)) })
    (resultingState, traceSomeTraceInfoZipped)
  }

  def applyTrace(state: PS, trace: TraceWithoutStates): PS = {
    applyTraceIntermediateResults(state, trace).last
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

  case class ExecutionState(ep: ExecutionPhase.Value, ps: PS)(tc: tracerContext.TracerContext, tn: Option[tracerContext.TraceNode]) {

    def checkTraceAssertions(state: PS, tc: tracerContext.TracerContext, loopID: List[Exp]): Boolean = {
      val traceNode = tracerContext.getLoopTrace(tc, loopID)
      val assertions = traceNode.trace.assertions
      state.runAssertions(assertions)
    }

    def startExecutingTrace(state: PS, tc: tracerContext.TracerContext, loopID: List[Exp]): ExecutionState = {
      Logger.log(s"Trace for loop $loopID already exists; EXECUTING TRACE", Logger.D)
      val traceNode = tracerContext.getLoopTrace(tc, loopID)
      ExecutionState(TE, state)(tc, Some(traceNode))
    }

    private def handleGuardFailure(rp: RestartPoint[Exp, HybridValue, HybridAddress], guardID: Integer, loopID: List[Exp]): ExecutionState = {
      def restartPs(): PS = {
        ps.restart(sem, rp)
      }
      def resumeNormalInterpretation(psRestarted: PS): ExecutionState = {
        ExecutionState(NI, psRestarted)(tc, None)
      }
      if (tracerContext.guardTraceExists(tc, guardID)) {
        /* Guard trace exists: check assertions and then either execute the trace
         * or just go back to normal interpretation */
        val psRestarted = restartPs()
        Logger.log(s"Trace for guard $guardID already exists; EXECUTING GUARD TRACE", Logger.E)
        val traceNode = tracerContext.getGuardTrace(tc, guardID)
        val assertions = traceNode.trace.assertions
        if (psRestarted.runAssertions(assertions)) {
          /* Assertions are still valid -> execute the trace */
          ExecutionState(TE, psRestarted)(tc, Some(traceNode))
        } else {
          /* Assertions are no longer valid -> just resume normal interpretation */
          resumeNormalInterpretation(psRestarted)
        }
      } else if (tracingFlags.DO_TRACING && tracingFlags.DO_GUARD_TRACING) {
        Logger.log(s"Started tracing guard $guardID", Logger.E)
        val psRestarted = restartPs()
        val tcTRStarted = tracerContext.startTracingGuard(tc, loopID, guardID, List[String](), psRestarted)
        ExecutionState(TR, psRestarted)(tcTRStarted, tn)
      } else {
        /* Guard trace does not exist yet: start tracing it */
        resumeNormalInterpretation(restartPs())
      }
    }

    def doTraceExecutingStep(): ExecutionState = {
      val (traceHead, updatedTraceNode) = tracerContext.stepTrace(tn.get, tc)
      val instructionStep = ps.applyAction(sem, traceHead)
      instructionStep match {
        case NormalInstructionStep(newPs, _) =>
          ExecutionState(ep, newPs)(tc, Some(updatedTraceNode))
        case GuardFailed(rp, guardID) =>
          Logger.log(s"Guard $traceHead failed", Logger.D)
          handleGuardFailure(rp, guardID, tracerContext.getLoopID(tn.get.label))
        case TraceEnded(rp) =>
          Logger.log("Non-looping trace finished executing", Logger.D)
          val psRestarted = ps.restart(sem, rp)
          ExecutionState(NI, psRestarted)(tc, None)
      }
    }

    def continueWithProgramState(state: PS, trace: TraceWithoutStates): ExecutionState = {
      val updatedPs = applyTrace(state, trace)
      ExecutionState(ep, updatedPs)(tc, tn)
    }

    def continueWithProgramStateTracing(state: PS, trace: TraceWithoutStates): ExecutionState = {
      val (newState, traceWithInfos) = applyTraceAndGetStates(ps, trace)
      val traceAppendedTc = tracerContext.appendTrace(tc, traceWithInfos)
      ExecutionState(ep, newState)(traceAppendedTc, tn)
    }

    def canStartLoopEncounteredRegular(newState: PS, trace: TraceWithoutStates, loopID: List[Exp]): ExecutionState = {
      Logger.log(s"Regular phase: CanStartLoop encountered for loop $loopID", Logger.D)
      val newTc = tracerContext.incLabelCounter(tc, loopID)
      val labelCounter = tracerContext.getLabelCounter(newTc, loopID)
      Logger.log(s"Regular phase: labelcounter equals $labelCounter", Logger.D)
      if (tracerContext.loopTraceExists(newTc, loopID)) {
        if (checkTraceAssertions(newState, newTc, loopID)) {
          startExecutingTrace(newState, newTc, loopID)
        } else {
          ExecutionState(NI, newState)(newTc, tn)
        }
      } else if (tracingFlags.DO_TRACING && labelCounter >= tracingFlags.TRACING_THRESHOLD) {
        Logger.log(s"Started tracing $loopID", Logger.I)
        val someBoundVariables = trace.find(_.isInstanceOf[ActionStepInT[Exp, HybridValue, HybridAddress]]).flatMap({
          case ActionStepInT(_, _, args, _, _, _, _, _) => Some(args)
          case _ => None /* Should not happen */
        })
        val tcTRStarted = tracerContext.startTracingLoop(newTc, loopID, someBoundVariables.getOrElse(List[String]()), newState)
        ExecutionState(TR, newState)(tcTRStarted, tn)
      } else {
        ExecutionState(NI, newState)(newTc, tn)
      }
    }

    def canStartLoopEncounteredTracing(state: PS, trace: TraceWithoutStates, loopID: List[Exp]): ExecutionState = {
      Logger.log(s"Tracing phase: CanStartLoop encountered of loop $loopID", Logger.D)
      val (newState, traceWithStates) = applyTraceAndGetStates(ps, trace)
      val traceAppendedTc = tracerContext.appendTrace(tc, traceWithStates)
      if (tracerContext.isTracingLoop(traceAppendedTc, loopID)) {
        Logger.log(s"Stopped tracing $loopID; LOOP DETECTED", Logger.I)
        numberOfTracesRecorded += 1
        val analysisOutput = findAnalysisOutput(newState)
        val tcTRStopped = tracerContext.stopTracing(traceAppendedTc, true, None, analysisOutput)
        startExecutingTrace(newState, tcTRStopped, loopID)
      } else {
        ExecutionState(ep, newState)(traceAppendedTc, tn)
      }
    }

    def canEndLoopEncounteredTracing(state: PS, trace: List[Action[Exp, HybridValue, HybridAddress]],
                                     restartPoint: RestartPoint[Exp, HybridValue, HybridAddress], loopID: List[Exp]): ExecutionState = {
      val (newState, traceWithStates) = applyTraceAndGetStates(ps, trace)
      if (tracerContext.isTracingLoop(tc, loopID)) {
        Logger.log(s"Stopped tracing $loopID; NO LOOP DETECTED", Logger.I)
        numberOfTracesRecorded += 1
        val traceEndedInstruction = sem.endTraceInstruction(restartPoint)
        val analysisOutput = findAnalysisOutput(newState)
        val tcTRStopped = tracerContext.stopTracing(tc, false, Some(traceEndedInstruction), analysisOutput)
        ExecutionState(NI, newState)(tcTRStopped, tn)
      } else {
        val traceAppendedTc = tracerContext.appendTrace(tc, traceWithStates)
        ExecutionState(TR, newState)(traceAppendedTc, tn)
      }
    }

    def handleSignalRegular(state: PS, trace: List[Action[Exp, HybridValue, HybridAddress]], signal: TracingSignal[Exp, HybridValue, HybridAddress]): ExecutionState = signal match {
      case TracingSignalEnd(_, _) => continueWithProgramState(state, trace)
      case TracingSignalStart(loopID) => canStartLoopEncounteredRegular(applyTrace(state, trace), trace, loopID)
    }

    def handleSignalTracing(state: PS, trace: List[Action[Exp, HybridValue, HybridAddress]], signal: TracingSignal[Exp, HybridValue, HybridAddress]): ExecutionState = signal match {
      case TracingSignalEnd(loopID, restartPoint) => canEndLoopEncounteredTracing(state, trace, restartPoint, loopID)
      case TracingSignalStart(loopID) => canStartLoopEncounteredTracing(state, trace, loopID)
    }

    def handleResponseRegular(response: Step[Exp, HybridValue, HybridAddress]): ExecutionState = response match {
      case Step(trace, TracingSignalFalse()) => continueWithProgramState(ps, trace)
      case Step(trace, signal) => handleSignalRegular(ps, trace, signal)
    }

    def handleResponseTracing(response: Step[Exp, HybridValue, HybridAddress]): ExecutionState = response match {
      case Step(trace, TracingSignalFalse()) => continueWithProgramStateTracing(ps, trace)
      case Step(trace, signal) => handleSignalTracing(ps, trace, signal)
    }

    def stepConcrete(): ExecutionState = {
      ep match {
        case NI => handleResponseRegular(ps.step(sem).get)
        case TE => doTraceExecutingStep()
        case TR => handleResponseTracing(ps.step(sem).get)
      }
    }

  }

  case class AAMOutput[State <: TracingProgramState[Exp, HybridValue, HybridAddress, Time], Annotation](halted: Set[State], count: Int, t: Double, graph: Option[Graph[State, Annotation]])
      extends Output[HybridValue] {

    /**
     * Returns the list of final values that can be reached
     */
    def finalValues = halted.flatMap(st => st.finalValues())

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
        (s) => if (halted.contains(s)) { "#FFFFDD" } else { s.graphNodeColor }, _.toString.take(20))
      case None =>
        Logger.log("Not generating graph because no graph was computed", Logger.E)
    }
  }

  var numberOfTracesRecorded = 0

  /**
    * Explores the state graph generated by State's step function.
    * @param s is the set of states that needs to be visited
    * @param nrVisited is the number of states already visited
    * @param graph is the graph in its current form
    * @return the final states as well as the computed graph
    */
  @scala.annotation.tailrec
  private def loop(s: ExecutionState, nrVisited: Integer, startingTime: Long, graph: Option[Graph[PS, String]]): AAMOutput[PS, String] = {
    def endEvalLoop(): AAMOutput[PS, String] = {
      if (GlobalFlags.PRINT_ACTIONS_EXECUTED) {
        ActionLogger.printActions()
      }
      AAMOutput[PS, String](Set(s.ps), nrVisited,
        (System.nanoTime - startingTime) / Math.pow(10, 9), graph)
    }
    if (s.ps.halted) {
      /* If the state is a final state, add it to the list of final states and
       * continue exploring the graph */
      endEvalLoop()
    } else {
      /* Otherwise, compute the successors of this state, update the graph, and push
       * the new successors on the todo list */
      val succ = s.stepConcrete()
      val newGraph = graph.map(_.addEdge(s.ps, "", succ.ps))
      loop(succ, nrVisited + 1, startingTime, newGraph)
    }
  }

  private def switchToAbstract(currentProgramState: PS): Unit = { //AAMOutput[APS, TraceWithoutStates] = {
    Logger.log("HybridMachine switching to abstract", Logger.E)
    HybridLattice.switchToAbstract
    HybridAddress.switchToAbstract
    val aam = new AAM[Exp, HybridValue, HybridAddress, Time]
    val (control, store, kstore, a, t) = currentProgramState.convertState(sem)
    val convertedControl = control match {
      case ConvertedControlError(reason) => aam.ControlError(reason)
      case ConvertedControlEval(exp, env) => aam.ControlEval(exp, env)
      case ConvertedControlKont(v) => aam.ControlKont(v)
    }
    val startState = aam.State(convertedControl, store, kstore, a, t)
    aam.loop(Set(startState), Set(), Set(), System.nanoTime, None, sem.absSem)
  }

  private def switchToConcrete(): Unit = {
    Logger.log("HybridMachine switching to concrete", Logger.E)
    HybridLattice.switchToConcrete
    HybridAddress.switchToConcrete
  }



  private def runStaticAnalysis(currentProgramState: PS): Unit = {
    val analysisOutput = switchToAbstract(currentProgramState)
    switchToConcrete()
    analysisOutput
  }

  private def findAnalysisOutput(currentProgramState: PS): Option[AAMOutput[PS, TraceWithoutStates]] = {
    if (tracingFlags.SWITCH_ABSTRACT) {
      runStaticAnalysis(currentProgramState)
      None
//      val analysisOutput = runStaticAnalysis(currentProgramState)
//      analysisOutput.toDotFile(s"abstract_$numberOfTracesRecorded.dot")
//      Some(analysisOutput)
    } else {
      None
    }
  }

  def injectExecutionState(exp: Exp): ExecutionState =
    new ExecutionState(NI, injectProgramState(exp, primitives, abs, time))(tracerContext.newTracerContext, None)

  /**
   * Performs the evaluation of an expression, possibly writing the output graph
   * in a file, and returns the set of final states reached
   */
  def eval(exp: Exp, graph: Boolean): Output[HybridValue] = {
    loop(injectExecutionState(exp), 0, System.nanoTime,
      if (graph) { Some(new Graph[PS, String]()) } else { None })
  }
}
