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

class HybridMachine[Exp : Expression]
  (override val sem: SemanticsTraced[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T],
   constantsAnalysisLauncher: ConstantsAnalysisLauncher[Exp],
   val tracer: Tracer[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T],
   tracingFlags: TracingFlags,
   injectProgramState: (Exp) =>
                       ConcreteTracingProgramState[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T])
    extends EvalKontMachineTraced[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T](sem) {

  type HybridValue = HybridLattice.L

  type PS = ConcreteTracingProgramState[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T]

  def name = "HybridMachine"

  var stepCount: Integer = 0

  var staticBoundAddresses: Option[Set[HybridAddress.A]] = None

  def applyTraceIntermediateResults(state: PS, trace: tracer.TraceWithoutStates): List[PS] = {
    trace.scanLeft(state)((currentState, action) => currentState.applyAction(sem, action) match {
      case ActionStep(updatedState, _) => updatedState
      case result => println(s"Unexpected result while applying action $action; got result $result"); currentState
    })
  }

  def applyTraceAndGetStates(state: PS, trace: tracer.TraceWithoutStates): (PS, tracer.TraceWithInfos) = {
    val intermediateStates = applyTraceIntermediateResults(state, trace)
    val resultingState = intermediateStates.last
    val traceStatesZipped = trace.zip(intermediateStates.tail)
    val traceSomeTraceInfoZipped: tracer.TraceWithInfos = traceStatesZipped.map({ case (instruction, state) =>
      (instruction, state.generateTraceInformation(instruction)) })
    (resultingState, traceSomeTraceInfoZipped)
  }

  def applyTrace(state: PS, trace: tracer.TraceWithoutStates): PS = {
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

  case class TracerState(ep: ExecutionPhase.Value, ps: PS)
                        (tc: tracer.TracerContext,
                         tn: Option[tracer.TraceNode[TraceFull[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T]]]) {

    def checkTraceAssertions(state: PS, tc: tracer.TracerContext, loopID: List[Exp]): Option[PS] = {
      val traceNode = tracer.getLoopTrace(tc, loopID)
      val assertions = traceNode.trace.assertions
      state.runHeader(sem, assertions)
    }

    def startExecutingTrace(state: PS, tc: tracer.TracerContext, loopID: List[Exp]): TracerState = {
      Logger.log(s"Trace for loop $loopID already exists; EXECUTING TRACE", Logger.V)
      val traceNode = tracer.getLoopTrace(tc, loopID)
      TracerState(TE, state)(tc, Some(traceNode))
    }

    private def handleGuardFailure(rp: RestartPoint[Exp, HybridValue, HybridAddress.A], guardID: Integer, loopID: List[Exp]): TracerState = {
      def restartPs(): PS = {
        ps.restart(sem, rp)
      }
      def resumeNormalInterpretation(psRestarted: PS): TracerState = {
        TracerState(NI, psRestarted)(tc, None)
      }
      if (tracer.guardTraceExists(tc, guardID)) {
        /* Guard trace exists: check assertions and then either execute the trace
         * or just go back to normal interpretation */
        val psRestarted = restartPs()
        Logger.log(s"Trace for guard $guardID already exists; EXECUTING GUARD TRACE", Logger.V)
        val traceNode = tracer.getGuardTrace(tc, guardID)
        val assertions = traceNode.trace.assertions
        psRestarted.runHeader(sem, assertions) match {
          case Some(newState) =>
          /* Assertions are still valid -> execute the trace */
          TracerState(TE, newState)(tc, Some(traceNode))
          case None =>
          /* Assertions are no longer valid -> just resume normal interpretation */
          resumeNormalInterpretation(psRestarted)
        }
      } else if (tracingFlags.DO_TRACING && tracingFlags.DO_GUARD_TRACING) {
        Logger.log(s"Started tracing guard $guardID", Logger.I)
        val curTraceLabel = tn.get.label
        val psRestarted = restartPs()
        val tcTRStarted = tracer.startTracingGuard(tc, loopID, guardID, curTraceLabel, List[(String, HybridAddress.A)](), psRestarted)
        TracerState(TR, psRestarted)(tcTRStarted, tn)
      } else {
        /* Guard trace does not exist yet: start tracing it */
        resumeNormalInterpretation(restartPs())
      }
    }

    def doTraceExecutingStep(): TracerState = {

      val (traceHead, updatedTraceNode, mustRerunHeader) = tracer.stepTrace(tn.get, tc)

      def executeStep(ps: PS): TracerState = {
        val instructionStep = ps.applyAction(sem, traceHead)
        instructionStep match {
          case ActionStep(newPs, _) =>
            TracerState(ep, newPs)(tc, Some(updatedTraceNode))
          case GuardFailed(rp, guardID) =>
            Logger.log(s"Guard $traceHead failed", Logger.D)
            handleGuardFailure(rp, guardID, tracer.getLoopID(tn.get.label))
          case TraceEnded(rp) =>
            Logger.log("Non-looping trace finished executing", Logger.D)
            val psRestarted = ps.restart(sem, rp)
            TracerState(NI, psRestarted)(tc, None)
        }
      }

      if (mustRerunHeader) {
        ps.runHeader(sem, updatedTraceNode.trace.assertions) match {
          case Some(headerRunPs) =>
            executeStep(headerRunPs)
          case None =>
            TracerState(NI, ps)(tc, None)
        }
      } else {
        executeStep(ps)
      }
    }

    def continueWithProgramState(state: PS, trace: tracer.TraceWithoutStates): TracerState = {
      val updatedPs = applyTrace(state, trace)
      TracerState(ep, updatedPs)(tc, tn)
    }

    def continueWithProgramStateTracing(state: PS, trace: tracer.TraceWithoutStates): TracerState = {
      val (newState, traceWithInfos) = applyTraceAndGetStates(ps, trace)
      val traceAppendedTc = tracer.appendTrace(tc, traceWithInfos)
      TracerState(ep, newState)(traceAppendedTc, tn)
    }

    def canStartLoopEncounteredRegular(newState: PS, trace: tracer.TraceWithoutStates, loopID: List[Exp]): TracerState = {
      Logger.log(s"Regular phase: CanStartLoop encountered for loop $loopID", Logger.D)
      val newTc = tracer.incLabelCounter(tc, loopID)
      val labelCounter = tracer.getLabelCounter(newTc, loopID)
      Logger.log(s"Regular phase: labelcounter equals $labelCounter", Logger.D)
      if (tracer.loopTraceExists(newTc, loopID)) {
        checkTraceAssertions(newState, newTc, loopID) match {
          case Some(newState2) =>
            startExecutingTrace(newState2, newTc, loopID)
          case None =>
            TracerState(NI, newState)(newTc, tn)
        }
      } else if (tracingFlags.DO_TRACING && labelCounter >= tracingFlags.TRACING_THRESHOLD) {
        Logger.log(s"Started tracing $loopID", Logger.I)
        val someBoundVariables = trace.find(_.isInstanceOf[ActionStepInT[Exp, HybridValue, HybridAddress.A]]).flatMap({
          case ActionStepInT(_, _, args, _, _, _, _, _) => Some(args)
          case _ => None /* Should not happen */
        })
        val initialBoundVariables = someBoundVariables match {
          case Some(variables) => variables.map( (variable) => (variable, newState.ρ.lookup(variable).get))
          case None => List[(String, HybridAddress.A)]()
        }
        val tcTRStarted = tracer.startTracingLoop(newTc, loopID, initialBoundVariables, newState)
        TracerState(TR, newState)(tcTRStarted, tn)
      } else {
        TracerState(NI, newState)(newTc, tn)
      }
    }

    def canStartLoopEncounteredTracing(state: PS,
                                       trace: tracer.TraceWithoutStates, loopID: List[Exp]): TracerState = {
      Logger.log(s"Tracing phase: CanStartLoop encountered of loop $loopID", Logger.D)
      val (newState, traceWithStates) = applyTraceAndGetStates(ps, trace) /* TODO better to use state? Shouldn't matter though */
      val traceAppendedTc = tracer.appendTrace(tc, traceWithStates)
      if (tracer.isTracingLoop(traceAppendedTc, loopID)) {
        Logger.log(s"Stopped tracing $loopID; LOOP DETECTED", Logger.I)
        numberOfTracesRecorded += 1
        val tcTRStopped = tracer.stopTracing(traceAppendedTc, true, None, state)
        checkTraceAssertions(newState, tcTRStopped, loopID) match  {
          case Some(headerExecutedState) =>
            startExecutingTrace(headerExecutedState, tcTRStopped, loopID)
          case None =>
            TracerState(NI, newState)(tcTRStopped, tn)
        }
      } else {
        TracerState(ep, newState)(traceAppendedTc, tn)
      }
    }

    def canEndLoopEncounteredTracing(state: PS, trace: List[ActionT[Exp, HybridValue, HybridAddress.A]],
                                     restartPoint: RestartPoint[Exp, HybridValue, HybridAddress.A],
                                     loopID: List[Exp]): TracerState = {
      Logger.log(s"Tracing phase: CanEndLoop encountered for loop $loopID", Logger.D)
      val (newState, traceWithStates) = applyTraceAndGetStates(ps, trace)
      if (tracer.isTracingLoop(tc, loopID)) {
        Logger.log(s"Stopped tracing $loopID; NO LOOP DETECTED", Logger.I)
        numberOfTracesRecorded += 1
        val traceEndedInstruction = sem.endTraceInstruction(restartPoint)
        val tcTRStopped = tracer.stopTracing(tc, false, Some(traceEndedInstruction), state)
        TracerState(NI, newState)(tcTRStopped, tn)
      } else {
        val traceAppendedTc = tracer.appendTrace(tc, traceWithStates)
        TracerState(TR, newState)(traceAppendedTc, tn)
      }
    }

    def handleSignalRegular(state: PS,
                            trace: List[ActionT[Exp, HybridValue, HybridAddress.A]],
                            signal: TracingSignal[Exp, HybridValue, HybridAddress.A]): TracerState = signal match {
      case SignalEndLoop(loopID, _) =>
        Logger.log(s"Regular phase: CanEndLoop encountered for loop $loopID", Logger.D)
        continueWithProgramState(state, trace)
      case SignalStartLoop(loopID) => canStartLoopEncounteredRegular(applyTrace(state, trace), trace, loopID)
    }

    def handleSignalTracing(state: PS,
                            trace: List[ActionT[Exp, HybridValue, HybridAddress.A]],
                            signal: TracingSignal[Exp, HybridValue, HybridAddress.A]): TracerState = signal match {
      case SignalEndLoop(loopID, restartPoint) => canEndLoopEncounteredTracing(state, trace, restartPoint, loopID)
      case SignalStartLoop(loopID) => canStartLoopEncounteredTracing(state, trace, loopID)
    }

    def handleResponseRegular(response: InterpreterStep[Exp, HybridValue, HybridAddress.A]): TracerState = response match {
      case InterpreterStep(trace, SignalFalse()) => continueWithProgramState(ps, trace)
      case InterpreterStep(trace, signal) => handleSignalRegular(ps, trace, signal)
    }

    def handleResponseTracing(response: InterpreterStep[Exp, HybridValue, HybridAddress.A]): TracerState = response match {
      case InterpreterStep(trace, SignalFalse()) => continueWithProgramStateTracing(ps, trace)
      case InterpreterStep(trace, signal) => handleSignalTracing(ps, trace, signal)
    }

    def stepConcrete(): TracerState = {
      stepCount += 1
      ep match {
        case NI => handleResponseRegular(ps.step(sem).get)
        case TE => doTraceExecutingStep()
        case TR => handleResponseTracing(ps.step(sem).get)
      }
    }

  }

  case class HybridOutput[State <: TracingProgramState[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T], Annotation]
    (halted: Set[State], count: Int, t: Double, graph: Option[Graph[State, Annotation]], timedOut: Boolean)
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
      case Some(g) => g.toDotFile(path, node => List(scala.xml.Text(node.toString.take(40))),
        (s) => if (halted.contains(s)) { Colors.Yellow } else { s.graphNodeColor}, _ => List())
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
  private def loop(s: TracerState, nrVisited: Integer, startingTime: Long, graph: Option[Graph[PS, String]], timeout: Option[Long]): HybridOutput[PS, String] = {
    val analysis_interval = 20
    def endEvalLoop(timeout: Boolean): HybridOutput[PS, String] = {
      if (GlobalFlags.PRINT_ACTIONS_EXECUTED) {
        ActionLogger.printActions()
      }
      HybridOutput[PS, String](Set(s.ps), nrVisited,
        (System.nanoTime - startingTime) / Math.pow(10, 9), graph, timeout)
    }

    if (timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
      endEvalLoop(true)
    } else if (s.ps.halted) {
      /* If the state is a final state, add it to the list of final states and
       * continue exploring the graph */
      endEvalLoop(false)
    } else {
      /* Otherwise, compute the successors of this state, update the graph, and push
       * the new successors on the todo list */
//      if (stepCount % analysis_interval == 0) {
//        println(stepCount)
//        constantsAnalysisLauncher.runStaticAnalysis(s.ps, Set())
//      }
      val succ = s.stepConcrete()
      val newGraph = graph.map(_.addEdge(s.ps, "", succ.ps))
      loop(succ, nrVisited + 1, startingTime, newGraph, timeout)
    }
  }

  def injectExecutionState(exp: Exp): TracerState =
    new TracerState(NI, injectProgramState(exp))(tracer.newTracerContext, None)

  /**
   * Performs the evaluation of an expression, possibly writing the output graph
   * in a file, and returns the set of final states reached
   */
  def eval(exp: Exp, graph: Boolean, timeout: Option[Long]): Output[HybridValue] = {
    val initialState = injectProgramState(exp)
    if (tracingFlags.DO_INITIAL_ANALYSIS) {
      val analysisResult = constantsAnalysisLauncher.runInitialStaticAnalysis(initialState)
      analysisResult match {
        case ConstantAddresses(constants, nonConstants) =>
          staticBoundAddresses = Some(nonConstants.asInstanceOf[Set[HybridAddress.A]])
        case _ =>
      }
    }
    loop(injectExecutionState(exp), 0, System.nanoTime,
      if (graph) {
        Some(new Graph[PS, String]())
      } else {
        None
      }, timeout)
  }
}