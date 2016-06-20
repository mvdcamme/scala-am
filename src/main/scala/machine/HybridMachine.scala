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
  (override val sem: SemanticsTraced[Exp, HybridLattice.L, HybridAddress.A, Time],
                                   val tracingFlags: TracingFlags,
   injectProgramState: (Exp, Timestamp[Time]) =>
                       ConcreteTracingProgramState[Exp, HybridLattice.L, HybridAddress.A, Time])
    extends EvalKontMachineTraced[Exp, HybridLattice.L, HybridAddress.A, Time](sem) {

  type HybridValue = HybridLattice.L

  type TraceInstruction = Action[Exp, HybridValue, HybridAddress.A]
  type TraceWithoutStates = List[TraceInstruction]
  type TraceInstructionInfo = (TraceInstruction, Option[TraceInformation[HybridValue]])
  type TraceWithInfos = List[TraceInstructionInfo]

  type PS = ConcreteTracingProgramState[Exp, HybridValue, HybridAddress.A, Time]

  case class TraceFull(startProgramState: PS, assertions: TraceWithoutStates, trace: TraceWithInfos)

  def name = "HybridMachine"

  val tracerContext: Tracer[Exp, HybridValue, HybridAddress.A, Time] =
    new Tracer[Exp, HybridValue, HybridAddress.A, Time](sem, new TraceOptimizer[Exp, HybridValue, HybridAddress.A, Time](sem, this), this)

  def applyTraceIntermediateResults(state: PS, trace: TraceWithoutStates): List[PS] = {
    trace.scanLeft(state)((currentState, action) => currentState.applyAction(sem, action) match {
      case ActionStep(updatedState, _) => updatedState
      case result => println(s"Unexpected result while applying action $action; got result $result"); currentState
      //case result => throw new Exception(s"Unexpected result while applying action $action; got result $result")
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

  case class TracerState(ep: ExecutionPhase.Value, ps: PS)(tc: tracerContext.TracerContext, tn: Option[tracerContext.TraceNode[tracerContext.TraceFull]]) {

    def checkTraceAssertions(state: PS, tc: tracerContext.TracerContext, loopID: List[Exp]): Option[PS] = {
      val traceNode = tracerContext.getLoopTrace(tc, loopID)
      val assertions = traceNode.trace.assertions
      state.runHeader(sem, assertions)
    }

    def startExecutingTrace(state: PS, tc: tracerContext.TracerContext, loopID: List[Exp]): TracerState = {
      Logger.log(s"Trace for loop $loopID already exists; EXECUTING TRACE", Logger.D)
      val traceNode = tracerContext.getLoopTrace(tc, loopID)
      TracerState(TE, state)(tc, Some(traceNode))
    }

    private def handleGuardFailure(rp: RestartPoint[Exp, HybridValue, HybridAddress.A], guardID: Integer, loopID: List[Exp]): TracerState = {
      def restartPs(): PS = {
        ps.restart(sem, rp)
      }
      def resumeNormalInterpretation(psRestarted: PS): TracerState = {
        TracerState(NI, psRestarted)(tc, None)
      }
      if (tracerContext.guardTraceExists(tc, guardID)) {
        /* Guard trace exists: check assertions and then either execute the trace
         * or just go back to normal interpretation */
        val psRestarted = restartPs()
        Logger.log(s"Trace for guard $guardID already exists; EXECUTING GUARD TRACE", Logger.D)
        val traceNode = tracerContext.getGuardTrace(tc, guardID)
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
        val psRestarted = restartPs()
        val tcTRStarted = tracerContext.startTracingGuard(tc, loopID, guardID, List[String](), psRestarted)
        TracerState(TR, psRestarted)(tcTRStarted, tn)
      } else {
        /* Guard trace does not exist yet: start tracing it */
        resumeNormalInterpretation(restartPs())
      }
    }

    def doTraceExecutingStep(): TracerState = {

      val (traceHead, updatedTraceNode, mustRerunHeader) = tracerContext.stepTrace(tn.get, tc)

      def executeStep(ps: PS): TracerState = {
        val instructionStep = ps.applyAction(sem, traceHead)
        instructionStep match {
          case ActionStep(newPs, _) =>
            TracerState(ep, newPs)(tc, Some(updatedTraceNode))
          case GuardFailed(rp, guardID) =>
            Logger.log(s"Guard $traceHead failed", Logger.D)
            handleGuardFailure(rp, guardID, tracerContext.getLoopID(tn.get.label))
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

    def continueWithProgramState(state: PS, trace: TraceWithoutStates): TracerState = {
      val updatedPs = applyTrace(state, trace)
      TracerState(ep, updatedPs)(tc, tn)
    }

    def continueWithProgramStateTracing(state: PS, trace: TraceWithoutStates): TracerState = {
      val (newState, traceWithInfos) = applyTraceAndGetStates(ps, trace)
      val traceAppendedTc = tracerContext.appendTrace(tc, traceWithInfos)
      TracerState(ep, newState)(traceAppendedTc, tn)
    }

    def canStartLoopEncounteredRegular(newState: PS, trace: TraceWithoutStates, loopID: List[Exp]): TracerState = {
      Logger.log(s"Regular phase: CanStartLoop encountered for loop $loopID", Logger.D)
      val newTc = tracerContext.incLabelCounter(tc, loopID)
      val labelCounter = tracerContext.getLabelCounter(newTc, loopID)
      Logger.log(s"Regular phase: labelcounter equals $labelCounter", Logger.D)
      if (tracerContext.loopTraceExists(newTc, loopID)) {
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
        val tcTRStarted = tracerContext.startTracingLoop(newTc, loopID, someBoundVariables.getOrElse(List[String]()), newState)
        TracerState(TR, newState)(tcTRStarted, tn)
      } else {
        TracerState(NI, newState)(newTc, tn)
      }
    }

    def canStartLoopEncounteredTracing(state: PS, trace: TraceWithoutStates, loopID: List[Exp]): TracerState = {
      Logger.log(s"Tracing phase: CanStartLoop encountered of loop $loopID", Logger.D)
      val (newState, traceWithStates) = applyTraceAndGetStates(ps, trace) /* TODO better to use state? Shouldn't matter though */
      val traceAppendedTc = tracerContext.appendTrace(tc, traceWithStates)
      if (tracerContext.isTracingLoop(traceAppendedTc, loopID)) {
        Logger.log(s"Stopped tracing $loopID; LOOP DETECTED", Logger.I)
        numberOfTracesRecorded += 1
        val analysisOutput = findAnalysisOutput(newState)
        val tcTRStopped = tracerContext.stopTracing(traceAppendedTc, true, None, analysisOutput)
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

    def canEndLoopEncounteredTracing(state: PS, trace: List[Action[Exp, HybridValue, HybridAddress.A]],
                                     restartPoint: RestartPoint[Exp, HybridValue, HybridAddress.A], loopID: List[Exp]): TracerState = {
      Logger.log(s"Regular phase: CanEndLoop encountered for loop $loopID", Logger.D)
      val (newState, traceWithStates) = applyTraceAndGetStates(ps, trace)
      if (tracerContext.isTracingLoop(tc, loopID)) {
        Logger.log(s"Stopped tracing $loopID; NO LOOP DETECTED", Logger.I)
        numberOfTracesRecorded += 1
        val traceEndedInstruction = sem.endTraceInstruction(restartPoint)
        val analysisOutput = findAnalysisOutput(newState)
        val tcTRStopped = tracerContext.stopTracing(tc, false, Some(traceEndedInstruction), analysisOutput)
        TracerState(NI, newState)(tcTRStopped, tn)
      } else {
        val traceAppendedTc = tracerContext.appendTrace(tc, traceWithStates)
        TracerState(TR, newState)(traceAppendedTc, tn)
      }
    }

    def handleSignalRegular(state: PS, trace: List[Action[Exp, HybridValue, HybridAddress.A]], signal: TracingSignal[Exp, HybridValue, HybridAddress.A]): TracerState = signal match {
      case SignalEndLoop(loopID, _) =>
        Logger.log(s"Regular phase: CanEndLoop encountered for loop $loopID", Logger.D)
        continueWithProgramState(state, trace)
      case SignalStartLoop(loopID) => canStartLoopEncounteredRegular(applyTrace(state, trace), trace, loopID)
    }

    def handleSignalTracing(state: PS, trace: List[Action[Exp, HybridValue, HybridAddress.A]], signal: TracingSignal[Exp, HybridValue, HybridAddress.A]): TracerState = signal match {
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
      ep match {
        case NI => handleResponseRegular(ps.step(sem).get)
        case TE => doTraceExecutingStep()
        case TR => handleResponseTracing(ps.step(sem).get)
      }
    }

  }

  case class AAMOutput[State <: TracingProgramState[Exp, HybridValue, HybridAddress.A, Time], Annotation](halted: Set[State], count: Int, t: Double, graph: Option[Graph[State, Annotation]], timedOut: Boolean)
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
  private def loop(s: TracerState, nrVisited: Integer, startingTime: Long, graph: Option[Graph[PS, String]], timeout: Option[Long]): AAMOutput[PS, String] = {
    def endEvalLoop(timeout: Boolean): AAMOutput[PS, String] = {
      if (GlobalFlags.PRINT_ACTIONS_EXECUTED) {
        ActionLogger.printActions()
      }
      AAMOutput[PS, String](Set(s.ps), nrVisited,
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
      val succ = s.stepConcrete()
      val newGraph = graph.map(_.addEdge(s.ps, "", succ.ps))
      loop(succ, nrVisited + 1, startingTime, newGraph, timeout)
    }
  }

//  TODO
//  private def switchToAbstract(currentProgramState: PS): Unit = {
//    Logger.log("HybridMachine switching to abstract", Logger.E)
//    //HybridLattice.switchToAbstract
//    HybridAddress.switchToAbstract
//    val aam = new AAM[Exp, TypeSetLattice, HybridAddress.A, Time]
//    val (control, store, kstore, a, t) = currentProgramState.convertState(sem)
//    val convertedControl = control match {
//      case ConvertedControlError(reason) => aam.ControlError(reason)
//      case ConvertedControlEval(exp, env) => aam.ControlEval(exp, env)
//      case ConvertedControlKont(v) => aam.ControlKont(v)
//    }
//    val startState = aam.State(convertedControl, store, kstore, a, t)
//    aam.loop(Set(startState), Set(), Set(), System.nanoTime, None, sem.absSem)
//  }

  private def switchToConcrete(): Unit = {
    Logger.log("HybridMachine switching to concrete", Logger.E)
    //HybridLattice.switchToConcrete
    HybridAddress.switchToConcrete
  }


//  TODO
//  private def runStaticAnalysis(currentProgramState: PS): Unit = {
//    val analysisOutput = switchToAbstract(currentProgramState)
//    switchToConcrete()
//    analysisOutput
//  }

  private def findAnalysisOutput(currentProgramState: PS): Option[AAMOutput[PS, TraceWithoutStates]] = {
    if (tracingFlags.SWITCH_ABSTRACT) {
//      TODO
//      val analysisOutput = runStaticAnalysis(currentProgramState)
//      analysisOutput.toDotFile(s"abstract_$numberOfTracesRecorded.dot")
//      Some(analysisOutput)
      None
    } else {
      None
    }
  }

  def injectExecutionState(exp: Exp): TracerState =
    new TracerState(NI, injectProgramState(exp, time))(tracerContext.newTracerContext, None)

  /**
   * Performs the evaluation of an expression, possibly writing the output graph
   * in a file, and returns the set of final states reached
   */
  def eval(exp: Exp, graph: Boolean, timeout: Option[Long]): Output[HybridValue] = {
    loop(injectExecutionState(exp), 0, System.nanoTime,
      if (graph) { Some(new Graph[PS, String]()) } else { None }, timeout)
  }
}
