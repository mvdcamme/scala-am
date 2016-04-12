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
  type TraceInstructionInfo = (TraceInstruction, Option[TraceInformation[HybridValue]])
  type TraceWithInfos = List[TraceInstructionInfo]
  type TraceWithoutStates = sem.Trace

  type PS = TracingProgramState[Exp, HybridValue, HybridAddress, Time]
  type APS = AbstractTracingProgramState[Exp, HybridValue, HybridAddress, Time]

  case class TraceFull(startProgramState: PS, assertions: TraceWithoutStates, trace: TraceWithInfos)
  
  def name = "HybridMachine"

  val tracerContext : TracerContext[Exp, HybridValue, HybridAddress, Time] =
    new TracerContext[Exp, HybridValue, HybridAddress, Time](sem, new TraceOptimizer[Exp, HybridValue, HybridAddress, Time](sem, this), this)

  def applyTraceIntermediateResults(state : PS, trace : sem.Trace) : List[PS] = {
    trace.scanLeft(state)((currentState, action) => currentState.applyAction(action) match {
      case NormalInstructionStep(updatedState, _) => updatedState
      case _ => throw new Exception(s"Unexpected result while applying action $action")
    })
  }

  def applyTraceAndGetStates(state : PS, trace : sem.Trace) : (PS, TraceWithInfos) = {
    val intermediateStates = applyTraceIntermediateResults(state, trace)
    val resultingState = intermediateStates.last
    val traceStatesZipped = trace.zip(intermediateStates.tail)
    val traceSomeTraceInfoZipped : TraceWithInfos = traceStatesZipped.map({ (instructionState) =>
      (instructionState._1, instructionState._2.generateTraceInformation(instructionState._1)) })
    (resultingState, traceSomeTraceInfoZipped)
  }

  def applyTrace(state : PS, trace : sem.Trace) : PS = {
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

  case class ExecutionState(ep: ExecutionPhase.Value, ps: PS)(tc : tracerContext.TracerContext, tn : Option[tracerContext.TraceNode]) {

    type InterpreterReturn = SemanticsTraced[Exp, HybridLattice.Hybrid, HybridAddress, Time]#InterpreterReturn

    def checkTraceAssertions(state: PS, tc : tracerContext.TracerContext, label : tracerContext.Label) : Boolean = {
      val traceNode = tracerContext.getTrace(tc, label)
      val assertions = traceNode.trace.assertions
      state.runAssertions(assertions)
    }

    def startExecutingTrace(state: PS, tc : tracerContext.TracerContext, label : tracerContext.Label): ExecutionState = {
      Logger.log(s"Trace with label $label already exists; EXECUTING TRACE", Logger.D)
      val traceNode = tracerContext.getTrace(tc, label)
      val assertions = traceNode.trace.assertions
      ExecutionState(TE, state)(tc, Some(traceNode))
    }

    def doTraceExecutingStep() : Set[ExecutionState] = {
      val (traceHead, updatedTraceNode) = tracerContext.stepTrace(tn.get, tc)
      val instructionStep = ps.applyAction(traceHead)
      instructionStep match {
        case NormalInstructionStep(newPs, _) =>
          Set(ExecutionState(ep, newPs)(tc, Some(updatedTraceNode)))
        case GuardFailed(rp) =>
          Logger.log(s"Guard $traceHead failed", Logger.D)
          val psRestarted = ps.restart(rp)
          Set(ExecutionState(NI, psRestarted)(tc, None))
        case TraceEnded(rp) =>
          Logger.log("Non-looping trace finished executing", Logger.D)
          val psRestarted = ps.restart(rp)
          Set(ExecutionState(NI, psRestarted)(tc, None))
      }
    }

    type TracingSignal = SemanticsTraced[Exp, HybridValue, HybridAddress, Time]#TracingSignal

    def continueWithProgramState(state : PS, trace: sem.Trace): ExecutionState = {
      val updatedPs = applyTrace(state, trace)
      ExecutionState(ep, updatedPs)(tc, tn)
    }

    def continueWithProgramStateTracing(state: PS, trace: sem.Trace): ExecutionState = {
      val (newState, traceWithInfos) = applyTraceAndGetStates(ps, trace)
      val traceAppendedTc = tracerContext.appendTrace(tc, traceWithInfos)
      ExecutionState(ep, newState)(traceAppendedTc, tn)
    }

    def canStartLoopEncounteredRegular(newState: PS, trace: sem.Trace, label: sem.Label): ExecutionState = {
      val newTc = tracerContext.incLabelCounter(tc, label)
      val labelCounter = tracerContext.getLabelCounter(newTc, label)
      if (tracerContext.traceExists(newTc, label)) {
        if (checkTraceAssertions(newState, newTc, label)) {
          startExecutingTrace(newState, newTc, label)
        } else {
          ExecutionState(NI, newState)(newTc, tn)
        }
      } else if (TracerFlags.DO_TRACING && labelCounter >= TracerFlags.TRACING_THRESHOLD) {
        Logger.log(s"Started tracing $label", Logger.I)
        val someBoundVariables = trace.find(_.isInstanceOf[ActionStepInTraced[Exp, HybridValue, HybridAddress]]).flatMap({
          case ActionStepInTraced(_, _, args, _, _, _, _, _) => Some(args)
          case _ => None /* Should not happen */
        })
        val tcTRStarted = tracerContext.startTracingLabel(newTc, label, someBoundVariables.getOrElse(List[String]()), newState)
        ExecutionState(TR, newState)(tcTRStarted, tn)
      } else {
        ExecutionState(NI, newState)(newTc, tn)
      }
    }

    def canStartLoopEncounteredTracing(state: PS, trace: sem.Trace, label: sem.Label): ExecutionState = {
      val (newState, traceWithStates) = applyTraceAndGetStates(ps, trace)
      val traceAppendedTc = tracerContext.appendTrace(tc, traceWithStates)
      if (tracerContext.isTracingLabel(traceAppendedTc, label)) {
        Logger.log(s"Stopped tracing $label; LOOP DETECTED", Logger.I)
        numberOfTracesRecorded += 1
        val analysisOutput = findAnalysisOutput(newState)
        val tcTRStopped = tracerContext.stopTracing(traceAppendedTc, true, None, analysisOutput)
        startExecutingTrace(newState, tcTRStopped, label)
      } else {
        ExecutionState(ep, newState)(traceAppendedTc, tn)
      }
    }

    def canEndLoopEncounteredTracing(state: PS, trace: sem.Trace,
                                     restartPoint: RestartPoint[Exp, HybridValue, HybridAddress], label : sem.Label) : ExecutionState = {
      val (newState, traceWithStates) = applyTraceAndGetStates(ps, trace)
      if (tracerContext.isTracingLabel(tc, label)) {
        Logger.log(s"Stopped tracing $label; NO LOOP DETECTED", Logger.I)
        numberOfTracesRecorded += 1
        val traceEndedInstruction = sem.endTraceInstruction(RestartTraceEnded())
        val analysisOutput = findAnalysisOutput(newState)
        val tcTRStopped = tracerContext.stopTracing(tc, false, Some(traceEndedInstruction), analysisOutput)
        ExecutionState(NI, newState)(tcTRStopped, tn)
      } else {
        val traceAppendedTc = tracerContext.appendTrace(tc, traceWithStates)
        ExecutionState(TR, newState)(traceAppendedTc, tn)
      }
    }

    def handleSignalRegular(state: PS, trace: sem.Trace, signal: TracingSignal): ExecutionState = signal match {
      case sem.TracingSignalEnd(_, _) => continueWithProgramState(state, trace)
      case sem.TracingSignalStart(label) => canStartLoopEncounteredRegular(applyTrace(state, trace), trace, label)
    }

    def handleSignalTracing(state: PS, trace: sem.Trace, signal: TracingSignal): ExecutionState = signal match {
      case sem.TracingSignalEnd(label, restartPoint) => canEndLoopEncounteredTracing(state, trace, restartPoint, label)
      case sem.TracingSignalStart(label) => canStartLoopEncounteredTracing(state, trace, label)
    }

    def handleResponseRegular(response: sem.InterpreterReturn): ExecutionState = response match {
      case sem.InterpreterReturn(trace, sem.TracingSignalFalse()) => continueWithProgramState(ps, trace)
      case sem.InterpreterReturn(trace, signal) => handleSignalRegular(ps, trace, signal)
    }

    def handleResponseTracing(response: sem.InterpreterReturn): ExecutionState = response match {
      case sem.InterpreterReturn(trace, sem.TracingSignalFalse()) => continueWithProgramStateTracing(ps, trace)
      case sem.InterpreterReturn(trace, signal) => handleSignalTracing(ps, trace, signal)
    }

    def stepConcrete(): ExecutionState = {
      ep match {
        case NI => handleResponseRegular(ps.step().get)
        case TE => doTraceExecutingStep()
        case TR => handleResponseTracing(ps.step().get)
      }
    }

  }

  case class AAMOutput[Annotation](halted: Set[PS], count: Int, t: Double, graph: Option[Graph[PS, Annotation]])
      extends Output[HybridValue] {

    /**
     * Returns the list of final values that can be reached
     */
    def finalValues = halted.flatMap(st => st.control match {
      case TracingControlKont(_) => Set[HybridValue](st.v)
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
          case TracingControlEval(_) => "#DDFFDD"
          case TracingControlKont(_) => "#FFDDDD"
          case TracingControlError(_) => "#FF0000"
        }}, _.toString.take(20))
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
  private def loop(s: ExecutionState, nrVisited: Integer, startingTime: Long, graph: Option[Graph[PS, String]]): AAMOutput[String] = {
    def endEvalLoop(): AAMOutput[String] = {
      if (TracerFlags.PRINT_ACTIONS_EXECUTED) {
        Logger.log("####### actions executed #######", Logger.E)
        for (ac <- ACTIONS_EXECUTED) {
          Logger.log(ac, Logger.E)
        }
        Logger.log("####### actions executed #######", Logger.E)
      }
      AAMOutput[String](Set(s.ps), nrVisited,
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
      val newGraph = graph.map(_.addEdges((s.ps, "", succ.ps)))
      loop(succ, nrVisited + 1, startingTime, newGraph)
    }
  }

  private def switchToAbstract(currentProgramState: PS) : AAMOutput[sem.Trace] = {
    Logger.log("HybridMachine switching to abstract", Logger.E)
    HybridLattice.switchToAbstract
    HybridAddress.switchToAbstract
    val convertedExecutionState = currentProgramState.convertState()
    val newTodo = Set[PS](convertedExecutionState._2)
    val newVisited, newHalted = Set[ProgramState]()
    val newGraph = new Graph[ProgramState, sem.Trace]()
    loopAbstract(newTodo, newVisited, newHalted, System.nanoTime, newGraph)
  }

  private def switchToConcrete() : Unit = {
    Logger.log("HybridMachine switching to concrete", Logger.E)
    HybridLattice.switchToConcrete
    HybridAddress.switchToConcrete
  }

  private def runStaticAnalysis(currentProgramState : PS) : AAMOutput[sem.Trace] = {
    val analysisOutput = switchToAbstract(currentProgramState)
    switchToConcrete()
    analysisOutput
  }

  private def findAnalysisOutput(currentProgramState : PS) : Option[AAMOutput[sem.Trace]] = {
    if (TracerFlags.SWITCH_ABSTRACT) {
      val analysisOutput = runStaticAnalysis(currentProgramState)
      analysisOutput.toDotFile(s"abstract_$numberOfTracesRecorded.dot")
      Some(analysisOutput)
    } else {
      None
    }
  }

  @scala.annotation.tailrec
  private def loopAbstract(todo: Set[APS], visited: Set[APS],
                           halted: Set[APS], startingTime: Long, graph: Graph[APS, sem.Trace]): AAMOutput[sem.Trace] = {
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
    new ExecutionState(NI, new ProgramState(exp))(tracerContext.newTracerContext, None)

  /**
   * Performs the evaluation of an expression, possibly writing the output graph
   * in a file, and returns the set of final states reached
   */
  def eval(exp: Exp, graph: Boolean): Output[HybridValue] = {
    loop(injectExecutionState(exp), 0, System.nanoTime,
      if (graph) { Some(new Graph[ProgramState, String]()) } else { None })
  }
}
