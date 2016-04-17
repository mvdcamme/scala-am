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

  type HybridValue = HybridLattice.Hybrid

  type TraceInstruction = Action[Exp, HybridValue, HybridAddress]
  type TraceWithoutStates = List[TraceInstruction]
  type TraceInstructionInfo = (TraceInstruction, Option[TraceInformation[HybridValue]])
  type TraceWithInfos = List[TraceInstructionInfo]

  type Label = List[Exp]

  /** The primitives are defined in AbstractValue.scala and are available through the Primitives class */
  implicit val primitives = new Primitives[HybridAddress, HybridValue]()

  type PS = ConcreteTracingProgramState[Exp, HybridValue, HybridAddress, Time]
  type APS = AbstractTracingProgramState[Exp, HybridValue, HybridAddress, Time]

  case class TraceFull(startProgramState: PS, assertions: TraceWithoutStates, trace: TraceWithInfos)
  
  def name = "HybridMachine"

  val tracerContext : TracerContext[Exp, HybridValue, HybridAddress, Time] =
    new TracerContext[Exp, HybridValue, HybridAddress, Time](sem, new TraceOptimizer[Exp, HybridValue, HybridAddress, Time](sem, this), this)

  def applyTraceIntermediateResults(state: PS, trace: TraceWithoutStates): List[PS] = {
    trace.scanLeft(state)((currentState, action) => currentState.applyAction(sem, action) match {
      case NormalInstructionStep(updatedState, _) => updatedState
      case _ => throw new Exception(s"Unexpected result while applying action $action")
    })
  }

  def applyTraceAndGetStates(state: PS, trace: TraceWithoutStates): (PS, TraceWithInfos) = {
    val intermediateStates = applyTraceIntermediateResults(state, trace)
    val resultingState = intermediateStates.last
    val traceStatesZipped = trace.zip(intermediateStates.tail)
    val traceSomeTraceInfoZipped : TraceWithInfos = traceStatesZipped.map({ (instructionState) =>
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

  case class ExecutionState(ep: ExecutionPhase.Value, ps: PS)(tc : tracerContext.TracerContext, tn : Option[tracerContext.TraceNode]) {

    def checkTraceAssertions(state: PS, tc : tracerContext.TracerContext, label: Label) : Boolean = {
      val traceNode = tracerContext.getTrace(tc, label)
      val assertions = traceNode.trace.assertions
      state.runAssertions(assertions)
    }

    def startExecutingTrace(state: PS, tc : tracerContext.TracerContext, label: Label): ExecutionState = {
      Logger.log(s"Trace with label $label already exists; EXECUTING TRACE", Logger.D)
      val traceNode = tracerContext.getTrace(tc, label)
      val assertions = traceNode.trace.assertions
      ExecutionState(TE, state)(tc, Some(traceNode))
    }

    def doTraceExecutingStep() : ExecutionState = {
      val (traceHead, updatedTraceNode) = tracerContext.stepTrace(tn.get, tc)
      val instructionStep = ps.applyAction(sem, traceHead)
      instructionStep match {
        case NormalInstructionStep(newPs, _) =>
          ExecutionState(ep, newPs)(tc, Some(updatedTraceNode))
        case GuardFailed(rp) =>
          Logger.log(s"Guard $traceHead failed", Logger.D)
          val psRestarted = ps.restart(sem, rp)
          ExecutionState(NI, psRestarted)(tc, None)
        case TraceEnded(rp) =>
          Logger.log("Non-looping trace finished executing", Logger.D)
          val psRestarted = ps.restart(sem, rp)
          ExecutionState(NI, psRestarted)(tc, None)
      }
    }

    def continueWithProgramState(state : PS, trace: TraceWithoutStates): ExecutionState = {
      val updatedPs = applyTrace(state, trace)
      ExecutionState(ep, updatedPs)(tc, tn)
    }

    def continueWithProgramStateTracing(state: PS, trace: TraceWithoutStates): ExecutionState = {
      val (newState, traceWithInfos) = applyTraceAndGetStates(ps, trace)
      val traceAppendedTc = tracerContext.appendTrace(tc, traceWithInfos)
      ExecutionState(ep, newState)(traceAppendedTc, tn)
    }

    def canStartLoopEncounteredRegular(newState: PS, trace: TraceWithoutStates, label: Label): ExecutionState = {
      Logger.log(s"Regular phase: CanStartLoop encountered with label $label", Logger.D)
      val newTc = tracerContext.incLabelCounter(tc, label)
      val labelCounter = tracerContext.getLabelCounter(newTc, label)
      Logger.log(s"Regular phase: labelcounter equals $labelCounter", Logger.D)
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

    def canStartLoopEncounteredTracing(state: PS, trace: TraceWithoutStates, label: Label): ExecutionState = {
      Logger.log(s"Tracing phase: CanStartLoop encountered with label $label", Logger.D)
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

    def canEndLoopEncounteredTracing(state: PS, trace: List[Action[Exp, HybridValue, HybridAddress]],
                                     restartPoint: RestartPoint[Exp, HybridValue, HybridAddress], label: Label) : ExecutionState = {
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

    def handleSignalRegular(state: PS, trace: List[Action[Exp, HybridValue, HybridAddress]], signal: TracingSignal[Exp, HybridValue, HybridAddress]): ExecutionState = signal match {
      case TracingSignalEnd(_, _) => continueWithProgramState(state, trace)
      case TracingSignalStart(label) => canStartLoopEncounteredRegular(applyTrace(state, trace), trace, label)
    }

    def handleSignalTracing(state: PS, trace: List[Action[Exp, HybridValue, HybridAddress]], signal: TracingSignal[Exp, HybridValue, HybridAddress]): ExecutionState = signal match {
      case TracingSignalEnd(label, restartPoint) => canEndLoopEncounteredTracing(state, trace, restartPoint, label)
      case TracingSignalStart(label) => canStartLoopEncounteredTracing(state, trace, label)
    }

    def handleResponseRegular(response: InterpreterReturn[Exp, HybridValue, HybridAddress]): ExecutionState = response match {
      case InterpreterReturn(trace, TracingSignalFalse()) => continueWithProgramState(ps, trace)
      case InterpreterReturn(trace, signal) => handleSignalRegular(ps, trace, signal)
    }

    def handleResponseTracing(response: InterpreterReturn[Exp, HybridValue, HybridAddress]): ExecutionState = response match {
      case InterpreterReturn(trace, TracingSignalFalse()) => continueWithProgramStateTracing(ps, trace)
      case InterpreterReturn(trace, signal) => handleSignalTracing(ps, trace, signal)
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
      if (TracerFlags.PRINT_ACTIONS_EXECUTED) {
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

  private def switchToAbstract(currentProgramState: PS): AAMOutput[APS, TraceWithoutStates] = {
    Logger.log("HybridMachine switching to abstract", Logger.E)
    HybridLattice.switchToAbstract
    HybridAddress.switchToAbstract
    val convertedExecutionState = currentProgramState.convertState(sem)._2
    val newTodo = Set[APS](convertedExecutionState)
    val newVisited, newHalted = Set[APS]()
    val newGraph = new Graph[APS, TraceWithoutStates]()
    loopAbstract(newTodo, newVisited, newHalted, System.nanoTime, newGraph)
  }

  private def switchToConcrete() : Unit = {
    Logger.log("HybridMachine switching to concrete", Logger.E)
    HybridLattice.switchToConcrete
    HybridAddress.switchToConcrete
  }

  private def runStaticAnalysis(currentProgramState : PS) : AAMOutput[APS, TraceWithoutStates] = {
    val analysisOutput = switchToAbstract(currentProgramState)
    switchToConcrete()
    analysisOutput
  }

  private def findAnalysisOutput(currentProgramState : PS) : Option[AAMOutput[APS, TraceWithoutStates]] = {
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
                           halted: Set[APS], startingTime: Long, graph: Graph[APS, TraceWithoutStates]): AAMOutput[APS, TraceWithoutStates] = {
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
          val succs = s.stepAbstract(sem)
          val newGraph = graph.addEdges(succs.map(s2 => (s, s2._2, s2._1)))
          loopAbstract(todo.tail ++ succs.map(_._1), visited + s, halted, startingTime, newGraph)
        }
      case None => AAMOutput[APS, TraceWithoutStates](halted, visited.size,
        (System.nanoTime - startingTime) / Math.pow(10, 9), Some(graph))
    }
  }

  def injectExecutionState(exp : Exp) : ExecutionState =
    new ExecutionState(NI, new ProgramState(exp, primitives, abs, time))(tracerContext.newTracerContext, None)

  /**
   * Performs the evaluation of an expression, possibly writing the output graph
   * in a file, and returns the set of final states reached
   */
  def eval(exp: Exp, graph: Boolean): Output[HybridValue] = {
    loop(injectExecutionState(exp), 0, System.nanoTime,
      if (graph) { Some(new Graph[PS, String]()) } else { None })
  }
}
