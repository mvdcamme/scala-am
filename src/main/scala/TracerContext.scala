/**
  * Created by mvdcamme on 02/02/16.
  */
class TracerContext[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp]
    (sem: SemanticsTraced[Exp, Abs, Addr, Time], traceOptimizer: TraceOptimizer[Exp, Abs, Addr, Time], hybridMachine: HybridMachine[Exp, Time]) {

  val semantics = sem
  type InstructionReturn = semantics.InstructionReturn
  //type ProgramState = HybridMachine[Exp, Time]#ProgramState
  type TraceInstruction = HybridMachine[Exp, Time]#TraceInstruction
  type Trace = HybridMachine[Exp, Time]#TraceWithInfos
  type TraceFull = HybridMachine[Exp, Time]#TraceFull

  type AnalysisOutput = HybridMachine[Exp, Time]#AAMOutput[HybridMachine[Exp, Time]#PS, HybridMachine[Exp, Time]#TraceWithoutStates]

  trait Label

  case class NormalLabel(loopID: List[Exp]) extends Label
  case class GuardLabel(loopID: List[Exp], guardID: Integer) extends Label

  def getLoopID(label: Label): List[Exp] = label match {
    case NormalLabel(loopID) => loopID
    case GuardLabel(loopID, _) => loopID
  }

  case class TraceInfo(label: Label, boundVariables: List[String], startState: HybridMachine[Exp, Time]#PS)
  case class TraceNode(label: Label, trace: TraceFull)

  case class TracerContext(traceInfo: Option[TraceInfo], labelCounters: Map[List[Exp], Integer],
                           traceNodes: List[TraceNode], trace: Trace)

  /*
   * Generating tracer context
   */

  def newTracerContext = new TracerContext(None, Map(), List(), List())

  /*
   * Start tracing
   */

  def startTracingLoop(tracerContext: TracerContext, loopID: List[Exp], boundVariables: List[String], startState: HybridMachine[Exp, Time]#PS): TracerContext = tracerContext match {
    case TracerContext(_, labelCounters, traceNodes, _) =>
      val label = NormalLabel(loopID)
      new TracerContext(Some(TraceInfo(label, boundVariables, startState)), labelCounters, traceNodes, List())
  }

  def startTracingGuard(tracerContext: TracerContext, loopID: List[Exp], guardID: Integer, boundVariables: List[String], startState: HybridMachine[Exp, Time]#PS): TracerContext = tracerContext match {
    case TracerContext(_, labelCounters, traceNodes, _) =>
      val label = GuardLabel(loopID, guardID)
      new TracerContext(Some(TraceInfo(label, boundVariables, startState)), labelCounters, traceNodes, List())
  }

  /*
   * Stop tracing
   */

  def stopTracing(tracerContext: TracerContext, isLooping: Boolean,
                  traceEndedInstruction: Option[TraceInstruction], someAnalysisOutput: Option[AnalysisOutput]): TracerContext = {
    var finishedTracerContext: TracerContext = tracerContext
    if (! isLooping) {
      finishedTracerContext = appendTrace(tracerContext, List((traceEndedInstruction.get, None)))
    }
    val traceNodeAddedTc = addTrace(finishedTracerContext, someAnalysisOutput)
    val newTrace = getLoopTrace(traceNodeAddedTc, getLoopID(tracerContext.traceInfo.get.label)).trace
    if (GlobalFlags.PRINT_ENTIRE_TRACE) {
      Logger.log("------------ START TRACE ------------", Logger.E)
      Logger.log("------------- ASSERTIONS ------------", Logger.E)
      for (action <- newTrace.assertions) {
        Logger.log(action, Logger.E)
      }
      Logger.log("-------------- ACTIONS --------------", Logger.E)
      for (action <- newTrace.trace) {
        Logger.log(action, Logger.E)
      }
      Logger.log("------------ END TRACE ------------", Logger.E)
    }
    clearTrace(traceNodeAddedTc)
  }

  /*
   * Finding traces
   */

  /* TODO refactor this */

  private def searchTraceWithLabelMatching(tc: TracerContext, labelPred: Label => Boolean): Option[TraceNode] =
    tc.traceNodes.find({traceNode => labelPred(traceNode.label)})

  private def searchLoopTrace(tc: TracerContext, loopID: List[Exp]): Option[TraceNode] =
    searchTraceWithLabelMatching(tc, NormalLabel(loopID) == _)

  private def searchGuardTrace(tc: TracerContext, guardID: Integer): Option[TraceNode] =
    searchTraceWithLabelMatching(tc, { case NormalLabel(_) => false
                                                  case GuardLabel(_, guardID2) => guardID == guardID2
    })

  def getLoopTrace(tc: TracerContext, loopID: List[Exp]): TraceNode = searchLoopTrace(tc, loopID) match {
    case Some(traceNode) => traceNode
    case None => throw new Exception(s"Retrieving non-existing loop-trace (should not happen): $NormalLabel(loopID)")
  }

  def getGuardTrace(tc: TracerContext, guardID: Integer): TraceNode = searchGuardTrace(tc, guardID) match {
    case Some(traceNode) => traceNode
    case None => throw new Exception(s"Retrieving non-existing guard-trace (should not happen): guardID = $guardID")
  }

  def loopTraceExists(tc: TracerContext, loopID: List[Exp]): Boolean = searchLoopTrace(tc, loopID) match {
    case Some(traceNode) => true
    case None => false
  }

  def guardTraceExists(tc: TracerContext, guardID: Integer): Boolean = searchGuardTrace(tc, guardID) match {
    case Some(traceNode) => true
    case None => false
  }

  /*
   * Recording traces
   */

  def appendTrace(tracerContext: TracerContext, newPart: Trace): TracerContext =
    new TracerContext(tracerContext.traceInfo, tracerContext.labelCounters, tracerContext.traceNodes,
                      newPart.reverse ++ tracerContext.trace)

  private def clearTrace(tracerContext: TracerContext): TracerContext = tracerContext match {
    case TracerContext(_, labelCounters, traceNodes, _) =>
      new TracerContext(None, labelCounters, traceNodes, List())
  }

  def isTracing(tracerContext: TracerContext): Boolean = tracerContext.traceInfo match {
    case Some(_) => true
    case None => false
  }

  def isTracingLoop(tracerContext: TracerContext, loopID: List[Exp]): Boolean = tracerContext.traceInfo match {
    case Some(traceInfo) =>
      val loopID2 = getLoopID(traceInfo.label)
      loopID == loopID2
    case None => false
  }

  /*
   * Executing traces
   */

  def stepTrace(traceNode: TraceNode, tracerContext: TracerContext): (TraceInstruction, TraceNode) = {
    var currentTraceNode = traceNode
    def resetTrace() = {
      Logger.log("Resetting the trace", Logger.D)
      val loopID = getLoopID(traceNode.label)
      currentTraceNode = getLoopTrace(tracerContext, loopID)
    }

    /*
     * Make sure the trace isn't empty
     */
    if (traceNode.trace.trace.isEmpty) {
      resetTrace()
    }

    val traceHead = currentTraceNode.trace.trace.head
    val updatedTraceNode = TraceNode(traceNode.label, traceNode.trace.copy(currentTraceNode.trace.startProgramState, currentTraceNode.trace.assertions, currentTraceNode.trace.trace.tail))
    (traceHead._1, updatedTraceNode)
  }

  /*
   * Adding traces
   */

  private def addTrace(tracerContext: TracerContext, someAnalysisOutput: Option[AnalysisOutput]): TracerContext = tracerContext match {
    case TracerContext(traceInfo, labelCounters, traceNodes, trace) =>
      val traceFull = hybridMachine.TraceFull(traceInfo.get.startState, List(), trace.reverse)
      val optimizedTraceFull: TraceFull = if (hybridMachine.tracingFlags.APPLY_OPTIMIZATIONS) { traceOptimizer.optimize(traceFull, traceInfo.get.boundVariables, someAnalysisOutput) } else { traceFull }
      new TracerContext(traceInfo, labelCounters, new TraceNode(traceInfo.get.label, optimizedTraceFull) :: traceNodes, trace)
  }

  private def replaceTrace(tracerContext: TracerContext, label: Label, newTrace: TraceFull) = {
    val newTraceNode = TraceNode(label, newTrace)
    val newTraceNodes = tracerContext.traceNodes.filter({ (traceNode) => traceNode.label != label})
    tracerContext.copy(traceNodes = newTraceNode :: newTraceNodes)
  }

  /*
   * Hotness counter
   */

  def getLabelCounter(tc: TracerContext, loopID: List[Exp]): Integer =
    tc.labelCounters.getOrElse(loopID, 0)

  def incLabelCounter(tc: TracerContext, loopID: List[Exp]): TracerContext = {
    val oldCounter = getLabelCounter(tc, loopID)
    val newLabelCounters: Map[List[Exp], Integer] = tc.labelCounters.updated(loopID, oldCounter + 1)
    tc.copy(labelCounters = newLabelCounters)
  }

}
