/**
  * Created by mvdcamme on 02/02/16.
  */
trait Label[Exp]

case class NormalLabel[Exp : Expression](label: List[Exp]) extends Label[Exp]
case class GuardLabel[Exp : Expression](label: List[Exp], guardID: Integer) extends Label[Exp]

class TracerContext[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp]
    (sem: SemanticsTraced[Exp, Abs, Addr, Time], traceOptimizer: TraceOptimizer[Exp, Abs, Addr, Time], hybridMachine: HybridMachine[Exp, Time]) {

  val semantics = sem
  type InstructionReturn = semantics.InstructionReturn
  //type ProgramState = HybridMachine[Exp, Time]#ProgramState
  type TraceInstruction = HybridMachine[Exp, Time]#TraceInstruction
  type Trace = HybridMachine[Exp, Time]#TraceWithInfos
  type TraceFull = HybridMachine[Exp, Time]#TraceFull

  type AnalysisOutput = HybridMachine[Exp, Time]#AAMOutput[HybridMachine[Exp, Time]#APS, HybridMachine[Exp, Time]#TraceWithoutStates]


  case class TraceInfo(label: Label[Exp], boundVariables: List[String], startState: HybridMachine[Exp, Time]#PS)
  case class TraceNode(label: Label[Exp], trace: TraceFull)

  case class TracerContext(traceInfo: Option[TraceInfo], labelCounters: Map[Label[Exp], Integer],
                           traceNodes: List[TraceNode], trace: Trace)

  /*
   * Generating tracer context
   */

  def newTracerContext = new TracerContext(None, Map(), List(), List())

  /*
   * Start tracing
   */

  def startTracingLabel(tracerContext: TracerContext, label: Label[Exp], boundVariables: List[String], startState: HybridMachine[Exp, Time]#PS): TracerContext = tracerContext match {
    case TracerContext(_, labelCounters, traceNodes, _) =>
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
    val newTrace = getTrace(traceNodeAddedTc, tracerContext.traceInfo.get.label).trace
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

  private def searchTrace(tc: TracerContext, label: Label[Exp]): Option[TraceNode] =
    tc.traceNodes.find({traceNode => traceNode.label == label})

  def getTrace(tracerContext: TracerContext, label: Label[Exp]): TraceNode = searchTrace(tracerContext, label) match {
    case Some(traceNode) => traceNode
    case None => throw new Exception(s"Retrieving non-existing trace (should not happen): $label")
  }

  def traceExists(tracerContext: TracerContext, label: Label[Exp]): Boolean = searchTrace(tracerContext, label) match {
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

  def isTracingLabel(tracerContext: TracerContext, label: Label[Exp]): Boolean = tracerContext.traceInfo match {
    case Some(traceInfo) => traceInfo.label == label
    case None => false
  }

  /*
   * Executing traces
   */

  def stepTrace(traceNode: TraceNode, tracerContext: TracerContext): (TraceInstruction, TraceNode) = {
    var currentTraceNode = traceNode
    def resetTrace() = {
      Logger.log("Resetting the trace", Logger.D)
      currentTraceNode = getTrace(tracerContext, traceNode.label)
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
      val optimizedTraceFull: TraceFull = if (hybridMachine.tracerFlags.APPLY_OPTIMIZATIONS) { traceOptimizer.optimize(traceFull, traceInfo.get.boundVariables, someAnalysisOutput) } else { traceFull }
      new TracerContext(traceInfo, labelCounters, new TraceNode(traceInfo.get.label, optimizedTraceFull) :: traceNodes, trace)
  }

  private def replaceTrace(tracerContext: TracerContext, label: Label[Exp], newTrace: TraceFull) = {
    val newTraceNode = TraceNode(label, newTrace)
    val newTraceNodes = tracerContext.traceNodes.filter({ (traceNode) => traceNode.label != label})
    tracerContext.copy(traceNodes = newTraceNode :: newTraceNodes)
  }

  /*
   * Hotness counter
   */

  def getLabelCounter(tc: TracerContext, label: Label[Exp]): Integer =
    tc.labelCounters.getOrElse(label, 0)

  def incLabelCounter(tc: TracerContext, label: Label[Exp]): TracerContext = {
    val oldCounter = getLabelCounter(tc, label)
    val newLabelCounters: Map[Label[Exp], Integer] = tc.labelCounters.updated(label, oldCounter + 1)
    tc.copy(labelCounters = newLabelCounters)
  }

}
