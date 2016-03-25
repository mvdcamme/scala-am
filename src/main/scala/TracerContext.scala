/**
  * Created by mvdcamme on 02/02/16.
  */
class TracerContext[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp]
    (sem : SemanticsTraced[Exp, Abs, Addr, Time], traceOptimizer : TraceOptimizer[Exp, Abs, Addr, Time], hybridMachine: HybridMachine[Exp, Time]) {

  val semantics = sem
  type Label = semantics.Label
  type InstructionReturn = semantics.InstructionReturn
  //type ProgramState = HybridMachine[Exp, Time]#ProgramState
  type TraceInstruction = HybridMachine[Exp, Time]#TraceInstruction
  type Trace = HybridMachine[Exp, Time]#TraceWithInfos
  type TraceFull = HybridMachine[Exp, Time]#TraceFull

  type AnalysisOutput = HybridMachine[Exp, Time]#AAMOutput[HybridMachine[Exp, Time]#TraceWithoutStates]


  case class TraceInfo(label : Label, boundVariables : List[String], startState : HybridMachine[Exp, Time]#ProgramState)
  case class TraceNode(label : Label, trace : TraceFull)

  case class TracerContext(traceInfo : Option[TraceInfo], labelCounters : Map[Label, Integer],
                           traceNodes : List[TraceNode], trace : Trace)

  /*
   * Generating tracer context
   */

  def newTracerContext = new TracerContext(None, Map(), List(), List())

  /*
   * Start tracing
   */

  def startTracingLabel(tracerContext: TracerContext, label: Label, boundVariables : List[String], startState : HybridMachine[Exp, Time]#ProgramState) : TracerContext = tracerContext match {
    case TracerContext(_, labelCounters, traceNodes, _) =>
      new TracerContext(Some(TraceInfo(label, boundVariables, startState)), labelCounters, traceNodes, List())
  }

  /*
   * Stop tracing
   */

  def stopTracing(tracerContext: TracerContext, isLooping : Boolean,
                  traceEndedInstruction: Option[TraceInstruction], someAnalysisOutput: Option[AnalysisOutput]) : TracerContext = {
    var finishedTracerContext : TracerContext = tracerContext
    if (! isLooping) {
      finishedTracerContext = appendTrace(tracerContext, List((traceEndedInstruction.get, None)))
    }
    val traceNodeAddedTc = addTrace(finishedTracerContext, someAnalysisOutput)
    val newTrace = getTrace(traceNodeAddedTc, tracerContext.traceInfo.get.label).trace
    println(s"Complete trace")
    if (TracerFlags.PRINT_ENTIRE_TRACE) {
      println("------------ START TRACE ------------")
      println("------------- ASSERTIONS ------------")
      for (action <- newTrace.assertions) {
        println(action)
      }
      println("-------------- ACTIONS --------------")
      for (action <- newTrace.trace) {
        println(action)
      }
      println("------------ END TRACE ------------")
    }
    clearTrace(traceNodeAddedTc)
  }

  /*
   * Finding traces
   */

  private def searchTrace(tracerContext: TracerContext, label: Label) : Option[TraceNode] = tracerContext match {
    case TracerContext(_, _, traceNodes, _) => traceNodes.find({traceNode => traceNode.label == label})
  }

  def getTrace(tracerContext: TracerContext, label: Label) : TraceNode = searchTrace(tracerContext, label) match {
    case Some(traceNode) => traceNode
    case None => throw new Exception(s"Retrieving non-existing trace (should not happen): $label")
  }

  def traceExists(tracerContext: TracerContext, label: Label) : Boolean = searchTrace(tracerContext, label) match {
    case Some(traceNode) => true
    case None => false
  }

  /*
   * Recording traces
   */

  def appendTrace(tracerContext: TracerContext, newPart : Trace) : TracerContext =
    new TracerContext(tracerContext.traceInfo, tracerContext.labelCounters, tracerContext.traceNodes,
                      tracerContext.trace ++ newPart)

  private def clearTrace(tracerContext: TracerContext) : TracerContext = tracerContext match {
    case TracerContext(_, labelCounters, traceNodes, _) =>
      new TracerContext(None, labelCounters, traceNodes, List())
  }

  def isTracing(tracerContext: TracerContext) : Boolean = tracerContext.traceInfo match {
    case Some(_) => true
    case None => false
  }

  def isTracingLabel(tracerContext: TracerContext, label: Label) : Boolean = tracerContext.traceInfo match {
    case Some(traceInfo) => traceInfo.label == label
    case None => false
  }

  /*
   * Executing traces
   */

  def stepTrace(traceNode: TraceNode, tracerContext: TracerContext) : (TraceInstruction, TraceNode) = {
    var currentTraceNode = traceNode
    def resetTrace() = {
      println("Resetting the trace")
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

  private def addTrace(tracerContext: TracerContext, someAnalysisOutput: Option[AnalysisOutput]) : TracerContext = tracerContext match {
    case TracerContext(traceInfo, labelCounters, traceNodes, trace) =>
      val traceFull = hybridMachine.TraceFull(traceInfo.get.startState, List(), trace)
      val optimizedTraceFull : TraceFull = traceOptimizer.optimize(traceFull, traceInfo.get.boundVariables, someAnalysisOutput)
      new TracerContext(traceInfo, labelCounters, new TraceNode(traceInfo.get.label, optimizedTraceFull) :: traceNodes, trace)
  }

  private def replaceTrace(tracerContext: TracerContext, label : Label, newTrace : TraceFull) = {
    val newTraceNode = TraceNode(label, newTrace)
    val newTraceNodes = tracerContext.traceNodes.filter({ (traceNode) => traceNode.label != label})
    tracerContext.copy(traceNodes = newTraceNode :: newTraceNodes)
  }

  /*
   * Hotness counter
   */

  def getLabelCounter(tc: TracerContext, label: Label) : Integer =
    tc.labelCounters.getOrElse(label, 0)

  def incLabelCounter(tc : TracerContext, label : Label) : TracerContext = tc match {
    case TracerContext(traceInfo, labelCounters, traceNodes, trace) =>
      val oldCounter = getLabelCounter(tc, label)
      val newLabelCounters : Map[Label, Integer] = labelCounters.updated(label, oldCounter + 1)
      new TracerContext(traceInfo, newLabelCounters, traceNodes, trace)
  }

}
