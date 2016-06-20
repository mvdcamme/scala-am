/**
  * Created by mvdcamme on 02/02/16.
  */
class Tracer[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
    (sem: SemanticsTraced[Exp, Abs, Addr, Time], traceOptimizer: TraceOptimizer[Exp, Abs, Addr, Time],
     hybridMachine: HybridMachine[Exp, Time]) {

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

  case class TraceInfo(boundVariables: List[String], startState: HybridMachine[Exp, Time]#PS)
  case class TraceNode[Trace](label: Label, trace: Trace, info: TraceInfo)

  case class TracerContext(labelCounters: Map[List[Exp], Integer], /* Stores the number of times each label has been encountered. */
                           traceNodes: List[TraceNode[TraceFull]], /* Stores all trace-nodes, i.e. the previously-recorded traces. */
                           curTraceNode: Option[TraceNode[Trace]]) /* Stores the trace that is currently being recorded. */

  /*
   * Generating tracer context
   */

  def newTracerContext = TracerContext(Map(), List(), None)

  /*
   * Start tracing
   */

  def startTracingLoop(tc: TracerContext, loopID: List[Exp], boundVariables: List[String], startState: HybridMachine[Exp, Time]#PS): TracerContext = tc match {
    case TracerContext(labelCounters, traceNodes, _) =>
      val label = NormalLabel(loopID)
      val traceInfo = TraceInfo(boundVariables, startState)
      val traceNode = TraceNode[Trace](label, Nil, traceInfo)
      TracerContext(labelCounters, traceNodes, Some(traceNode))
  }

  def startTracingGuard(tc: TracerContext, loopID: List[Exp], guardID: Integer, boundVariables: List[String], startState: HybridMachine[Exp, Time]#PS): TracerContext = tc match {
    case TracerContext(labelCounters, traceNodes, _) =>
      val label = GuardLabel(loopID, guardID)
      val traceInfo = TraceInfo(boundVariables, startState)
      val traceNode = TraceNode[Trace](label, Nil, traceInfo)
      TracerContext(labelCounters, traceNodes, Some(traceNode))
  }

  /*
   * Stop tracing
   */

  def stopTracing(tc: TracerContext, isLooping: Boolean,
                  traceEndedInstruction: Option[TraceInstruction], someAnalysisOutput: Option[AnalysisOutput]): TracerContext = {
    var finishedTc: TracerContext = tc
    if (! isLooping) {
      finishedTc = appendTrace(tc, List((traceEndedInstruction.get, None)))
    }
    val traceNodeAddedTc = addTrace(finishedTc, someAnalysisOutput)
    val newTrace = getTrace(traceNodeAddedTc, tc.curTraceNode.get.label).trace
    if (GlobalFlags.PRINT_ENTIRE_TRACE) {
      Logger.log("------------ START TRACE ------------", Logger.E)
      Logger.log("--------------- HEADER --------------", Logger.E)
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

  private def searchTraceWithLabelMatching(tc: TracerContext, labelPred: Label => Boolean): Option[TraceNode[TraceFull]] =
    tc.traceNodes.find({traceNode => labelPred(traceNode.label)})

  private def searchLoopTrace(tc: TracerContext, loopID: List[Exp]): Option[TraceNode[TraceFull]] =
    searchTraceWithLabelMatching(tc, NormalLabel(loopID) == _)

  private def searchGuardTrace(tc: TracerContext, guardID: Integer): Option[TraceNode[TraceFull]] =
    searchTraceWithLabelMatching(tc, { case NormalLabel(_) => false
                                                  case GuardLabel(_, guardID2) => guardID == guardID2
    })

  private def getTrace(tc: TracerContext, label: Label): TraceNode[TraceFull] = searchTraceWithLabelMatching(tc, _ == label) match {
    case Some(traceNode) => traceNode
    case None => throw new Exception(s"Retrieving non-existing trace (should not happen): $label")
  }

  def getLoopTrace(tc: TracerContext, loopID: List[Exp]): TraceNode[TraceFull] = searchLoopTrace(tc, loopID) match {
    case Some(traceNode) => traceNode
    case None => throw new Exception(s"Retrieving non-existing loop-trace (should not happen): ${NormalLabel(loopID)}")
  }

  def getGuardTrace(tc: TracerContext, guardID: Integer): TraceNode[TraceFull] = searchGuardTrace(tc, guardID) match {
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

  def appendTrace(tc: TracerContext, newPart: Trace): TracerContext = {
    val newCurTraceNode = tc.curTraceNode.get.copy(trace = newPart.reverse ++ tc.curTraceNode.get.trace)
    tc.copy(curTraceNode = Some(newCurTraceNode))
  }

  private def clearTrace(tc: TracerContext): TracerContext = tc.copy(curTraceNode = None)

  def isTracing(tc: TracerContext): Boolean = tc.curTraceNode match {
    case Some(_) => true
    case None => false
  }

  def isTracingLoop(tc: TracerContext, loopID: List[Exp]): Boolean = tc.curTraceNode match {
    case Some(traceNode) =>
      val loopID2 = getLoopID(traceNode.label)
      loopID == loopID2
    case None => false
  }

  /*
   * Executing traces
   */

  def stepTrace(traceNode: TraceNode[TraceFull], tc: TracerContext): (TraceInstruction, TraceNode[TraceFull], Boolean) = {
    var currentTraceNode = traceNode
    def resetTrace() = {
      Logger.log("Resetting the trace", Logger.D)
      val loopID = getLoopID(traceNode.label)
      currentTraceNode = getLoopTrace(tc, loopID)
    }

    /*
     * Make sure the trace isn't empty
     */
    if (traceNode.trace.trace.isEmpty) {
      resetTrace()
    }

    val mustRerunHeader = traceNode.label != currentTraceNode.label

    val traceHead = currentTraceNode.trace.trace.head
    val updatedTraceNode = traceNode.copy(trace = traceNode.trace.copy(currentTraceNode.trace.startProgramState, currentTraceNode.trace.assertions, currentTraceNode.trace.trace.tail))
    (traceHead._1, updatedTraceNode, mustRerunHeader)
  }

  /*
   * Adding traces
   */

  private def addTrace(tc: TracerContext, someAnalysisOutput: Option[AnalysisOutput]): TracerContext = tc match {
    case TracerContext(labelCounters, traceNodes, Some(curTraceNode)) =>
      val traceFull = hybridMachine.TraceFull(curTraceNode.info.startState, List(), curTraceNode.trace.reverse)
      val optimizedTraceFull: TraceFull = if (hybridMachine.tracingFlags.APPLY_OPTIMIZATIONS) {
        traceOptimizer.optimize(traceFull, curTraceNode.info.boundVariables, someAnalysisOutput)
      } else {
        traceFull
      }
      TracerContext(labelCounters, TraceNode[TraceFull](curTraceNode.label, optimizedTraceFull, curTraceNode.info) :: traceNodes, Some(curTraceNode))
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
