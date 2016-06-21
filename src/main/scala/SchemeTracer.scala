/**
  * Created by mvdcamme on 02/02/16.
  */
class SchemeTracer[Abs : JoinLattice, Addr : Address, Time : Timestamp]
    (sem: SemanticsTraced[SchemeExp, Abs, Addr, Time],
     tracingFlags: TracingFlags,
     traceOptimizer: SchemeTraceOptimizer[Addr, Time]) extends Tracer[SchemeExp, Time] {

  val semantics = sem
  type InstructionReturn = semantics.InstructionReturn

  def getLoopID(label: Label): List[SchemeExp] = label match {
    case NormalLabel(loopID) => loopID
    case GuardLabel(loopID, _) => loopID
  }

  case class SchemeTracerContext(labelCounters: Map[List[SchemeExp], Integer], /* Stores the number of times each label has been encountered. */
                                 traceNodes: List[TraceNode[TraceFull[SchemeExp, Time]]], /* Stores all trace-nodes, i.e. the previously-recorded traces. */
                                 curTraceNode: Option[TraceNode[Trace]]) /* Stores the trace that is currently being recorded. */
    extends TracerContext

  /*
   * Generating tracer context
   */

  def newTracerContext = SchemeTracerContext(Map(), List(), None)

  /*
   * Start tracing
   */

  def startTracingLoop(tc: TracerContext,
                       loopID: List[SchemeExp],
                       boundVariables: List[String],
                       startState: HybridMachine[SchemeExp, Time]#PS): SchemeTracerContext = tc match {
    case SchemeTracerContext(labelCounters, traceNodes, _) =>
      val label = NormalLabel(loopID)
      val traceInfo = TraceInfo(boundVariables, startState)
      val traceNode = TraceNode[Trace](label, Nil, traceInfo)
      SchemeTracerContext(labelCounters, traceNodes, Some(traceNode))
  }

  def startTracingGuard(tc: TracerContext,
                        loopID: List[SchemeExp],
                        guardID: Integer,
                        boundVariables: List[String],
                        startState: HybridMachine[SchemeExp, Time]#PS): SchemeTracerContext = tc match {
    case SchemeTracerContext(labelCounters, traceNodes, _) =>
      val label = GuardLabel(loopID, guardID)
      val traceInfo = TraceInfo(boundVariables, startState)
      val traceNode = TraceNode[Trace](label, Nil, traceInfo)
      SchemeTracerContext(labelCounters, traceNodes, Some(traceNode))
  }

  /*
   * Stop tracing
   */

  def stopTracing(tc: TracerContext, isLooping: Boolean,
                  traceEndedInstruction: Option[TraceInstruction],
                  someAnalysisOutput: Option[AnalysisOutput]): SchemeTracerContext = tc match {
    case tc: SchemeTracerContext =>
      var finishedTc: SchemeTracerContext = tc
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

  private def searchTraceWithLabelMatching(tc: TracerContext,
                                           labelPred: Label => Boolean): Option[TraceNode[TraceFull[SchemeExp, Time]]] = tc match {
    case tc: SchemeTracerContext =>
      tc.traceNodes.find({traceNode => labelPred(traceNode.label)})
  }

  private def searchLoopTrace(tc: TracerContext,
                              loopID: List[SchemeExp]): Option[TraceNode[TraceFull[SchemeExp, Time]]] = tc match {
    case tc: SchemeTracerContext =>
      searchTraceWithLabelMatching(tc, NormalLabel(loopID) == _)
  }

  private def searchGuardTrace(tc: TracerContext, guardID: Integer): Option[TraceNode[TraceFull[SchemeExp, Time]]] = tc match {
    case tc: SchemeTracerContext =>
      searchTraceWithLabelMatching(tc, { case NormalLabel(_) => false
                                         case GuardLabel(_, guardID2) => guardID == guardID2 })
  }

  private def getTrace(tc: TracerContext, label: Label): TraceNode[TraceFull[SchemeExp, Time]] = tc match {
    case tc: SchemeTracerContext =>
      searchTraceWithLabelMatching(tc, _ == label) match {
        case Some(traceNode) => traceNode
        case None => throw new Exception(s"Retrieving non-existing trace (should not happen): $label")
      }
  }

  def getLoopTrace(tc: TracerContext, loopID: List[SchemeExp]): TraceNode[TraceFull[SchemeExp, Time]] = tc match {
    case tc: SchemeTracerContext =>
      searchLoopTrace(tc, loopID) match {
        case Some(traceNode) => traceNode
        case None => throw new Exception(s"Retrieving non-existing loop-trace (should not happen): ${NormalLabel(loopID)}")
      }
  }

  def getGuardTrace(tc: TracerContext, guardID: Integer): TraceNode[TraceFull[SchemeExp, Time]] = tc match {
    case tc: SchemeTracerContext =>
      searchGuardTrace(tc, guardID) match {
        case Some(traceNode) => traceNode
        case None => throw new Exception(s"Retrieving non-existing guard-trace (should not happen): guardID = $guardID")
      }
  }

  def loopTraceExists(tc: TracerContext, loopID: List[SchemeExp]): Boolean = tc match {
    case tc: SchemeTracerContext =>
      searchLoopTrace(tc, loopID) match {
        case Some(traceNode) => true
        case None => false
      }
  }

  def guardTraceExists(tc: TracerContext, guardID: Integer): Boolean = tc match {
    case tc: SchemeTracerContext =>
      searchGuardTrace(tc, guardID) match {
        case Some(traceNode) => true
        case None => false
      }
  }

  /*
   * Recording traces
   */

  def appendTrace(tc: TracerContext, newPart: Trace): SchemeTracerContext = tc match {
    case tc: SchemeTracerContext =>
      val newCurTraceNode = tc.curTraceNode.get.copy(trace = newPart.reverse ++ tc.curTraceNode.get.trace)
      tc.copy(curTraceNode = Some(newCurTraceNode))
  }

  private def clearTrace(tc: TracerContext): SchemeTracerContext = tc match {
    case tc: SchemeTracerContext =>
      tc.copy(curTraceNode = None)
  }

  def isTracing(tc: TracerContext): Boolean = tc match {
    case tc: SchemeTracerContext =>
      tc.curTraceNode match {
        case Some(_) => true
        case None => false
      }
  }


  def isTracingLoop(tc: TracerContext, loopID: List[SchemeExp]): Boolean = tc match {
    case tc: SchemeTracerContext =>
      tc.curTraceNode match {
        case Some(traceNode) =>
          val loopID2 = getLoopID(traceNode.label)
          loopID == loopID2
        case None => false
      }
  }


  /*
   * Executing traces
   */

  def stepTrace(traceNode: TraceNode[TraceFull[SchemeExp, Time]], tc: TracerContext): (TraceInstruction, TraceNode[TraceFull[SchemeExp, Time]], Boolean) = tc match {
    case tc: SchemeTracerContext =>
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

  private def addTrace(tc: TracerContext, someAnalysisOutput: Option[AnalysisOutput]): SchemeTracerContext = tc match {
    case SchemeTracerContext(labelCounters, traceNodes, Some(curTraceNode)) =>
      val traceFull = TraceFull[SchemeExp, Time](curTraceNode.info.startState, List(), curTraceNode.trace.reverse)
      val optimizedTraceFull: TraceFull[SchemeExp, Time] = if (tracingFlags.APPLY_OPTIMIZATIONS) {
        traceOptimizer.optimize(traceFull, curTraceNode.info.boundVariables, someAnalysisOutput)
      } else {
        traceFull
      }
      SchemeTracerContext(labelCounters, TraceNode[TraceFull[SchemeExp, Time]](curTraceNode.label, optimizedTraceFull, curTraceNode.info) :: traceNodes, Some(curTraceNode))
  }

  /*
   * Hotness counter
   */

  def getLabelCounter(tc: TracerContext, loopID: List[SchemeExp]): Integer = tc match {
    case tc: SchemeTracerContext =>
      tc.labelCounters.getOrElse(loopID, 0)
  }

  def incLabelCounter(tc: TracerContext, loopID: List[SchemeExp]): SchemeTracerContext = tc match {
    case tc: SchemeTracerContext =>
      val oldCounter = getLabelCounter(tc, loopID)
      val newLabelCounters: Map[List[SchemeExp], Integer] = tc.labelCounters.updated(loopID, oldCounter + 1)
      tc.copy(labelCounters = newLabelCounters)
  }

}
