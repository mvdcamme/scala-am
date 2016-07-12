/**
  * Created by mvdcamme on 02/02/16.
  */
class SchemeTracer[Abs : JoinLattice, Addr : Address, Time : Timestamp]
    (sem: SemanticsTraced[SchemeExp, Abs, Addr, Time],
     tracingFlags: TracingFlags,
     someTraceOptimizer: Option[TraceOptimizer[SchemeExp, Abs, Addr, Time]])
  extends Tracer[SchemeExp, Abs, Addr, Time] {

  val semantics = sem
  type InstructionReturn = semantics.InstructionReturn

  def getLoopID(label: Label[SchemeExp]): List[SchemeExp] = label match {
    case NormalLabel(loopID) => loopID
    case GuardLabel(loopID, _) => loopID
  }

  type TracerContext = SchemeTracerContext


  case class SchemeTracerContext(
     /* Stores the number of times each label has been encountered. */
     labelCounters: Map[List[SchemeExp], Integer],
     /* Stores all trace-nodes, i.e. the previously-recorded traces. */
     traceNodes: List[TraceNode[TraceFull[SchemeExp, Abs, Addr, Time]]],
     /* Stores the trace that is currently being recorded. */
     curTraceNode: Option[TraceNode[Trace]])

  /*
   * Generating tracer context
   */

  def newTracerContext = SchemeTracerContext(Map(), List(), None)

  /*
   * Start tracing
   */

  def startTracingLoop(tc: TracerContext,
                       loopID: List[SchemeExp],
                       boundVariables: List[(String, Addr)],
                       startState: ConcreteTracingProgramState[SchemeExp, Abs, Addr, Time])
                      :SchemeTracerContext = tc match {
    case SchemeTracerContext(labelCounters, traceNodes, _) =>
      val label = NormalLabel(loopID)
      val traceInfo = TraceInfo(boundVariables, startState, None)
      val traceNode = TraceNode[Trace](label, Nil, traceInfo)
      SchemeTracerContext(labelCounters, traceNodes, Some(traceNode))
  }

  def startTracingGuard(tc: TracerContext,
                        loopID: List[SchemeExp],
                        guardID: Integer,
                        parentTraceLabel: Label[SchemeExp],
                        boundVariables: List[(String, Addr)],
                        startState: ConcreteTracingProgramState[SchemeExp, Abs, Addr, Time])
                      :SchemeTracerContext = tc match {
    case SchemeTracerContext(labelCounters, traceNodes, _) =>
      val parentTraceNode = getLoopTrace(tc, loopID)
      val parentTraceInitialBoundVariables = parentTraceNode.trace.info.boundVariables
      val allInitialBoundVariables = boundVariables ++ parentTraceInitialBoundVariables
      Logger.log(s"Including initial bound variables of parent loop trace $parentTraceInitialBoundVariables for guard trace", Logger.V)
      val label = GuardLabel(loopID, guardID)
      val traceInfo = TraceInfo(allInitialBoundVariables, startState, Some(parentTraceLabel))
      val traceNode = TraceNode[Trace](label, Nil, traceInfo)
      SchemeTracerContext(labelCounters, traceNodes, Some(traceNode))
  }

  /*
   * Stop tracing
   */

  def stopTracing(tc: TracerContext, isLooping: Boolean,
                  traceEndedInstruction: Option[TraceInstruction],
                  state: ConcreteTracingProgramState[SchemeExp, Abs, Addr, Time]): SchemeTracerContext = {
      var finishedTc: SchemeTracerContext = tc
      if (! isLooping) {
        finishedTc = appendTrace(tc, List((traceEndedInstruction.get, TraceInfos.nil[Abs, Addr])))
      }
      val traceNodeAddedTc = addTrace(finishedTc, state)
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
                                           labelPred: Label[SchemeExp] => Boolean): Option[TraceNode[TraceFull[SchemeExp, Abs, Addr, Time]]] =
    tc.traceNodes.find({traceNode => labelPred(traceNode.label)})

  private def searchLoopTrace(tc: TracerContext,
                              loopID: List[SchemeExp]): Option[TraceNode[TraceFull[SchemeExp, Abs, Addr, Time]]] =
    searchTraceWithLabelMatching(tc, NormalLabel(loopID) == _)

  private def searchGuardTrace(tc: TracerContext, guardID: Integer): Option[TraceNode[TraceFull[SchemeExp, Abs, Addr, Time]]] =
      searchTraceWithLabelMatching(tc, { case NormalLabel(_) => false
                                         case GuardLabel(_, guardID2) => guardID == guardID2 })

  private def getTrace(tc: TracerContext, label: Label[SchemeExp]): TraceNode[TraceFull[SchemeExp, Abs, Addr, Time]] = searchTraceWithLabelMatching(tc, _ == label) match {
    case Some(traceNode) => traceNode
    case None => throw new Exception(s"Retrieving non-existing trace (should not happen): $label")
  }

  def getLoopTrace(tc: TracerContext, loopID: List[SchemeExp]): TraceNode[TraceFull[SchemeExp, Abs, Addr, Time]] = searchLoopTrace(tc, loopID) match {
    case Some(traceNode) => traceNode
    case None => throw new Exception(s"Retrieving non-existing loop-trace (should not happen): ${NormalLabel(loopID)}")
  }

  def getGuardTrace(tc: TracerContext, guardID: Integer): TraceNode[TraceFull[SchemeExp, Abs, Addr, Time]] = searchGuardTrace(tc, guardID) match {
    case Some(traceNode) => traceNode
    case None => throw new Exception(s"Retrieving non-existing guard-trace (should not happen): guardID = $guardID")
  }

  def loopTraceExists(tc: TracerContext, loopID: List[SchemeExp]): Boolean = searchLoopTrace(tc, loopID) match {
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

  def appendTrace(tc: TracerContext, newPart: Trace): SchemeTracerContext = {
    val newCurTraceNode = tc.curTraceNode.get.copy(trace = newPart.reverse ++ tc.curTraceNode.get.trace)
    tc.copy(curTraceNode = Some(newCurTraceNode))
  }

  private def clearTrace(tc: TracerContext): SchemeTracerContext =
      tc.copy(curTraceNode = None)

  def isTracing(tc: TracerContext): Boolean = tc.curTraceNode match {
    case Some(_) => true
    case None => false
  }


  def isTracingLoop(tc: TracerContext, loopID: List[SchemeExp]): Boolean = tc.curTraceNode match {
    case Some(traceNode) =>
      val loopID2 = getLoopID(traceNode.label)
      loopID == loopID2
    case None => false
  }


  /*
   * Executing traces
   */

  def stepTrace(traceNode: TraceNode[TraceFull[SchemeExp, Abs, Addr, Time]], tc: TracerContext)
               :(TraceInstruction, TraceNode[TraceFull[SchemeExp, Abs, Addr, Time]], Boolean) = {
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
    val updatedTraceNode = traceNode.copy(trace = traceNode.trace.copy(trace = currentTraceNode.trace.trace.tail))
    (traceHead._1, updatedTraceNode, mustRerunHeader)
  }

  /*
   * Adding traces
   */

  private def addTrace(tc: TracerContext, state: ConcreteTracingProgramState[SchemeExp, Abs, Addr, Time]): SchemeTracerContext = tc match {
    case SchemeTracerContext(labelCounters, traceNodes, Some(curTraceNode)) =>
      val curInfo = curTraceNode.info
      val newTraceInfo = TraceInfo[SchemeExp, Abs, Addr, Time](curInfo.boundVariables, curInfo.startState, curInfo.parentTraceLabel)
      val traceFull = TraceFull[SchemeExp, Abs, Addr, Time](newTraceInfo, List(), curTraceNode.trace.reverse)
      val optimizedTraceFull: TraceFull[SchemeExp, Abs, Addr, Time] = someTraceOptimizer match {
        case Some(traceOptimizer) =>
          traceOptimizer.optimize(traceFull, state)
        case None => traceFull
      }
      SchemeTracerContext(labelCounters,
                          TraceNode[TraceFull[SchemeExp, Abs, Addr, Time]](curTraceNode.label, optimizedTraceFull, curTraceNode.info) :: traceNodes,
                          Some(curTraceNode))
  }

  /*
   * Hotness counter
   */

  def getLabelCounter(tc: TracerContext, loopID: List[SchemeExp]): Integer =
    tc.labelCounters.getOrElse(loopID, 0)

  def incLabelCounter(tc: TracerContext, loopID: List[SchemeExp]): SchemeTracerContext = {
    val oldCounter = getLabelCounter(tc, loopID)
    val newLabelCounters: Map[List[SchemeExp], Integer] = tc.labelCounters.updated(loopID, oldCounter + 1)
    tc.copy(labelCounters = newLabelCounters)
  }

}
