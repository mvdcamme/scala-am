/**
  * Created by mvdcamme on 02/02/16.
  */
class TracerContext[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp]
    (sem : SemanticsTraced[Exp, Abs, Addr, Time], traceOptimizer : TraceOptimizer[Exp, Abs, Addr, Time]) {

  val PRINT_ENTIRE_TRACE = false

  val semantics = sem
  type Label = semantics.Label
  type Trace = semantics.Trace
  type InstructionReturn = semantics.InstructionReturn
  type TraceInstruction = semantics.TraceInstruction
  type RestartPoint = semantics.RestartPoint
  type ExecutionPhase = semantics.ExecutionPhase.ExecutionPhase

  val NI = semantics.ExecutionPhase.NI
  val TE = semantics.ExecutionPhase.TE
  val TR = semantics.ExecutionPhase.TR

  case class TraceNode(label : Label, trace : Trace)

  case class TracerContext(label : Option[Label], labelCounters : Map[Label, Integer],
                           traceNodes : List[TraceNode], trace : Trace,
                           executionPhase : ExecutionPhase, traceExecuting : Option[TraceNode])

  /*
   * Generating tracer context
   */

  def newTracerContext = new TracerContext(None, Map(), List(), List(), NI, None)

  /*
   * Start tracing
   */

  def startTracingLabel(tracerContext: TracerContext, label: Label) : TracerContext = tracerContext match {
    case TracerContext(_, labelCounters, traceNodes, _, ep, te) =>
      new TracerContext(Some(label), labelCounters, traceNodes, List(), TR, te)
  }

  /*
   * Stop tracing
   */

  def stopTracing(tracerContext: TracerContext, isLooping : Boolean, restartPoint: Option[RestartPoint]) : TracerContext = {
    var finishedTracerContext : TracerContext = tracerContext
    if (! isLooping) {
      finishedTracerContext = appendTrace(tracerContext, List(semantics.endTraceInstruction(restartPoint.get)))
    }
    println(s"Complete trace")
    if (PRINT_ENTIRE_TRACE) {
      println("------------ START TRACE ------------")
      for (action <- finishedTracerContext.trace) {
        println(action)
      }
      println("------------ END TRACE ------------")
    }
    val traceNodeAddedTc = addTrace(finishedTracerContext)
    val newTrace = traceNodeAddedTc.trace
    clearTrace(traceNodeAddedTc)
  }

  /*
   * Finding traces
   */

  private def searchTrace(tracerContext: TracerContext, label: Label) : Option[TraceNode] = tracerContext match {
    case TracerContext(_, _, traceNodes, _, _, _) => traceNodes.find({traceNode => traceNode.label == label})
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
    new TracerContext(tracerContext.label, tracerContext.labelCounters, tracerContext.traceNodes,
                      tracerContext.trace ++ newPart, tracerContext.executionPhase, tracerContext.traceExecuting)

  private def clearTrace(tracerContext: TracerContext) : TracerContext = tracerContext match {
    case TracerContext(_, labelCounters, traceNodes, _, _, _) =>
      new TracerContext(None, labelCounters, traceNodes, List(), NI, None)
  }

  def isTracing(tracerContext: TracerContext) : Boolean = tracerContext.label match {
    case Some(_) => true
    case None => false
  }

  def isTracingLabel(tracerContext: TracerContext, label: Label) : Boolean = tracerContext.label match {
    case Some(tcLabel) => tcLabel == label
    case None => false
  }

  /*
   * Executing traces
   */

  def isExecuting(tracerContext: TracerContext) : Boolean = tracerContext.executionPhase match {
    case TE => true
    case _ => false
  }

  def isTraceEmpty(tracerContext: TracerContext) : Boolean = tracerContext.traceExecuting match {
    case Some(traceNode) => traceNode.trace.isEmpty
    case None => throw new Exception("Error: no trace is being executed")
  }

  def stepTrace(tracerContext: TracerContext) : (TraceInstruction, TracerContext) = tracerContext.traceExecuting match {
    case Some(TraceNode(label, trace)) =>

      var traceNodeExecuting = TraceNode(label, trace)
      def resetTrace(traceNode : TraceNode) : TraceNode = {
        println("Resetting the trace")
        getTrace(tracerContext, traceNode.label)
      }

      /*
       * Make sure the trace isn't empty
       */
      if (trace.isEmpty) {
        traceNodeExecuting = resetTrace(traceNodeExecuting)
      }

      val traceHead = traceNodeExecuting.trace.head

      val newTraceNode = TraceNode(label, traceNodeExecuting.trace.tail)
      val newTc = new TracerContext(tracerContext.label, tracerContext.labelCounters, tracerContext.traceNodes,
                                    tracerContext.trace, tracerContext.executionPhase, Some(newTraceNode))
      (traceHead, newTc)
    case None => throw new Exception("Error: no trace is being executed")
  }

  def stopExecuting(tracerContext: TracerContext) : TracerContext =
    new TracerContext(None, tracerContext.labelCounters, tracerContext.traceNodes, List(), NI, None)

  /*
   * Adding traces
   */

  private def addTrace(tracerContext: TracerContext) : TracerContext = tracerContext match {
    case TracerContext(label, labelCounters, traceNodes, trace, ep, te) =>
      val optimizedTrace = traceOptimizer.optimize(trace)
      new TracerContext(label, labelCounters, new TraceNode(label.get, optimizedTrace) :: traceNodes, trace, ep, te)
  }

  /*
   * Hotness counter
   */

  def getLabelCounter(tc: TracerContext, label: Label) : Integer =
    tc.labelCounters.getOrElse(label, 0)

  def incLabelCounter(tc : TracerContext, label : Label) : TracerContext = tc match {
    case TracerContext(labelTracing, labelCounters, traceNodes, trace, ep, te) =>
      val oldCounter = getLabelCounter(tc, label)
      val newLabelCounters : Map[Label, Integer] = labelCounters.updated(label, oldCounter + 1)
      new TracerContext(labelTracing, newLabelCounters, traceNodes, trace, ep, te)
  }

}
