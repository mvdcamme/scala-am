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

  case class TraceNode(label : Label, trace : Trace)

  case class TracerContext(label : Option[Label], labelCounters : Map[Label, Integer],
                           traceNodes : List[TraceNode], trace : Trace)

  /*
   * Generating tracer context
   */

  def newTracerContext = new TracerContext(None, Map(), List(), List())

  /*
   * Start tracing
   */

  def startTracingLabel(tracerContext: TracerContext, label: Label) : TracerContext = tracerContext match {
    case TracerContext(_, labelCounters, traceNodes, _) =>
      new TracerContext(Some(label), labelCounters, traceNodes, List())
  }

  /*
   * Stop tracing
   */

  def stopTracing(tracerContext: TracerContext, isLooping : Boolean, restartPoint: Option[RestartPoint[Exp, Abs, Addr]]) : TracerContext = {
    var finishedTracerContext : TracerContext = tracerContext
    if (! isLooping) {
      finishedTracerContext = appendTrace(tracerContext, List(semantics.endTraceInstruction(restartPoint.get)))
    }
    val traceNodeAddedTc = addTrace(finishedTracerContext)
    val newTrace = getTrace(traceNodeAddedTc, tracerContext.label.get).trace
    println(s"Complete trace")
    if (PRINT_ENTIRE_TRACE) {
      println("------------ START TRACE ------------")
      for (action <- newTrace) {
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
    new TracerContext(tracerContext.label, tracerContext.labelCounters, tracerContext.traceNodes,
                      tracerContext.trace ++ newPart)

  private def clearTrace(tracerContext: TracerContext) : TracerContext = tracerContext match {
    case TracerContext(_, labelCounters, traceNodes, _) =>
      new TracerContext(None, labelCounters, traceNodes, List())
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

  def stepTrace(traceNode: TraceNode, tracerContext: TracerContext) : (TraceInstruction, TraceNode) = {
    var currentTraceNode = traceNode
    def resetTrace() = {
      println("Resetting the trace")
      currentTraceNode = getTrace(tracerContext, traceNode.label)
    }

    /*
     * Make sure the trace isn't empty
     */
    if (traceNode.trace.isEmpty) {
      resetTrace()
    }

    val traceHead = currentTraceNode.trace.head
    val updatedTraceNode = TraceNode(traceNode.label, currentTraceNode.trace.tail)
    (traceHead, updatedTraceNode)
  }

  /*
   * Adding traces
   */

  private def addTrace(tracerContext: TracerContext) : TracerContext = tracerContext match {
    case TracerContext(label, labelCounters, traceNodes, trace) =>
      val optimizedTrace = traceOptimizer.optimize(trace)
      new TracerContext(label, labelCounters, new TraceNode(label.get, optimizedTrace) :: traceNodes, trace)
  }

  /*
   * Hotness counter
   */

  def getLabelCounter(tc: TracerContext, label: Label) : Integer =
    tc.labelCounters.getOrElse(label, 0)

  def incLabelCounter(tc : TracerContext, label : Label) : TracerContext = tc match {
    case TracerContext(labelTracing, labelCounters, traceNodes, trace) =>
      val oldCounter = getLabelCounter(tc, label)
      val newLabelCounters : Map[Label, Integer] = labelCounters.updated(label, oldCounter + 1)
      new TracerContext(labelTracing, newLabelCounters, traceNodes, trace)
  }

}
