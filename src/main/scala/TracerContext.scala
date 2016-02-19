/**
  * Created by mvdcamme on 02/02/16.
  */
class TracerContext[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp](sem : SemanticsTraced[Exp, Abs, Addr, Time]) {

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

  case class TracerContext(label : Option[Label], traceNodes : List[TraceNode], trace : Trace, executionPhase : ExecutionPhase, traceExecuting : Option[TraceNode])

  /*
   * Generating tracer context
   */

  def newTracerContext = new TracerContext(None, List(), List(), NI, None)

  /*
   * Start tracing
   */

  def startTracingLabel(tracerContext: TracerContext, label: Label) : TracerContext = tracerContext match {
    case TracerContext(_, traceNodes, _, ep, te) => new TracerContext(Some(label), traceNodes, List(), ep, te)
  }

  /*
   * Stop tracing
   */

  def stopTracing(tracerContext: TracerContext, isLooping : Boolean, restartPoint: Option[RestartPoint]) : TracerContext = {
    var finishedTracerContext : TracerContext = tracerContext
    if (! isLooping) {
      finishedTracerContext = appendTrace(tracerContext, List(semantics.endTraceInstruction(restartPoint.get)))
    }
    val newTracerContext = addTrace(finishedTracerContext)
    val newTrace = newTracerContext.trace
    println(s"Complete trace: $newTrace")
    clearTrace(newTracerContext)
  }

  /*
   * Finding traces
   */

  private def searchTrace(tracerContext: TracerContext, label: Label) : Option[TraceNode] = tracerContext match {
    case TracerContext(_, traceNodes, _, _, _) => traceNodes.find({traceNode => traceNode.label == label})
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
    new TracerContext(tracerContext.label, tracerContext.traceNodes, tracerContext.trace ++ newPart, tracerContext.executionPhase, tracerContext.traceExecuting)

  private def clearTrace(tracerContext: TracerContext) : TracerContext = tracerContext match {
    case TracerContext(_, traceNodes, _, ep, te) => new TracerContext(None, traceNodes, List(), ep, te)
  }

  def isTracing(tracerContext: TracerContext) : Boolean = tracerContext.label match {
    case Some(_) => true
    case None => false
  }

  def isTracingLabel(tracerContext: TracerContext, label: Label) : Boolean = tracerContext.label match {
    case Some(tcLabel) => println(s"Currently tracing $tcLabel; comparing with $label"); tcLabel == label
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
      val newTc = new TracerContext(tracerContext.label, tracerContext.traceNodes, tracerContext.trace, tracerContext.executionPhase, Some(newTraceNode))
      (traceHead, newTc)
    case None => throw new Exception("Error: no trace is being executed")
  }

  def stopExecuting(tracerContext: TracerContext) : TracerContext =
    new TracerContext(None, tracerContext.traceNodes, List(), NI, None)

  /*
   * Adding traces
   */

  private def addTrace(tracerContext: TracerContext) : TracerContext = tracerContext match {
    case TracerContext(label, traceNodes, trace, ep, te) => new TracerContext(label, new TraceNode(label.get, trace) :: traceNodes, trace, ep, te)
  }


}
