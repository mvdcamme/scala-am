/**
  * Created by mvdcamme on 02/02/16.
  */
class TracerContext[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp](sem : Semantics[Exp, Abs, Addr, Time]) {

  val semantics = sem
  type Label = semantics.Label
  type Trace = semantics.Trace
  type InstructionReturn = semantics.InstructionReturn
  type TraceInstruction = semantics.TraceInstruction
  type RestartPoint = semantics.RestartPoint

  case class TraceNode(label : Label, trace : Trace)

  case class TracerContext(label : Option[Label], traceNodes : List[TraceNode], trace : Trace)

  def newTracerContext = new TracerContext(None, List(), List())

  /*
   * Start tracing
   */

  def startTracingLabel(tracerContext: TracerContext, label: Label): TracerContext = tracerContext match {
    case TracerContext(_, traceNodes, _) => new TracerContext(Some(label), traceNodes, List())
  }

  /*
   * Stop tracing
   */

  def stopTracing(tracerContext: TracerContext, isLooping : Boolean, restartPoint: Some[RestartPoint]) {
    var finishedTracerContext : TracerContext = tracerContext
    if (isLooping) {
      finishedTracerContext = appendTrace(tracerContext, List(semantics.loopTraceInstruction))
    } else {
      finishedTracerContext = appendTrace(tracerContext, List(semantics.endTraceInstruction(restartPoint.get)))
    }
    val newTracerContext = addTrace(finishedTracerContext)
    clearTrace(newTracerContext)
  }

  /*
   * Finding traces
   */

  private def searchTrace(tracerContext: TracerContext, label: Label) : Option[TraceNode] = tracerContext match {
    case TracerContext(_, traceNodes, _) => traceNodes.find({traceNode => traceNode.label == label})
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
    new TracerContext(tracerContext.label, tracerContext.traceNodes, tracerContext.trace ++ newPart)

  private def clearTrace(tracerContext: TracerContext) : TracerContext = tracerContext match {
    case TracerContext(label, traceNodes, trace) => new TracerContext(label, traceNodes, List())
  }

  def isTracingLabel(tracerContext: TracerContext, label: Label) : Boolean =
  tracerContext.label == label

  /*
   * Adding traces
   */

  private def addTrace(tracerContext: TracerContext) : TracerContext = tracerContext match {
    case TracerContext(label, traceNodes, trace) => new TracerContext(label, (new TraceNode(label.get, trace) :: traceNodes), trace)
  }


}
