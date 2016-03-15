/**
  * Created by mvdcamme on 02/02/16.
  */
class TracerContext[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp]
    (sem : SemanticsTraced[Exp, Abs, Addr, Time], traceOptimizer : TraceOptimizer[Exp, Abs, Addr, Time]) {

  val PRINT_ENTIRE_TRACE = true

  val semantics = sem
  type Label = semantics.Label
  type InstructionReturn = semantics.InstructionReturn
  type ProgramState = HybridMachine[Exp, Time]#ProgramState
  type TraceInstruction = HybridMachine[Exp, Time]#TraceInstruction
  type Trace = HybridMachine[Exp, Time]#TraceWithStates
  type AssertedTrace = HybridMachine[Exp, Time]#AssertedTrace


  case class TraceInfo(label : Label, boundVariables : List[String])
  case class TraceNode(label : Label, trace : AssertedTrace)

  case class TracerContext(traceInfo : Option[TraceInfo], labelCounters : Map[Label, Integer],
                           traceNodes : List[TraceNode], trace : Trace)

  /*
   * Generating tracer context
   */

  def newTracerContext = new TracerContext(None, Map(), List(), List())

  /*
   * Start tracing
   */

  def startTracingLabel(tracerContext: TracerContext, label: Label, boundVariables : List[String]) : TracerContext = tracerContext match {
    case TracerContext(_, labelCounters, traceNodes, _) =>
      new TracerContext(Some(TraceInfo(label, boundVariables)), labelCounters, traceNodes, List())
  }

  /*
   * Stop tracing
   */

  def stopTracing(tracerContext: TracerContext, isLooping : Boolean, traceEndedInstruction: Option[TraceInstruction]) : TracerContext = {
    var finishedTracerContext : TracerContext = tracerContext
    if (! isLooping) {
      finishedTracerContext = appendTrace(tracerContext, List((traceEndedInstruction.get, None)))
    }
    val traceNodeAddedTc = addTrace(finishedTracerContext)
    val newTrace = getTrace(traceNodeAddedTc, tracerContext.traceInfo.get.label).trace
    println(s"Complete trace")
    if (PRINT_ENTIRE_TRACE) {
      println("------------ START TRACE ------------")
      println("------------- ASSERTIONS ------------")
      for (action <- newTrace._1) {
        println(action)
      }
      println("-------------- ACTIONS --------------")
      for (action <- newTrace._2) {
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
    if (traceNode.trace._2.isEmpty) {
      resetTrace()
    }

    val traceHead = currentTraceNode.trace._2.head
    val updatedTraceNode = TraceNode(traceNode.label, (currentTraceNode.trace._1, currentTraceNode.trace._2.tail))
    (traceHead._1, updatedTraceNode)
  }

  /*
   * Adding traces
   */

  private def addTrace(tracerContext: TracerContext) : TracerContext = tracerContext match {
    case TracerContext(traceInfo, labelCounters, traceNodes, trace) =>
      val optimizedAssertedTrace : AssertedTrace = traceOptimizer.optimize(trace, traceInfo.get.boundVariables)
      new TracerContext(traceInfo, labelCounters, new TraceNode(traceInfo.get.label, optimizedAssertedTrace) :: traceNodes, trace)
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
