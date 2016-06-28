case class TraceInfo[Exp : Expression, Time : Timestamp](boundVariables: List[String], startState: HybridMachine[Exp, Time]#PS)

trait Tracer[Exp, Time] {

  /* An opaque type storing bookkeeping information for the actual, concrete tracer */
  type TracerContext

  type TraceInstruction = Action[Exp, HybridLattice.L, HybridAddress.A]
  type TraceWithoutStates = List[TraceInstruction]
  type TraceInstructionInfo = (TraceInstruction, Option[TraceInformation[HybridLattice.L]])
  type TraceWithInfos = List[TraceInstructionInfo]

  type Trace = TraceWithInfos

  trait Label

  case class NormalLabel(loopID: List[Exp]) extends Label
  case class GuardLabel(loopID: List[Exp], guardID: Integer) extends Label

  case class TraceNode[Trace](label: Label, trace: Trace, info: TraceInfo[Exp, Time])


  def getLoopID(label: Label): List[Exp]

  /*
   * Generating tracer context
   */

  def newTracerContext: TracerContext

  /*
   * Start tracing
   */

  def startTracingLoop(tc: TracerContext,
                       loopID: List[Exp],
                       boundVariables: List[String],
                       startState: HybridMachine[Exp, Time]#PS): TracerContext

  def startTracingGuard(tc: TracerContext,
                        loopID: List[Exp],
                        guardID: Integer,
                        boundVariables: List[String],
                        startState: HybridMachine[Exp, Time]#PS): TracerContext

  /*
   * Stop tracing
   */

  def stopTracing(tc: TracerContext, isLooping: Boolean,
                  traceEndedInstruction: Option[TraceInstruction], someAnalysisOutput: StaticAnalysisResult): TracerContext

  /*
   * Finding traces
   */

  def getLoopTrace(tc: TracerContext, loopID: List[Exp]): TraceNode[TraceFull[Exp, Time]]

  def getGuardTrace(tc: TracerContext, guardID: Integer): TraceNode[TraceFull[Exp, Time]]

  def loopTraceExists(tc: TracerContext, loopID: List[Exp]): Boolean

  def guardTraceExists(tc: TracerContext, guardID: Integer): Boolean

  /*
   * Recording traces
   */

  def appendTrace(tc: TracerContext, newPart: Trace): TracerContext

  def isTracingLoop(tc: TracerContext, loopID: List[Exp]): Boolean

  /*
   * Executing traces
   */

  def stepTrace(traceNode: TraceNode[TraceFull[Exp, Time]],
                tc: TracerContext): (TraceInstruction, TraceNode[TraceFull[Exp, Time]], Boolean)

  /*
   * Hotness counter
   */

  def getLabelCounter(tc: TracerContext, loopID: List[Exp]): Integer

  def incLabelCounter(tc: TracerContext, loopID: List[Exp]): TracerContext

}

case class TraceFull[Exp : Expression, Time : Timestamp]
  (info: TraceInfo[Exp, Time],
   assertions: List[Action[Exp, HybridLattice.L, HybridAddress.A]],
   trace: List[(Action[Exp, HybridLattice.L, HybridAddress.A], Option[TraceInformation[HybridLattice.L]])])