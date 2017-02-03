case class TraceInfo[Exp: Expression, Addr: Address, Time: Timestamp](
    boundVariables: List[(String, Addr)],
    startState: ConcreteTracingProgramState[Exp, Addr, Time],
    parentTraceLabel: Option[Label[Exp]])

trait Label[Exp]

case class NormalLabel[Exp: Expression](loopID: List[Exp]) extends Label[Exp]
case class GuardLabel[Exp: Expression](loopID: List[Exp], guardID: Integer)
    extends Label[Exp]

trait Tracer[Exp, Abs, Addr, Time] {

  /* An opaque type storing bookkeeping information for the actual, concrete tracer */
  type TracerContext

  type TraceInstruction = ActionTrace[Exp, Abs, Addr]
  type TraceWithoutStates = List[TraceInstruction]
  type TraceInstructionInfo = (TraceInstruction, CombinedInfos[Abs, Addr])
  type TraceWithInfos = List[TraceInstructionInfo]

  type Trace = TraceWithInfos

  case class TraceNode[Trace](label: Label[Exp],
                              trace: Trace,
                              info: TraceInfo[Exp, Addr, Time])

  def getLoopID(label: Label[Exp]): List[Exp]

  /*
   * Generating tracer context
   */

  def newTracerContext: TracerContext

  /*
   * Start tracing
   */

  def startTracingLoop(
      tc: TracerContext,
      loopID: List[Exp],
      boundVariables: List[(String, Addr)],
      startState: ConcreteTracingProgramState[Exp, Addr, Time]): TracerContext

  def startTracingGuard(
      tc: TracerContext,
      loopID: List[Exp],
      guardID: Integer,
      parentTraceLabel: Label[Exp],
      boundVariables: List[(String, Addr)],
      startState: ConcreteTracingProgramState[Exp, Addr, Time]): TracerContext

  /*
   * Stop tracing
   */

  def stopTracing(
      tc: TracerContext,
      isLooping: Boolean,
      traceEndedInstruction: Option[TraceInstruction],
      state: ConcreteTracingProgramState[Exp, Addr, Time]): TracerContext

  /*
   * Finding traces
   */

  def getLoopTrace(
      tc: TracerContext,
      loopID: List[Exp]): TraceNode[TraceFull[Exp, Abs, Addr, Time]]

  def getGuardTrace(
      tc: TracerContext,
      guardID: Integer): TraceNode[TraceFull[Exp, Abs, Addr, Time]]

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

  def stepTrace(traceNode: TraceNode[TraceFull[Exp, Abs, Addr, Time]],
                tc: TracerContext)
    : (TraceInstruction, TraceNode[TraceFull[Exp, Abs, Addr, Time]], Boolean)

  /*
   * Hotness counter
   */

  def getLabelCounter(tc: TracerContext, loopID: List[Exp]): Integer

  def incLabelCounter(tc: TracerContext, loopID: List[Exp]): TracerContext

}

case class TraceFull[Exp: Expression,
                     Abs: JoinLattice,
                     Addr: Address,
                     Time: Timestamp](
                                       info: TraceInfo[Exp, Addr, Time],
                                       assertions: List[ActionTrace[Exp, Abs, Addr]],
                                       trace: List[(ActionTrace[Exp, Abs, Addr], CombinedInfos[Abs, Addr])])
