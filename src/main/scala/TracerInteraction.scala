/**
  * Created by mvdcamme on 02/02/16.
  */

class TracerInteraction[Exp : Expression, ProgramState, RestartPoint] {

  type Label = List[Exp]

  /*
   * Enumeration of possible execution phases
   */
  object ExecutionPhase extends Enumeration {
    type ExecutionPhase = Value
    val NI = Value("NormalInterpretation")
    val TR = Value("TraceRecording")
    val TE = Value("TraceExecution")
  }

  /*
   * Tracing signals
   */
  trait TracingSignal
  case class TracingSignalFalse extends TracingSignal
  case class TracingSignalStart(label : Label) extends TracingSignal
  case class TracingSignalEnd(label : Label) extends TracingSignal

  /*
   * Interpreter return
   */
  case class InterpreterReturn(programState : ProgramState, trace : Trace, tracingSignal: TracingSignal)

  /*
   * Instruction return
   */
  trait InstructionReturn
  case class TraceStep(programState: ProgramState) extends InstructionReturn
  case class GuardFailed(restartPoint : RestartPoint) extends InstructionReturn
  case class EndTrace(restartPoint: RestartPoint) extends InstructionReturn
  case class LoopTrace extends InstructionReturn

  /*
   * Trace instruction
   */
  type TraceInstruction = ProgramState => InstructionReturn

  /*
   * Trace
   */
  type Trace = List[TraceInstruction]


}


