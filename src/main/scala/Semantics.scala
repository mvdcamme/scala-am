/**
 * This is where the interface of a language's semantics is defined. By defining
 * the semantics of a language, you get an abstract abstract machine for free
 * (but you might need to adapt existing lattices to support values from your
 * language).
 *
 * Semantics should be defined as small-step operational semantics. To define a
 * semantics, you have to implement the Semantics trait. You'll need to
 * specialize it on the type of expression of your language (e.g., for ANF,
 * ANFSemantics specializes on ANFExp). To do so, you need to define what
 * actions should be taken when:
 *   1. Evaluating an expression e (stepEval)
 *   2. Continuing evaluation when a value v has been reached (stepKont)
 *
 * To have a simple overview of how semantics should be defined, look at the
 * ANFSemantics.scala, as it defines semantics of ANF Scheme, a very lightweight
 * language. A more complex definition resides in SchemeSemantics.scala.
 */

trait Semantics[Exp, Abs, Addr, Time] {
  implicit def abs : AbstractValue[Abs]
  implicit def addr : Address[Addr]
  implicit def exp : Expression[Exp]
  implicit def time : Timestamp[Time]



  /******************************************************************************************************************
   *                                              END TRACER_INTERACTION                                            *
  *******************************************************************************************************************/

  type Label = List[Exp]
  type RestartPoint = (Exp, Environment[Addr])

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
  case class TracingSignalFalse() extends TracingSignal
  case class TracingSignalStart(label : Label) extends TracingSignal
  case class TracingSignalEnd(label : Label, restartPoint: RestartPoint) extends TracingSignal

  /*
   * Interpreter return
   */
  //case class InterpreterReturn(programState : ProgramState, trace : Trace, tracingSignal: TracingSignal)
  case class InterpreterReturn(trace : Trace, tracingSignal: TracingSignal)

  /*
   * Instruction return
   */
  trait InstructionReturn
  case class TraceStep() extends InstructionReturn
  case class GuardFailed(restartPoint : RestartPoint) extends InstructionReturn
  case class EndTrace(restartPoint: RestartPoint) extends InstructionReturn
  case class LoopTrace() extends InstructionReturn

  /*
   * Trace instruction
   */
  type TraceInstruction = Action[Exp, Abs, Addr]

  case class ActionEndTrace(restartPoint : RestartPoint) extends Action[Exp, Abs, Addr]

  val endTraceInstruction : RestartPoint => TraceInstruction = new ActionEndTrace(_)

  /*
   * Trace
   */
  type Trace = List[TraceInstruction]

  protected def interpreterReturn(action: Action[Exp, Abs, Addr]) : InterpreterReturn =
    new InterpreterReturn(List(action), new TracingSignalFalse)

  protected def interpreterReturnStart(action: Action[Exp, Abs, Addr], label : Label) : InterpreterReturn =
    new InterpreterReturn(List(action), new TracingSignalStart(label))


  /******************************************************************************************************************
   *                                              END TRACER_INTERACTION                                            *
   ******************************************************************************************************************/

  def atomicEval(e: Exp, ρ: Environment[Addr], σ: Store[Addr, Abs]): Option[(Abs, Set[Addr])]

  def bindArgs(l: List[(String, (Exp, Abs))], ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): (Environment[Addr], Store[Addr, Abs])

  /**
    * Defines how to convert continuation frames: all addresses and values included either directly or
    * indirectly, i.e., through the environment, should be converted via the given conversion functions
    * for addresses and values
    * @param convertAddress The conversion function for addresses
    * @param convertValue The conversion function for values
    * @param frame The frame whose values and addresses should be converted
    * @return The converted frame
    */
  def convertFrame(convertAddress : Addr => Addr, convertValue : Abs => Abs)(frame : Frame) : Frame

  /**
   * Defines what actions should be taken when an expression e needs to be
   * evaluated, in environment e with store σ
   */
  def stepEval(e: Exp, ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[InterpreterReturn]
  /**
   * Defines what actions should be taken when a value v has been reached, and
   * the topmost frame is frame
   */
  def stepKont(v: Abs, frame: Frame, σ: Store[Addr, Abs], t: Time): Set[InterpreterReturn]

  /**
   * Defines how to parse a program
   */
  def parse(program: String): Exp
}

/**
 * The different kinds of actions that can be taken by the abstract machine
 */
abstract class Action[Exp : Expression, Abs : AbstractValue, Addr : Address]

abstract class ActionGuard[Exp : Expression, Abs : AbstractValue, Addr : Address, RestartPoint](rp: RestartPoint) extends Action[Exp, Abs, Addr] {
    val restartPoint = rp
}
case class ActionGuardTrue[Exp : Expression, Abs : AbstractValue, Addr : Address, RestartPoint](rp: RestartPoint) extends ActionGuard[Exp, Abs, Addr, RestartPoint](rp)
case class ActionGuardFalse[Exp : Expression, Abs : AbstractValue, Addr : Address, RestartPoint](rp: RestartPoint) extends ActionGuard[Exp, Abs, Addr, RestartPoint](rp)

case class ActionPushVal[Exp : Expression, Abs : AbstractValue, Addr : Address]() extends Action[Exp, Abs, Addr]
case class ActionPrimCall[Exp : Expression, Abs : AbstractValue, Addr : Address](n : Integer, fExp : Exp, argsExps : List[Exp]) extends Action[Exp, Abs, Addr]
case class ActionLookupVariable[Exp : Expression, Abs : AbstractValue, Addr : Address](varName : String, read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]()) extends Action[Exp, Abs, Addr]
case class ActionExtendEnv[Exp : Expression, Abs : AbstractValue, Addr : Address](varName : String) extends Action[Exp, Abs, Addr]
case class ActionPushEnv[Exp : Expression, Abs : AbstractValue, Addr : Address]
(e: Exp, frame : Frame, ρ: Environment[Addr], σ: Store[Addr, Abs], read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]()) extends Action[Exp, Abs, Addr]

/**
 * A value is reached by the interpreter. As a result, a continuation will be
 * popped with the given reached value.
 */
case class ActionReachedValue[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (v: Abs, σ: Store[Addr, Abs],
    read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]()) extends Action[Exp, Abs, Addr]
/**
 * A frame needs to be pushed on the stack, and the interpretation continues by
 * evaluating expression e in environment ρ.
 */
case class ActionPush[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (e: Exp, frame : Frame, σ: Store[Addr, Abs],
    read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]()) extends Action[Exp, Abs, Addr]
/**
 * Evaluation continues with expression e in environment ρ
 */
case class ActionEval[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (e: Exp, ρ: Environment[Addr], σ: Store[Addr, Abs],
    read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]()) extends Action[Exp, Abs, Addr]

/**
 * Similar to ActionEval, but only used when stepping inside a function's body
 * (clo is therefore the function stepped into). The number of arguments should
 * also be provided, as they can be needed by the abstract machine.
 */
case class ActionStepIn[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (fexp: Exp, clo: (Exp, Environment[Addr]), e: Exp, args : List[String], argsv : List[Exp], n : Integer,
    read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]()) extends Action[Exp, Abs, Addr]
/**
 * An error has been reached
 */
case class ActionError[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (reason: String) extends Action[Exp, Abs, Addr]
/**
 * Spawns a new thread that evaluates expression e in environment ρ. The current
 * thread continues its execution by performing action act.
 */
case class ActionSpawn[TID : ThreadIdentifier, Exp : Expression, Abs : AbstractValue, Addr : Address]
  (t: TID, e: Exp, ρ: Environment[Addr], act: Action[Exp, Abs, Addr],
    read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]()) extends Action[Exp, Abs, Addr]
/**
 * Waits for the execution of a thread, with tid as its identifier.
 */
case class ActionJoin[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (tid: Abs, σ: Store[Addr, Abs],
    read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]()) extends Action[Exp, Abs, Addr]

/**
 * Base class for semantics that define some helper methods
 */
abstract class BaseSemantics[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Semantics[Exp, Abs, Addr, Time] {
  /* wtf scala */
  def abs = implicitly[AbstractValue[Abs]]
  def addr = implicitly[Address[Addr]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[Time]]

  /**
   * Binds arguments in the environment and store. Arguments are given as a list
   * of triple, where each triple is made of:
   *   - the name of the argument
   *   - the expression evaluated to get the argument's value
   *   - the value of the argument
   */
  def bindArgs(l: List[(String, (Exp, Abs))], ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): (Environment[Addr], Store[Addr, Abs]) =
    l.foldLeft((ρ, σ))({ case ((ρ, σ), (name, (exp, value))) => {
      val a = addr.variable(name, t)
      (ρ.extend(name, a), σ.extend(a, value))
    }})
}
