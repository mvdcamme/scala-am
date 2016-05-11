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

trait BasicSemantics[Exp, Abs, Addr, Time] {
  implicit def abs: AbstractValue[Abs]
  implicit def addr: Address[Addr]
  implicit def exp: Expression[Exp]
  implicit def time: Timestamp[Time]

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
  def convertFrame(convertAddress: Addr => Addr, convertValue: Abs => Abs)(frame: Frame): Frame

  /**
    * Defines how to parse a program
    */
  def parse(program: String): Exp
}

trait Semantics[Exp, Abs, Addr, Time] extends BasicSemantics[Exp, Abs, Addr, Time] {
  /**
    * Defines what actions should be taken when an expression e needs to be
    * evaluated, in environment e with store σ
    */
  def stepEval(e: Exp, ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[Action[Exp, Abs, Addr]]
  /**
    * Defines what actions should be taken when a value v has been reached, and
    * the topmost frame is frame
    */
  def stepKont(v: Abs, frame: Frame, σ: Store[Addr, Abs], t: Time): Set[Action[Exp, Abs, Addr]]
}

trait SemanticsTraced[Exp, Abs, Addr, Time] extends BasicSemantics[Exp, Abs, Addr, Time] {

  class InvalidArityException extends Exception

  def bindClosureArgs(clo: Abs, argsv: List[(Exp, Abs)], σ: Store[Addr, Abs], t: Time): Set[Option[(Environment[Addr], Store[Addr, Abs], Exp)]]

  /*
   * Instruction return
   */
  trait InstructionReturn

  case class TraceStep() extends InstructionReturn
  case class GuardFailed(restartPoint: RestartPoint[Exp, Abs, Addr]) extends InstructionReturn
  case class EndTrace(restartPoint: RestartPoint[Exp, Abs, Addr]) extends InstructionReturn
  case class LoopTrace() extends InstructionReturn

  val endTraceInstruction: RestartPoint[Exp, Abs, Addr] => Action[Exp, Abs, Addr] = new ActionEndTrace(_)

  protected def interpreterReturn(actions: List[Action[Exp, Abs, Addr]]): Step[Exp, Abs, Addr] =
    new Step(actions, new TracingSignalFalse)

  protected def interpreterReturnStart(actions: List[Action[Exp, Abs, Addr]], label: List[Exp]): Step[Exp, Abs, Addr] =
    new Step(actions, new TracingSignalStart(label))

  /**
    * Defines what actions should be taken when an expression e needs to be
    * evaluated, in environment e with store σ
    */
  def stepEval(e: Exp, ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[Step[Exp, Abs, Addr]]

  /**
    * Defines what actions should be taken when a value v has been reached, and
    * the topmost frame is frame
    */
  def stepKont(v: Abs, frame: Frame, σ: Store[Addr, Abs], t: Time): Set[Step[Exp, Abs, Addr]]

  def getClosureBody(frame: Frame): Option[List[Exp]]

  def absSem: Semantics[Exp, Abs, Addr, Time]

  def convertToAbsSemanticsFrame(frame: Frame,
                                 ρ: Environment[Addr],
                                 vStack: List[Storable[Abs, Addr]]):
    (Frame, List[Storable[Abs, Addr]], Environment[Addr])
}

/**
  * Base class for semantics that define some helper methods
  */
abstract class BaseSemantics[Exp: Expression, Abs: AbstractValue, Addr: Address, Time: Timestamp]
  extends Semantics[Exp, Abs, Addr, Time] {
  /* wtf scala */
  def abs = implicitly[AbstractValue[Abs]]
  def addr = implicitly[Address[Addr]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[Time]]

  /**
    * Binds arguments in the environment and store. Arguments are given as a list
    * of triple, where each triple is made of:
    * - the name of the argument
    * - the expression evaluated to get the argument's value
    * - the value of the argument
    */
  def bindArgs(l: List[(String, (Exp, Abs))], ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): (Environment[Addr], Store[Addr, Abs]) =
    l.foldLeft((ρ, σ))({ case ((ρ, σ), (name, (exp, value))) => {
      val a = addr.variable(name, t)
      (ρ.extend(name, a), σ.extend(a, value))
    }
    })
}

abstract class BaseSemanticsTraced[Exp: Expression, Abs: AbstractValue, Addr: Address, Time: Timestamp]
  (override val absSem: Semantics[Exp, Abs, Addr, Time])
  extends SemanticsTraced[Exp, Abs, Addr, Time] {
  /* wtf scala */
  def abs = implicitly[AbstractValue[Abs]]
  def addr = implicitly[Address[Addr]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[Time]]

  /**
    * Binds arguments in the environment and store. Arguments are given as a list
    * of triple, where each triple is made of:
    * - the name of the argument
    * - the expression evaluated to get the argument's value
    * - the value of the argument
    */
  def bindArgs(l: List[(String, (Exp, Abs))], ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): (Environment[Addr], Store[Addr, Abs]) =
    l.foldLeft((ρ, σ))({ case ((ρ, σ), (name, (exp, value))) => {
      val a = addr.variable(name, t)
      (ρ.extend(name, a), σ.extend(a, value))
    }
    })
}


/*********************************************************************************************************************
 *                                                        ACTIONS                                                    *
 *********************************************************************************************************************/

/**
  * The different kinds of actions that can be taken by the abstract machine
  */
abstract class Action[Exp : Expression, Abs : AbstractValue, Addr : Address]

/**
  * A value is reached by the interpreter. As a result, a continuation will be
  * popped with the given reached value.
  */
case class ActionReachedValue[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (v: Abs, σ: Store[Addr, Abs],
   read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]

/**
  * A frame needs to be pushed on the stack, and the interpretation continues by
  * evaluating expression e in environment ρ
  */
case class ActionPush[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (e: Exp, frame: Frame, ρ: Environment[Addr], σ: Store[Addr, Abs],
   read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]

/**
  * Evaluation continues with expression e in environment ρ
  */
case class ActionEval[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (e: Exp, ρ: Environment[Addr], σ: Store[Addr, Abs],
   read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]

/**
  * Similar to ActionEval, but only used when stepping inside a function's body
  * (clo is therefore the function stepped into). The expressions and values of
  * the arguments should also be provided, as they can be needed by the abstract
  * machine.
  */
case class ActionStepIn[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (fexp: Exp, clo: (Exp, Environment[Addr]), e: Exp,
   ρ: Environment[Addr], σ: Store[Addr, Abs], argsv: List[(Exp, Abs)],
   read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]

/**
  * An error has been reached
  */
case class ActionError[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (reason: String)
  extends Action[Exp, Abs, Addr]

/**
  * Spawns a new thread that evaluates expression e in environment ρ. The current
  * thread continues its execution by performing action act.
  */
case class ActionSpawn[TID: ThreadIdentifier, Exp : Expression, Abs : AbstractValue, Addr : Address]
  (t: TID, e: Exp, ρ: Environment[Addr], act: Action[Exp, Abs, Addr],
   read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]

/**
  * Waits for the execution of a thread, with tid as its identifier.
  */
case class ActionJoin[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (tid: Abs, σ: Store[Addr, Abs],
   read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]


/*******************************************************************************************************************
*                                                   TRACED ACTIONS                                                 *
********************************************************************************************************************/

trait RestartPoint[Exp, Abs, Addr]

case class RestartAssertion[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends RestartPoint[Exp, Abs, Addr]
case class RestartFromControl[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (newControl: Exp)
  extends RestartPoint[Exp, Abs, Addr]
case class RestartGuardDifferentClosure[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (action : ActionStepInT[Exp, Abs, Addr])
  extends RestartPoint[Exp, Abs, Addr]
case class RestartGuardDifferentPrimitive[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (action : ActionPrimCallT[Exp, Abs, Addr])
  extends RestartPoint[Exp, Abs, Addr]
case class RestartTraceEnded[Exp, Abs, Addr]()
  extends RestartPoint[Exp, Abs, Addr]

abstract class ActionGuardT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (val rp: RestartPoint[Exp, Abs, Addr])
  extends Action[Exp, Abs, Addr]

case class ActionAllocVarsT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (varNames : List[String])
  extends Action[Exp, Abs, Addr]
case class ActionCreateClosureT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (λ : Exp)
  extends Action[Exp, Abs, Addr]
case class ActionEndClosureCallT[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends Action[Exp, Abs, Addr]
case class ActionEndPrimCallT[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends Action[Exp, Abs, Addr]
case class ActionEndTrace[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (restartPoint: RestartPoint[Exp, Abs, Addr])
  extends Action[Exp, Abs, Addr]

/**
* An error has been reached
*/
case class ActionErrorT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (reason: String)
  extends Action[Exp, Abs, Addr]
/**
  * A frame needs to be pushed on the stack, and the interpretation continues by
  * evaluating expression e in environment ρ.
  */
case class ActionEvalPushT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (e: Exp, frame: Frame, read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]
/**
  * Evaluation continues with expression e in environment ρ
  */
case class ActionEvalT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (e: Exp, read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]
case class ActionExtendEnvT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (varName : String)
  extends Action[Exp, Abs, Addr]
case class ActionExtendStoreT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (addr: Addr, lit: Abs)
  extends Action[Exp, Abs, Addr]

case class ActionGuardFalseT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (override val rp: RestartPoint[Exp, Abs, Addr])
  extends ActionGuardT[Exp, Abs, Addr](rp)
case class ActionGuardAssertFreeVariable[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (variable : String, originalValue : Abs, override val rp: RestartPoint[Exp, Abs, Addr])
  extends ActionGuardT[Exp, Abs, Addr](rp)
case class ActionGuardTrueT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (override val rp: RestartPoint[Exp, Abs, Addr])
  extends ActionGuardT[Exp, Abs, Addr](rp)
case class ActionGuardSameClosure[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (recordedClosure : Abs, override val rp : RestartGuardDifferentClosure[Exp, Abs, Addr])
  extends ActionGuardT[Exp, Abs, Addr](rp)
case class ActionGuardSamePrimitive[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (recordedPrimitive : Abs, override val rp : RestartGuardDifferentPrimitive[Exp, Abs, Addr])
  extends ActionGuardT[Exp, Abs, Addr](rp)
/**
* Waits for the execution of a thread, with tid as its identifier.
*/
case class ActionJoinTraced[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (tid: Abs, σ: Store[Addr, Abs],
read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]
case class ActionLookupVariableT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (varName : String, read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]
case class ActionLookupVariablePushT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (varName : String, read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]
case class ActionPopKontT[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends Action[Exp, Abs, Addr]
case class ActionPrimCallT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (n: Integer, fExp : Exp, argsExps : List[Exp])
  extends Action[Exp, Abs, Addr]
case class ActionPushKontT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (frame: Frame)
  extends Action[Exp, Abs, Addr]
case class ActionPushValT[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends Action[Exp, Abs, Addr]
/**
* A value is reached by the interpreter. As a result, a continuation will be
* popped with the given reached value.
*/
case class ActionReachedValueT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (v: Abs, read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]
case class ActionReachedValuePushT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (v : Abs, read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]
case class ActionRestoreEnvT[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends Action[Exp, Abs, Addr]
case class ActionRestoreSaveEnvT[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends Action[Exp, Abs, Addr]
case class ActionSaveEnvT[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends Action[Exp, Abs, Addr]
case class ActionSetVarT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (variable : String)
  extends Action[Exp, Abs, Addr]
/**
* Spawns a new thread that evaluates expression e in environment ρ. The current
* thread continues its execution by performing action act.
*/
case class ActionSpawnT[TID : ThreadIdentifier, Exp : Expression, Abs : AbstractValue, Addr : Address]
  (t: TID, e: Exp, ρ: Environment[Addr], act: Action[Exp, Abs, Addr],
   read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]
case class ActionSpecializePrimitive[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (expectedType : AbstractType, primitive: Abs, originalPrimitive : Abs, n : Integer, fExp : Exp, argsExps : List[Exp])
  extends Action[Exp, Abs, Addr]
/**
* Similar to ActionEval, but only used when stepping inside a function's body
* (clo is therefore the function stepped into). The number of arguments should
* also be provided, as they can be needed by the abstract machine.
*/
case class ActionStepInT[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (fexp: Exp, e: Exp, args : List[String], argsv : List[Exp], n : Integer,
   frame : Frame, read: Set[Addr] = Set[Addr](), write: Set[Addr] = Set[Addr]())
  extends Action[Exp, Abs, Addr]
case class ActionStartFunCallT[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends Action[Exp, Abs, Addr]
