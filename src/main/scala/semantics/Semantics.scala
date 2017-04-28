import scalaz._

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
  implicit def abs: JoinLattice[Abs]
  implicit def addr: Address[Addr]
  implicit def exp: Expression[Exp]
  implicit def time: Timestamp[Time]

  /**
    * Binds arguments in the environment and store. Arguments are given as a list
    * of triple, where each triple is made of:
    *   - the name of the argument
    *   - the expression evaluated to get the argument's value
    *   - the value of the argument
    */
  def bindArgs(l: List[(String, (Exp, Abs))],
               ρ: Environment[Addr],
               σ: Store[Addr, Abs],
               t: Time): (Environment[Addr], Store[Addr, Abs], List[(Addr, Abs)]) =
    l.foldLeft[(Environment[Addr], Store[Addr, Abs], List[(Addr, Abs)])]((ρ, σ, Nil))({
      case ((env, store, boundAddresses), (name, (exp, value))) => {
        val a = addr.variable(name, value, t)
        (env.extend(name, a), store.extend(a, value), (a, value) :: boundAddresses)
      }
    })

  /**
    * Defines how to parse a program
    */
  def parse(program: String): Exp
}

case class EdgeInformation[Exp : Expression, Abs : JoinLattice, Addr : Address](
    action: Action[Exp, Abs, Addr],
    actions: List[ActionReplay[Exp, Abs, Addr]],
    semanticsFilters: Set[SemanticsFilterAnnotation])

trait Semantics[Exp, Abs, Addr, Time]
    extends BasicSemantics[Exp, Abs, Addr, Time] {

  def noEdgeInfos(action: Action[Exp, Abs, Addr], actionRs: List[ActionReplay[Exp, Abs, Addr]]): EdgeInformation[Exp, Abs, Addr] =
    EdgeInformation(action, actionRs, Set())

  def noEdgeInfos(action: Action[Exp, Abs, Addr], actionR: ActionReplay[Exp, Abs, Addr]): EdgeInformation[Exp, Abs, Addr] =
    noEdgeInfos(action, List(actionR))

  def noEdgeInfosSet(action: Action[Exp, Abs, Addr], actionRs: List[ActionReplay[Exp, Abs, Addr]]): Set[EdgeInformation[Exp, Abs, Addr]] =
    Set(noEdgeInfos(action, actionRs))

  def noEdgeInfosSet(action: Action[Exp, Abs, Addr], actionR: ActionReplay[Exp, Abs, Addr]): Set[EdgeInformation[Exp, Abs, Addr]] =
    noEdgeInfosSet(action, List(actionR))

  def simpleAction(action: Action[Exp, Abs, Addr]): Set[EdgeInformation[Exp, Abs, Addr]] =
    simpleAction(Set(action))

  def simpleAction(actions: Set[Action[Exp, Abs, Addr]]): Set[EdgeInformation[Exp, Abs, Addr]] =
    actions.map(EdgeInformation(_, Nil, Set()))

  /**
    * Defines what actions should be taken when an expression e needs to be
    * evaluated, in environment env with store store
    */
  def stepEval(e: Exp,
               env: Environment[Addr],
               store: Store[Addr, Abs],
               t: Time): Set[EdgeInformation[Exp, Abs, Addr]]

  /**
    * Defines what actions should be taken when a value v has been reached, and
    * the topmost frame is frame
    */
  def stepKont(v: Abs,
               frame: Frame,
               store: Store[Addr, Abs],
               t: Time): Set[EdgeInformation[Exp, Abs, Addr]]

  /** Defines the elements in the initial environment/store */
  def initialBindings: Iterable[(String, Addr, Abs)] = List()
  def initialEnv: Iterable[(String, Addr)] =
    initialBindings.map({ case (name, a, _) => (name, a) })
  def initialStore: Iterable[(Addr, Abs)] =
    initialBindings.map({ case (_, a, v) => (a, v) })

}

trait ConvertableSemantics[Exp, Abs, Addr, Time]
  extends BasicSemantics[Exp, Abs, Addr, Time] {

  def primitives: Primitives[Addr, Abs]

  def convertToAbsSemanticsFrame(frame: Frame,
                                 ρ: Environment[Addr],
                                 vStack: List[Storable[Abs, Addr]],
                                 absSem: BaseSchemeSemantics[Abs, Addr, Time])
  : (Option[Frame], List[Storable[Abs, Addr]], Environment[Addr])

  def convertAbsInFrame[OtherAbs: IsConvertableLattice](frame: SchemeFrame[Abs, Addr, Time],
                                                         convertValue: (Abs) => OtherAbs,
                                                         convertEnv: (Environment[Addr]) => Environment[Addr],
                                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time])
  : SchemeFrame[OtherAbs, Addr, Time]
}

trait SemanticsTraced[Exp, Abs, Addr, Time]
    extends BasicSemantics[Exp, Abs, Addr, Time] {

  class InvalidArityException extends Exception

  def bindClosureArgs(
      clo: Abs,
      argsv: List[(Exp, Abs)],
      σ: Store[Addr, Abs],
      t: Time): Set[Either[Int, (Environment[Addr], Store[Addr, Abs], Exp)]]

  /*
   * Instruction return
   */
  trait InstructionReturn

  case class TraceStep() extends InstructionReturn
  case class GuardFailed(restartPoint: RestartPoint[Exp, Abs, Addr])
      extends InstructionReturn
  case class EndTrace(restartPoint: RestartPoint[Exp, Abs, Addr])
      extends InstructionReturn
  case class LoopTrace() extends InstructionReturn

  val endTraceInstruction: RestartPoint[Exp, Abs, Addr] => ActionTrace[Exp,
                                                                   Abs,
                                                                   Addr] =
    new ActionEndTrace(_)

  protected def interpreterStep(actions: List[ActionTrace[Exp, Abs, Addr]])
    : InterpreterStep[Exp, Abs, Addr] =
    new InterpreterStep(actions, new SignalFalse)

  protected def interpreterStepStart(
                                      actions: List[ActionTrace[Exp, Abs, Addr]],
                                      label: List[Exp]): InterpreterStep[Exp, Abs, Addr] =
    new InterpreterStep(actions, new SignalStartLoop(label))

  /**
    * Defines what actions should be taken when an expression e needs to be
    * evaluated, in environment e with store σ
    */
  def stepEval(e: Exp,
               ρ: Environment[Addr],
               σ: Store[Addr, Abs],
               t: Time): Set[InterpreterStep[Exp, Abs, Addr]]

  /**
    * Defines what actions should be taken when a value v has been reached, and
    * the topmost frame is frame
    */
  def stepKont(v: Abs,
               frame: Frame,
               σ: Store[Addr, Abs],
               t: Time): Set[InterpreterStep[Exp, Abs, Addr]]

  /** Defines the elements in the initial environment/store */
  def initialBindings: Iterable[(String, Addr, Abs)] = List()
  def initialEnv: Iterable[(String, Addr)] =
    initialBindings.map({ case (name, a, _) => (name, a) })
  def initialStore: Iterable[(Addr, Abs)] =
    initialBindings.map({ case (_, a, v) => (a, v) })

  def getClosureBody(frame: Frame): Option[List[Exp]]
}

/**
  * The different kinds of effects that can be generated by the semantics
  */
trait EffectKind
case object WriteEffect extends EffectKind
case object ReadEffect extends EffectKind
object EffectKind {
  implicit val isMonoid: Monoid[EffectKind] = new Monoid[EffectKind] {
    def zero: EffectKind = ReadEffect
    def append(x: EffectKind, y: => EffectKind): EffectKind = x match {
      case ReadEffect => y
      case WriteEffect => WriteEffect
    }
  }
}

abstract class Effect[Addr: Address] {
  val kind: EffectKind
  val target: Addr
}

case class EffectReadVariable[Addr: Address](target: Addr)
    extends Effect[Addr] {
  val kind = ReadEffect
  override def toString = s"R$target"
}
case class EffectReadConsCar[Addr: Address](target: Addr)
    extends Effect[Addr] {
  val kind = ReadEffect
  override def toString = s"Rcar($target)"
}
case class EffectReadConsCdr[Addr: Address](target: Addr)
    extends Effect[Addr] {
  val kind = ReadEffect
  override def toString = s"Rcdr($target)"
}
case class EffectReadVector[Addr: Address](target: Addr) extends Effect[Addr] {
  val kind = ReadEffect
  override def toString = s"Rvec($target)"
}
case class EffectWriteVariable[Addr: Address](target: Addr)
    extends Effect[Addr] {
  val kind = WriteEffect
  override def toString = s"W$target"
}
case class EffectWriteConsCar[Addr: Address](target: Addr)
    extends Effect[Addr] {
  val kind = WriteEffect
  override def toString = s"Wcar($target)"
}
case class EffectWriteConsCdr[Addr: Address](target: Addr)
    extends Effect[Addr] {
  val kind = WriteEffect
  override def toString = s"Wcdr($target)"
}
case class EffectWriteVector[Addr: Address](target: Addr)
    extends Effect[Addr] {
  val kind = WriteEffect
  override def toString = s"Wvec($target)"
}
case class EffectAcquire[Addr: Address](target: Addr) extends Effect[Addr] {
  val kind = WriteEffect
  override def toString = s"Acq$target"
}
case class EffectRelease[Addr: Address](target: Addr) extends Effect[Addr] {
  val kind = WriteEffect
  override def toString = s"Rel$target"
}

/**
  * The different kinds of actions that can be taken by the abstract machine
  */
abstract class Action[Exp: Expression, Abs: JoinLattice, Addr: Address]

/**
  * A value is reached by the interpreter. As a result, a continuation will be
  * popped with the given reached value.
  */
case class ActionReachedValue[Exp: Expression, Abs: JoinLattice, Addr: Address](
    v: Abs,
    store: Store[Addr, Abs],
    effects: Set[Effect[Addr]] = Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr]

/**
  * A frame needs to be pushed on the stack, and the interpretation continues by
  * evaluating expression e in environment env
  */
case class ActionPush[Exp: Expression, Abs: JoinLattice, Addr: Address](
    frame: Frame,
    e: Exp,
    env: Environment[Addr],
    store: Store[Addr, Abs],
    effects: Set[Effect[Addr]] = Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr]

/**
  * Evaluation continues with expression e in environment env
  */
case class ActionEval[Exp: Expression, Abs: JoinLattice, Addr: Address](
    e: Exp,
    env: Environment[Addr],
    store: Store[Addr, Abs],
    effects: Set[Effect[Addr]] = Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr]

/**
  * Similar to ActionEval, but only used when stepping inside a function's body
  * (clo is therefore the function stepped into). The expressions and values of
  * the arguments should also be provided, as they can be needed by the abstract
  * machine.
  */
case class ActionStepIn[Exp: Expression, Abs: JoinLattice, Addr: Address](
    fexp: Exp,
    frame: Frame,
    clo: (Exp, Environment[Addr]),
    e: Exp,
    env: Environment[Addr],
    store: Store[Addr, Abs],
    argsv: List[(Exp, Abs)],
    effects: Set[Effect[Addr]] = Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr]

/**
  * An error has been reached
  */
case class ActionError[Exp: Expression, Abs: JoinLattice, Addr: Address](
    error: SemanticError)
    extends Action[Exp, Abs, Addr]
trait SemanticError
case class OperatorNotApplicable(name: String, arguments: List[String])
    extends SemanticError
case class ArityError(name: String, expected: Int, got: Int)
    extends SemanticError
case class VariadicArityError(name: String, min: Int, got: Int)
    extends SemanticError
case class TypeError(name: String,
                     operand: String,
                     expected: String,
                     got: String)
    extends SemanticError
case class UserError(reason: String, pos: scala.util.parsing.input.Position)
    extends SemanticError
case class UnboundVariable(name: String) extends SemanticError
case class UnboundAddress(addr: String) extends SemanticError
case class NotSupported(reason: String) extends SemanticError

/**
  * Spawns a new thread that evaluates expression e in environment ρ. The current
  * thread continues its execution by performing action act.
  */
case class ActionSpawn[TID: ThreadIdentifier,
                       Exp: Expression,
                       Abs: JoinLattice,
                       Addr: Address](t: TID,
                                      e: Exp,
                                      env: Environment[Addr],
                                      act: Action[Exp, Abs, Addr],
                                      effects: Set[Effect[Addr]] =
                                        Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr]

/**
  * Waits for the execution of a thread, with tid as its identifier.
  */
case class ActionJoin[Exp: Expression, Abs: JoinLattice, Addr: Address](
    tid: Abs,
    store: Store[Addr, Abs],
    effects: Set[Effect[Addr]] = Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr]

/**
  * Base class for semantics that define some helper methods
  */
abstract class BaseSemantics[
    Exp: Expression, Abs: JoinLattice, Addr: Address, Time: Timestamp]
    extends Semantics[Exp, Abs, Addr, Time] {
  /* wtf scala */
  def abs = implicitly[JoinLattice[Abs]]
  def addr = implicitly[Address[Addr]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[Time]]
}

abstract class BaseSemanticsTraced[Exp: Expression, Abs: JoinLattice,
Addr: Address, Time: Timestamp](val primitives: Primitives[Addr, Abs])
    extends SemanticsTraced[Exp, Abs, Addr, Time] {
  /* wtf scala */
  def abs = implicitly[JoinLattice[Abs]]
  def addr = implicitly[Address[Addr]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[Time]]
}