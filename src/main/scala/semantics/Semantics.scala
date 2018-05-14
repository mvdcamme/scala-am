import scalaz._

import backend.expression._

import concolic.SymbolicEnvironment

case class EdgeInformation[Exp : Expression, Abs : JoinLattice, Addr : Address](
  action: Action[Exp, Abs, Addr],
  actions: List[ActionReplay[Exp, Abs, Addr]],
  semanticsFilters: Set[SemanticsFilterAnnotation]) {
  /**
    * Results in a new EdgeInformation containing the action of the @param other [[EdgeInformation]], and
    * combines the [[actions]] and [[semanticsFilters]] of the two [[EdgeInformation]]s together.
    * @param other
    * @return
    */
  def merge(other: EdgeInformation[Exp, Abs, Addr]): EdgeInformation[Exp, Abs, Addr] = {
    EdgeInformation(other.action, actions ++ other.actions, semanticsFilters ++ other.semanticsFilters)
  }
}

trait GeneratesEdgeInfo[Exp, Abs, Addr, Time] {

  import scala.language.implicitConversions
  def noEdgeInfos(action: Action[Exp, Abs, Addr], actionRs: List[ActionReplay[Exp, Abs, Addr]])(
    implicit unused1: Expression[Exp], unused2: JoinLattice[Abs], unused3: Address[Addr]): EdgeInformation[Exp, Abs, Addr] =
    EdgeInformation(action, actionRs, Set())

  def noEdgeInfos(action: Action[Exp, Abs, Addr], actionR: ActionReplay[Exp, Abs, Addr])(
    implicit unused1: Expression[Exp], unused2: JoinLattice[Abs], unused3: Address[Addr]): EdgeInformation[Exp, Abs, Addr] =
    noEdgeInfos(action, List(actionR))

  def noEdgeInfosSet(action: Action[Exp, Abs, Addr], actionRs: List[ActionReplay[Exp, Abs, Addr]])(
    implicit unused1: Expression[Exp], unused2: JoinLattice[Abs], unused3: Address[Addr]): Set[EdgeInformation[Exp, Abs, Addr]] =
    Set(noEdgeInfos(action, actionRs))

  def noEdgeInfosSet(action: Action[Exp, Abs, Addr], actionR: ActionReplay[Exp, Abs, Addr])(
    implicit unused1: Expression[Exp], unused2: JoinLattice[Abs], unused3: Address[Addr]): Set[EdgeInformation[Exp, Abs, Addr]] =
    noEdgeInfosSet(action, List(actionR))

  implicit def simpleAction(action: Action[Exp, Abs, Addr])(
    implicit unused1: Expression[Exp], unused2: JoinLattice[Abs], unused3: Address[Addr]): EdgeInformation[Exp, Abs, Addr] =
    EdgeInformation(action, Nil, Set())

  def simpleActionSet(action: Action[Exp, Abs, Addr])(
    implicit unused1: Expression[Exp], unused2: JoinLattice[Abs], unused3: Address[Addr]): Set[EdgeInformation[Exp, Abs, Addr]] =
    simpleActionSet(Set(action))

  implicit def simpleActionSet(actions: Set[Action[Exp, Abs, Addr]])(
    implicit unused1: Expression[Exp], unused2: JoinLattice[Abs], unused3: Address[Addr]): Set[EdgeInformation[Exp, Abs, Addr]] =
    actions.map(EdgeInformation(_, Nil, Set()))

  def addEvalActionT(action: ActionEval[Exp, Abs, Addr])(
    implicit unused1: Expression[Exp], unused2: JoinLattice[Abs], unused3: Address[Addr]): EdgeInformation[Exp, Abs, Addr] =
    noEdgeInfos(action, Nil)
  def addEvalActionT(action: ActionConcolicEval[Exp, Abs, Addr])(
    implicit unused1: Expression[Exp], unused2: JoinLattice[Abs], unused3: Address[Addr]): EdgeInformation[Exp, Abs, Addr] =
    noEdgeInfos(action, ActionEvalR[Exp, Abs, Addr](action.normalAction.e, action.normalAction.env))

  def addEvalActionTSet(action: ActionConcolicEval[Exp, Abs, Addr])(
    implicit unused1: Expression[Exp], unused2: JoinLattice[Abs], unused3: Address[Addr]): Set[EdgeInformation[Exp, Abs, Addr]] =
    Set(addEvalActionT(action))

  def addPushActionR(action: ActionConcolicPush[Exp, Abs, Addr])(
    implicit unused1: Expression[Exp], unused2: JoinLattice[Abs], unused3: Address[Addr]): EdgeInformation[Exp, Abs, Addr] =
    noEdgeInfos(action, ActionEvalPushR[Exp, Abs, Addr](action.normalAction.e, action.normalAction.env, action.normalAction.frame))

  protected def addPushActionRSet(action: ActionConcolicPush[Exp, Abs, Addr])(
    implicit unused1: Expression[Exp], unused2: JoinLattice[Abs], unused3: Address[Addr]): Set[EdgeInformation[Exp, Abs, Addr]] =
    Set(addPushActionR(action))
}

abstract class ConvertableSemantics[Exp: Expression, Abs: JoinLattice, Addr: Address, Time: Timestamp]
  extends BasicSemantics[Exp, Abs, Addr, Time] with GeneratesEdgeInfo[Exp, Abs, Addr, Time] {

  type EdgeInfo = EdgeInformation[Exp, Abs, Addr]
  type EdgeInfos = Set[EdgeInformation[Exp, Abs, Addr]]

  def frameReaches(frame: ConvertableSchemeFrame[Abs, Addr, Time], valueReaches: Abs => Reached[Addr],
                   envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
                   addressReaches: Addr => Reached[Addr]): Reached[Addr]

  /**
    * Defines what actions should be taken when an expression e needs to be
    * evaluated, in environment env with store store
    */
  def stepEval(e: Exp, env: Environment[Addr], store: Store[Addr, Abs], t: Time): EdgeInfos
  /**
    * Defines what actions should be taken when a value v has been reached, and
    * the topmost frame is frame
    */
  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time): EdgeInfos

  def primitives: Primitives[Addr, Abs]


}

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

abstract class BasicSemantics [Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp] {
  /**
    * Defines how to parse a program
    */
  def parse(program: String): Exp

  /** Defines the elements in the initial environment/store */
  def initialBindings: Iterable[(String, Addr, Abs)] = List()
  def initialEnv: Iterable[(String, Addr)] = initialBindings.map({ case (name, a, _) => (name, a) })
  def initialStore: Iterable[(Addr, Abs)] = initialBindings.map({ case (_, a, v) => (a, v) })

  object Action extends ActionHelpers[Exp, Abs, Addr]

  /* Some helper methods */
  import scala.language.implicitConversions
  implicit def mfToActions(mf: MayFail[Set[Action[Exp, Abs, Addr]]]): Set[Action[Exp, Abs, Addr]] =
    mf match {
      case MayFailSuccess(l) => l
      case MayFailError(errs) => errs.toSet.map((err: SemanticError) => ActionError(err))
      case MayFailBoth(l, errs) => l ++ errs.toSet.map((err: SemanticError) => ActionError(err))
    }
  implicit def mfActionToActions(mf: MayFail[Action[Exp, Abs, Addr]]): Set[Action[Exp, Abs, Addr]] =
  /* mf.map does not use the correct map apparently? */
    mfToActions(MayFail.isFunctor.map(mf)(x => Set[Action[Exp, Abs, Addr]](x)))
  implicit def actionToSet(action: Action[Exp, Abs, Addr]): Set[Action[Exp, Abs, Addr]] =
    Set(action)

  /**
    * Binds arguments in the environment and store. Arguments are given as a list
    * of triple, where each triple is made of:
    *   - the name of the argument
    *   - the position of the argument
    *   - the value of the argument
    */
  protected def bindArgs(l: List[(Identifier, Abs)], env: Environment[Addr], store: Store[Addr, Abs], t: Time): (Environment[Addr], Store[Addr, Abs]) =
    l.foldLeft((env, store))({ case ((env, store), (id, value)) => {
      val a = Address[Addr].variable(id, value, t)
      (env.extend(id.name, a), store.extend(a, value))
    }})

  /**
    * Similar to bindArgs(List[(Identifier, Abs)], Environment[Addr], Store[Addr, Abs], Time),
    * but also returns the list of addresses that was allocated while binding the arguments.
    */
  protected def bindArgsAndReturnAddresses(l: List[(Identifier, Exp, Abs)], ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): (Environment[Addr], Store[Addr, Abs], List[(Addr, Abs)]) =
    l.foldLeft((ρ, σ, Nil: List[(Addr, Abs)]))({ case ((env, store, boundAddresses), (id, exp, value)) => {
      val a = Address[Addr].variable(id, value, t)
      (env.extend(id.name, a), store.extend(a, value), (a, value) :: boundAddresses)
    }})
}

abstract class Semantics[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp] extends BasicSemantics[Exp, Abs, Addr, Time] {
  /**
    * Defines what actions should be taken when an expression e needs to be
    * evaluated, in environment env with store store
    */
  def stepEval(e: Exp, env: Environment[Addr], store: Store[Addr, Abs], t: Time): Set[Action[Exp, Abs, Addr]]
  /**
    * Defines what actions should be taken when a value v has been reached, and
    * the topmost frame is frame
    */
  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time): Set[Action[Exp, Abs, Addr]]

  /** WIP */
  def stepReceive(self: Any /* TODO */, mname: String, margsv: List[Abs], d: Exp,
    env: Environment[Addr], store: Store[Addr, Abs], t: Time): Set[Action[Exp, Abs, Addr]] =
    throw new Exception("Semantics do not support message-based concurrency")
}

class SemanticsWithAnalysis[L, Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp](sem: Semantics[Exp, Abs, Addr, Time], analysis: Analysis[L, Exp, Abs, Addr, Time])
    extends Semantics[Exp, Abs, Addr, Time] {
  private var st: L = analysis.init
  def stepEval(e: Exp, env: Environment[Addr], store: Store[Addr, Abs], t: Time) = {
    st = analysis.join(st, analysis.stepEval(e, env, store, t, st))
    sem.stepEval(e, env, store, t)
  }
  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time) = {
    st = analysis.join(st, analysis.stepKont(v, frame, store, t, st))
    sem.stepKont(v, frame, store, t)
  }
  def parse(program: String) = sem.parse(program)
  def analysisResult: L = st
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

object Effect {
  def none[Addr : Address]: Set[Effect[Addr]] = Set.empty
  def readVariable[Addr : Address](target: Addr): Effect[Addr] = EffectReadVariable(target)
  def writeVariable[Addr : Address](target: Addr): Effect[Addr] = EffectWriteVariable(target)
}

case class EffectReadVariable[Addr : Address](target: Addr)
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
abstract class Action[Exp : Expression, Abs : JoinLattice, Addr : Address] {
  def addEffects(effects: Set[Effect[Addr]]): Action[Exp, Abs, Addr]
  def effects: Set[Effect[Addr]]
}
sealed abstract class ActionConcolic[Exp : Expression, Abs : JoinLattice, Addr : Address] extends Action[Exp, Abs, Addr] {
  def normalAction: Action[Exp, Abs, Addr]
  def effects: Set[Effect[Addr]] = normalAction.effects
}
class ActionHelpers[Exp : Expression, Abs : JoinLattice, Addr : Address] {
  type Effs = Set[Effect[Addr]]
  type Env = Environment[Addr]
  type Sto = Store[Addr, Abs]
  type Act = Action[Exp, Abs, Addr]
  def none: Set[Act] = Set.empty
  def value(v: Abs, store: Sto, effects: Effs = Set.empty): Act =
    new ActionReachedValue(v, store, effects)
  def push(frame: Frame, e: Exp, env: Env, store: Sto, effects: Effs = Set.empty): Act =
    new ActionPush(frame, e, env, store, effects)
  def eval(e: Exp, env: Env, store: Sto, effects: Effs = Set.empty): Act =
    new ActionEval(e, env, store, effects)
  def stepIn(fexp: Exp, clo: (Exp, Env), e: Exp, env: Env, store: Sto, argsv: List[(Exp, Abs)], effects: Effs = Set.empty): Act =
    new ActionStepIn(fexp, clo, e, env, store, argsv, effects)
  def error(err: SemanticError): Act =
    new ActionError(err)
  def spawn[TID : ThreadIdentifier](t: TID, e: Exp, env: Env, store: Sto, act: Act, effects: Effs = Set.empty): Act =
    new ActionSpawn(t, e, env, store, act, effects)
  def join[TID : ThreadIdentifier](t: TID, store: Sto, effects: Effs = Set.empty): Act =
    new ActionJoin(t, store, effects)
}

/**
 * A value is reached by the interpreter. As a result, a continuation will be
 * popped with the given reached value.
 */
case class ActionReachedValue[Exp : Expression, Abs : JoinLattice, Addr : Address]
  (v: Abs, store: Store[Addr, Abs], effects: Set[Effect[Addr]] = Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(effects = effects ++ effs)
}
case class ActionConcolicReachedValue[Exp: Expression, Abs: JoinLattice, Addr: Address](
  normalAction: ActionReachedValue[Exp, Abs, Addr],
  concolicExp: Option[ConcolicExpression])
  extends ActionConcolic[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(normalAction = normalAction.addEffects(effs))
}

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
    extends Action[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(effects = effects ++ effs)
}
case class ActionConcolicPush[Exp: Expression, Abs: JoinLattice, Addr: Address](
  normalAction: ActionPush[Exp, Abs, Addr],
  frame: ConcolicFrame, /* TODO Debugging: also add frame here to make sure we're using a ConcolicFrame. This frame should be the same as the one included in the normalAction. */
  symEnv: SymbolicEnvironment)
  extends ActionConcolic[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(normalAction = normalAction.addEffects(effs))
}

/**
  * Evaluation continues with expression e in environment env
  */
case class ActionEval[Exp: Expression, Abs: JoinLattice, Addr: Address](
    e: Exp,
    env: Environment[Addr],
    store: Store[Addr, Abs],
    effects: Set[Effect[Addr]] = Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(effects = effects ++ effs)
}
case class ActionConcolicEval[Exp: Expression, Abs: JoinLattice, Addr: Address](
  normalAction: ActionEval[Exp, Abs, Addr],
  symEnv: SymbolicEnvironment
) extends ActionConcolic[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(normalAction = normalAction.addEffects(effs))
}
/**
  * Similar to ActionEval, but only used when stepping inside a function's body
  * (clo is therefore the function stepped into). The expressions and values of
  * the arguments should also be provided, as they can be needed by the abstract
  * machine.
  */
case class ActionStepIn[Exp: Expression, Abs: JoinLattice, Addr: Address](
    fexp: Exp,
    clo: (Exp, Environment[Addr]),
    e: Exp,
    env: Environment[Addr],
    store: Store[Addr, Abs],
    argsv: List[(Exp, Abs)],
    effects: Set[Effect[Addr]] = Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(effects = effects ++ effs)
}
case class ActionConcolicStepIn[Exp: Expression, Abs: JoinLattice, Addr: Address](
  normalAction: ActionStepIn[Exp, Abs, Addr],
  symEnv: SymbolicEnvironment
) extends ActionConcolic[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(normalAction = normalAction.addEffects(effs))
}
/**
 * An error has been reached
 */
case class ActionError[Exp : Expression, Abs : JoinLattice, Addr : Address]
  (error: SemanticError) extends Action[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this /* no effects stored in this action */
  def effects: Set[Effect[Addr]] = Set()
}
case class ActionConcolicError[Exp : Expression, Abs : JoinLattice, Addr : Address](
  normalAction: ActionError[Exp, Abs, Addr])
  extends ActionConcolic[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(normalAction = normalAction.addEffects(effs))
}

trait SemanticError
case class OperatorNotApplicable(name: String, arguments: List[String]) extends SemanticError
case class ArityError(name: String, expected: Int, got: Int) extends SemanticError
case class VariadicArityError(name: String, min: Int, got: Int) extends SemanticError
case class TypeError(name: String, operand: String, expected: String, got: String) extends SemanticError
case class UserError(reason: String, pos: Position) extends SemanticError
case class UnboundVariable(name: Identifier) extends SemanticError
case class UnboundAddress(addr: String) extends SemanticError
case class MessageNotSupported(actor: String, message: String, supported: List[String]) extends SemanticError
case class NotSupported(reason: String) extends SemanticError

/**
 * Spawns a new thread that evaluates expression e in environment ρ. The current
 * thread continues its execution by performing action act.
 */
case class ActionSpawn[TID : ThreadIdentifier, Exp : Expression, Abs : JoinLattice, Addr : Address]
  (t: TID, e: Exp, env: Environment[Addr], store: Store[Addr, Abs], act: Action[Exp, Abs, Addr],
    effects: Set[Effect[Addr]] = Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(effects = effects ++ effs)
}
/**
 * Waits for the execution of a thread, with tid as its identifier.
 */
case class ActionJoin[TID : ThreadIdentifier, Exp : Expression, Abs : JoinLattice, Addr : Address]
  (t: TID, store: Store[Addr, Abs], effects: Set[Effect[Addr]] = Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(effects = effects ++ effs)
}

class ActorActionHelpers[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp, PID : ThreadIdentifier] {
  type Effs = Set[Effect[Addr]]
  type Env = Environment[Addr]
  type Sto = Store[Addr, Abs]
  type Act = Action[Exp, Abs, Addr]
  def send(pid: PID, name: String, msg: List[Abs], vres: Abs, effs: Effs = Set.empty): Act =
    new ActorActionSend(pid, name, msg, vres, effs)
  def create(name: String, actd: Exp, exp: Exp, env: Env, store: Sto, fres: PID => Abs, effs: Effs = Set.empty): Act =
    new ActorActionCreate(name, actd, exp, env, store, fres, effs)
  def become(name: String, actd: Exp, env: Env, store: Sto, vres: Abs, effs: Effs = Set.empty): Act =
    new ActorActionBecome(name, actd, env, store, vres, effs)
  def terminate: Act = new ActorActionTerminate[Exp, Abs, Addr]()
}

case class ActorActionSend[PID : ThreadIdentifier, Exp : Expression, Abs : JoinLattice, Addr : Address]
  (p: PID, name: String, msg: List[Abs], vres: Abs,
    effects: Set[Effect[Addr]] = Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(effects = effects ++ effs)
}

case class ActorActionCreate[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp, PID : ThreadIdentifier]
  (name: String, actd: Exp, e: Exp, env: Environment[Addr], store: Store[Addr, Abs], fres: PID => Abs,
    effects: Set[Effect[Addr]] = Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(effects = effects ++ effs)
}

case class ActorActionBecome[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp, PID : ThreadIdentifier]
  (name: String, actd: Exp, env: Environment[Addr], store: Store[Addr, Abs], vres: Abs,
    effects: Set[Effect[Addr]] = Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(effects = effects ++ effs)
}

case class ActorActionTerminate[Exp : Expression, Abs : JoinLattice, Addr : Address]
  (effects: Set[Effect[Addr]] = Set[Effect[Addr]]())
    extends Action[Exp, Abs, Addr] {
  def addEffects(effs: Set[Effect[Addr]]) = this.copy(effects = effects ++ effs)
}