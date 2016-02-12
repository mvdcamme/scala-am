/**
 * Basic Scheme semantics, without any optimization
 */
class BaseSchemeSemantics[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends BaseSemantics[SchemeExp, Abs, Addr, Time] {

  trait SchemeFrame extends Frame {
    def subsumes(that: Frame) = that.equals(this)
    override def toString = s"${this.getClass.getSimpleName}"
  }
  case class FrameWhileBody(condition : SchemeExp, body : List[SchemeExp], exps : List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameWhileCondition(condition : SchemeExp, body : List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameFuncallOperator(fexp: SchemeExp, args: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameFuncallOperands(f: Abs, fexp: SchemeExp, cur: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameIf(cons: SchemeExp, alt: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameLet(variable: String, bindings: List[(String, Abs)], toeval: List[(String, SchemeExp)], body: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameLetStar(variable: String, bindings: List[(String, SchemeExp)], body: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameLetrec(addr: Addr, bindings: List[(Addr, SchemeExp)], body: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameSet(variable: String, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameBegin(rest: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCond(cons: List[SchemeExp], clauses: List[(SchemeExp, List[SchemeExp])], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCase(clauses: List[(List[SchemeValue], List[SchemeExp])], default: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameAnd(rest: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameOr(rest: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameDefine(variable: String, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCasOld(variable: String, enew: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCasNew(variable: String, old: Abs, ρ: Environment[Addr]) extends SchemeFrame
  object FrameHalt extends SchemeFrame {
    override def toString() = "FHalt"
  }

  def convertFrame(convertAddress : Addr => Addr, convertValue : Abs => Abs)(frame : Frame) : Frame = frame match {
    case FrameFuncallOperator(fexp, args, ρ) => FrameFuncallOperator(fexp, args, ρ.map(convertAddress))
    case FrameFuncallOperands(f, fexp, cur, args, toeval, ρ) => FrameFuncallOperands(convertValue(f), fexp, cur, args.map({tuple => (tuple._1, convertValue(tuple._2))}), toeval, ρ.map(convertAddress))
    case FrameIf(cons, alt, ρ) => FrameIf(cons, alt, ρ.map(convertAddress))
    case FrameLet(variable, bindings, toeval, body, ρ) => FrameLet(variable, bindings.map({tuple => (tuple._1, convertValue(tuple._2))}), toeval, body, ρ.map(convertAddress))
    case FrameLetStar(variable, bindings, body, ρ) => FrameLetStar(variable, bindings, body, ρ.map(convertAddress))
    case FrameLetrec(addr, bindings, body, ρ) => FrameLetrec(convertAddress(addr), bindings, body, ρ.map(convertAddress))
    case FrameSet(variable, ρ) => FrameSet(variable, ρ.map(convertAddress))
    case FrameBegin(rest, ρ) => FrameBegin(rest, ρ.map(convertAddress))
    case FrameCond(cond, clauses, ρ) => FrameCond(cond, clauses, ρ.map(convertAddress))
    case FrameCase(clauses, default, ρ) => FrameCase(clauses, default, ρ.map(convertAddress))
    case FrameAnd(rest, ρ) => FrameAnd(rest, ρ.map(convertAddress))
    case FrameOr(rest, ρ) => FrameOr(rest, ρ.map(convertAddress))
    case FrameDefine(variable, ρ) => FrameDefine(variable, ρ.map(convertAddress))
    case FrameCasOld(variable, enew, ρ) => FrameCasOld(variable, enew, ρ.map(convertAddress))
    case FrameCasNew(variable, old, ρ) => FrameCasNew(variable, convertValue(old), ρ.map(convertAddress))
    case FrameWhileBody(condition, body, exps, ρ) => FrameWhileBody(condition, body, exps, ρ.map(convertAddress))
    case FrameWhileCondition(condition, body, ρ) => FrameWhileCondition(condition, body, ρ.map(convertAddress))
  }

  protected def evalBody(body: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs]): Action[SchemeExp, Abs, Addr] = body match {
    case Nil => ActionReachedValue(abs.inject(false), σ)
    case List(exp) => ActionEval(exp, ρ, σ)
    case exp :: rest => ActionPush(exp, FrameBegin(rest, ρ), ρ, σ)
  }

  def conditional(v: Abs, t: Action[SchemeExp, Abs, Addr], f: Action[SchemeExp, Abs, Addr]): Set[Action[SchemeExp, Abs, Addr]] =
    (if (abs.isTrue(v)) Set(t) else Set()) ++ (if (abs.isFalse(v)) Set(f) else Set())

  def evalCall(function: Abs, fexp: SchemeExp, argsv: List[(SchemeExp, Abs)], ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[Action[SchemeExp, Abs, Addr]] = {
    val fromClo: Set[Action[SchemeExp, Abs, Addr]] = abs.getClosures[SchemeExp, Addr](function).map({
      case (SchemeLambda(args, body), ρ1) =>
        if (args.length == argsv.length) {
          bindArgs(args.zip(argsv), ρ1, σ, t) match {
            case (ρ2, σ) =>
              if (body.length == 1)
                ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body), ρ1), body.head, ρ2, σ, argsv)
              else
                ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body), ρ1), SchemeBegin(body), ρ2, σ, argsv)
          }
        } else { ActionError[SchemeExp, Abs, Addr](s"Arity error when calling $fexp (${args.length} arguments expected, got ${argsv.length})") }
      case (λ, _) => ActionError[SchemeExp, Abs, Addr](s"Incorrect closure with lambda-expression ${λ}")
    })
    val fromPrim = abs.getPrimitive(function) match {
      case Some(prim) => prim.call(fexp, argsv, σ, t) match {
        case Right((res, σ2)) => Set(ActionReachedValue[SchemeExp, Abs, Addr](res, σ2))
        case Left(err) => Set(ActionError[SchemeExp, Abs, Addr](err))
      }
      case None => Set()
    }
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      Set(ActionError(s"Called value is not a function: $function"))
    } else {
      fromClo ++ fromPrim
    }
  }

  protected def evalValue(v: Value): Option[Abs] = v match {
    case ValueString(s) => Some(abs.inject(s))
    case ValueInteger(n) => Some(abs.inject(n))
    case ValueBoolean(b) => Some(abs.inject(b))
    case _ => None
  }

  protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[Action[SchemeExp, Abs, Addr]] = toeval match {
    case Nil => evalCall(f, fexp, args.reverse, ρ, σ, t)
    case e :: rest => Set(ActionPush(e, FrameFuncallOperands(f, fexp, e, args, rest, ρ), ρ, σ))
  }
  protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[Action[SchemeExp, Abs, Addr]] =
    funcallArgs(f, fexp, List(), args, ρ, σ, t)

  protected def evalQuoted(exp: SExp, σ: Store[Addr, Abs], t: Time): (Abs, Store[Addr, Abs]) = exp match {
    case SExpIdentifier(sym) => (abs.injectSymbol(sym), σ)
    case SExpPair(car, cdr) => {
      val care: SchemeExp = SchemeIdentifier(car.toString).setPos(car.pos)
      val cdre: SchemeExp = SchemeIdentifier(cdr.toString).setPos(cdr.pos)
      val cara = addr.cell(care, t)
      val (carv, σ2) = evalQuoted(car, σ, t)
      val cdra = addr.cell(cdre, t)
      val (cdrv, σ3) = evalQuoted(cdr, σ2, t)
      (abs.cons(cara, cdra), σ3.extend(cara, carv).extend(cdra, cdrv))
    }
    case SExpValue(v) => (v match {
      case ValueString(str) => abs.inject(str)
      case ValueCharacter(c) => throw new Exception("character not yet supported")
      case ValueSymbol(sym) => abs.injectSymbol(sym) /* shouldn't happen */
      case ValueInteger(n) => abs.inject(n)
      case ValueFloat(n) => throw new Exception("floats not yet supported")
      case ValueBoolean(b) => abs.inject(b)
      case ValueNil() => abs.nil
    }, σ)
    case SExpQuoted(q) => evalQuoted(SExpPair(SExpIdentifier("quote"), SExpPair(q, SExpValue(ValueNil()))), σ, t)
  }

  def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time) = e match {
    case λ: SchemeLambda => Set(ActionReachedValue(abs.inject[SchemeExp, Addr]((λ, ρ)), σ))
    case SchemeWhile(condition, body) => Set(ActionPush(condition, FrameWhileCondition(condition, body, ρ), ρ, σ))
    case SchemeFuncall(f, args) => Set(ActionPush(f, FrameFuncallOperator(f, args, ρ), ρ, σ))
    case SchemeIf(cond, cons, alt) => Set(ActionPush(cond, FrameIf(cons, alt, ρ), ρ, σ))
    case SchemeLet(Nil, body) => Set(evalBody(body, ρ, σ))
    case SchemeLet((v, exp) :: bindings, body) => Set(ActionPush(exp, FrameLet(v, List(), bindings, body, ρ), ρ, σ))
    case SchemeLetStar(Nil, body) => Set(evalBody(body, ρ, σ))
    case SchemeLetStar((v, exp) :: bindings, body) => Set(ActionPush(exp, FrameLetStar(v, bindings, body, ρ), ρ, σ))
    case SchemeLetrec(Nil, body) => Set(evalBody(body, ρ, σ))
    case SchemeLetrec((v, exp) :: bindings, body) => {
      val variables = v :: bindings.map(_._1)
      val addresses = variables.map(v => addr.variable(v, t))
      val (ρ1, σ1) = variables.zip(addresses).foldLeft((ρ, σ))({ case ((ρ, σ), (v, a)) => (ρ.extend(v, a), σ.extend(a, abs.bottom)) })
      Set(ActionPush(exp, FrameLetrec(addresses.head, addresses.tail.zip(bindings.map(_._2)), body, ρ1), ρ1, σ1))
    }
    case SchemeSet(variable, exp) => Set(ActionPush(exp, FrameSet(variable, ρ), ρ, σ))
    case SchemeBegin(body) => Set(evalBody(body, ρ, σ))
    case SchemeCond(Nil) => Set(ActionError(s"cond without clauses"))
    case SchemeCond((cond, cons) :: clauses) => Set(ActionPush(cond, FrameCond(cons, clauses, ρ), ρ, σ))
    case SchemeCase(key, clauses, default) => Set(ActionPush(key, FrameCase(clauses, default, ρ), ρ, σ))
    case SchemeAnd(Nil) => Set(ActionReachedValue(abs.inject(true), σ))
    case SchemeAnd(exp :: exps) => Set(ActionPush(exp, FrameAnd(exps, ρ), ρ, σ))
    case SchemeOr(Nil) => Set(ActionReachedValue(abs.inject(false), σ))
    case SchemeOr(exp :: exps) => Set(ActionPush(exp, FrameOr(exps, ρ), ρ, σ))
    case SchemeDefineVariable(name, exp) => Set(ActionPush(exp, FrameDefine(name, ρ), ρ, σ))
    case SchemeDefineFunction(name, args, body) => {
      val a = addr.variable(name, t)
      val v = abs.inject[SchemeExp, Addr]((SchemeLambda(args, body), ρ))
      val ρ1 = ρ.extend(name, a)
      val σ1 = σ.extend(a, v)
      Set(ActionReachedValue(v, σ))
    }
    case SchemeIdentifier(name) => ρ.lookup(name) match {
      case Some(a) => Set(ActionReachedValue(σ.lookup(a), σ, Set[Addr](a))) /* reads on a */
      case None => Set(ActionError(s"Unbound variable: $name"))
    }
    case SchemeQuoted(quoted) => evalQuoted(quoted, σ, t) match {
      case (value, σ2) => Set(ActionReachedValue(value, σ2))
    }
    case SchemeValue(v) => evalValue(v) match {
      case Some(v) => Set(ActionReachedValue(v, σ))
      case None => Set(ActionError(s"Unhandled value: $v"))
    }
    case SchemeCas(variable, eold, enew) => Set(ActionPush(eold, FrameCasOld(variable, enew, ρ), ρ, σ))
    case SchemeAcquire(variable) => ρ.lookup(variable) match {
      case Some(a) => {
        val v = σ.lookup(a)
        /* Only performs a step if the lock is possibly unlocked (true is unlocked, false is locked) */
        if (abs.isTrue(v)) Set(ActionReachedValue(abs.inject(true), σ.update(a, abs.inject(false)))) else Set()
      }
      case None => Set(ActionError(s"Unbound variable: $variable"))
    }
    case SchemeRelease(variable) => ρ.lookup(variable) match {
      case Some(a) => Set(ActionReachedValue(abs.inject(true), σ.update(a, abs.inject(true))))
      case None => Set(ActionError(s"Unbound variable: $variable"))
    }
  }

  protected def evalWhileBody(condition: SchemeExp, body: List[SchemeExp], exps: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs]): Action[SchemeExp, Abs, Addr] = exps match {
    case Nil => ActionPush(condition, FrameWhileCondition(condition, body, ρ), ρ, σ)
    case List(exp) => ActionPush(exp, FrameWhileBody(condition, body, Nil, ρ), ρ, σ)
    case exp :: rest => ActionPush(exp, FrameWhileBody(condition, body, rest, ρ), ρ, σ)
  }

  def stepKont(v: Abs, frame: Frame, σ: Store[Addr, Abs], t: Time) = frame match {
    case FrameHalt => Set()
    case FrameWhileBody(condition, body, exps, ρ) => Set{evalWhileBody(condition, body, exps, ρ, σ)}
    case FrameWhileCondition(condition, body, ρ) => {
      (if (abs.isTrue(v)) Set[Action[SchemeExp, Abs, Addr]](evalWhileBody(condition, body, body, ρ, σ)) else Set[Action[SchemeExp, Abs, Addr]]()) ++
      (if (abs.isFalse(v)) Set[Action[SchemeExp, Abs, Addr]](ActionReachedValue(abs.inject(false), σ)) else Set[Action[SchemeExp, Abs, Addr]]())
    }
    case FrameFuncallOperator(fexp, args, ρ) => funcallArgs(v, fexp, args, ρ, σ, t)
    case FrameFuncallOperands(f, fexp, exp, args, toeval, ρ) => funcallArgs(f, fexp, (exp, v) :: args, toeval, ρ, σ, t)
    case FrameIf(cons, alt, ρ) =>
      conditional(v, ActionEval(cons, ρ, σ), ActionEval(alt, ρ, σ))
    case FrameLet(name, bindings, Nil, body, ρ) => {
      val variables = name :: bindings.reverse.map(_._1)
      val addresses = variables.map(v => addr.variable(v, t))
      val (ρ1, σ1) = ((name, v) :: bindings).zip(addresses).foldLeft((ρ, σ))({
        case ((ρ, σ), ((variable, value), a)) => (ρ.extend(variable, a), σ.extend(a, value))
      })
      Set(evalBody(body, ρ1, σ1))
    }
    case FrameLet(name, bindings, (variable, e) :: toeval, body, ρ) =>
      Set(ActionPush(e, FrameLet(variable, (name, v) :: bindings, toeval, body, ρ), ρ, σ))
    case FrameLetStar(name, bindings, body, ρ) => {
      val a = addr.variable(name, t)
      val ρ1 = ρ.extend(name, a)
      val σ1 = σ.extend(a, v)
      bindings match {
        case Nil => Set(evalBody(body, ρ1, σ1))
        case (variable, exp) :: rest => Set(ActionPush(exp, FrameLetStar(variable, rest, body, ρ1), ρ1, σ1))
      }
    }
    case FrameLetrec(a, Nil, body, ρ) => Set(evalBody(body, ρ, σ.update(a, v)))
    case FrameLetrec(a, (a1, exp) :: rest, body, ρ) =>
      Set(ActionPush(exp, FrameLetrec(a1, rest, body, ρ), ρ, σ.update(a, v)))
    case FrameSet(name, ρ) => ρ.lookup(name) match {
      case Some(a) => Set(ActionReachedValue(abs.inject(false), σ.update(a, v), Set[Addr](), Set[Addr](a))) /* writes on a */
      case None => Set(ActionError(s"Unbound variable: $name"))
    }
    case FrameBegin(body, ρ) => Set(evalBody(body, ρ, σ))
    case FrameCond(cons, clauses, ρ) =>
      conditional(v, if (cons.isEmpty) { ActionReachedValue(v, σ) } else { evalBody(cons, ρ, σ) },
        clauses match {
          case Nil => ActionReachedValue(abs.inject(false), σ)
          case (exp, cons2) :: rest => ActionPush(exp, FrameCond(cons2, rest, ρ), ρ, σ)
        })
    case FrameCase(clauses, default, ρ) => {
      val fromClauses = clauses.flatMap({ case (values, body) =>
        if (values.exists(v2 => evalValue(v2.value) match {
          case None => false
          case Some(v2) => abs.subsumes(v, v2)
        }))
          /* TODO: precision could be improved by restricting v to v2 */
          Set[Action[SchemeExp, Abs, Addr]](evalBody(body, ρ, σ))
        else
          Set[Action[SchemeExp, Abs, Addr]]()
      })
      /* TODO: precision could be improved in cases where we know that default is not
       * reachable */
      fromClauses.toSet + evalBody(default, ρ, σ)
    }
    case FrameAnd(Nil, ρ) =>
      conditional(v, ActionReachedValue(v, σ), ActionReachedValue(abs.inject(false), σ))
    case FrameAnd(e :: rest, ρ) =>
      conditional(v, ActionPush(e, FrameAnd(rest, ρ), ρ, σ), ActionReachedValue(abs.inject(false), σ))
    case FrameOr(Nil, ρ) =>
      conditional(v, ActionReachedValue(v, σ), ActionReachedValue(abs.inject(false), σ))
    case FrameOr(e :: rest, ρ) =>
      conditional(v, ActionReachedValue(v, σ), ActionPush(e, FrameOr(rest, ρ), ρ, σ))
    case FrameDefine(name, ρ) => throw new Exception(s"TODO: define not handled (no global environment)")
    case FrameCasOld(variable, enew, ρ) =>
      Set(ActionPush(enew, FrameCasNew(variable, v, ρ), ρ, σ))
    case FrameCasNew(variable, old, ρ) =>
      ρ.lookup(variable) match {
        case Some(a) => conditional(abs.binaryOp(BinaryOperator.Eq)(σ.lookup(a), old),
          /* Compare and swap succeeds */
          ActionReachedValue(abs.inject(true), σ.update(a, v)),
          /* Compare and swap fails */
          ActionReachedValue(abs.inject(false), σ))
        case None => Set(ActionError(s"Unbound variable: $variable"))
      }
  }

  def parse(program: String): SchemeExp = Scheme.parse(program)
}

/**
 * Extend base Scheme semantics with:
 *   - atomic evaluation: parts of some constructs can be evaluated atomically
 *     without needing to introduce more states in the state graph. For example,
 *     (+ 1 1) can directly be evaluated to 2 without modifying the store. Also,
 *     to evaluate (+ 1 (f)), we can directly push the continuation and jump to
 *     the evaluation of (f), instead of evaluating +, and 1 in separate states.
 */
class SchemeSemantics[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends BaseSchemeSemantics[Abs, Addr, Time] {

  /** Tries to perform atomic evaluation of an expression. Returns the result of
    * the evaluation if it succeeded, otherwise returns None */
  protected def atomicEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs]): Option[(Abs, Set[Addr])] = e match {
    case λ: SchemeLambda => Some((abs.inject[SchemeExp, Addr]((λ, ρ)), Set[Addr]()))
    case SchemeIdentifier(name) => ρ.lookup(name).map(a => (σ.lookup(a), Set[Addr](a)))
    case SchemeValue(v) => evalValue(v).map(value => (value, Set[Addr]()))
    case _ => None
  }

  protected def addRead(action: Action[SchemeExp, Abs, Addr], read: Set[Addr]): Action[SchemeExp, Abs, Addr] = action match {
    case ActionReachedValue(v, σ, read2, write) => ActionReachedValue(v, σ, read ++ read2, write)
    case ActionPush(e, frame, ρ, σ, read2, write) => ActionPush(e, frame, ρ, σ, read ++ read2, write)
    case ActionEval(e, ρ, σ, read2, write) => ActionEval(e, ρ, σ, read ++ read2, write)
    case ActionStepIn(fexp, clo, e, ρ, σ, argsv, read2, write) => ActionStepIn(fexp, clo, e, ρ, σ, argsv, read ++ read2, write)
    case ActionError(err) => action
  }

  override protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[Action[SchemeExp, Abs, Addr]] = {
      toeval match {
        case Nil => evalCall(f, fexp, args.reverse, ρ, σ, t)
        case e :: rest => atomicEval(e, ρ, σ) match {
          case Some((v, as)) => funcallArgs(f, fexp, (e, v) :: args, rest, ρ, σ, t).map(addRead(_, as))
          case None => Set(ActionPush(e, FrameFuncallOperands(f, fexp, e, args, rest, ρ), ρ, σ))
        }
    }
  }

  /**
   * Optimize the following pattern: when we see an ActionPush(exp, frame, ρ, σ)
   * where exp is an atomic expression, we can atomically evaluate exp to get v,
   * and call stepKont(v, σ, frame).
   */
  protected def optimizeAtomic(actions: Set[Action[SchemeExp, Abs, Addr]], t: Time): Set[Action[SchemeExp, Abs, Addr]] = {
    actions.flatMap({
      case ActionPush(exp, frame, ρ, σ, read, write) => atomicEval(exp, ρ, σ) match {
        case Some((v, read2)) => stepKont(v, frame, σ, t).map(addRead(_, read ++ read2))
        case None => Set[Action[SchemeExp, Abs, Addr]](ActionPush(exp, frame, ρ, σ, read, write))
      }
      case action => Set[Action[SchemeExp, Abs, Addr]](action)
    })
  }

  override def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time) =
    optimizeAtomic(super.stepEval(e, ρ, σ, t), t)

  override def stepKont(v: Abs, frame: Frame, σ: Store[Addr, Abs], t: Time) =
    optimizeAtomic(super.stepKont(v, frame, σ, t), t)
}

class ConcurrentSchemeSemantics[Abs : AbstractValue, Addr : Address, Time : Timestamp, TID : ThreadIdentifier]
    extends SchemeSemantics[Abs, Addr, Time] {
  def thread = implicitly[ThreadIdentifier[TID]]

  case class FrameJoin(ρ: Environment[Addr]) extends SchemeFrame

  override def addRead(action: Action[SchemeExp, Abs, Addr], read: Set[Addr]) = action match {
    case ActionSpawn(t: TID, e, ρ, act, read2, write) => ActionSpawn(t, e, ρ, act, read ++ read2, write)
    case ActionJoin(tid, σ, read2, write) => ActionJoin(tid, σ, read ++ read2, write)
    case _ => super.addRead(action, read)
  }

  override def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time) = e match {
    case SchemeSpawn(exp) =>
      val tid = thread.thread[SchemeExp, Time](exp, t)
      Set(ActionSpawn(tid, exp, ρ, ActionReachedValue(abs.injectTid(tid), σ)))
    case SchemeJoin(exp) => optimizeAtomic(Set(ActionPush(exp, FrameJoin(ρ), ρ, σ)), t)
    case _ => super.stepEval(e, ρ, σ, t)
  }

  override def stepKont(v: Abs, frame: Frame, σ: Store[Addr, Abs], t: Time) = frame match {
    case FrameJoin(ρ) => Set(ActionJoin(v, σ))
    case _ => super.stepKont(v, frame, σ, t)
  }
}