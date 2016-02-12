/**
  * Created by mvdcamme on 02/02/16.
  */

/**
  * Basic Scheme semantics, without any optimization
  */
abstract class BaseSchemeSemanticsTraced[Abs : AbstractValue, Addr : Address, Time : Timestamp]
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
    case exp :: rest => ActionPush(exp, FrameBegin(rest, ρ))
  }

  def conditional(v: Abs, t: Action[SchemeExp, Abs, Addr], f: Action[SchemeExp, Abs, Addr]): Set[InterpreterReturn] =
    (if (abs.isTrue(v)) Set[InterpreterReturn](interpreterReturn(t)) else Set[InterpreterReturn]()) ++
    (if (abs.isFalse(v)) Set[InterpreterReturn](interpreterReturn(f)) else Set[InterpreterReturn]())

  /*
   * TODO: Debugging: run tests only using if-expressions. Remove function ASAP and use function "conditional" instead!
   */
  def conditionalIf(v: Abs, t: Action[SchemeExp, Abs, Addr], tRestart : RestartPoint, f: Action[SchemeExp, Abs, Addr], fRestart : RestartPoint): Set[InterpreterReturn] =
    (if (abs.isTrue(v)) Set[InterpreterReturn](new InterpreterReturn(List(ActionGuardTrue(fRestart), t), TracingSignalFalse())) else Set[InterpreterReturn]()) ++
    (if (abs.isFalse(v)) Set[InterpreterReturn](new InterpreterReturn(List(ActionGuardFalse(tRestart), f), TracingSignalFalse())) else Set[InterpreterReturn]())

  /**
   * @param argsv are the Scheme-expressions of the operands of the call
   */
  def evalCall(function: Abs, fexp: SchemeExp, argsv: List[(SchemeExp, Abs)], ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[InterpreterReturn] = {

    /*
     * The number of values to pop. In the case of a primitive application, these are all operands that were evaluated + the operator itself
     */
    val valsToPop = argsv.length + 1

    val actions : Trace = List(ActionPushVal())

    val fromClo: Set[InterpreterReturn] = abs.getClosures[SchemeExp, Addr](function).map({
      case (SchemeLambda(args, body), ρ1) =>
        if (body.length == 1)
          InterpreterReturn(actions :+ ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body), ρ1), body.head, args, argsv.map(_._1), valsToPop), new TracingSignalStart(body))
        else
          InterpreterReturn(actions :+ ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body), ρ1), SchemeBegin(body), args, argsv.map(_._1), valsToPop), new TracingSignalStart(body))
      case (λ, _) => interpreterReturn(ActionError[SchemeExp, Abs, Addr](s"Incorrect closure with lambda-expression ${λ}"))
    })

    val fromPrim = abs.getPrimitive(function) match {
      case Some(prim) => Set(InterpreterReturn(actions :+ ActionPrimCall[SchemeExp, Abs, Addr](valsToPop, fexp, argsv.map(_._1)), new TracingSignalFalse()))
      case None => Set()
    }
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      Set(new InterpreterReturn(actions :+ ActionError[SchemeExp, Abs, Addr](s"Called value is not a function: $function"), new TracingSignalFalse))
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

  protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[InterpreterReturn] = toeval match {
    case Nil => evalCall(f, fexp, args.reverse, ρ, σ, t)
    case e :: rest => Set(InterpreterReturn(List(ActionPushVal(), ActionPush(e, FrameFuncallOperands(f, fexp, e, args, rest, ρ))), new TracingSignalFalse))
  }
  protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[InterpreterReturn] =
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

  def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time) : Set[InterpreterReturn] = e match {
    case λ: SchemeLambda => Set(interpreterReturn(ActionReachedValue(abs.inject[SchemeExp, Addr]((λ, ρ)), σ)))
    case SchemeWhile(condition, body) => Set(interpreterReturn(ActionPush(condition, FrameWhileCondition(condition, body, ρ))))
    case SchemeFuncall(f, args) => Set(interpreterReturn(ActionPush(f, FrameFuncallOperator(f, args, ρ))))
    case SchemeIf(cond, cons, alt) => Set(interpreterReturn(ActionPush(cond, FrameIf(cons, alt, ρ))))
    case SchemeLet(Nil, body) => Set(interpreterReturn(evalBody(body, ρ, σ)))
    case SchemeLet((v, exp) :: bindings, body) => Set(interpreterReturn(ActionPush(exp, FrameLet(v, List(), bindings, body, ρ))))
    case SchemeLetStar(Nil, body) => Set(interpreterReturn(evalBody(body, ρ, σ)))
    case SchemeLetStar((v, exp) :: bindings, body) => Set(interpreterReturn(ActionPush(exp, FrameLetStar(v, bindings, body, ρ))))
    case SchemeLetrec(Nil, body) => Set(interpreterReturn(evalBody(body, ρ, σ)))
    case SchemeLetrec((v, exp) :: bindings, body) => {
      val variables = v :: bindings.map(_._1)
      val addresses = variables.map(v => addr.variable(v, t))
      val (ρ1, σ1) = variables.zip(addresses).foldLeft((ρ, σ))({ case ((ρ, σ), (v, a)) => (ρ.extend(v, a), σ.extend(a, abs.bottom)) })
      Set(interpreterReturn(ActionPushEnv(exp, FrameLetrec(addresses.head, addresses.tail.zip(bindings.map(_._2)), body, ρ1), ρ1, σ1)))
    }
    case SchemeSet(variable, exp) => Set(interpreterReturn(ActionPush(exp, FrameSet(variable, ρ))))
    case SchemeBegin(body) => Set(interpreterReturn(evalBody(body, ρ, σ)))
    case SchemeCond(Nil) => Set(interpreterReturn(ActionError(s"cond without clauses")))
    case SchemeCond((cond, cons) :: clauses) => Set(interpreterReturn(ActionPush(cond, FrameCond(cons, clauses, ρ))))
    case SchemeCase(key, clauses, default) => Set(interpreterReturn(ActionPush(key, FrameCase(clauses, default, ρ))))
    case SchemeAnd(Nil) => Set(interpreterReturn(ActionReachedValue(abs.inject(true), σ)))
    case SchemeAnd(exp :: exps) => Set(interpreterReturn(ActionPush(exp, FrameAnd(exps, ρ))))
    case SchemeOr(Nil) => Set(interpreterReturn(ActionReachedValue(abs.inject(false), σ)))
    case SchemeOr(exp :: exps) => Set(interpreterReturn(ActionPush(exp, FrameOr(exps, ρ))))
    case SchemeDefineVariable(name, exp) => Set(interpreterReturn(ActionPush(exp, FrameDefine(name, ρ))))
    case SchemeDefineFunction(name, args, body) => {
      /*
       * TODO switch to extended environment
       */
      val a = addr.variable(name, t)
      val v = abs.inject[SchemeExp, Addr]((SchemeLambda(args, body), ρ))
      val ρ1 = ρ.extend(name, a)
      val σ1 = σ.extend(a, v)
      Set(interpreterReturn(ActionReachedValue(v, σ)))
    }
    case SchemeIdentifier(name) => ρ.lookup(name) match {
      case Some(a) => Set(interpreterReturn(ActionLookupVariable(name, Set[Addr](a)))) /* reads on a */
      case None => Set(interpreterReturn(ActionError(s"Unbound variable: $name")))
    }
    case SchemeQuoted(quoted) => evalQuoted(quoted, σ, t) match {
      case (value, σ2) => Set(interpreterReturn(ActionReachedValue(value, σ2)))
    }
    case SchemeValue(v) => evalValue(v) match {
      case Some(v) => Set(interpreterReturn(ActionReachedValue(v, σ)))
      case None => Set(interpreterReturn(ActionError(s"Unhandled value: $v")))
    }
    case SchemeCas(variable, eold, enew) => Set(interpreterReturn(ActionPush(eold, FrameCasOld(variable, enew, ρ))))
    case SchemeAcquire(variable) => ρ.lookup(variable) match {
      case Some(a) => {
        val v = σ.lookup(a)
        /* Only performs a step if the lock is possibly unlocked (true is unlocked, false is locked) */
        if (abs.isTrue(v)) Set(interpreterReturn(ActionReachedValue(abs.inject(true), σ.update(a, abs.inject(false))))) else Set()
      }
      case None => Set(interpreterReturn(ActionError(s"Unbound variable: $variable")))
    }
    case SchemeRelease(variable) => ρ.lookup(variable) match {
      case Some(a) => Set(interpreterReturn(ActionReachedValue(abs.inject(true), σ.update(a, abs.inject(true)))))
      case None => Set(interpreterReturn(ActionError(s"Unbound variable: $variable")))
    }
  }

  protected def evalWhileBody(condition: SchemeExp, body: List[SchemeExp], exps: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs]): Action[SchemeExp, Abs, Addr] = exps match {
    case Nil => ActionPush(condition, FrameWhileCondition(condition, body, ρ))
    case List(exp) => ActionPush(exp, FrameWhileBody(condition, body, Nil, ρ))
    case exp :: rest => ActionPush(exp, FrameWhileBody(condition, body, rest, ρ))
  }

  def stepKont(v: Abs, frame: Frame, σ: Store[Addr, Abs], t: Time) : Set[InterpreterReturn] = frame match {
    case FrameHalt => Set()
    case FrameWhileBody(condition, body, exps, ρ) => Set(interpreterReturn(evalWhileBody(condition, body, exps, ρ, σ)))
    case FrameWhileCondition(condition, body, ρ) => {
      (if (abs.isTrue(v)) Set[InterpreterReturn](interpreterReturnStart(evalWhileBody(condition, body, body, ρ, σ), body)) else Set[InterpreterReturn]()) ++
      (if (abs.isFalse(v)) Set[InterpreterReturn](interpreterReturn(ActionReachedValue(abs.inject(false), σ))) else Set[InterpreterReturn]())
    }
    case FrameFuncallOperator(fexp, args, ρ) => funcallArgs(v, fexp, args, ρ, σ, t)
    case FrameFuncallOperands(f, fexp, exp, args, toeval, ρ) => funcallArgs(f, fexp, (exp, v) :: args, toeval, ρ, σ, t)
    case FrameIf(cons, alt, ρ) =>
      conditionalIf(v, ActionEval(cons, ρ, σ), (cons, ρ), ActionEval(alt, ρ, σ), (alt, ρ))
    case FrameLet(name, bindings, Nil, body, ρ) => {
      val variables = name :: bindings.reverse.map(_._1)
      val addresses = variables.map(v => addr.variable(v, t))
      val (ρ1, σ1) = ((name, v) :: bindings).zip(addresses).foldLeft((ρ, σ))({
        case ((ρ, σ), ((variable, value), a)) => (ρ.extend(variable, a), σ.extend(a, value))
      })
      Set(interpreterReturn(evalBody(body, ρ1, σ1)))
    }
    case FrameLet(name, bindings, (variable, e) :: toeval, body, ρ) =>
      Set(interpreterReturn(ActionPush(e, FrameLet(variable, (name, v) :: bindings, toeval, body, ρ))))
    case FrameLetStar(name, bindings, body, ρ) => {
      val a = addr.variable(name, t)
      val ρ1 = ρ.extend(name, a)
      val σ1 = σ.extend(a, v)
      bindings match {
        case Nil => Set(interpreterReturn(evalBody(body, ρ1, σ1)))
        case (variable, exp) :: rest => Set(InterpreterReturn(List(ActionExtendEnv(name), ActionPush(exp, FrameLetStar(variable, rest, body, ρ1))), new TracingSignalFalse))
      }
    }
    case FrameLetrec(a, Nil, body, ρ) => Set(interpreterReturn(evalBody(body, ρ, σ.update(a, v))))
    case FrameLetrec(a, (a1, exp) :: rest, body, ρ) =>
      Set(InterpreterReturn(List(ActionSetVar(a), ActionPush(exp, FrameLetrec(a1, rest, body, ρ))), new TracingSignalFalse))
    case FrameSet(name, ρ) => ρ.lookup(name) match {
      case Some(a) => Set(interpreterReturn(ActionReachedValue(abs.inject(false), σ.update(a, v), Set[Addr](), Set[Addr](a)))) /* writes on a */
      case None => Set(interpreterReturn(ActionError(s"Unbound variable: $name")))
    }
    case FrameBegin(body, ρ) => Set(interpreterReturn(evalBody(body, ρ, σ)))
    case FrameCond(cons, clauses, ρ) =>
      conditional(v, if (cons.isEmpty) { ActionReachedValue(v, σ) } else { evalBody(cons, ρ, σ) },
        clauses match {
          case Nil => ActionReachedValue(abs.inject(false), σ)
          case (exp, cons2) :: rest => ActionPush(exp, FrameCond(cons2, rest, ρ))
        })
    case FrameCase(clauses, default, ρ) => {
      val fromClauses = clauses.flatMap({ case (values, body) =>
        if (values.exists(v2 => evalValue(v2.value) match {
          case None => false
          case Some(v2) => abs.subsumes(v, v2)
        }))
        /* TODO: precision could be improved by restricting v to v2 */
          Set[InterpreterReturn](interpreterReturn(evalBody(body, ρ, σ)))
        else
          Set[InterpreterReturn]()
      })
      /* TODO: precision could be improved in cases where we know that default is not
       * reachable */
      fromClauses.toSet + interpreterReturn(evalBody(default, ρ, σ))
    }
    case FrameAnd(Nil, ρ) =>
      conditional(v, ActionReachedValue(v, σ), ActionReachedValue(abs.inject(false), σ))
    case FrameAnd(e :: rest, ρ) =>
      conditional(v, ActionPush(e, FrameAnd(rest, ρ)), ActionReachedValue(abs.inject(false), σ))
    case FrameOr(Nil, ρ) =>
      conditional(v, ActionReachedValue(v, σ), ActionReachedValue(abs.inject(false), σ))
    case FrameOr(e :: rest, ρ) =>
      conditional(v, ActionReachedValue(v, σ), ActionPush(e, FrameOr(rest, ρ)))
    case FrameDefine(name, ρ) => throw new Exception(s"TODO: define not handled (no global environment)")
    case FrameCasOld(variable, enew, ρ) =>
      Set(interpreterReturn(ActionPush(enew, FrameCasNew(variable, v, ρ))))
    case FrameCasNew(variable, old, ρ) =>
      ρ.lookup(variable) match {
        case Some(a) => conditional(abs.binaryOp(BinaryOperator.Eq)(σ.lookup(a), old),
          /* Compare and swap succeeds */
          ActionReachedValue(abs.inject(true), σ.update(a, v)),
          /* Compare and swap fails */
          ActionReachedValue(abs.inject(false), σ))
        case None => Set(interpreterReturn(ActionError(s"Unbound variable: $variable")))
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
class SchemeSemanticsTraced[Abs : AbstractValue, Addr : Address, Time : Timestamp]
  extends BaseSchemeSemanticsTraced[Abs, Addr, Time] {

  /** Tries to perform atomic evaluation of an expression. Returns the result of
    * the evaluation if it succeeded, otherwise returns None */
  def atomicEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs]): Option[(Abs, Set[Addr])] = e match {
    case λ: SchemeLambda => Some((abs.inject[SchemeExp, Addr]((λ, ρ)), Set[Addr]()))
    case SchemeIdentifier(name) => ρ.lookup(name).map(a => (σ.lookup(a), Set[Addr](a)))
    case SchemeValue(v) => evalValue(v).map(value => (value, Set[Addr]()))
    case _ => None
  }

  protected def addRead(action: Action[SchemeExp, Abs, Addr], read: Set[Addr]): Action[SchemeExp, Abs, Addr] = action match {
    case ActionReachedValue(v, σ, read2, write) => ActionReachedValue(v, σ, read ++ read2, write)
    case ActionPush(e, frame, read2, write) => ActionPush(e, frame, read ++ read2, write)
    case ActionEval(e, ρ, σ, read2, write) => ActionEval(e, ρ, σ, read ++ read2, write)
    case ActionStepIn(fexp, clo, e, args, argsv, n, read2, write) => ActionStepIn(fexp, clo, e, args, argsv, n, read ++ read2, write)
    case ActionError(err) => ActionError(err)
  }

  /**
    * Optimize the following pattern: when we see an ActionPush(exp, frame, ρ, σ)
    * where exp is an atomic expression, we can atomically evaluate exp to get v,
    * and call stepKont(v, σ, frame).
    */
//  protected def optimizeAtomic(actions: Set[InterpreterReturn], t: Time): Set[InterpreterReturn] = {
//    actions.flatMap({ itpRet => itpRet.trace.flatMap {
//      case ActionPush(exp, frameGen, ρ, σ, read, write) => atomicEval(exp, ρ, σ) match {
//        case Some((v, read2)) => stepKont(v, frameGen, σ, t).map({itpRet => new InterpreterReturn(itpRet.trace.map(addRead(_, read ++ read2)), itpRet.tracingSignal)})
//        case None => Set[InterpreterReturn](new InterpreterReturn(List(ActionPush(exp, frame, ρ, σ, read, write)), itpRet.tracingSignal))
//      }
//      case action => Set[InterpreterReturn](new InterpreterReturn(List(action), itpRet.tracingSignal))
//    }})
//  }

  /*
   * TODO: ignore any optimizations for now
   */
  /*
  override def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time) =
    optimizeAtomic(super.stepEval(e, ρ, σ, t), t)

  override def stepKont(v: Abs, frame: Frame, σ: Store[Addr, Abs], t: Time) =
    optimizeAtomic(super.stepKont(v, frame, σ, t), t)
    */
}

