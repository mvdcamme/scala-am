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
  case class FrameIf(cons: SchemeExp, alt: SchemeExp) extends SchemeFrame
  case class FrameLet(variable: String, bindings: List[(String, Abs)], toeval: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeFrame
  case class FrameLetStar(variable: String, bindings: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeFrame
  case class FrameLetrec(variable: String, bindings: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeFrame
  case class FrameSet(variable: String, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameBegin(rest: List[SchemeExp]) extends SchemeFrame
  case class FrameCond(cons: List[SchemeExp], clauses: List[(SchemeExp, List[SchemeExp])], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCase(clauses: List[(List[SchemeValue], List[SchemeExp])], default: List[SchemeExp]) extends SchemeFrame
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
    case FrameIf(cons, alt) => FrameIf(cons, alt)
    case FrameLet(variable, bindings, toeval, body) => FrameLet(variable, bindings.map({tuple => (tuple._1, convertValue(tuple._2))}), toeval, body)
    case FrameLetStar(variable, bindings, body) => FrameLetStar(variable, bindings, body)
    case FrameLetrec(variable, bindings, body) => FrameLetrec(variable, bindings, body)
    case FrameSet(variable, ρ) => FrameSet(variable, ρ.map(convertAddress))
    case FrameBegin(rest) => FrameBegin(rest)
    case FrameCond(cond, clauses, ρ) => FrameCond(cond, clauses, ρ.map(convertAddress))
    case FrameCase(clauses, default) => FrameCase(clauses, default)
    case FrameAnd(rest, ρ) => FrameAnd(rest, ρ.map(convertAddress))
    case FrameOr(rest, ρ) => FrameOr(rest, ρ.map(convertAddress))
    case FrameDefine(variable, ρ) => FrameDefine(variable, ρ.map(convertAddress))
    case FrameCasOld(variable, enew, ρ) => FrameCasOld(variable, enew, ρ.map(convertAddress))
    case FrameCasNew(variable, old, ρ) => FrameCasNew(variable, convertValue(old), ρ.map(convertAddress))
    case FrameWhileBody(condition, body, exps, ρ) => FrameWhileBody(condition, body, exps, ρ.map(convertAddress))
    case FrameWhileCondition(condition, body, ρ) => FrameWhileCondition(condition, body, ρ.map(convertAddress))
  }

  protected def evalBody(body: List[SchemeExp]): Trace = body match {
    case Nil => List(ActionReachedValue(abs.inject(false)))
    case List(exp) => List(ActionEval(exp))
    case exp :: rest => List(ActionSaveEnv(), ActionPush(exp, FrameBegin(rest)))
  }

  def conditional(v: Abs, t: Trace, f: Trace): Set[InterpreterReturn] =
    (if (abs.isTrue(v)) Set[InterpreterReturn](InterpreterReturn(t, new TracingSignalFalse)) else Set[InterpreterReturn]()) ++
    (if (abs.isFalse(v)) Set[InterpreterReturn](InterpreterReturn(f, new TracingSignalFalse)) else Set[InterpreterReturn]())

  /*
   * TODO: Debugging: run tests only using if-expressions. Remove function ASAP and use function "conditional" instead!
   */
  def conditionalIf(v: Abs, t: Trace, tRestart : RestartPoint, f: Trace, fRestart : RestartPoint): Set[InterpreterReturn] =
    (if (abs.isTrue(v)) Set[InterpreterReturn](new InterpreterReturn(ActionRestoreEnv[SchemeExp, Abs, Addr]() :: ActionGuardTrue[SchemeExp, Abs, Addr, RestartPoint](fRestart) :: t, TracingSignalFalse())) else Set[InterpreterReturn]()) ++
    (if (abs.isFalse(v)) Set[InterpreterReturn](new InterpreterReturn(ActionRestoreEnv[SchemeExp, Abs, Addr]() :: ActionGuardFalse[SchemeExp, Abs, Addr, RestartPoint](tRestart) :: f, TracingSignalFalse())) else Set[InterpreterReturn]())

  /**
   * @param argsv are the Scheme-expressions of the operands of the call
   */
  def evalCall(function: Abs, fexp: SchemeExp, argsv: List[(SchemeExp, Abs)], ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[InterpreterReturn] = {

    /*
     * The number of values to pop. In the case of a primitive application, these are all operands that were evaluated + the operator itself
     */
    val valsToPop = argsv.length + 1

    val actions : Trace = List(ActionRestoreEnv(), ActionPushVal())

    val fromClo: Set[InterpreterReturn] = abs.getClosures[SchemeExp, Addr](function).map({
      case (SchemeLambda(args, body), ρ1) =>
        if (body.length == 1)
          InterpreterReturn(actions :+ ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body), ρ1), body.head, args, argsv.map(_._1), valsToPop), new TracingSignalStart(body))
        else
          InterpreterReturn(actions :+ ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body), ρ1), SchemeBegin(body), args, argsv.map(_._1), valsToPop), new TracingSignalStart(body))
      case (λ, _) => interpreterReturn(List(ActionError[SchemeExp, Abs, Addr](s"Incorrect closure with lambda-expression ${λ}")))
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
    case e :: rest => Set(InterpreterReturn(List(ActionRestoreEnv(), ActionPushVal(), ActionSaveEnv(), ActionPush(e, FrameFuncallOperands(f, fexp, e, args, rest, ρ))), new TracingSignalFalse))
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
    case λ: SchemeLambda => Set(interpreterReturn(List(ActionReachedValue(abs.inject[SchemeExp, Addr]((λ, ρ))))))
    case SchemeWhile(condition, body) => Set(interpreterReturn(List(ActionPush(condition, FrameWhileCondition(condition, body, ρ)))))
    case SchemeFuncall(f, args) => Set(interpreterReturn(List(ActionSaveEnv(), ActionPush(f, FrameFuncallOperator(f, args, ρ)))))
    case SchemeIf(cond, cons, alt) => Set(interpreterReturn(List(ActionSaveEnv(), ActionPush(cond, FrameIf(cons, alt)))))
    case SchemeLet(Nil, body) => Set(interpreterReturn(evalBody(body)))
    case SchemeLet((v, exp) :: bindings, body) => Set(interpreterReturn(List(ActionSaveEnv(), ActionPush(exp, FrameLet(v, List(), bindings, body)))))
    case SchemeLetStar(Nil, body) => Set(interpreterReturn(evalBody(body)))
    case SchemeLetStar((v, exp) :: bindings, body) => Set(interpreterReturn(List(ActionSaveEnv(), ActionPush(exp, FrameLetStar(v, bindings, body)))))
    case SchemeLetrec(Nil, body) => Set(interpreterReturn(evalBody(body)))
    case SchemeLetrec((v, exp) :: bindings, body) => {
      val variables = v :: bindings.map(_._1)
      Set(interpreterReturn(List(ActionAllocVars(variables), ActionSaveEnv(),
                                 ActionPush(exp, FrameLetrec(variables.head, variables.tail.zip(bindings.map(_._2)), body)))))
    }
    case SchemeSet(variable, exp) => Set(interpreterReturn(List(ActionPush(exp, FrameSet(variable, ρ)))))
    case SchemeBegin(body) => Set(interpreterReturn(ActionRestoreEnv[SchemeExp, Abs, Addr]() :: evalBody(body)))
    case SchemeCond(Nil) => Set(interpreterReturn(List(ActionError(s"cond without clauses"))))
    case SchemeCond((cond, cons) :: clauses) => Set(interpreterReturn(List(ActionPush(cond, FrameCond(cons, clauses, ρ)))))
    case SchemeCase(key, clauses, default) => Set(interpreterReturn(List(ActionPush(key, FrameCase(clauses, default)))))
    case SchemeAnd(Nil) => Set(interpreterReturn(List(ActionReachedValue(abs.inject(true)))))
    case SchemeAnd(exp :: exps) => Set(interpreterReturn(List(ActionPush(exp, FrameAnd(exps, ρ)))))
    case SchemeOr(Nil) => Set(interpreterReturn(List(ActionReachedValue(abs.inject(false)))))
    case SchemeOr(exp :: exps) => Set(interpreterReturn(List(ActionPush(exp, FrameOr(exps, ρ)))))
    case SchemeDefineVariable(name, exp) => Set(interpreterReturn(List(ActionPush(exp, FrameDefine(name, ρ)))))
    case SchemeDefineFunction(name, args, body) => {
      /*
       * TODO switch to extended environment
       */
      val a = addr.variable(name, t)
      val v = abs.inject[SchemeExp, Addr]((SchemeLambda(args, body), ρ))
      val ρ1 = ρ.extend(name, a)
      val σ1 = σ.extend(a, v)
      Set(interpreterReturn(List(ActionReachedValue(v))))
    }
    case SchemeIdentifier(name) => ρ.lookup(name) match {
      case Some(a) => Set(interpreterReturn(List(ActionLookupVariable(name, Set[Addr](a))))) /* reads on a */
      case None => Set(interpreterReturn(List(ActionError(s"Unbound variable: $name"))))
    }
//    case SchemeQuoted(quoted) => evalQuoted(quoted, σ, t) match { TODO Take care of this: add instruction to swap the store? Since a quoted expression is static anyway?
//      case (value, σ2) => Set(interpreterReturn(ActionReachedValue(value, σ2)))
//    }
    case SchemeValue(v) => evalValue(v) match {
      case Some(v) => Set(interpreterReturn(List(ActionReachedValue(v))))
      case None => Set(interpreterReturn(List(ActionError(s"Unhandled value: $v"))))
    }
    case SchemeCas(variable, eold, enew) => Set(interpreterReturn(List(ActionPush(eold, FrameCasOld(variable, enew, ρ)))))
    case SchemeAcquire(variable) => ρ.lookup(variable) match {
      case Some(a) => {
        val v = σ.lookup(a)
        /* Only performs a step if the lock is possibly unlocked (true is unlocked, false is locked) */
        if (abs.isTrue(v)) Set(InterpreterReturn(List(ActionLiteral(abs.inject(false)), ActionSetVar(variable), ActionReachedValue(abs.inject(true))), new TracingSignalFalse())) else Set()
      }
      case None => Set(interpreterReturn(List(ActionError(s"Unbound variable: $variable"))))
    }
    case SchemeRelease(variable) => ρ.lookup(variable) match {
      case Some(a) => Set(InterpreterReturn(List(ActionLiteral(abs.inject(true)), ActionSetVar(variable), ActionReachedValue(abs.inject(true))), new TracingSignalFalse()))
      case None => Set(interpreterReturn(List(ActionError(s"Unbound variable: $variable"))))
    }
  }

  protected def evalWhileBody(condition: SchemeExp, body: List[SchemeExp], exps: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs]): Action[SchemeExp, Abs, Addr] = exps match {
    case Nil => ActionPush(condition, FrameWhileCondition(condition, body, ρ))
    case List(exp) => ActionPush(exp, FrameWhileBody(condition, body, Nil, ρ))
    case exp :: rest => ActionPush(exp, FrameWhileBody(condition, body, rest, ρ))
  }

  def stepKont(v: Abs, frame: Frame, σ: Store[Addr, Abs], t: Time) : Set[InterpreterReturn] = frame match {
    case FrameHalt => Set()
    case FrameWhileBody(condition, body, exps, ρ) => Set(interpreterReturn(List(evalWhileBody(condition, body, exps, ρ, σ))))
    case FrameWhileCondition(condition, body, ρ) => {
      (if (abs.isTrue(v)) Set[InterpreterReturn](interpreterReturnStart(evalWhileBody(condition, body, body, ρ, σ), body)) else Set[InterpreterReturn]()) ++
      (if (abs.isFalse(v)) Set[InterpreterReturn](interpreterReturn(List(ActionReachedValue(abs.inject(false))))) else Set[InterpreterReturn]())
    }
    case FrameFuncallOperator(fexp, args, ρ) => funcallArgs(v, fexp, args, ρ, σ, t)
    case FrameFuncallOperands(f, fexp, exp, args, toeval, ρ) => funcallArgs(f, fexp, (exp, v) :: args, toeval, ρ, σ, t)
    case FrameIf(cons, alt) =>
      conditionalIf(v, List(ActionEval(cons)), cons, List(ActionEval(alt)), alt)
    case FrameLet(name, bindings, Nil, body) => {
      val variables = name :: bindings.map(_._1)
      Set(interpreterReturn(ActionRestoreEnv[SchemeExp, Abs, Addr]() :: ActionPushVal[SchemeExp, Abs, Addr]() :: ActionDefineVars[SchemeExp, Abs, Addr](variables) :: evalBody(body)))
    }
    case FrameLet(name, bindings, (variable, e) :: toeval, body) =>
      Set(interpreterReturn(List(ActionRestoreEnv[SchemeExp, Abs, Addr](), ActionPushVal[SchemeExp, Abs, Addr](), ActionSaveEnv(), ActionPush(e, FrameLet(variable, (name, v) :: bindings, toeval, body)))))
    case FrameLetStar(name, bindings, body) => {
      val actions = List(ActionRestoreEnv[SchemeExp, Abs, Addr](), ActionExtendEnv[SchemeExp, Abs, Addr](name))
      bindings match {
        case Nil => Set(interpreterReturn(actions ++ evalBody(body)))
        case (variable, exp) :: rest => Set(InterpreterReturn(actions ++ List(ActionSaveEnv[SchemeExp, Abs, Addr](), ActionPush(exp, FrameLetStar(variable, rest, body))), new TracingSignalFalse))
      }
    }
    case FrameLetrec(a, Nil, body) =>
      val actions : Trace = List(ActionRestoreEnv[SchemeExp, Abs, Addr](), ActionSetVar[SchemeExp, Abs, Addr](a)) ++ evalBody(body)
      Set(InterpreterReturn(actions, new TracingSignalFalse))
    case FrameLetrec(var1, (var2, exp) :: rest, body) =>
      val actions : Trace = List(ActionRestoreEnv(), ActionSetVar(var1), ActionPush(exp, FrameLetrec(var2, rest, body)), ActionSaveEnv())
      Set(InterpreterReturn(actions, new TracingSignalFalse))
    case FrameSet(name, ρ) => ρ.lookup(name) match {
      case Some(a) => Set(InterpreterReturn(List(ActionSetVar(name), ActionReachedValue(abs.inject(false), Set[Addr](), Set[Addr](a))), new TracingSignalFalse)) /* writes on a */
      case None => Set(interpreterReturn(List(ActionError(s"Unbound variable: $name"))))
    }
    case FrameBegin(body) => Set(interpreterReturn(ActionRestoreEnv[SchemeExp, Abs, Addr]() :: evalBody(body)))
    case FrameCond(cons, clauses, ρ) =>
      conditional(v, if (cons.isEmpty) { List(ActionReachedValue(v)) } else { evalBody(cons) },
        clauses match {
          case Nil => List(ActionReachedValue(abs.inject(false)))
          case (exp, cons2) :: rest => List(ActionPush(exp, FrameCond(cons2, rest, ρ)))
        })
    case FrameCase(clauses, default) => {
      val fromClauses = clauses.flatMap({ case (values, body) =>
        if (values.exists(v2 => evalValue(v2.value) match {
          case None => false
          case Some(v2) => abs.subsumes(v, v2)
        }))
        /* TODO: precision could be improved by restricting v to v2 */
          Set[InterpreterReturn](interpreterReturn(List[Action[SchemeExp, Abs, Addr]](ActionRestoreEnv()) ++ evalBody(body)))
        else
          Set[InterpreterReturn]()
      })
      /* TODO: precision could be improved in cases where we know that default is not
       * reachable */
      fromClauses.toSet + interpreterReturn(List[Action[SchemeExp, Abs, Addr]](ActionRestoreEnv()) ++ evalBody(default))
    }
    case FrameAnd(Nil, ρ) =>
      conditional(v, List(ActionReachedValue(v)), List(ActionReachedValue(abs.inject(false))))
    case FrameAnd(e :: rest, ρ) =>
      conditional(v, List(ActionPush(e, FrameAnd(rest, ρ))), List(ActionReachedValue(abs.inject(false))))
    case FrameOr(Nil, ρ) =>
      conditional(v, List(ActionReachedValue(v)), List(ActionReachedValue(abs.inject(false))))
    case FrameOr(e :: rest, ρ) =>
      conditional(v, List(ActionReachedValue(v)), List(ActionPush(e, FrameOr(rest, ρ))))
    case FrameDefine(name, ρ) => throw new Exception(s"TODO: define not handled (no global environment)")
    case FrameCasOld(variable, enew, ρ) =>
      Set(interpreterReturn(List(ActionPush(enew, FrameCasNew(variable, v, ρ)))))
    case FrameCasNew(variable, old, ρ) =>
      ρ.lookup(variable) match {
        case Some(a) => conditional(abs.binaryOp(BinaryOperator.Eq)(σ.lookup(a), old),
          /* Compare and swap succeeds */
          List(ActionSetVar(variable), ActionReachedValue(abs.inject(true))),
          /* Compare and swap fails */
          List(ActionReachedValue(abs.inject(false))))
        case None => Set(interpreterReturn(List(ActionError(s"Unbound variable: $variable"))))
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
    case ActionReachedValue(v, read2, write) => ActionReachedValue(v, read ++ read2, write)
    case ActionPush(e, frame, read2, write) => ActionPush(e, frame, read ++ read2, write)
    case ActionEval(e, read2, write) => ActionEval(e, read ++ read2, write)
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

