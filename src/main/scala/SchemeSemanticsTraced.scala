/**
  * Created by mvdcamme on 02/02/16.
  */

/**
  * Basic Scheme semantics, without any optimization
  */
abstract class BaseSchemeSemanticsTraced[Abs : AbstractValue, Addr : Address, Time : Timestamp]
  extends BaseSemanticsTraced[SchemeExp, Abs, Addr, Time] {

  /*
   * Some unparametrized actions.
   * Defined here so that we don't have to give all type parameters each time these actions are used.
   */
  val actionPopKont = ActionPopKontTraced[SchemeExp, Abs, Addr]()
  val actionPushVal = ActionPushValTraced[SchemeExp, Abs, Addr]()
  val actionRestoreEnv = ActionRestoreEnvTraced[SchemeExp, Abs, Addr]()
  val actionSaveEnv = ActionSaveEnvTraced[SchemeExp, Abs, Addr]()

  trait SchemeFrame extends Frame {
    def subsumes(that: Frame) = that.equals(this)
    override def toString = s"${this.getClass.getSimpleName}"
  }
  case class FrameAnd(rest: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameBegin(rest: List[SchemeExp]) extends SchemeFrame
  case class FrameCase(clauses: List[(List[SchemeValue], List[SchemeExp])], default: List[SchemeExp]) extends SchemeFrame
  case class FrameCasOld(variable: String, enew: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCasNew(variable: String, old: Abs, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCond(cons: List[SchemeExp], clauses: List[(SchemeExp, List[SchemeExp])], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameDefine(variable: String, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameFunBody(body : List[SchemeExp], rest: List[SchemeExp]) extends SchemeFrame
  case class FrameFuncallOperands(f: Abs, fexp: SchemeExp, cur: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameFuncallOperator(fexp: SchemeExp, args: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameIf(cons: SchemeExp, alt: SchemeExp) extends SchemeFrame
  case class FrameLet(variable: String, bindings: List[(String, Abs)], toeval: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeFrame
  case class FrameLetrec(variable: String, bindings: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeFrame
  case class FrameLetStar(variable: String, bindings: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeFrame
  case class FrameOr(rest: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameSet(variable: String, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameWhileBody(condition : SchemeExp, body : List[SchemeExp], exps : List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameWhileCondition(condition : SchemeExp, body : List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  object FrameHalt extends SchemeFrame {
    override def toString() = "FHalt"
  }

  def convertFrame(convertAddress : Addr => Addr, convertValue : Abs => Abs)(frame : Frame) : Frame = frame match {
    case FrameAnd(rest, ρ) => FrameAnd(rest, ρ.map(convertAddress))
    case FrameBegin(rest) => FrameBegin(rest)
    case FrameCase(clauses, default) => FrameCase(clauses, default)
    case FrameCasOld(variable, enew, ρ) => FrameCasOld(variable, enew, ρ.map(convertAddress))
    case FrameCasNew(variable, old, ρ) => FrameCasNew(variable, convertValue(old), ρ.map(convertAddress))
    case FrameCond(cond, clauses, ρ) => FrameCond(cond, clauses, ρ.map(convertAddress))
    case FrameDefine(variable, ρ) => FrameDefine(variable, ρ.map(convertAddress))
    case FrameFunBody(body, toeval) => FrameFunBody(body, toeval)
    case FrameFuncallOperands(f, fexp, cur, args, toeval, ρ) => FrameFuncallOperands(convertValue(f), fexp, cur, args.map({tuple => (tuple._1, convertValue(tuple._2))}), toeval, ρ.map(convertAddress))
    case FrameFuncallOperator(fexp, args, ρ) => FrameFuncallOperator(fexp, args, ρ.map(convertAddress))
    case FrameIf(cons, alt) => FrameIf(cons, alt)
    case FrameLet(variable, bindings, toeval, body) => FrameLet(variable, bindings.map({tuple => (tuple._1, convertValue(tuple._2))}), toeval, body)
    case FrameLetrec(variable, bindings, body) => FrameLetrec(variable, bindings, body)
    case FrameLetStar(variable, bindings, body) => FrameLetStar(variable, bindings, body)
    case FrameOr(rest, ρ) => FrameOr(rest, ρ.map(convertAddress))
    case FrameSet(variable, ρ) => FrameSet(variable, ρ.map(convertAddress))
    case FrameWhileBody(condition, body, exps, ρ) => FrameWhileBody(condition, body, exps, ρ.map(convertAddress))
    case FrameWhileCondition(condition, body, ρ) => FrameWhileCondition(condition, body, ρ.map(convertAddress))
  }

  /**
    * @param frameGen A function that generates the next frame to push, given the non-evaluated expressions of the body
    */
  protected def evalBody(body: List[SchemeExp], frameGen : List[SchemeExp] => Frame): Trace = body match {
    case Nil => List(ActionReachedValueTraced(abs.inject(false)), actionPopKont)
    case List(exp) => List(ActionEvalTraced(exp))
    case exp :: rest => List(actionSaveEnv, ActionPushTraced(exp, frameGen(rest)))
  }

  def conditional(v: Abs, t: Trace, f: Trace): Set[InterpreterReturn] =
    (if (abs.isTrue(v)) Set[InterpreterReturn](InterpreterReturn(t, new TracingSignalFalse)) else Set[InterpreterReturn]()) ++
    (if (abs.isFalse(v)) Set[InterpreterReturn](InterpreterReturn(f, new TracingSignalFalse)) else Set[InterpreterReturn]())

  /*
   * TODO: Debugging: run tests only using if-expressions. Remove function ASAP and use function "conditional" instead!
   */
  def conditionalIf(v: Abs, t: Trace, tRestart : RestartPoint, f: Trace, fRestart : RestartPoint): Set[InterpreterReturn] =
    (if (abs.isTrue(v)) Set[InterpreterReturn](new InterpreterReturn(actionRestoreEnv :: ActionGuardTrueTraced[SchemeExp, Abs, Addr, RestartPoint](fRestart) :: t, TracingSignalFalse())) else Set[InterpreterReturn]()) ++
    (if (abs.isFalse(v)) Set[InterpreterReturn](new InterpreterReturn(actionRestoreEnv :: ActionGuardFalseTraced[SchemeExp, Abs, Addr, RestartPoint](tRestart) :: f, TracingSignalFalse())) else Set[InterpreterReturn]())

  /**
   * @param argsv are the Scheme-expressions of the operands of the call
   */
  def evalCall(function: Abs, fexp: SchemeExp, argsv: List[(SchemeExp, Abs)], ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[InterpreterReturn] = {

    /*
     * The number of values to pop. In the case of a primitive application, these are all operands that were evaluated + the operator itself
     */
    val valsToPop = argsv.length + 1

    val actions : Trace = List(actionRestoreEnv, actionPushVal)

    val fromClo: Set[InterpreterReturn] = abs.getClosures[SchemeExp, Addr](function).map({
      case (SchemeLambda(args, body), ρ1) =>
        InterpreterReturn(actions :+ ActionStepInTraced(fexp, (SchemeLambda(args, body), ρ1), body.head, args,
                                                   argsv.map(_._1), valsToPop, FrameFunBody(body, body.tail)),
                          new TracingSignalStart(body))
      case (λ, _) => interpreterReturn(List(ActionErrorTraced[SchemeExp, Abs, Addr](s"Incorrect closure with lambda-expression ${λ}")))
    })

    val fromPrim = abs.getPrimitive(function) match {
      case Some(prim) => Set(InterpreterReturn(actions :+ ActionPrimCallTraced(valsToPop, fexp, argsv.map(_._1)) :+ actionPopKont, new TracingSignalFalse()))
      case None => Set()
    }
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      Set(new InterpreterReturn(actions :+ ActionErrorTraced[SchemeExp, Abs, Addr](s"Called value is not a function: $function"), new TracingSignalFalse))
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
    case e :: rest => Set(InterpreterReturn(List(actionRestoreEnv, actionPushVal, actionSaveEnv, ActionPushTraced(e, FrameFuncallOperands(f, fexp, e, args, rest, ρ))), new TracingSignalFalse))
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
    case λ: SchemeLambda => Set(interpreterReturn(List(ActionReachedValueTraced(abs.inject[SchemeExp, Addr]((λ, ρ))), actionPopKont)))
    case SchemeAcquire(variable) => ρ.lookup(variable) match {
      case Some(a) => {
        val v = σ.lookup(a)
        /* Only performs a step if the lock is possibly unlocked (true is unlocked, false is locked) */
        if (abs.isTrue(v)) Set(InterpreterReturn(List(ActionLiteralTraced(abs.inject(false)), ActionSetVarTraced(variable),
                                                      ActionReachedValueTraced(abs.inject(true)), actionPopKont),
                                                 new TracingSignalFalse())) else Set()
      }
      case None => Set(interpreterReturn(List(ActionErrorTraced(s"Unbound variable: $variable"))))
    }
    case SchemeAnd(Nil) => Set(interpreterReturn(List(ActionReachedValueTraced(abs.inject(true)), actionPopKont)))
    case SchemeAnd(exp :: exps) => Set(interpreterReturn(List(ActionPushTraced(exp, FrameAnd(exps, ρ)))))
    case SchemeBegin(body) => Set(interpreterReturn(actionRestoreEnv :: evalBody(body, FrameBegin)))
    case SchemeCase(key, clauses, default) => Set(interpreterReturn(List(ActionPushTraced(key, FrameCase(clauses, default)))))
    case SchemeCond(Nil) => Set(interpreterReturn(List(ActionErrorTraced(s"cond without clauses"))))
    case SchemeCond((cond, cons) :: clauses) => Set(interpreterReturn(List(ActionPushTraced(cond, FrameCond(cons, clauses, ρ)))))
    case SchemeCas(variable, eold, enew) => Set(interpreterReturn(List(ActionPushTraced(eold, FrameCasOld(variable, enew, ρ)))))
    case SchemeDefineVariable(name, exp) => Set(interpreterReturn(List(ActionPushTraced(exp, FrameDefine(name, ρ)))))
    case SchemeDefineFunction(name, args, body) => {
      /*
       * TODO switch to extended environment
       */
      val a = addr.variable(name, t)
      val v = abs.inject[SchemeExp, Addr]((SchemeLambda(args, body), ρ))
      val ρ1 = ρ.extend(name, a)
      val σ1 = σ.extend(a, v)
      Set(interpreterReturn(List(ActionReachedValueTraced(v), actionPopKont)))
    }
    case SchemeFuncall(f, args) => Set(interpreterReturn(List(actionSaveEnv, ActionPushTraced(f, FrameFuncallOperator(f, args, ρ)))))
    case SchemeIdentifier(name) => ρ.lookup(name) match {
      case Some(a) => Set(interpreterReturn(List(ActionLookupVariableTraced(name, Set[Addr](a)), actionPopKont))) /* reads on a */
      case None => Set(interpreterReturn(List(ActionErrorTraced(s"Unbound variable: $name"))))
    }
    case SchemeIf(cond, cons, alt) => Set(interpreterReturn(List(actionSaveEnv, ActionPushTraced(cond, FrameIf(cons, alt)))))
    case SchemeLet(Nil, body) => Set(interpreterReturn(evalBody(body, FrameBegin)))
    case SchemeLet((v, exp) :: bindings, body) => Set(interpreterReturn(List(actionSaveEnv, ActionPushTraced(exp, FrameLet(v, List(), bindings, body)))))
    case SchemeLetrec(Nil, body) => Set(interpreterReturn(evalBody(body, FrameBegin)))
    case SchemeLetrec((v, exp) :: bindings, body) => {
      val variables = v :: bindings.map(_._1)
      Set(interpreterReturn(List(ActionAllocVarsTraced(variables), actionSaveEnv,
                            ActionPushTraced(exp, FrameLetrec(variables.head, variables.tail.zip(bindings.map(_._2)), body)))))
    }
    case SchemeLetStar(Nil, body) => Set(interpreterReturn(evalBody(body, FrameBegin)))
    case SchemeLetStar((v, exp) :: bindings, body) => Set(interpreterReturn(List(actionSaveEnv, ActionPushTraced(exp, FrameLetStar(v, bindings, body)))))
    case SchemeOr(Nil) => Set(interpreterReturn(List(ActionReachedValueTraced(abs.inject(false)), actionPopKont)))
    case SchemeOr(exp :: exps) => Set(interpreterReturn(List(ActionPushTraced(exp, FrameOr(exps, ρ)))))
    case SchemeSet(variable, exp) => Set(interpreterReturn(List(ActionPushTraced(exp, FrameSet(variable, ρ)))))
//    case SchemeQuoted(quoted) => evalQuoted(quoted, σ, t) match { TODO Take care of this: add instruction to swap the store? Since a quoted expression is static anyway?
//      case (value, σ2) => Set(interpreterReturn(ActionReachedValue(value, σ2), actionPopKont))
//    }
    case SchemeRelease(variable) => ρ.lookup(variable) match {
      case Some(a) => Set(InterpreterReturn(List(ActionLiteralTraced(abs.inject(true)), ActionSetVarTraced(variable),
                                                 ActionReachedValueTraced(abs.inject(true)), actionPopKont), new TracingSignalFalse()))
      case None => Set(interpreterReturn(List(ActionErrorTraced(s"Unbound variable: $variable"))))
    }
    case SchemeValue(v) => evalValue(v) match {
      case Some(v) => Set(interpreterReturn(List(ActionReachedValueTraced(v), actionPopKont)))
      case None => Set(interpreterReturn(List(ActionErrorTraced(s"Unhandled value: $v"))))
    }
    case SchemeWhile(condition, body) => Set(interpreterReturn(List(ActionPushTraced(condition, FrameWhileCondition(condition, body, ρ)))))
  }

  protected def evalWhileBody(condition: SchemeExp, body: List[SchemeExp], exps: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs]): Action[SchemeExp, Abs, Addr] = exps match {
    case Nil => ActionPushTraced(condition, FrameWhileCondition(condition, body, ρ))
    case List(exp) => ActionPushTraced(exp, FrameWhileBody(condition, body, Nil, ρ))
    case exp :: rest => ActionPushTraced(exp, FrameWhileBody(condition, body, rest, ρ))
  }

  def stepKont(v: Abs, frame: Frame, σ: Store[Addr, Abs], t: Time) : Set[InterpreterReturn] = frame match {
    case FrameAnd(Nil, ρ) =>
      conditional(v, List(ActionReachedValueTraced(v), actionPopKont), List(ActionReachedValueTraced(abs.inject(false)), actionPopKont))
    case FrameAnd(e :: rest, ρ) =>
      conditional(v, List(ActionPushTraced(e, FrameAnd(rest, ρ))), List(ActionReachedValueTraced(abs.inject(false)), actionPopKont))
    case FrameBegin(body) => Set(interpreterReturn(actionRestoreEnv :: evalBody(body, FrameBegin)))
    case FrameCase(clauses, default) => {
      val fromClauses = clauses.flatMap({ case (values, body) =>
        if (values.exists(v2 => evalValue(v2.value) match {
          case None => false
          case Some(v2) => abs.subsumes(v, v2)
        }))
        /* TODO: precision could be improved by restricting v to v2 */
          Set[InterpreterReturn](interpreterReturn(List[Action[SchemeExp, Abs, Addr]](actionRestoreEnv) ++ evalBody(body, FrameBegin)))
        else
          Set[InterpreterReturn]()
      })
      /* TODO: precision could be improved in cases where we know that default is not
       * reachable */
      fromClauses.toSet + interpreterReturn(actionRestoreEnv :: evalBody(default, FrameBegin))
    }
    case FrameCasOld(variable, enew, ρ) =>
      Set(interpreterReturn(List(ActionPushTraced(enew, FrameCasNew(variable, v, ρ)))))
    case FrameCasNew(variable, old, ρ) =>
      ρ.lookup(variable) match {
        case Some(a) => conditional(abs.binaryOp(BinaryOperator.Eq)(σ.lookup(a), old),
          /* Compare and swap succeeds */
          List(ActionSetVarTraced(variable), ActionReachedValueTraced(abs.inject(true)), actionPopKont),
          /* Compare and swap fails */
          List(ActionReachedValueTraced(abs.inject(false)), actionPopKont))
        case None => Set(interpreterReturn(List(ActionErrorTraced(s"Unbound variable: $variable"))))
      }
    case FrameCond(cons, clauses, ρ) =>
      conditional(v, if (cons.isEmpty) { List(ActionReachedValueTraced(v), actionPopKont) } else { evalBody(cons, FrameBegin) },
        clauses match {
          case Nil => List(ActionReachedValueTraced(abs.inject(false)), actionPopKont)
          case (exp, cons2) :: rest => List(ActionPushTraced(exp, FrameCond(cons2, rest, ρ)))
        })
    case FrameDefine(name, ρ) => throw new Exception(s"TODO: define not handled (no global environment)")
    case FrameHalt => Set()
    case FrameFunBody(body, Nil) => Set(InterpreterReturn(List(actionRestoreEnv, actionPopKont), TracingSignalEnd(body, RestartTraceEnded())))
    case FrameFunBody(body, toeval) => Set(interpreterReturn(actionRestoreEnv :: evalBody(toeval, FrameFunBody(body, _))))
    case FrameFuncallOperands(f, fexp, exp, args, toeval, ρ) => funcallArgs(f, fexp, (exp, v) :: args, toeval, ρ, σ, t)
    case FrameFuncallOperator(fexp, args, ρ) => funcallArgs(v, fexp, args, ρ, σ, t)
    case FrameIf(cons, alt) =>
      conditionalIf(v, List(ActionEvalTraced(cons)), RestartGuardFailed(cons), List(ActionEvalTraced(alt)), RestartGuardFailed(alt))
    case FrameLet(name, bindings, Nil, body) => {
      val variables = name :: bindings.map(_._1)
      Set(interpreterReturn(actionRestoreEnv :: actionPushVal :: ActionDefineVarsTraced[SchemeExp, Abs, Addr](variables) :: evalBody(body, FrameBegin)))
    }
    case FrameLet(name, bindings, (variable, e) :: toeval, body) =>
      Set(interpreterReturn(List(actionRestoreEnv, actionPushVal, actionSaveEnv, ActionPushTraced(e, FrameLet(variable, (name, v) :: bindings, toeval, body)))))
    case FrameLetrec(a, Nil, body) =>
      val actions : Trace = List(actionRestoreEnv, ActionSetVarTraced[SchemeExp, Abs, Addr](a)) ++ evalBody(body, FrameBegin)
      Set(InterpreterReturn(actions, new TracingSignalFalse))
    case FrameLetrec(var1, (var2, exp) :: rest, body) =>
      val actions : Trace = List(actionRestoreEnv, ActionSetVarTraced(var1), ActionPushTraced(exp, FrameLetrec(var2, rest, body)), actionSaveEnv)
      Set(InterpreterReturn(actions, new TracingSignalFalse))
    case FrameLetStar(name, bindings, body) => {
      val actions = List(actionRestoreEnv, ActionExtendEnvTraced[SchemeExp, Abs, Addr](name))
      bindings match {
        case Nil => Set(interpreterReturn(actions ++ evalBody(body, FrameBegin)))
        case (variable, exp) :: rest => Set(InterpreterReturn(actions ++ List(actionSaveEnv, ActionPushTraced(exp, FrameLetStar(variable, rest, body))), new TracingSignalFalse))
      }
    }
    case FrameOr(Nil, ρ) =>
      conditional(v, List(ActionReachedValueTraced(v), actionPopKont), List(ActionReachedValueTraced(abs.inject(false)), actionPopKont))
    case FrameOr(e :: rest, ρ) =>
      conditional(v, List(ActionReachedValueTraced(v), actionPopKont), List(ActionPushTraced(e, FrameOr(rest, ρ))))
    case FrameSet(name, ρ) => ρ.lookup(name) match {
      case Some(a) => Set(InterpreterReturn(List(ActionSetVarTraced(name), ActionReachedValueTraced(abs.inject(false), Set[Addr](), Set[Addr](a)), actionPopKont), new TracingSignalFalse)) /* writes on a */
      case None => Set(interpreterReturn(List(ActionErrorTraced(s"Unbound variable: $name"))))
    }
    case FrameWhileBody(condition, body, exps, ρ) => Set(interpreterReturn(List(evalWhileBody(condition, body, exps, ρ, σ))))
    case FrameWhileCondition(condition, body, ρ) => {
      (if (abs.isTrue(v)) Set[InterpreterReturn](interpreterReturnStart(evalWhileBody(condition, body, body, ρ, σ), body)) else Set[InterpreterReturn]()) ++
        (if (abs.isFalse(v)) Set[InterpreterReturn](interpreterReturn(List(ActionReachedValueTraced(abs.inject(false)), actionPopKont))) else Set[InterpreterReturn]())
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
    case ActionReachedValueTraced(v, read2, write) => ActionReachedValueTraced(v, read ++ read2, write)
    case ActionPushTraced(e, frame, read2, write) => ActionPushTraced(e, frame, read ++ read2, write)
    case ActionEvalTraced(e, read2, write) => ActionEvalTraced(e, read ++ read2, write)
    case ActionStepInTraced(fexp, clo, e, args, argsv, n, frame, read2, write) => ActionStepInTraced(fexp, clo, e, args, argsv, n, frame, read ++ read2, write)
    case ActionErrorTraced(err) => ActionErrorTraced(err)
  }
}

