/**
  * Basic Scheme semantics, without any optimization
  */
abstract class BaseSchemeSemanticsTraced[Abs : AbstractValue, Addr : Address, Time : Timestamp]
  extends BaseSemanticsTraced[SchemeExp, Abs, Addr, Time] {

  /*
   * Some unparametrized actions.
   * Defined here so that we don't have to give all type parameters each time these actions are used.
   */
  val actionEndPrimCall = ActionEndPrimCallT[SchemeExp, Abs, Addr]()
  val actionEndClosureCall = ActionEndClosureCallT[SchemeExp, Abs, Addr]()
  val actionPopKont = ActionPopKontT[SchemeExp, Abs, Addr]()
  val actionPushVal = ActionPushValT[SchemeExp, Abs, Addr]()
  val actionRestoreEnv = ActionRestoreEnvT[SchemeExp, Abs, Addr]()
  val actionSaveEnv = ActionSaveEnvT[SchemeExp, Abs, Addr]()
  val actionStartFunCall = ActionStartFunCallT[SchemeExp, Abs, Addr]()

  trait SchemeFrameT extends Frame {
    def subsumes(that: Frame) = that.equals(this)
    override def toString = s"${this.getClass.getSimpleName}"
  }
  case class FrameBeginT(rest: List[SchemeExp]) extends SchemeFrameT
  case class FrameCaseT(clauses: List[(List[SchemeValue], List[SchemeExp])], default: List[SchemeExp]) extends SchemeFrameT
  case class FrameCasOldT(variable: String, enew: SchemeExp, ρ: Environment[Addr]) extends SchemeFrameT
  case class FrameCasNewT(variable: String, old: Abs, ρ: Environment[Addr]) extends SchemeFrameT
  case class FrameDefineT(variable: String, ρ: Environment[Addr]) extends SchemeFrameT
  case class FrameFunBodyT(body: List[SchemeExp], rest: List[SchemeExp]) extends SchemeFrameT
  case class FrameFuncallOperandsT(f: Abs, fexp: SchemeExp, cur: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp]) extends SchemeFrameT
  case class FrameFuncallOperatorT(fexp: SchemeExp, args: List[SchemeExp]) extends SchemeFrameT
  case class FrameIfT(cons: SchemeExp, alt: SchemeExp) extends SchemeFrameT
  case class FrameLetT(variable: String, bindings: List[(String, Abs)], toeval: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeFrameT
  case class FrameLetrecT(variable: String, bindings: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeFrameT
  case class FrameLetStarT(variable: String, bindings: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeFrameT
  case class FrameSetT(variable: String) extends SchemeFrameT
  case class FrameWhileBodyT(condition: SchemeExp, body: List[SchemeExp], exps: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrameT
  case class FrameWhileConditionT(condition: SchemeExp, body: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrameT
  object FrameHaltT extends SchemeFrameT {
    override def toString() = "FHalt"
  }

  def convertFrame(convertAddress: Addr => Addr, convertValue: Abs => Abs)(frame: Frame): Frame = frame match {
    case FrameBeginT(rest) => FrameBeginT(rest)
    case FrameCaseT(clauses, default) => FrameCaseT(clauses, default)
    case FrameCasOldT(variable, enew, ρ) => FrameCasOldT(variable, enew, ρ.map(convertAddress))
    case FrameCasNewT(variable, old, ρ) => FrameCasNewT(variable, convertValue(old), ρ.map(convertAddress))
    case FrameDefineT(variable, ρ) => FrameDefineT(variable, ρ.map(convertAddress))
    case FrameFunBodyT(body, toeval) => FrameFunBodyT(body, toeval)
    case FrameFuncallOperandsT(f, fexp, cur, args, toeval) => FrameFuncallOperandsT(convertValue(f), fexp, cur, args.map({ tuple => (tuple._1, convertValue(tuple._2))}), toeval)
    case FrameFuncallOperatorT(fexp, args) => FrameFuncallOperatorT(fexp, args)
    case FrameIfT(cons, alt) => FrameIfT(cons, alt)
    case FrameLetT(variable, bindings, toeval, body) => FrameLetT(variable, bindings.map({ tuple => (tuple._1, convertValue(tuple._2))}), toeval, body)
    case FrameLetrecT(variable, bindings, body) => FrameLetrecT(variable, bindings, body)
    case FrameLetStarT(variable, bindings, body) => FrameLetStarT(variable, bindings, body)
    case FrameSetT(variable) => FrameSetT(variable)
    case FrameWhileBodyT(condition, body, exps, ρ) => FrameWhileBodyT(condition, body, exps, ρ.map(convertAddress))
    case FrameWhileConditionT(condition, body, ρ) => FrameWhileConditionT(condition, body, ρ.map(convertAddress))
  }

  private def popEnvFromStack(generateFrameFun: Environment[Addr] => Frame, vStack: List[Storable[Abs, Addr]]):
    Option[(Frame, List[Storable[Abs, Addr]], Environment[Addr])] = {
    val ρ = vStack.head.getEnv
    Some((generateFrameFun(ρ), vStack.tail, ρ))
  }

  def newConvertFrame(frame: Frame,
                      newSem: Semantics[SchemeExp, Abs, Addr, Time],
                      ρ: Environment[Addr],
                      vStack: List[Storable[Abs, Addr]]): Option[(Frame, List[Storable[Abs, Addr]], Environment[Addr])] = newSem match {
    case newSem : SchemeSemantics[Abs, Addr, Time] => frame match {
      case FrameBeginT(rest) => popEnvFromStack(newSem.FrameBegin(rest, _), vStack)
      case FrameFunBodyT(body, toeval) => popEnvFromStack(newSem.FrameBegin(toeval, _), vStack)
      case FrameFuncallOperandsT(f, fexp, cur, args, toeval) =>
        val ρ = vStack.head.getEnv
        val remainingVStack = vStack.tail
        val n = args.length + 1 /* We have to add 1 because the operator has also been pushed onto the vstack */
        val (argsValues, remainingVStack2) = remainingVStack.splitAt(n)
        val newArgs = args.map(_._1).zip(argsValues.map(_.getVal))
        Some((newSem.FrameFuncallOperands(f.asInstanceOf[Abs], fexp, cur, newArgs, toeval, ρ), remainingVStack2, ρ))
      case FrameFuncallOperatorT(fexp, args) => popEnvFromStack(newSem.FrameFuncallOperator(fexp, args, _), vStack)
      case FrameIfT(cons, alt) => popEnvFromStack(newSem.FrameIf(cons, alt, _), vStack)
      case FrameLetT(variable, bindings, toeval, body) =>
        /* When pushing a FrameLetT continuation on the continuation stack, we possibly push a value on the value stack,
        in case we have just evaluated an expression for the let-bindings, and we always push an environment.
        The stack should therefore have an environment at the top, followed by n values where n is the number of bindings
        already evaluated (which equals bindings.length).
        */
        val ρ = vStack.head.getEnv
        val remainingVStack = vStack.tail
        val n = bindings.length
        val (bindingValues, remainingVStack2) = remainingVStack.splitAt(n)
        val newBindings = bindings.map(_._1).zip(bindingValues.map(_.getVal))
        Some((newSem.FrameLet(variable, newBindings, toeval, body, ρ), remainingVStack2, ρ))
      case FrameLetrecT(variable, bindings, body) =>
        val addr = ρ.lookup(variable).get
        val updatedBindings = bindings.map({ case (variable, exp) => (ρ.lookup(variable).get, exp) })
        popEnvFromStack(newSem.FrameLetrec(addr, updatedBindings, body, _), vStack)
      case FrameLetStarT(variable, bindings, body) =>
        /* When evaluating a FrameLetStarT continuation, we also push the value v on the stack (similar to the case for
         * FrameLetT, but this is immediately followed by an ActionExtendEnv which pops this value back from the stack.
         * There are therefore never any values for the let* bindings on the value stack. */
        popEnvFromStack(newSem.FrameLetStar(variable, bindings, body, _), vStack)
      case FrameSetT(variable) => popEnvFromStack(newSem.FrameSet(variable, _), vStack)
    }
  }

  def newConvertKStore(newSem: Semantics[SchemeExp, Abs, Addr, Time],
                       kontStore: KontStore[KontAddr],
                       ρ: Environment[Addr],
                       a: KontAddr,
                       vStack: List[Storable[Abs, Addr]]): KontStore[KontAddr] = {
    def loop(newKontStore: KontStore[KontAddr], a: KontAddr, vStack: List[Storable[Abs, Addr]], ρ: Environment[Addr]): KontStore[KontAddr] = a match {
      case HaltKontAddress => newKontStore
      case _ =>
        val Kont(frame, next) = kontStore.lookup(a).head
        val someNewFrame = newConvertFrame(frame, newSem, ρ, vStack)
        val (updatedNewKontStore, updatedNewVStack, updatedNewρ) = someNewFrame.fold((newKontStore, vStack, ρ))({
          case (convertedFrame, newVStack, updatedρ) => (newKontStore.extend(a, Kont(convertedFrame, next)), newVStack, updatedρ)
        })
        loop(updatedNewKontStore, next, updatedNewVStack, updatedNewρ)
    }
    loop(new KontStore[KontAddr](), a, vStack, ρ)
  }

  /**
    * @param frameGen A function that generates the next frame to push, given the non-evaluated expressions of the body
    */
  protected def evalBody(body: List[SchemeExp], frameGen: List[SchemeExp] => Frame): List[Action[SchemeExp, Abs, Addr]] = body match {
    case Nil => List(actionPopKont)
    case exp :: rest => List(actionSaveEnv, ActionEvalPushT(exp, frameGen(rest)))
  }

  def conditional(v: Abs, t: List[Action[SchemeExp, Abs, Addr]], f: List[Action[SchemeExp, Abs, Addr]]): Set[Step[SchemeExp, Abs, Addr]] =
    (if (abs.isTrue(v)) Set[Step[SchemeExp, Abs, Addr]](Step[SchemeExp, Abs, Addr](t, new TracingSignalFalse)) else Set[Step[SchemeExp, Abs, Addr]]()) ++
    (if (abs.isFalse(v)) Set[Step[SchemeExp, Abs, Addr]](Step[SchemeExp, Abs, Addr](f, new TracingSignalFalse)) else Set[Step[SchemeExp, Abs, Addr]]())

  /*
   * TODO: Debugging: run tests only using if-expressions. Remove function ASAP and use function "conditional" instead!
   */
  def conditionalIf(v: Abs, t: List[Action[SchemeExp, Abs, Addr]],
                    tRestart: RestartPoint[SchemeExp, Abs, Addr],
                    f: List[Action[SchemeExp, Abs, Addr]],
                    fRestart: RestartPoint[SchemeExp, Abs, Addr]): Set[Step[SchemeExp, Abs, Addr]] =
    (if (abs.isTrue(v)) {
      Set[Step[SchemeExp, Abs, Addr]](Step[SchemeExp, Abs, Addr](actionRestoreEnv :: ActionGuardTrueT[SchemeExp, Abs, Addr](fRestart) :: t, TracingSignalFalse()))
    } else Set[Step[SchemeExp, Abs, Addr]]()) ++
    (if (abs.isFalse(v)) {
      Set[Step[SchemeExp, Abs, Addr]](Step[SchemeExp, Abs, Addr](actionRestoreEnv :: ActionGuardFalseT[SchemeExp, Abs, Addr](tRestart) :: f, TracingSignalFalse()))
    } else Set[Step[SchemeExp, Abs, Addr]]())

  /**
   * @param argsv are the Scheme-expressions of the operands of the call
   */
  def evalCall(function: Abs, fexp: SchemeExp, argsv: List[(SchemeExp, Abs)], σ: Store[Addr, Abs], t: Time): Set[Step[SchemeExp, Abs, Addr]] = {

    /*
     * The number of values to pop: the number of operands + the operator.
     */
    val valsToPop = argsv.length + 1

    val actions: List[Action[SchemeExp, Abs, Addr]] = List(actionRestoreEnv, actionPushVal)

    val fromClo: Set[Step[SchemeExp, Abs, Addr]] = abs.getClosures[SchemeExp, Addr](function).map({
      case (SchemeLambda(args, body), ρ1) =>
        val stepInFrame = ActionStepInT(fexp, body.head, args, argsv.map(_._1), valsToPop, FrameFunBodyT(body, body.tail))
        Step(actions :+
                          ActionGuardSameClosure[SchemeExp, Abs, Addr](function, RestartGuardDifferentClosure(stepInFrame)) :+
                          stepInFrame :+
                          actionEndClosureCall,
                          new TracingSignalStart(body))
      case (λ, _) => interpreterReturn(List(ActionErrorT[SchemeExp, Abs, Addr](s"Incorrect closure with lambda-expression ${λ}")))
    })

    val fromPrim = abs.getPrimitive(function) match {
      case Some(prim) =>
        val primCallAction = ActionPrimCallT(valsToPop, fexp, argsv.map(_._1))
        Set(Step(actions :+
                              ActionGuardSamePrimitive[SchemeExp, Abs, Addr](function, RestartGuardDifferentPrimitive(primCallAction)) :+
                              primCallAction :+
                              actionEndPrimCall :+
                              actionPopKont,
          new TracingSignalFalse[SchemeExp, Abs, Addr]()))
      case None => Set()
    }
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      Set(new Step(actions :+ ActionErrorT[SchemeExp, Abs, Addr](s"Called value is not a function: $function"), new TracingSignalFalse))
    } else {
      fromClo ++ fromPrim
    }
  }

  protected def evalValue(v: Value): Option[Abs] = v match {
    case ValueFloat(f) => Some(abs.inject(f))
    case ValueString(s) => Some(abs.inject(s))
    case ValueInteger(n) => Some(abs.inject(n))
    case ValueBoolean(b) => Some(abs.inject(b))
    case _ => None
  }

  protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], σ: Store[Addr, Abs], t: Time): Set[Step[SchemeExp, Abs, Addr]] = toeval match {
    case Nil => evalCall(f, fexp, args.reverse, σ, t)
    case e :: rest => Set(Step(List(actionRestoreEnv, actionPushVal, actionSaveEnv, ActionEvalPushT(e, FrameFuncallOperandsT(f, fexp, e, args, rest))), new TracingSignalFalse))
  }
  protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[SchemeExp], σ: Store[Addr, Abs], t: Time): Set[Step[SchemeExp, Abs, Addr]] =
    funcallArgs(f, fexp, List(), args, σ, t)

  protected def evalQuoted(exp: SExp, t: Time): (Abs, List[Action[SchemeExp, Abs, Addr]]) = exp match {
    case SExpIdentifier(sym) => (abs.injectSymbol(sym), List())
    case SExpPair(car, cdr) => {
      val care: SchemeExp = SchemeIdentifier(car.toString).setPos(car.pos)
      val cdre: SchemeExp = SchemeIdentifier(cdr.toString).setPos(cdr.pos)
      val cara = addr.cell(care, t)
      val (carv, actionsCar) = evalQuoted(car, t)
      val cdra = addr.cell(cdre, t)
      val (cdrv, actionsCdr) = evalQuoted(cdr, t)
      (abs.cons(cara, cdra), (actionsCar :+ ActionExtendStoreT[SchemeExp, Abs, Addr](cara, carv)) ++
                             (actionsCdr :+ ActionExtendStoreT[SchemeExp, Abs, Addr](cdra, cdrv)))
    }
    case SExpValue(v) => (v match {
      case ValueString(str) => abs.inject(str)
      case ValueCharacter(c) => throw new Exception("character not yet supported")
      case ValueSymbol(sym) => abs.injectSymbol(sym) /* shouldn't happen */
      case ValueInteger(n) => abs.inject(n)
      case ValueFloat(n) => abs.inject(n)
      case ValueBoolean(b) => abs.inject(b)
      case ValueNil() => abs.nil
    }, List())
    case SExpQuoted(q) => evalQuoted(SExpPair(SExpIdentifier("quote"), SExpPair(q, SExpValue(ValueNil()))), t)
  }

  def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs], t: Time): Set[Step[SchemeExp, Abs, Addr]] = e match {
    case λ: SchemeLambda => Set(interpreterReturn(List(ActionCreateClosureT(λ), actionPopKont)))
    case SchemeAcquire(variable) => ρ.lookup(variable) match {
      case Some(a) => {
        val v = σ.lookup(a)
        /* Only performs a step if the lock is possibly unlocked (true is unlocked, false is locked) */
        if (abs.isTrue(v)) Set(Step(List(ActionReachedValueT(abs.inject(false)), ActionSetVarT(variable),
                                                      ActionReachedValueT(abs.inject(true)), actionPopKont),
                                                 new TracingSignalFalse())) else Set()
      }
      case None => Set(interpreterReturn(List(ActionErrorT(s"Unbound variable: $variable"))))
    }
    case SchemeBegin(body) => Set(interpreterReturn(evalBody(body, FrameBeginT)))
    case SchemeCase(key, clauses, default) => Set(interpreterReturn(List(ActionEvalPushT(key, FrameCaseT(clauses, default)))))
    case SchemeCas(variable, eold, enew) => Set(interpreterReturn(List(ActionEvalPushT(eold, FrameCasOldT(variable, enew, ρ)))))
    case SchemeDefineVariable(name, exp) => Set(interpreterReturn(List(ActionEvalPushT(exp, FrameDefineT(name, ρ)))))
    case SchemeDefineFunction(name, args, body) => {
      /*
       * TODO switch to extended environment
       */
      val a = addr.variable(name, t)
      val v = abs.inject[SchemeExp, Addr]((SchemeLambda(args, body), ρ))
      val ρ1 = ρ.extend(name, a)
      val σ1 = σ.extend(a, v)
      Set(interpreterReturn(List(ActionReachedValueT(v), actionPopKont)))
    }
    case SchemeFuncall(f, args) => Set(interpreterReturn(List(actionStartFunCall, actionSaveEnv, ActionEvalPushT(f, FrameFuncallOperatorT(f, args)))))
    case SchemeIdentifier(name) => ρ.lookup(name) match {
      case Some(a) => Set(interpreterReturn(List(ActionLookupVariableT(name), actionPopKont))) /* reads on a */
      case None => Set(interpreterReturn(List(ActionErrorT(s"Unbound variable: $name"))))
    }
    case SchemeIf(cond, cons, alt) => Set(interpreterReturn(List(actionSaveEnv, ActionEvalPushT(cond, FrameIfT(cons, alt)))))
    case SchemeLet(Nil, body) => Set(interpreterReturn(evalBody(body, FrameBeginT)))
    case SchemeLet((v, exp) :: bindings, body) => Set(interpreterReturn(List(actionSaveEnv, ActionEvalPushT(exp, FrameLetT(v, List(), bindings, body)))))
    case SchemeLetrec(Nil, body) => Set(interpreterReturn(evalBody(body, FrameBeginT)))
    case SchemeLetrec((v, exp) :: bindings, body) => {
      val variables = v :: bindings.map(_._1)
      Set(interpreterReturn(List(ActionAllocVarsT(variables), actionSaveEnv,
                            ActionEvalPushT(exp, FrameLetrecT(variables.head, variables.tail.zip(bindings.map(_._2)), body)))))
    }
    case SchemeLetStar(Nil, body) => Set(interpreterReturn(evalBody(body, FrameBeginT)))
    case SchemeLetStar((v, exp) :: bindings, body) => Set(interpreterReturn(List(actionSaveEnv, ActionEvalPushT(exp, FrameLetStarT(v, bindings, body)))))
    case SchemeSet(variable, exp) => Set(interpreterReturn(List(actionSaveEnv, ActionEvalPushT(exp, FrameSetT(variable)))))
    case SchemeQuoted(quoted) =>
      val (value, actions) = evalQuoted(quoted, t)
      Set(interpreterReturn(actions :+ ActionReachedValueT[SchemeExp, Abs, Addr](value) :+ actionPopKont))
    case SchemeRelease(variable) => ρ.lookup(variable) match {
      case Some(a) => Set(Step(List(ActionReachedValueT(abs.inject(true)), ActionSetVarT(variable),
                                                 ActionReachedValueT(abs.inject(true)), actionPopKont), new TracingSignalFalse()))
      case None => Set(interpreterReturn(List(ActionErrorT(s"Unbound variable: $variable"))))
    }
    case SchemeValue(v) => evalValue(v) match {
      case Some(v) => Set(interpreterReturn(List(ActionReachedValueT(v), actionPopKont)))
      case None => Set(interpreterReturn(List(ActionErrorT(s"Unhandled value: $v"))))
    }
    case SchemeWhile(condition, body) => Set(interpreterReturn(List(ActionEvalPushT(condition, FrameWhileConditionT(condition, body, ρ)))))
  }

  protected def evalWhileBody(condition: SchemeExp, body: List[SchemeExp], exps: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs]): Action[SchemeExp, Abs, Addr] = exps match {
    case Nil => ActionEvalPushT(condition, FrameWhileConditionT(condition, body, ρ))
    case List(exp) => ActionEvalPushT(exp, FrameWhileBodyT(condition, body, Nil, ρ))
    case exp :: rest => ActionEvalPushT(exp, FrameWhileBodyT(condition, body, rest, ρ))
  }

  def stepKont(v: Abs, frame: Frame, σ: Store[Addr, Abs], t: Time): Set[Step[SchemeExp, Abs, Addr]] = frame match {
    case FrameBeginT(Nil) => Set(interpreterReturn(List(actionRestoreEnv, actionPopKont)))
    case FrameBeginT(body) => Set(interpreterReturn(actionRestoreEnv :: evalBody(body, FrameBeginT)))
    case FrameCaseT(clauses, default) => {
      val fromClauses = clauses.flatMap({ case (values, body) =>
        if (values.exists(v2 => evalValue(v2.value) match {
          case None => false
          case Some(v2) => abs.subsumes(v, v2)
        }))
        /* TODO: precision could be improved by restricting v to v2 */
          Set[Step[SchemeExp, Abs, Addr]](interpreterReturn(List[Action[SchemeExp, Abs, Addr]](actionRestoreEnv) ++ evalBody(body, FrameBeginT)))
        else
          Set[Step[SchemeExp, Abs, Addr]]()
      })
      /* TODO: precision could be improved in cases where we know that default is not
       * reachable */
      fromClauses.toSet + interpreterReturn(actionRestoreEnv :: evalBody(default, FrameBeginT))
    }
    case FrameCasOldT(variable, enew, ρ) =>
      Set(interpreterReturn(List(ActionEvalPushT(enew, FrameCasNewT(variable, v, ρ)))))
    case FrameCasNewT(variable, old, ρ) =>
      ρ.lookup(variable) match {
        case Some(a) => conditional(abs.binaryOp(BinaryOperator.Eq)(σ.lookup(a), old),
          /* Compare and swap succeeds */
          List(ActionSetVarT(variable), ActionReachedValueT(abs.inject(true)), actionPopKont),
          /* Compare and swap fails */
          List(ActionReachedValueT(abs.inject(false)), actionPopKont))
        case None => Set(interpreterReturn(List(ActionErrorT(s"Unbound variable: $variable"))))
      }
    case FrameDefineT(name, ρ) => throw new Exception(s"TODO: define not handled (no global environment)")
    case FrameHaltT => Set()
    case FrameFunBodyT(body, Nil) => Set(Step(List(actionRestoreEnv, actionPopKont), TracingSignalEnd(body, RestartTraceEnded())))
    case FrameFunBodyT(body, toeval) => Set(interpreterReturn(actionRestoreEnv :: evalBody(toeval, FrameFunBodyT(body, _))))
    case FrameFuncallOperandsT(f, fexp, exp, args, toeval) => funcallArgs(f, fexp, (exp, v) :: args, toeval, σ, t)
    case FrameFuncallOperatorT(fexp, args) => funcallArgs(v, fexp, args, σ, t)
    case FrameIfT(cons, alt) =>
      conditionalIf(v, List(ActionEvalT(cons)) , RestartFromControl(cons), List(ActionEvalT(alt)), RestartFromControl(alt))
    case FrameLetT(name, bindings, Nil, body) => {
      val variables = name :: bindings.map(_._1)
      val actions = actionRestoreEnv ::
                    actionPushVal ::
                    variables.map({ variable => ActionExtendEnvT[SchemeExp, Abs, Addr](variable) }) ++
                    evalBody(body, FrameBeginT)
      Set(interpreterReturn(actions))
    }
    case FrameLetT(name, bindings, (variable, e) :: toeval, body) =>
      Set(interpreterReturn(List(actionRestoreEnv, actionPushVal, actionSaveEnv, ActionEvalPushT(e, FrameLetT(variable, (name, v) :: bindings, toeval, body)))))
    case FrameLetrecT(a, Nil, body) =>
      val actions: List[Action[SchemeExp, Abs, Addr]] = List(actionRestoreEnv,
                                                             ActionSetVarT[SchemeExp, Abs, Addr](a)) ++
                                                        evalBody(body, FrameBeginT)
      Set(Step(actions, new TracingSignalFalse))
    case FrameLetrecT(var1, (var2, exp) :: rest, body) =>
      val actions: List[Action[SchemeExp, Abs, Addr]] = List(actionRestoreEnv,
                                                             ActionSetVarT(var1),
                                                             ActionEvalPushT(exp, FrameLetrecT(var2, rest, body)),
                                                             actionSaveEnv)
      Set(Step(actions, new TracingSignalFalse))
    case FrameLetStarT(name, bindings, body) =>
      val actions: List[Action[SchemeExp, Abs, Addr]] = List(actionRestoreEnv,
                                                             actionPushVal,
                                                             ActionExtendEnvT[SchemeExp, Abs, Addr](name))
      bindings match {
        case Nil => Set(interpreterReturn(actions ++ evalBody(body, FrameBeginT)))
        case (variable, exp) :: rest => Set(Step(actions ++ List(actionSaveEnv, ActionEvalPushT(exp, FrameLetStarT(variable, rest, body))), new TracingSignalFalse))
      }
    case FrameSetT(name) =>
      Set(Step(List(actionRestoreEnv, ActionSetVarT(name), ActionReachedValueT(abs.inject(false), Set[Addr](), Set[Addr]()), actionPopKont), new TracingSignalFalse)) /* writes on a */
    case FrameWhileBodyT(condition, body, exps, ρ) => Set(interpreterReturn(List(evalWhileBody(condition, body, exps, ρ, σ))))
    case FrameWhileConditionT(condition, body, ρ) =>
      (if (abs.isTrue(v)) Set[Step[SchemeExp, Abs, Addr]](interpreterReturnStart(evalWhileBody(condition, body, body, ρ, σ), body)) else Set[Step[SchemeExp, Abs, Addr]]()) ++
        (if (abs.isFalse(v)) Set[Step[SchemeExp, Abs, Addr]](interpreterReturn(List(ActionReachedValueT(abs.inject(false)), actionPopKont))) else Set[Step[SchemeExp, Abs, Addr]]())
  }

  def parse(program: String): SchemeExp = Scheme.parse(program)

  def bindClosureArgs(clo: Abs, argsv: List[(SchemeExp, Abs)], σ: Store[Addr, Abs], t: Time): Set[Option[(Environment[Addr], Store[Addr, Abs], SchemeExp)]] = {
    abs.getClosures[SchemeExp, Addr](clo).map({
      case (SchemeLambda(args, body), ρ1) =>
        if (args.length == argsv.length) {
          val (ρ2, σ2) = bindArgs(args.zip(argsv), ρ1, σ, t)
          val formattedBody = if (body.length == 1) { body.head } else { SchemeBegin(body) }
          Some((ρ2, σ2, formattedBody))
        } else {
          None
        }
    })
  }

  def getClosureBody(frame: Frame): Option[List[SchemeExp]] = frame match {
    case FrameFunBodyT(body, Nil) => Some(body)
    case _ => None
  }
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

  protected def addRead(action: Action[SchemeExp, Abs, Addr], read: Set[Addr]): Action[SchemeExp, Abs, Addr] = action match {
    case ActionReachedValueT(v, read2, write) => ActionReachedValueT(v, read ++ read2, write)
    case ActionEvalPushT(e, frame, read2, write) => ActionEvalPushT(e, frame, read ++ read2, write)
    case ActionEvalT(e, read2, write) => ActionEvalT(e, read ++ read2, write)
    case ActionStepInT(fexp, e, args, argsv, n, frame, read2, write) => ActionStepInT(fexp, e, args, argsv, n, frame, read ++ read2, write)
    case ActionErrorT(err) => ActionErrorT(err)
  }


}

