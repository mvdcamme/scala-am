/**
  * Basic Traced Scheme semantics, without any optimization
  */
abstract class BaseSchemeSemanticsTraced[Abs: IsSchemeLattice, Addr: Address,
Time: Timestamp](override val primitives: SchemePrimitives[Addr, Abs])
    extends BaseSemanticsTraced[SchemeExp, Abs, Addr, Time](primitives)
    with ConvertableSemantics[SchemeExp, Abs, Addr, Time] {

  def sabs = implicitly[IsSchemeLattice[Abs]]

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
  case class FrameCaseT(clauses: List[(List[SchemeValue], List[SchemeExp])],
                        default: List[SchemeExp])
      extends SchemeFrameT
  case class FrameDefineT(variable: String) extends SchemeFrameT
  case class FrameFunBodyT(body: List[SchemeExp], rest: List[SchemeExp])
      extends SchemeFrameT
  case class FrameFuncallOperandsT(f: Abs,
                                   fexp: SchemeExp,
                                   cur: SchemeExp,
                                   args: List[SchemeExp],
                                   toeval: List[SchemeExp])
      extends SchemeFrameT
  case class FrameFuncallOperatorT(fexp: SchemeExp, args: List[SchemeExp])
      extends SchemeFrameT
  case class FrameIfT(cons: SchemeExp, alt: SchemeExp) extends SchemeFrameT
  case class FrameLetT(variable: String,
                       bindings: List[String],
                       toeval: List[(String, SchemeExp)],
                       body: List[SchemeExp])
      extends SchemeFrameT
  case class FrameLetrecT(variable: String,
                          bindings: List[(String, SchemeExp)],
                          body: List[SchemeExp])
      extends SchemeFrameT
  case class FrameLetStarT(variable: String,
                           bindings: List[(String, SchemeExp)],
                           body: List[SchemeExp])
      extends SchemeFrameT
  case class FrameSetT(variable: String) extends SchemeFrameT
  case class FrameWhileBodyT(condition: SchemeExp,
                             body: List[SchemeExp],
                             exps: List[SchemeExp])
      extends SchemeFrameT
  case class FrameWhileConditionT(condition: SchemeExp, body: List[SchemeExp])
      extends SchemeFrameT
  object FrameHaltT extends SchemeFrameT {
    override def toString() = "FHalt"
  }

  def convertAbsInFrame[OtherAbs: IsConvertableLattice](
      frame: Frame,
      concBaseSem: BaseSchemeSemantics[Abs,
                                       HybridAddress.A,
                                       HybridTimestamp.T],
      abstSem: BaseSchemeSemantics[OtherAbs,
                                   HybridAddress.A,
                                   HybridTimestamp.T],
      convertValue: (Abs) => (OtherAbs)): Frame = frame match {
    case frame: FrameFuncallOperands[Abs, HybridAddress.A, HybridTimestamp.T] =>
      FrameFuncallOperands[OtherAbs, HybridAddress.A, HybridTimestamp.T](convertValue(frame.f),
                                                                         frame.fexp,
                                                                         frame.cur,
                                                                         frame.args.map((tuple) => (tuple._1, convertValue(tuple._2))),
                                                                         frame.toeval,
                                                                         frame.env)
    case frame: FrameLet[Abs, HybridAddress.A, HybridTimestamp.T] =>
      FrameLet[OtherAbs, HybridAddress.A, HybridTimestamp.T](frame.variable,
                                                             frame.bindings.map((tuple) => (tuple._1, convertValue(tuple._2))),
                                                             frame.toeval,
                                                             frame.body,
                                                             frame.env,
                                                             frame.optInputVariable)
    case _ => frame
  }

  private def popEnvFromVStack[OtherAbs](
      generateFrameFun: Environment[Addr] => Frame,
      vStack: List[Storable[OtherAbs, Addr]])
    : (Option[Frame], List[Storable[OtherAbs, Addr]], Environment[Addr]) = {
    val ρ = vStack.head.getEnv
    val frame = generateFrameFun(ρ)
    (Some(frame), vStack.tail, ρ)
  }

  private def popEnvAndValuesFromVStack[OtherAbs](
      generateFrameFun: (Environment[Addr], List[OtherAbs]) => Frame,
      n: Integer,
      vStack: List[Storable[OtherAbs, Addr]])
    : (Option[Frame], List[Storable[OtherAbs, Addr]], Environment[Addr]) = {
    val ρ = vStack.head.getEnv
    val remainingVStack = vStack.tail
    val (values, remainingVStack2) = remainingVStack.splitAt(n)
    val frame = generateFrameFun(ρ, values.map(_.getVal))
    (Some(frame), remainingVStack2, ρ)
  }

  /* AAM does not allocate a new FrameBegin if the function's body consists of only one expression,
   * so no new frame should be allocated now either. But we still have to remove the environment
   * from the value stack. */
  private def popEmptyFrameBeginFromVStack[OtherAbs](
      vStack: List[Storable[OtherAbs, Addr]])
    : (Option[Frame], List[Storable[OtherAbs, Addr]], Environment[Addr]) = {
    val ρ = vStack.head.getEnv
    (None, vStack.tail, ρ)
  }

  /*
   * Converts a SchemeSemanticsFrame to a Frame of the abstract semantics.
   * SchemeSemanticsFrames do not save all required fields, such as environments or already-evaluated values,
   * in the frame itself, like the frames employed by the abstract semantics, but rather save those on the vstack.
   *
   * When converting a frame, these values and environments must therefore be popped from the vstack, in the process
   * completely emptying this stack.
   *
   * To understand which values or environments should be popped from the vstack and inserted into the new, converted
   * frame, take note of which values and environments are saved on the vstack when the original
   * SchemeSemanticsFrame was generated.
   */
  def convertToAbsSemanticsFrame(frame: Frame,
                                 ρ: Environment[Addr],
                                 vStack: List[Storable[Abs, Addr]],
                                 absSem: BaseSchemeSemantics[Abs, Addr, Time])
    : (Option[Frame], List[Storable[Abs, Addr]], Environment[Addr]) =
    frame match {
      case FrameBeginT(rest) =>
        rest match {
          case Nil =>
            popEmptyFrameBeginFromVStack(vStack)
          case _ =>
            popEnvFromVStack(FrameBegin(rest, _), vStack)
        }
      case FrameCaseT(clauses, default) =>
        popEnvFromVStack(FrameCase(clauses, default, _), vStack)
      case FrameDefineT(variable) =>
        /* A FrameDefineT is not handled by these semantics:
         * neither the vStack nor the environment therefore have to be updated */
        (Some(FrameDefine(variable, ρ, None)), vStack, ρ)
      case FrameFunBodyT(body, toeval) =>
        if (toeval.isEmpty) {
          popEmptyFrameBeginFromVStack(vStack)
        } else {
          popEnvFromVStack(FrameBegin(toeval, _), vStack)
        }
      case FrameFuncallOperandsT(f, fexp, cur, args, toeval) =>
        val n = args.length + 1 /* We have to add 1 because the operator has also been pushed onto the vstack */
        val generateFrameFun = (ρ: Environment[Addr], values: List[Abs]) => {
          val newArgs = args.zip(values)
          FrameFuncallOperands(f, fexp, cur, newArgs, toeval, ρ)
        }
        popEnvAndValuesFromVStack(generateFrameFun, n, vStack)
      case FrameFuncallOperatorT(fexp, args) =>
        popEnvFromVStack(FrameFuncallOperator(fexp, args, _), vStack)
      case FrameIfT(cons, alt) =>
        popEnvFromVStack(FrameIf(cons, alt, _, ???), vStack)
      case FrameLetT(variable, bindings, toeval, body) =>
        /* When pushing a FrameLetT continuation on the continuation stack, we possibly push a value on the value stack,
         * in case we have just evaluated an expression for the let-bindings, and we always push an environment.
         * The stack should therefore have an environment at the top, followed by n values where n is the number of bindings
         * already evaluated (which equals bindings.length). */
        val n = bindings.length
        val generateFrameFun = (ρ: Environment[Addr], values: List[Abs]) => {
          val newBindings = bindings.zip(values)
          FrameLet(variable, newBindings, toeval, body, ρ, None)
        }
        popEnvAndValuesFromVStack(generateFrameFun, n, vStack)
      case FrameLetrecT(variable, bindings, body) =>
        val addr = ρ.lookup(variable).get
        val updatedBindings = bindings.map({
          case (variable, exp) => (ρ.lookup(variable).get, variable, exp)
        })
        popEnvFromVStack(FrameLetrec(addr, updatedBindings, body, _, None),
                         vStack)
      case FrameLetStarT(variable, bindings, body) =>
        /* When evaluating a FrameLetStarT continuation, we also push the value v on the stack (similar to the case for
         * FrameLetT, but this is immediately followed by an ActionExtendEnv which pops this value back from the stack.
         * There are therefore never any values for the let* bindings on the value stack. */
        popEnvFromVStack(FrameLetStar(variable, bindings, body, _, None),
                         vStack)
      case FrameSetT(variable) =>
        popEnvFromVStack(FrameSet(variable, _, None), vStack)
    }

  /**
    * @param frameGen A function that generates the next frame to push, given the non-evaluated expressions of the body
    */
  protected def evalBody(body: List[SchemeExp],
                         frameGen: List[SchemeExp] => Frame)
    : List[ActionTrace[SchemeExp, Abs, Addr]] = body match {
    case Nil => List(actionPopKont)
    case exp :: rest =>
      List(actionSaveEnv, ActionEvalPushT(exp, frameGen(rest)))
  }

  def conditional(v: Abs,
                  t: List[ActionTrace[SchemeExp, Abs, Addr]],
                  f: List[ActionTrace[SchemeExp, Abs, Addr]])
    : Set[InterpreterStep[SchemeExp, Abs, Addr]] =
    (if (sabs.isTrue(v))
       Set[InterpreterStep[SchemeExp, Abs, Addr]](
         InterpreterStep[SchemeExp, Abs, Addr](t, new SignalFalse))
     else Set[InterpreterStep[SchemeExp, Abs, Addr]]()) ++
      (if (sabs.isFalse(v))
         Set[InterpreterStep[SchemeExp, Abs, Addr]](
           InterpreterStep[SchemeExp, Abs, Addr](f, new SignalFalse))
       else Set[InterpreterStep[SchemeExp, Abs, Addr]]())

  /*
   * TODO: Debugging: run tests only using if-expressions. Remove function ASAP and use function "conditional" instead!
   */
  def conditionalIf(v: Abs,
                    t: List[ActionTrace[SchemeExp, Abs, Addr]],
                    tRestart: RestartPoint[SchemeExp, Abs, Addr],
                    f: List[ActionTrace[SchemeExp, Abs, Addr]],
                    fRestart: RestartPoint[SchemeExp, Abs, Addr])
    : Set[InterpreterStep[SchemeExp, Abs, Addr]] =
    (if (sabs.isTrue(v)) {
       Set[InterpreterStep[SchemeExp, Abs, Addr]](
         InterpreterStep[SchemeExp, Abs, Addr](
           actionRestoreEnv :: ActionGuardTrueT[SchemeExp, Abs, Addr](
             fRestart,
             GuardIDCounter.incCounter()) :: t,
           SignalFalse[SchemeExp, Abs, Addr]()))
     } else Set[InterpreterStep[SchemeExp, Abs, Addr]]()) ++
      (if (sabs.isFalse(v)) {
         Set[InterpreterStep[SchemeExp, Abs, Addr]](
           InterpreterStep[SchemeExp, Abs, Addr](
             actionRestoreEnv :: ActionGuardFalseT[SchemeExp, Abs, Addr](
               tRestart,
               GuardIDCounter.incCounter()) :: f,
             SignalFalse[SchemeExp, Abs, Addr]()))
       } else Set[InterpreterStep[SchemeExp, Abs, Addr]]())

  /**
    * @param actualPars are the Scheme-expressions of the operands of the call
    */
  def evalCall(function: Abs,
               fexp: SchemeExp,
               actualPars: List[SchemeExp],
               σ: Store[Addr, Abs],
               t: Time): Set[InterpreterStep[SchemeExp, Abs, Addr]] = {

    /*
     * The number of values to pop: the number of operands + the operator.
     */
    val valsToPop = actualPars.length + 1

    val commonActions: List[ActionTrace[SchemeExp, Abs, Addr]] =
      List(actionRestoreEnv, actionPushVal)

    val fromClo: Set[InterpreterStep[SchemeExp, Abs, Addr]] = sabs
      .getClosures[SchemeExp, Addr](function)
      .map({
        case (SchemeLambda(formalPars, body, _), ρ1) =>
          val stepInAction = ActionStepInT(fexp,
                                           body.head,
                                           formalPars,
                                           actualPars,
                                           valsToPop,
                                           FrameFunBodyT(body, Nil))
          val rp = RestartGuardDifferentClosure(stepInAction)
          val guard = ActionGuardSameClosure[SchemeExp, Abs, Addr](
            function,
            rp,
            GuardIDCounter.incCounter())
          val allActions = commonActions :+ guard :+ stepInAction :+ actionEndClosureCall
          InterpreterStep(allActions, new SignalStartLoop(body))
        case (λ, _) =>
          interpreterStep(
            List(ActionErrorT[SchemeExp, Abs, Addr](
              TypeError(λ.toString, "operator", "closure", "not a closure"))))
      })

    val fromPrim = sabs.getPrimitives(function).map( (prim) => {
        val primCallAction = ActionPrimCallT(valsToPop, fexp, actualPars, function)
        val allActions = commonActions :+ primCallAction :+ actionEndPrimCall :+ actionPopKont
        InterpreterStep[SchemeExp, Abs, Addr](
          allActions,
          SignalFalse[SchemeExp, Abs, Addr]())
      })
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      val allActions = commonActions :+ ActionErrorT[SchemeExp, Abs, Addr](
          TypeError(function.toString,
                    "operator",
                    "function",
                    "not a function"))
      Set(
        new InterpreterStep[SchemeExp, Abs, Addr](
          allActions,
          SignalFalse[SchemeExp, Abs, Addr]()))
    } else {
      fromClo ++ fromPrim
    }
  }

  protected def evalValue(v: Value): Option[Abs] = v match {
    case ValueFloat(f) => Some(sabs.inject(f))
    case ValueString(s) => Some(sabs.inject(s))
    case ValueInteger(n) => Some(sabs.inject(n))
    case ValueBoolean(b) => Some(sabs.inject(b))
    case _ => None
  }

  protected def funcallArgs(
      f: Abs,
      fexp: SchemeExp,
      args: List[SchemeExp],
      toeval: List[SchemeExp],
      σ: Store[Addr, Abs],
      t: Time): Set[InterpreterStep[SchemeExp, Abs, Addr]] = toeval match {
    case Nil => evalCall(f, fexp, args.reverse, σ, t)
    case e :: rest =>
      val actions = List(
        actionRestoreEnv,
        actionPushVal,
        actionSaveEnv,
        ActionEvalPushT(e, FrameFuncallOperandsT(f, fexp, e, args, rest)))
      Set(InterpreterStep(actions, SignalFalse()))
  }
  protected def funcallArgs(
      f: Abs,
      fexp: SchemeExp,
      args: List[SchemeExp],
      σ: Store[Addr, Abs],
      t: Time): Set[InterpreterStep[SchemeExp, Abs, Addr]] =
    funcallArgs(f, fexp, List(), args, σ, t)

  protected def evalQuoted(
      exp: SExp,
      t: Time): (Abs, List[ActionTrace[SchemeExp, Abs, Addr]]) = exp match {
    case SExpIdentifier(sym, _) => (sabs.injectSymbol(sym), List())
    case SExpPair(car, cdr, _) => {
      val care: SchemeExp = SchemeIdentifier(car.toString, car.pos)
      val cdre: SchemeExp = SchemeIdentifier(cdr.toString, cdr.pos)
      val cara = addr.cell(care, t)
      val (carv, actionsCar) = evalQuoted(car, t)
      val cdra = addr.cell(cdre, t)
      val (cdrv, actionsCdr) = evalQuoted(cdr, t)
      (sabs.cons(cara, cdra),
       (actionsCar :+ ActionExtendStoreT[SchemeExp, Abs, Addr](cara, carv)) ++
         (actionsCdr :+ ActionExtendStoreT[SchemeExp, Abs, Addr](cdra, cdrv)))
    }
    case SExpValue(v, _) =>
      (v match {
        case ValueString(str) => sabs.inject(str)
        case ValueCharacter(c) =>
          throw new Exception("character not yet supported")
        case ValueSymbol(sym) => sabs.injectSymbol(sym) /* shouldn't happen */
        case ValueInteger(n) => sabs.inject(n)
        case ValueFloat(n) => sabs.inject(n)
        case ValueBoolean(b) => sabs.inject(b)
        case ValueNil => sabs.nil
      }, List())
    case SExpQuoted(q, pos) =>
      evalQuoted(SExpPair(SExpIdentifier("quote", pos),
                          SExpPair(q, SExpValue(ValueNil, pos), pos),
                          pos),
                 t)
  }

  def stepEval(e: SchemeExp,
               ρ: Environment[Addr],
               σ: Store[Addr, Abs],
               t: Time): Set[InterpreterStep[SchemeExp, Abs, Addr]] = e match {
    case λ: SchemeLambda =>
      Set(interpreterStep(List(ActionCreateClosureT(λ), actionPopKont)))
    case SchemeBegin(body, _) =>
      Set(interpreterStep(evalBody(body, FrameBeginT)))
    case SchemeCase(key, clauses, default, _) =>
      Set(
        interpreterStep(
          List(actionSaveEnv,
               ActionEvalPushT(key, FrameCaseT(clauses, default)))))
    case SchemeDefineVariable(name, exp, _) =>
      Set(interpreterStep(List(ActionEvalPushT(exp, FrameDefineT(name)))))
    case SchemeDefineFunction(name, args, body, pos) =>
      val λ = SchemeLambda(args, body, pos)
      Set(interpreterStep(List(ActionCreateClosureT(λ), actionPopKont)))
    case SchemeFuncall(f, args, _) =>
      Set(
        interpreterStep(
          List(actionStartFunCall,
               actionSaveEnv,
               ActionEvalPushT(f, FrameFuncallOperatorT(f, args)))))
    case SchemeIdentifier(name, _) =>
      ρ.lookup(name) match {
        case Some(a) =>
          Set(interpreterStep(
            List(ActionLookupVariableT(name), actionPopKont))) /* reads on a */
        case None =>
          Set(interpreterStep(List(ActionErrorT(UnboundVariable(name)))))
      }
    case SchemeIf(cond, cons, alt, _) =>
      Set(
        interpreterStep(
          List(actionSaveEnv, ActionEvalPushT(cond, FrameIfT(cons, alt)))))
    case SchemeLet(Nil, body, _) =>
      Set(interpreterStep(evalBody(body, FrameBeginT)))
    case SchemeLet((v, exp) :: bindings, body, _) =>
      Set(
        interpreterStep(
          List(actionSaveEnv,
               ActionEvalPushT(exp, FrameLetT(v, List(), bindings, body)))))
    case SchemeLetrec(Nil, body, _) =>
      Set(interpreterStep(evalBody(body, FrameBeginT)))
    case SchemeLetrec((v, exp) :: bindings, body, _) => {
      val variables = v :: bindings.map(_._1)
      Set(
        interpreterStep(
          List(ActionAllocVarsT(variables),
               actionSaveEnv,
               ActionEvalPushT(
                 exp,
                 FrameLetrecT(variables.head,
                              variables.tail.zip(bindings.map(_._2)),
                              body)))))
    }
    case SchemeLetStar(Nil, body, _) =>
      Set(interpreterStep(evalBody(body, FrameBeginT)))
    case SchemeLetStar((v, exp) :: bindings, body, _) =>
      Set(
        interpreterStep(
          List(actionSaveEnv,
               ActionEvalPushT(exp, FrameLetStarT(v, bindings, body)))))
    case SchemeQuoted(quoted, _) =>
      val (value, actions) = evalQuoted(quoted, t)
      Set(
        interpreterStep(actions :+ ActionReachedValueT[SchemeExp, Abs, Addr](
          value) :+ actionPopKont))
    case SchemeSet(variable, exp, _) =>
      Set(
        interpreterStep(
          List(actionSaveEnv, ActionEvalPushT(exp, FrameSetT(variable)))))
    case SchemeStartAnalysis(_) =>
      Set(
        InterpreterStep(
          List(ActionReachedValueT(sabs.inject(false)), actionPopKont),
          SignalStartAnalysis()))
    case SchemeValue(v, _) =>
      evalValue(v) match {
        case Some(v) =>
          Set(interpreterStep(List(ActionReachedValueT(v), actionPopKont)))
        case None =>
          Set(
            interpreterStep(
              List(ActionErrorT(NotSupported(s"Unhandled value: $v")))))
      }
  }

  protected def evalWhileBody(
      condition: SchemeExp,
      body: List[SchemeExp],
      exps: List[SchemeExp]): List[ActionTrace[SchemeExp, Abs, Addr]] =
    exps match {
      case Nil =>
        List(actionSaveEnv,
             ActionEvalPushT(condition, FrameWhileConditionT(condition, body)))
      case List(exp) =>
        List(actionSaveEnv,
             ActionEvalPushT(exp, FrameWhileBodyT(condition, body, Nil)))
      case exp :: rest =>
        List(actionSaveEnv,
             ActionEvalPushT(exp, FrameWhileBodyT(condition, body, rest)))
    }

  def stepKont(v: Abs,
               frame: Frame,
               σ: Store[Addr, Abs],
               t: Time): Set[InterpreterStep[SchemeExp, Abs, Addr]] =
    frame match {
      case FrameBeginT(Nil) =>
        Set(interpreterStep(List(actionRestoreEnv, actionPopKont)))
      case FrameBeginT(body) =>
        Set(interpreterStep(actionRestoreEnv :: evalBody(body, FrameBeginT)))
      case FrameCaseT(clauses, default) => {
        val fromClauses = clauses.flatMap({
          case (values, body) =>
            if (values.exists(v2 =>
                  evalValue(v2.value) match {
                    case None => false
                    case Some(v2) => abs.subsumes(v, v2)
                }))
              /* TODO: precision could be improved by restricting v to v2 */
              Set[InterpreterStep[SchemeExp, Abs, Addr]](
                interpreterStep(List[ActionTrace[SchemeExp, Abs, Addr]](
                  actionRestoreEnv) ++ evalBody(body, FrameBeginT)))
            else
              Set[InterpreterStep[SchemeExp, Abs, Addr]](
                interpreterStep(List(actionRestoreEnv)))
        })
        /* TODO: precision could be improved in cases where we know that default is not
         * reachable */
        fromClauses.toSet + interpreterStep(
          actionRestoreEnv :: evalBody(default, FrameBeginT))
      }
      case FrameDefineT(name) =>
        throw new Exception(
          s"TODO: define not handled (no global environment)")
      case FrameHaltT => Set()
      case FrameFunBodyT(body, Nil) =>
        Set(
          InterpreterStep(List(actionRestoreEnv, actionPopKont),
                          SignalEndLoop(body, RestartTraceEnded())))
      case FrameFunBodyT(body, toeval) =>
        Set(
          interpreterStep(
            actionRestoreEnv :: evalBody(toeval, FrameFunBodyT(body, _))))
      case FrameFuncallOperandsT(f, fexp, exp, args, toeval) =>
        funcallArgs(f, fexp, exp :: args, toeval, σ, t)
      case FrameFuncallOperatorT(fexp, args) =>
        funcallArgs(v, fexp, args, σ, t)
      case FrameIfT(cons, alt) =>
        conditionalIf(v,
                      List(ActionEvalT(cons)),
                      RestartFromControl(cons),
                      List(ActionEvalT(alt)),
                      RestartFromControl(alt))
      case FrameLetT(name, bindings, Nil, body) => {
        val variables = name :: bindings
        val actions = actionRestoreEnv ::
            actionPushVal ::
              variables.map({ variable =>
              ActionExtendEnvT[SchemeExp, Abs, Addr](variable)
            }) ++
                evalBody(body, FrameBeginT)
        Set(interpreterStep(actions))
      }
      case FrameLetT(name, bindings, (variable, e) :: toeval, body) =>
        Set(
          interpreterStep(
            List(actionRestoreEnv,
                 actionPushVal,
                 actionSaveEnv,
                 ActionEvalPushT(
                   e,
                   FrameLetT(variable, name :: bindings, toeval, body)))))
      case FrameLetrecT(a, Nil, body) =>
        val actions: List[ActionTrace[SchemeExp, Abs, Addr]] = List(
            actionRestoreEnv,
            ActionSetVarT[SchemeExp, Abs, Addr](a)) ++
            evalBody(body, FrameBeginT)
        Set(InterpreterStep(actions, new SignalFalse))
      case FrameLetrecT(var1, (var2, exp) :: rest, body) =>
        val actions: List[ActionTrace[SchemeExp, Abs, Addr]] = List(
          actionRestoreEnv,
          ActionSetVarT(var1),
          ActionEvalPushT(exp, FrameLetrecT(var2, rest, body)),
          actionSaveEnv)
        Set(InterpreterStep(actions, new SignalFalse))
      case FrameLetStarT(name, bindings, body) =>
        val actions: List[ActionTrace[SchemeExp, Abs, Addr]] = List(
          actionRestoreEnv,
          actionPushVal,
          ActionExtendEnvT[SchemeExp, Abs, Addr](name))
        bindings match {
          case Nil =>
            Set(interpreterStep(actions ++ evalBody(body, FrameBeginT)))
          case (variable, exp) :: rest =>
            Set(
              InterpreterStep(
                actions ++ List(
                  actionSaveEnv,
                  ActionEvalPushT(exp, FrameLetStarT(variable, rest, body))),
                new SignalFalse))
        }
      case FrameSetT(name) =>
        Set(
          InterpreterStep(List(actionRestoreEnv,
                               ActionSetVarT(name),
                               ActionReachedValueT(sabs.inject(false),
                                                   Set[Addr](),
                                                   Set[Addr]()),
                               actionPopKont),
                          new SignalFalse)) /* writes on a */
      case FrameWhileBodyT(condition, body, exps) =>
        Set(
          interpreterStep(
            actionRestoreEnv :: evalWhileBody(condition, body, exps)))
      case FrameWhileConditionT(condition, body) =>
        (if (sabs.isTrue(v))
           Set[InterpreterStep[SchemeExp, Abs, Addr]](
             interpreterStepStart(
               actionRestoreEnv :: evalWhileBody(condition, body, body),
               body))
         else Set[InterpreterStep[SchemeExp, Abs, Addr]]()) ++
          (if (sabs.isFalse(v))
             Set[InterpreterStep[SchemeExp, Abs, Addr]](
               interpreterStep(
                 List(actionRestoreEnv,
                      ActionReachedValueT(sabs.inject(false)),
                      actionPopKont)))
           else Set[InterpreterStep[SchemeExp, Abs, Addr]]())
    }

  def parse(program: String): SchemeExp = Scheme.parse(program)

  override def initialBindings = primitives.bindings

  def bindClosureArgs(clo: Abs,
                      argsv: List[(SchemeExp, Abs)],
                      σ: Store[Addr, Abs],
                      t: Time)
    : Set[Either[Int, (Environment[Addr], Store[Addr, Abs], SchemeExp)]] = {
    sabs
      .getClosures[SchemeExp, Addr](clo)
      .map({
        case (SchemeLambda(args, body, pos), ρ1) =>
          if (args.length == argsv.length) {
            val (ρ2, σ2, stateChanges) = bindArgs(args.zip(argsv), ρ1, σ, t)
            val formattedBody = if (body.length == 1) { body.head } else {
              SchemeBegin(body, pos)
            }
            Right((ρ2, σ2, formattedBody))
          } else {
            Left(args.length)
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
class SchemeSemanticsTraced[Abs: IsSchemeLattice, Addr: Address,
Time: Timestamp](primitives: SchemePrimitives[Addr, Abs])
    extends BaseSchemeSemanticsTraced[Abs, Addr, Time](primitives) {

  override def convertAbsInFrame[OtherAbs: IsConvertableLattice](frame: SchemeFrame[Abs, Addr, Time],
    convertValue: (Abs) => OtherAbs,
    convertEnv: (Environment[Addr]) => Environment[Addr],
    abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time])
    : SchemeFrame[OtherAbs, Addr, Time] = ???

  protected def addRead(action: ActionTrace[SchemeExp, Abs, Addr],
                        read: Set[Addr]): ActionTrace[SchemeExp, Abs, Addr] =
    action match {
      case ActionReachedValueT(v, read2, write, storeChanges) =>
        ActionReachedValueT(v, read ++ read2, write, storeChanges)
      case ActionEvalPushT(e, frame, read2, write) =>
        ActionEvalPushT(e, frame, read ++ read2, write)
      case ActionEvalT(e, read2, write) => ActionEvalT(e, read ++ read2, write)
      case ActionStepInT(fexp, e, args, argsv, n, frame, read2, write) =>
        ActionStepInT(fexp, e, args, argsv, n, frame, read ++ read2, write)
      case ActionErrorT(err) => ActionErrorT(err)
    }

}
