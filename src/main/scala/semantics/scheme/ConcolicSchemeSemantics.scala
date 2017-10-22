import ConcreteConcreteLattice.ConcreteValue

trait ConcolicSchemeSemantics[Exp, Abs, Addr, Time] {

  def stepEval(e: Exp,
               env: Environment[Addr],
               store: Store[Addr, Abs],
               t: Time): EdgeInformation[Exp, Abs, Addr]

  def stepKont(v: Abs,
               concolicValue: Option[ConcolicExpression],
               frame: Frame,
               store: Store[Addr, Abs],
               t: Time): EdgeInformation[Exp, Abs, Addr]
}

/**
  * Basic Scheme semantics, without any optimization
  */
class ConcolicBaseSchemeSemantics[Addr: Address, Time: Timestamp](
    val primitives: SchemePrimitives[Addr, ConcreteValue])
    extends BaseSemantics[SchemeExp, ConcreteValue, Addr, Time]()(Expression.SchemeExpExpression, ConcreteConcreteLattice.isSchemeLattice, implicitly[Address[Addr]], implicitly[Timestamp[Time]])
    with ConcolicSchemeSemantics[SchemeExp, ConcreteValue, Addr, Time]
    with ConvertableSemantics[SchemeExp, ConcreteValue, Addr, Time] {

  implicit override val abs: JoinLattice[ConcreteValue] = ConcreteConcreteLattice.isSchemeLattice
  implicit def sabs: IsSchemeLattice[ConcreteValue] = ConcreteConcreteLattice.isSchemeLattice

  def convertToAbsSemanticsFrame(frame: Frame,
                                 ρ: Environment[Addr],
                                 vStack: List[Storable[ConcreteValue, Addr]],
                                 absSem: BaseSchemeSemantics[ConcreteValue, Addr, Time]): (Option[Frame], List[Storable[ConcreteValue, Addr]], Environment[Addr]) =
    (Some(frame), vStack, ρ)

  protected def evalBody(
      body: List[SchemeExp],
      env: Environment[Addr],
      store: Store[Addr, ConcreteValue]): EdgeInformation[SchemeExp, ConcreteValue, Addr] = body match {
    case Nil =>
      noEdgeInfos(ActionReachedValue[SchemeExp, ConcreteValue, Addr](sabs.inject(false), None, store),
                  List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](sabs.inject(false))))
    case List(exp) =>
      noEdgeInfos(ActionEval(exp, env, store), ActionEvalR[SchemeExp, ConcreteValue, Addr](exp, env))
    case exp :: rest =>
      addPushActionR(ActionPush(FrameBegin(rest, env), exp, env, store))
  }

  def convertAbsInFrame[OtherAbs: IsConvertableLattice](
      frame: SchemeFrame[ConcreteValue, Addr, Time],
      convertValue: (ConcreteValue) => OtherAbs,
      convertEnv: (Environment[Addr]) => Environment[Addr],
      abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time])
    : SchemeFrame[OtherAbs, Addr, Time] =
    frame.convert(convertValue, convertEnv, abstSem)

  def frameReaches(frame: SchemeFrame[ConcreteValue, Addr, Time],
                   valueReaches: ConcreteValue => Set[Addr],
                   envReaches: Environment[Addr] => Set[Addr],
                   addressReaches: Addr => Set[Addr]): Set[Addr] =
    frame.reaches(valueReaches, envReaches, addressReaches)

  def conditional(v: ConcreteValue,
                  t: => EdgeInformation[SchemeExp, ConcreteValue, Addr],
                  f: => EdgeInformation[SchemeExp, ConcreteValue, Addr]): EdgeInformation[SchemeExp, ConcreteValue, Addr] = {
    def addFilter(x: EdgeInformation[SchemeExp, ConcreteValue, Addr],
                  filter: SemanticsFilterAnnotation): EdgeInformation[SchemeExp, ConcreteValue, Addr] = x match {
      case EdgeInformation(action, actionTs, semanticsFilters) =>
        EdgeInformation(action, actionTs, semanticsFilters + filter)
    }

    if (sabs.isTrue(v)) addFilter(t, ThenBranchTaken) else addFilter(f, ElseBranchTaken)
  }

  protected def addEvalActionT(action: ActionEval[SchemeExp, ConcreteValue, Addr]): EdgeInformation[SchemeExp, ConcreteValue, Addr] =
    noEdgeInfos(action, ActionEvalR[SchemeExp, ConcreteValue, Addr](action.e, action.env))

  protected def addPushActionR(action: ActionPush[SchemeExp, ConcreteValue, Addr]): EdgeInformation[SchemeExp, ConcreteValue, Addr] =
    noEdgeInfos(action, ActionEvalPushR[SchemeExp, ConcreteValue, Addr](action.e, action.env, action.frame))

  protected def addPushDataActionT(currentValue: ConcreteValue,
                                   currentFrame: Frame,
                                   frameGenerator: FrameGenerator[ConcreteValue],
                                   actionGenerator: Frame => ActionPush[SchemeExp, ConcreteValue, Addr]): EdgeInformation[SchemeExp, ConcreteValue, Addr] = {
    val newFrame = frameGenerator(currentValue, currentFrame)
    val currentAction = actionGenerator(newFrame)
    noEdgeInfos(currentAction, ActionEvalPushDataR(currentAction.e, currentAction.env, frameGenerator))
  }

  def evalCall(function: ConcreteValue,
               fexp: SchemeExp,
               argsv: List[(SchemeExp, ConcreteValue, Option[ConcolicExpression])],
               store: Store[Addr, ConcreteValue],
               t: Time): EdgeInformation[SchemeExp, ConcreteValue, Addr] = {
    val fromClo: Set[EdgeInformation[SchemeExp, ConcreteValue, Addr]] = sabs
      .getClosures[SchemeExp, Addr](function)
      .map({
        case (lambda@(SchemeLambda(args, body, pos)), env1) =>
          val cloCall = ActionClosureCallR[SchemeExp, ConcreteValue, Addr](fexp, lambda, env1)
          if (args.length == argsv.length) {
            val argsZipped: List[(String, (SchemeExp, ConcreteValue))] = args.zip(argsv)
            // Create a StatementConstraint for each parameter
            val updatedArgsZipped: List[(String, (SchemeExp, ConcreteValue))] = argsZipped.map({
              case (varName, (exp, value)) =>
                val optInputVariable = SemanticsConcolicHelper.handleDefine(varName, exp)
                val concolicValue = optInputVariable match {
                  case Some(inputVariable) =>
                    lookupInputVariable(inputVariable, value)
                  case None =>
                    value
                }
                (varName, (exp, value))
            })
            bindArgs(updatedArgsZipped, env1, store, t) match {
              case (env2, store, boundAddresses) =>

                val defAddr = ActionDefineAddressesPopR[SchemeExp, ConcreteValue, Addr](boundAddresses.map(_._1))
                val timeTick = ActionTimeTickExpR[SchemeExp, ConcreteValue, Addr](fexp)
                val makeActionRs = (edgeAnnotation: ActionReplay[SchemeExp, ConcreteValue, Addr]) =>
                  List(defAddr, edgeAnnotation, timeTick, cloCall)
                if (body.length == 1) {
                  val action = ActionStepIn[SchemeExp, ConcreteValue, Addr](fexp, (SchemeLambda(args, body, pos), env1), body.head, env2, store, argsv)
                  EdgeInformation(action, makeActionRs(ActionEvalR[SchemeExp, ConcreteValue, Addr](body.head, env2)), Set())
                }
                else {
                  val action = ActionStepIn[SchemeExp, ConcreteValue, Addr](fexp, (SchemeLambda(args, body, pos), env1), SchemeBegin(body, pos), env2, store, argsv)
                  EdgeInformation(action, makeActionRs(ActionEvalR[SchemeExp, ConcreteValue, Addr](SchemeBegin(body, pos),
                    env2)), Set())
                }
            }
          } else {
            val error = ArityError(fexp.toString, args.length, argsv.length)
            val actionError = ActionErrorT[SchemeExp, ConcreteValue, Addr](error)
            EdgeInformation(ActionError[SchemeExp, ConcreteValue, Addr](error), List(actionError, cloCall), Set())
          }
        case (lambda, env) =>
          val cloCall = ActionClosureCallR[SchemeExp, ConcreteValue, Addr](fexp, lambda, env)
          val error = TypeError(lambda.toString, "operator", "closure", "not a closure")
          val actionError = ActionErrorT[SchemeExp, ConcreteValue, Addr](error)
          noEdgeInfos(ActionError[SchemeExp, ConcreteValue, Addr](error), List(actionError, cloCall))
      })
    val fromPrim = sabs.getPrimitives[Addr, ConcreteValue](function).flatMap( (prim) => {
      val n = argsv.size + 1 // Number of values to pop: all arguments + the operator
      val applyPrim = ActionPrimCallT[SchemeExp, ConcreteValue, Addr](n, fexp, argsv.map(_._1), sabs.inject(prim))
      prim.call(fexp, argsv.map( (tuple) => (tuple._1, tuple._2)), store, t).collect[EdgeInformation[SchemeExp, ConcreteValue, Addr]]({
        case (res, store2, effects) =>
          val symbolicValue = if (argsv.map(_._3).forall(_.isDefined)) {
            // symbolicCall only takes place if all concolic argument expressions are defined
            prim.symbolicCall(argsv.map(_._2), argsv.map(_._3.get))
          } else {
            None
          }
          val action = ActionReachedValue[SchemeExp, ConcreteValue, Addr](res, symbolicValue, store2, effects)
          Set(EdgeInformation[SchemeExp, ConcreteValue, Addr](action, List(applyPrim), Set()))
      },
        err => {
          val actionError = ActionErrorT[SchemeExp, ConcreteValue, Addr](err)
          Set(EdgeInformation[SchemeExp, ConcreteValue, Addr](ActionError[SchemeExp, ConcreteValue, Addr](err), List(actionError), Set()))
        })
    })
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      simpleAction(ActionError[SchemeExp, ConcreteValue, Addr](TypeError(function.toString, "operator", "function", "not a function")))
    } else {
      (fromClo ++ fromPrim).head
    }
  }

  protected def evalValue(v: Value): Option[ConcreteValue] = v match {
    case ValueString(s) => Some(sabs.inject(s))
    case ValueInteger(n) => Some(sabs.inject(n))
    case ValueFloat(n) => Some(sabs.inject(n))
    case ValueBoolean(b) => Some(sabs.inject(b))
    case _ => None
  }

  protected def funcallArgs(f: ConcreteValue,
                            currentFrame: Frame,
                            fexp: SchemeExp,
                            args: List[(SchemeExp, ConcreteValue)],
                            toeval: List[SchemeExp],
                            env: Environment[Addr],
                            store: Store[Addr, ConcreteValue],
                            t: Time,
                            currentValue: ConcreteValue,
                            frameGeneratorGenerator: (SchemeExp, List[SchemeExp]) => FrameGenerator[ConcreteValue]): EdgeInformation[SchemeExp, ConcreteValue, Addr] =
    toeval match {
      case Nil =>
        evalCall(f, fexp, args.reverse, store, t)
      case e :: rest =>
        val frameGenerator = frameGeneratorGenerator(e, rest)
        val actionGenerator = (frame: Frame) => ActionPush(frame, e, env, store)
        addPushDataActionT(currentValue, currentFrame, frameGenerator, actionGenerator)
    }

  case class PlaceOperatorFuncallOperandsFrameGenerator(fexp: SchemeExp,
                                                        e: SchemeExp,
                                                        args: List[(SchemeExp, ConcreteValue, Option[ConcolicExpression])],
                                                        rest: List[SchemeExp],
                                                        env: Environment[Addr])
    extends FrameGenerator[ConcreteValue] {
    def apply(value: ConcreteValue, frame: Frame): Frame = {
      FrameConcolicFuncallOperands(value, fexp, e, args, rest, env)
    }
  }

  case class LetFrameGenerator(variable: String,
                               frameVariable: String,
                               bindings: List[(String, ConcreteValue)],
                               toeval: List[(String, SchemeExp)],
                               body: List[SchemeExp],
                               env: Environment[Addr],
                               optInputVariable: Option[String])
    extends FrameGenerator[ConcreteValue] {
    def apply(value: ConcreteValue, other: Frame): Frame = {
      FrameLet(variable, (frameVariable, value) :: other.asInstanceOf[FrameLet[ConcreteValue, Addr, Time]].bindings, toeval, body, env, optInputVariable)
    }
  }

  case class PlaceOperandFuncallOperandsFrameGenerator(f: ConcreteValue,
                                                       fexp: SchemeExp,
                                                       e: SchemeExp,
                                                       cur: SchemeExp,
                                                       args: List[(SchemeExp, ConcreteValue, Option[ConcolicExpression])],
                                                       rest: List[SchemeExp],
                                                       env: Environment[Addr])
    extends FrameGenerator[ConcreteValue] {
    def apply(value: ConcreteValue, other: Frame): Frame = {
      FrameConcolicFuncallOperands(f, fexp, e, (cur, value) :: other.asInstanceOf[FrameConcolicFuncallOperands[ConcreteValue, Addr, Time]].args, rest, env)
    }
  }

  /*
   * To be called after popping a FrameFuncallOperator continuation.
   */
  protected def funcallArgs(f: ConcreteValue,
                            currentFrame: Frame,
                            fexp: SchemeExp,
                            args: List[SchemeExp],
                            env: Environment[Addr],
                            store: Store[Addr, ConcreteValue],
                            t: Time): EdgeInformation[SchemeExp, ConcreteValue, Addr] = {
    /*
     * As this function is called after popping a FrameFuncallOperator, the value that has just been
     * evaluated equals the operator-value.
     */
    val currentValue = f
    args match {
      case Nil =>
        evalCall(f, fexp, List(), store, t)
      case e :: rest =>
        val frameGenerator = PlaceOperatorFuncallOperandsFrameGenerator(fexp, e, Nil, rest, env)
        val actionGenerator = (frame: Frame) => ActionPush(frame, e, env, store)
        addPushDataActionT(currentValue, currentFrame, frameGenerator, actionGenerator)
    }
  }

  protected def evalQuoted(exp: SExp,
                           store: Store[Addr, ConcreteValue],
                           t: Time): (ConcreteValue, Store[Addr, ConcreteValue], List[StoreChangeSemantics[ConcreteValue, Addr]]) = exp
  match {
    case SExpIdentifier(sym, _) => (sabs.injectSymbol(sym), store, Nil)
    case SExpPair(car, cdr, _) => {
      val care: SchemeExp = SchemeIdentifier(car.toString, car.pos)
      val cdre: SchemeExp = SchemeIdentifier(cdr.toString, cdr.pos)
      val cara = addr.cell(care, t)
      val (carv, store2, stateChangesCar) = evalQuoted(car, store, t)
      val cdra = addr.cell(cdre, t)
      val (cdrv, store3, stateChangesCdr) = evalQuoted(cdr, store2, t)
      (sabs.cons(cara, cdra), store3.extend(cara, carv).extend(cdra, cdrv),
       List(StoreExtendSemantics[ConcreteValue, Addr](cara, carv), StoreExtendSemantics[ConcreteValue, Addr](cdra, cdrv)) ++
         stateChangesCar ++
         stateChangesCdr)
    }
    case SExpValue(v, _) =>
      (v match {
        case ValueString(str) => sabs.inject(str)
        case ValueCharacter(c) => sabs.inject(c)
        case ValueSymbol(sym) => sabs.injectSymbol(sym) /* shouldn't happen */
        case ValueInteger(n) => sabs.inject(n)
        case ValueFloat(n) => sabs.inject(n)
        case ValueBoolean(b) => sabs.inject(b)
        case ValueNil => sabs.nil
      }, store, Nil)
    case SExpQuoted(q, pos) =>
      evalQuoted(SExpPair(SExpIdentifier("quote", pos),
                          SExpPair(q, SExpValue(ValueNil, pos), pos),
                          pos),
                 store,
                 t)
  }

  def stepEval(e: SchemeExp,
               env: Environment[Addr],
               store: Store[Addr, ConcreteValue],
               t: Time) = e match { // Cases in stepEval shouldn't generate any splits in abstract graph
    case λ: SchemeLambda =>
      val action = ActionReachedValue[SchemeExp, ConcreteValue, Addr](sabs.inject[SchemeExp, Addr]((λ, env)), None, store)
      val actionEdge = List(ActionCreateClosureT[SchemeExp, ConcreteValue, Addr](λ, Some(env)))
      noEdgeInfos(action, actionEdge)
    case SchemeFuncall(f, args, _) =>
      addPushActionR(ActionPush[SchemeExp, ConcreteValue, Addr](FrameFuncallOperator[ConcreteValue, Addr, Time](f, args, env), f, env, store))
    case e @ SchemeIf(cond, cons, alt, _) =>
      addPushActionR(ActionPush(FrameIf[ConcreteValue, Addr, Time](cons, alt, env, e), cond, env, store))
    case SchemeLet(Nil, body, _) =>
      evalBody(body, env, store)
    case SchemeLet((v, exp) :: bindings, body, _) =>
      val optInputVariable = SemanticsConcolicHelper.handleDefine(v, exp)
      addPushActionR(ActionPush(FrameLet[ConcreteValue, Addr, Time](v, List(), bindings, body, env, optInputVariable), exp, env, store))
    case SchemeLetStar(Nil, body, _) =>
      evalBody(body, env, store)
    case SchemeLetStar((v, exp) :: bindings, body, _) =>
      val optInputVariable = SemanticsConcolicHelper.handleDefine(v, exp)
      addPushActionR(ActionPush(FrameLetStar[ConcreteValue, Addr, Time](v, bindings, body, env, optInputVariable), exp, env, store))
    case SchemeLetrec(Nil, body, _) =>
      evalBody(body, env, store)
    case SchemeLetrec((v, exp) :: bindings, body, _) =>
      val optInputVariable = SemanticsConcolicHelper.handleDefine(v, exp)
      case class ValuesToUpdate(env: Environment[Addr],
                                store: Store[Addr, ConcreteValue],
                                stateChanges: List[StoreChangeSemantics[ConcreteValue, Addr]])
      type X = (ValuesToUpdate, (String, Addr))
      val variables = v :: bindings.map(_._1)
      val addresses = variables.map(v => addr.variable(v, abs.bottom, t))
      val addressesVariables: List[(Addr, String)] = addresses.zip(variables)
      val initial = ValuesToUpdate(env, store, Nil)
      val ValuesToUpdate(env1, store1, stateChanges) = variables
        .zip(addresses)
        .foldLeft[ValuesToUpdate](initial)({
        case (ValuesToUpdate(env, store, storeChanges), (v, a)) =>
          ValuesToUpdate(env.extend(v, a),
                         store.extend(a, abs.bottom),
                         StoreExtendSemantics[ConcreteValue, Addr](a, abs.bottom) :: storeChanges)
      })
      val action = ActionPush(FrameLetrec(addresses.head, addressesVariables.tail.zip(bindings.map(_._2)).map( (binding) => (binding._1._1, binding._1._2, binding._2) ), body, env1, optInputVariable),
                              exp, env1, store1)
      val actionEdge = ActionAllocAddressesR[SchemeExp, ConcreteValue, Addr](addresses)
      noEdgeInfos(action, List(actionEdge, ActionEvalPushR(exp, action.env, action.frame)))
    case SchemeSet(variable, exp, _) =>
      val optInputVariable = SemanticsConcolicHelper.handleSet(variable, exp)
      addPushActionR(ActionPush(FrameSet(variable, env, optInputVariable), exp, env, store))
    case SchemeBegin(body, _) =>
      evalBody(body, env, store)
    case SchemeCond(Nil, _) =>
      simpleAction(ActionError[SchemeExp, ConcreteValue, Addr](NotSupported("cond without clauses")))
    case SchemeCond((cond, cons) :: clauses, _) =>
      addPushActionR(ActionPush(FrameCond(cons, clauses, env), cond, env, store))
    case SchemeCase(key, clauses, default, _) =>
      addPushActionR(ActionPush(FrameCase(clauses, default, env), key, env, store))
    case SchemeAnd(Nil, _) =>
      val valueTrue = sabs.inject(true)
      noEdgeInfos(ActionReachedValue[SchemeExp, ConcreteValue, Addr](valueTrue, None, store),
                     List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](valueTrue)))
    case SchemeAnd(exp :: exps, _) =>
      addPushActionR(ActionPush(FrameAnd(exps, env), exp, env, store))
    case SchemeOr(Nil, _) =>
      val valueFalse = sabs.inject(false)
      noEdgeInfos(ActionReachedValue[SchemeExp, ConcreteValue, Addr](valueFalse, None, store),
                     List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](valueFalse)))
    case SchemeOr(exp :: exps, _) =>
      addPushActionR(ActionPush(FrameOr(exps, env), exp, env, store))
    case SchemeDefineVariable(name, exp, _) =>
      val optInputVariable = SemanticsConcolicHelper.handleDefine(name, exp)
      addPushActionR(ActionPush(FrameDefine(name, env, optInputVariable), exp, env, store))
    case SchemeDefineFunction(name, args, body, pos) =>
      val a = addr.variable(name, abs.bottom, t)
      val lambda = SchemeLambda(args, body, pos)
      val v = sabs.inject[SchemeExp, Addr]((lambda, env))
      val action = ActionReachedValue[SchemeExp, ConcreteValue, Addr](v, None, store)
      val actionEdges = List(ActionCreateClosureT[SchemeExp, ConcreteValue, Addr](lambda, Some(env)),
                             ActionDefineAddressesR[SchemeExp, ConcreteValue, Addr](List(a)))
      noEdgeInfos(action, actionEdges)
    case SchemeIdentifier(name, _) =>
      env.lookup(name) match {
        case Some(a) =>
          store.lookup(a) match {
            case Some(v) =>
              val action = ActionReachedValue[SchemeExp, ConcreteValue, Addr](v, Reporter.lookupVariable(name), store, Set(EffectReadVariable(a)))
              noEdgeInfos(action, List(ActionLookupAddressR[SchemeExp, ConcreteValue, Addr](a)))
            case None =>
              simpleAction(ActionError[SchemeExp, ConcreteValue, Addr](UnboundAddress(a.toString)))
          }
        case None =>
          simpleAction(ActionError[SchemeExp, ConcreteValue, Addr](UnboundVariable(name)))
      }
    case SchemeQuoted(quoted, _) =>
      evalQuoted(quoted, store, t) match {
        case (value, store2, storeChanges) =>
          val action = ActionReachedValue[SchemeExp, ConcreteValue, Addr](value, None, store2)
          val actionEdges = List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](value, storeChanges = storeChanges))
          noEdgeInfos(action, actionEdges)
      }
    /* The start-analysis expression only has an effect in the SchemeSemanticsTraced; in these semantics, it just
     * evaluates to #f. */
    case SchemeStartAnalysis(_) =>
      val falseValue = sabs.inject(false)
      noEdgeInfos(ActionReachedValue[SchemeExp, ConcreteValue, Addr](falseValue, None, store),
                     List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](falseValue)))
    case SchemeValue(v, _) =>
      evalValue(v) match {
        case Some(v) =>
          val optionConcolicValue: Option[ConcolicExpression] = ConcreteConcreteLattice.simplify(v) match {
            case Some(ValueInteger(i)) =>
              Some(ConcolicInt(i))
            case _ =>
              None
          }
          noEdgeInfos(ActionReachedValue[SchemeExp, ConcreteValue, Addr](v, optionConcolicValue, store),
                      List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](v)))
        case None => simpleAction(ActionError[SchemeExp, ConcreteValue, Addr](NotSupported(s"Unhandled value: $v")))
      }
  }

  private def lookupInputVariable(inputVariable: String, defaultValue: ConcreteValue): ConcreteValue = ConcolicSolver.getInput(inputVariable) match {
    case Some(concolicInt) =>
      sabs.inject(concolicInt)
    case None =>
      // No concolic value defined for this input variable
      defaultValue
  }

  private def getConcolicValue(frameLet: FrameMayHaveInputVariable, defaultValue: ConcreteValue): ConcreteValue = frameLet.optInputVariable match {
    case Some(inputVariable) =>
      lookupInputVariable(inputVariable, defaultValue)
    case None =>
      // No input variable defined: frameLet was not binding an input variable
      defaultValue
  }

  def stepKont(v: ConcreteValue,
               concolicValue: Option[ConcolicExpression],
               frame: Frame,
               store: Store[Addr, ConcreteValue],
               t: Time) =
    frame match {
      case frame: FrameFuncallOperator[ConcreteValue, Addr, Time] =>
        funcallArgs(v, frame, frame.fexp, frame.args, frame.env, store, t)
      case frame: FrameConcolicFuncallOperands[ConcreteValue, Addr, Time] =>
        val frameGeneratorGenerator =
          (e: SchemeExp, rest: List[SchemeExp]) =>
            PlaceOperandFuncallOperandsFrameGenerator(frame.f, frame.fexp, e, frame.cur, frame.args, rest, frame.env)
        funcallArgs(frame.f, frame, frame.fexp, (frame.cur, v) :: frame.args, frame.toeval, frame.env, store, t, v,
          frameGeneratorGenerator)
      case frame: FrameIf[ConcreteValue, Addr, Time] =>
        SemanticsConcolicHelper.handleIf(frame.ifExp, sabs.isTrue(v))
        conditional(v, addEvalActionT(ActionEval(frame.cons, frame.env, store)), addEvalActionT(ActionEval(frame.alt, frame.env, store)))
      case frame: FrameLet[ConcreteValue, Addr, Time] => val concolicV = getConcolicValue(frame, v); frame.toeval match {
        case Nil =>
          val variables = frame.variable :: frame.bindings.reverse.map(_._1)
          val addresses = variables.map(variable => addr.variable(variable, concolicV, t))
          val (env1, store1) = ((frame.variable, concolicV) :: frame.bindings)
            .zip(addresses)
            .foldLeft((frame.env, store))({
              case ((env, store), ((variable, value), a)) =>
                (env.extend(variable, a), store.extend(a, value))
            })
          val EdgeInformation(action, actionEdges, filters) = evalBody(frame.body, env1, store1)
          EdgeInformation(action, ActionDefineAddressesPopR[SchemeExp, ConcreteValue, Addr](addresses) :: actionEdges, filters)
        case (variable, e) :: toeval =>
          val optInputVariable = SemanticsConcolicHelper.handleDefine(variable, e)
          val newFrameGenerator = LetFrameGenerator(variable, frame.variable, frame.bindings, toeval, frame.body, frame.env, optInputVariable)
          val actionGenerator = (currentFrame: Frame) => ActionPush(currentFrame, e, frame.env, store)
          addPushDataActionT(concolicV, frame, newFrameGenerator, actionGenerator)
      }
      case frame: FrameLetStar[ConcreteValue, Addr, Time] =>
        val concolicV = getConcolicValue(frame, v)
        val a = addr.variable(frame.variable, abs.bottom, t)
        val env1 = frame.env.extend(frame.variable, a)
        val store1 = store.extend(a, concolicV)
        val actionEdges = List(ActionDefineAddressesPopR[SchemeExp, ConcreteValue, Addr](List(a)))
        frame.bindings match {
          case Nil =>
            val EdgeInformation(actions, actionEdges2, edgeInfos) = evalBody(frame.body, env1, store1)
            EdgeInformation(actions, actionEdges ++ actionEdges2, edgeInfos)
          case (variable, exp) :: rest =>
            val optInputVariable = SemanticsConcolicHelper.handleDefine(variable, exp)
            val action = ActionPush(FrameLetStar(variable, rest, frame.body, env1, optInputVariable), exp, env1, store1)
            noEdgeInfos(action, actionEdges :+ ActionEvalPushR(exp, action.env, action.frame))
        }
      case frame: FrameLetrec[ConcreteValue, Addr, Time] => val concolicV = getConcolicValue(frame, v); frame.bindings match {
        case Nil =>
          // If frame defines an input variable, and if a value is bound to this input variable, use that bound value
          val EdgeInformation(action, actionEdges, edgeInfos) = evalBody(frame.body, frame.env, store.update(frame.addr, concolicV))
          EdgeInformation(action, ActionSetAddressR[SchemeExp, ConcreteValue, Addr](frame.addr) :: actionEdges, edgeInfos)
        case (a1, varName, exp) :: rest =>
          val optInputVariable = SemanticsConcolicHelper.handleDefine(varName, exp)
          val action = ActionPush(FrameLetrec(a1, rest, frame.body, frame.env, optInputVariable), exp, frame.env, store.update(frame.addr, concolicV))
          val actionEdges = List[ActionReplay[SchemeExp, ConcreteValue, Addr]](ActionSetAddressR(frame.addr),
                                                                     ActionEvalPushR(exp, action.env, action.frame))
          noEdgeInfos(action, actionEdges)
      }
      case frame: FrameSet[ConcreteValue, Addr, Time] =>
        val concolicValue = getConcolicValue(frame, v)
        frame.env.lookup(frame.variable) match {
          case Some(a) =>
            val valueFalse = sabs.inject(false)
            val actionEdges = List[ActionReplay[SchemeExp, ConcreteValue, Addr]](ActionSetAddressR(a),
                                                                       ActionReachedValueT(valueFalse))
            noEdgeInfos(ActionReachedValue[SchemeExp, ConcreteValue, Addr](valueFalse, None, store.update(a, concolicValue), Set(EffectWriteVariable(a))),
                        actionEdges)
          case None =>
            simpleAction(ActionError[SchemeExp, ConcreteValue, Addr](UnboundVariable(frame.variable)))
        }
      case frame: FrameBegin[ConcreteValue, Addr, Time] => frame.rest match {
        case List(SchemePopSymEnv(_)) =>
          Reporter.popEnvironment()
          val action = ActionReachedValue[SchemeExp, ConcreteValue, Addr](v, concolicValue, store)
          val actionR = ActionReachedValueT[SchemeExp, ConcreteValue, Addr](v)
          noEdgeInfos(action, actionR)
        case _ =>
          evalBody(frame.rest, frame.env, store)
      }
      case frame: FrameCond[ConcreteValue, Addr, Time] =>
        val falseValue = sabs.inject(false)
        conditional(v,
                    if (frame.cons.isEmpty) {
                      noEdgeInfos(ActionReachedValue[SchemeExp, ConcreteValue, Addr](v, concolicValue, store),
                                  List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](v)))
                    } else {
                      evalBody(frame.cons, frame.env, store)
                    },
                    frame.clauses match {
                      case Nil =>
                        noEdgeInfos(ActionReachedValue[SchemeExp, ConcreteValue, Addr](falseValue, None, store),
                                    List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](falseValue)))
                      case (exp, cons2) :: rest =>
                        addPushActionR(ActionPush(FrameCond(cons2, rest, frame.env), exp, frame.env, store))
                    })
      case frame: FrameCase[ConcreteValue, Addr, Time] =>
        val fromClauses = frame.clauses.flatMap({
          case (values, body) =>
            if (values.exists(v2 =>
              evalValue(v2.value) match {
                case None => false
                case Some(v2) => sabs.subsumes(v, v2)
              }))
            /* TODO: precision could be improved by restricting v to v2 */
              Set[EdgeInformation[SchemeExp, ConcreteValue, Addr]](evalBody(body, frame.env, store))
            else
              Set[EdgeInformation[SchemeExp, ConcreteValue, Addr]]()
        })
        /* TODO: precision could be improved in cases where we know that default is not reachable */
        if (fromClauses.nonEmpty) {
          fromClauses.head
        } else {
          evalBody(frame.default, frame.env, store)
        }
      case frame: FrameAnd[ConcreteValue, Addr, Time] => frame.rest match {
        case Nil =>
          val falseValue = sabs.inject(false)
          conditional(v,
                      noEdgeInfos(ActionReachedValue[SchemeExp, ConcreteValue, Addr](v, concolicValue, store),
                                  List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](v))),
                      noEdgeInfos(ActionReachedValue[SchemeExp, ConcreteValue, Addr](falseValue, None, store),
                                  List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](falseValue))))
        case e :: rest =>
          val falseValue = sabs.inject(false)
          conditional(v,
                      addPushActionR(ActionPush(FrameAnd(rest, frame.env), e, frame.env, store)),
                      noEdgeInfos(ActionReachedValue[SchemeExp, ConcreteValue, Addr](falseValue, None, store),
                                  List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](falseValue))))
      }
      case frame: FrameOr[ConcreteValue, Addr, Time] => frame.rest match {
        case Nil =>
          val falseValue = sabs.inject(false)
          conditional(v,
            noEdgeInfos(ActionReachedValue[SchemeExp, ConcreteValue, Addr](v, concolicValue, store),
                        List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](v))),
            noEdgeInfos(ActionReachedValue[SchemeExp, ConcreteValue, Addr](falseValue, None, store),
                        List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](falseValue))))
        case e :: rest =>
          conditional(v,
            noEdgeInfos(ActionReachedValue[SchemeExp, ConcreteValue, Addr](v, concolicValue, store),
                        List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](v))),
            addPushActionR(ActionPush(FrameOr(rest, frame.env), e, frame.env, store)))
      }
      case frame: FrameDefine[ConcreteValue, Addr, Time] =>
        throw new Exception("TODO: define not handled (no global environment)")
    }

  def parse(program: String): SchemeExp = Scheme.parse(program)
  override def initialBindings = primitives.bindings
}