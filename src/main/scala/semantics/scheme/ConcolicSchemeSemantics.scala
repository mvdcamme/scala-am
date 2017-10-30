import scalaz.Scalaz._
import scalaz._

/**
  * Basic Scheme semantics, without any optimization
  */
class ConcolicBaseSchemeSemantics[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    val primitives: SchemePrimitives[Addr, Abs])
    extends BaseSemantics[SchemeExp, Abs, Addr, Time]
    with ConvertableSemantics[SchemeExp, Abs, Addr, Time] {

  def sabs = implicitly[IsSchemeLattice[Abs]]

  def convertToAbsSemanticsFrame(frame: Frame,
                                 ρ: Environment[Addr],
                                 vStack: List[Storable[Abs, Addr]],
                                 absSem: BaseSchemeSemantics[Abs, Addr, Time])
  : (Option[Frame], List[Storable[Abs, Addr]], Environment[Addr]) = (Some(frame), vStack, ρ)

  protected def evalBody(
      body: List[SchemeExp],
      env: Environment[Addr],
      store: Store[Addr, Abs]): EdgeInformation[SchemeExp, Abs, Addr] = body match {
    case Nil =>
      noEdgeInfos(ActionReachedValue[SchemeExp, Abs, Addr](sabs.inject(false), store),
                  List(ActionReachedValueT[SchemeExp, Abs, Addr](sabs.inject(false))))
    case List(exp) =>
      noEdgeInfos(ActionEval(exp, env, store), ActionEvalR(exp, env))
    case exp :: rest =>
      addPushActionR(ActionPush(FrameBegin(rest, env), exp, env, store))
  }

  def convertAbsInFrame[OtherAbs: IsConvertableLattice](
      frame: SchemeFrame[Abs, Addr, Time],
      convertValue: (Abs) => OtherAbs,
      convertEnv: (Environment[Addr]) => Environment[Addr],
      abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time])
    : SchemeFrame[OtherAbs, Addr, Time] =
    frame.convert(convertValue, convertEnv, abstSem)

  def frameReaches(frame: SchemeFrame[Abs, Addr, Time],
                   valueReaches: Abs => Set[Addr],
                   envReaches: Environment[Addr] => Set[Addr],
                   addressReaches: Addr => Set[Addr]): Set[Addr] =
    frame.reaches(valueReaches, envReaches, addressReaches)

  def conditional(v: Abs,
                  t: => Set[EdgeInformation[SchemeExp, Abs, Addr]],
                  f: => Set[EdgeInformation[SchemeExp, Abs, Addr]]): Set[EdgeInformation[SchemeExp, Abs, Addr]] = {
    def addFilter(x: Set[EdgeInformation[SchemeExp, Abs, Addr]],
                     filter: SemanticsFilterAnnotation): Set[EdgeInformation[SchemeExp, Abs, Addr]] =
      x.map({
        case EdgeInformation(action, actionTs, semanticsFilters) =>
          EdgeInformation(action, actionTs, semanticsFilters + filter)
      })

    (if (sabs.isTrue(v)) addFilter(t, ThenBranchTaken) else Set[EdgeInformation[SchemeExp, Abs, Addr]]()) ++
    (if (sabs.isFalse(v)) addFilter(f, ElseBranchTaken) else Set[EdgeInformation[SchemeExp, Abs, Addr]]())
  }

  protected def addEvalActionT(action: ActionEval[SchemeExp, Abs, Addr]): Set[EdgeInformation[SchemeExp, Abs, Addr]] =
    noEdgeInfosSet(action, ActionEvalR(action.e, action.env))

  protected def addPushActionR(action: ActionPush[SchemeExp, Abs, Addr]): EdgeInformation[SchemeExp, Abs, Addr] =
    noEdgeInfos(action, ActionEvalPushR(action.e, action.env, action.frame))

  protected def addPushActionRSet(action: ActionPush[SchemeExp, Abs, Addr]): Set[EdgeInformation[SchemeExp, Abs, Addr]] =
    Set(addPushActionR(action))

  protected def addPushDataActionT(currentValue: Abs,
                                   currentFrame: Frame,
                                   frameGenerator: FrameGenerator[Abs],
                                   actionGenerator: Frame => ActionPush[SchemeExp, Abs, Addr]):
  Set[EdgeInformation[SchemeExp, Abs, Addr]] = {
    val newFrame = frameGenerator(currentValue, currentFrame)
    val currentAction = actionGenerator(newFrame)
    noEdgeInfosSet(currentAction, ActionEvalPushDataR(currentAction.e, currentAction.env, frameGenerator))
  }

  def evalCall(function: Abs,
               fexp: SchemeExp,
               argsv: List[(SchemeExp, Abs)],
               store: Store[Addr, Abs],
               t: Time): Set[EdgeInformation[SchemeExp, Abs, Addr]] = {
    val fromClo: Set[EdgeInformation[SchemeExp, Abs, Addr]] = sabs
      .getClosures[SchemeExp, Addr](function)
      .map({
        case (lambda@(SchemeLambda(args, body, pos)), env1) =>
          val cloCall = ActionClosureCallR[SchemeExp, Abs, Addr](fexp, lambda, env1)
          if (args.length == argsv.length) {
            val argsZipped: List[(String, (SchemeExp, Abs))] = args.zip(argsv)
            // Create a StatementConstraint for each parameter
            val updatedArgsZipped: List[(String, (SchemeExp, Abs))] = argsZipped.map({
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

                val defAddr = ActionDefineAddressesPopR[SchemeExp, Abs, Addr](boundAddresses.map(_._1))
                val timeTick = ActionTimeTickExpR[SchemeExp, Abs, Addr](fexp)
                val makeActionRs = (edgeAnnotation: ActionReplay[SchemeExp, Abs, Addr]) =>
                  List(defAddr, edgeAnnotation, timeTick, cloCall)
                if (body.length == 1) {
                  val action = ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body, pos), env1), body.head, env2, store, argsv)
                  EdgeInformation(action, makeActionRs(ActionEvalR(body.head, env2)), Set())
                }
                else {
                  val action = ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body, pos), env1), SchemeBegin(body, pos), env2, store, argsv)
                  EdgeInformation(action, makeActionRs(ActionEvalR[SchemeExp, Abs, Addr](SchemeBegin(body, pos),
                    env2)), Set())
                }
            }
          } else {
            val error = ArityError(fexp.toString, args.length, argsv.length)
            val actionError = ActionErrorT[SchemeExp, Abs, Addr](error)
            EdgeInformation(ActionError[SchemeExp, Abs, Addr](error), List(actionError, cloCall), Set())
          }
        case (lambda, env) =>
          val cloCall = ActionClosureCallR[SchemeExp, Abs, Addr](fexp, lambda, env)
          val error = TypeError(lambda.toString, "operator", "closure", "not a closure")
          val actionError = ActionErrorT[SchemeExp, Abs, Addr](error)
          noEdgeInfos(ActionError[SchemeExp, Abs, Addr](error), List(actionError, cloCall))
      })
    /* TODO take into account store changes made by the application of the primitives */
    val fromPrim = sabs.getPrimitives[Addr, Abs](function).flatMap( (prim) => {
      val n = argsv.size + 1 // Number of values to pop: all arguments + the operator
      val applyPrim = ActionPrimCallT[SchemeExp, Abs, Addr](n, fexp, argsv.map(_._1), sabs.inject(prim))
      prim.call(fexp, argsv, store, t).collect[EdgeInformation[SchemeExp, Abs, Addr]]({
        case (res, store2, effects) =>
          val action = ActionReachedValue[SchemeExp, Abs, Addr](res, store2, effects)
          Set(EdgeInformation[SchemeExp, Abs, Addr](action, List(applyPrim), Set()))
      },
        err => {
          val actionError = ActionErrorT[SchemeExp, Abs, Addr](err)
          Set(EdgeInformation[SchemeExp, Abs, Addr](ActionError[SchemeExp, Abs, Addr](err), List(actionError), Set()))
        })
    })
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      simpleAction(ActionError[SchemeExp, Abs, Addr](TypeError(function.toString, "operator", "function", "not a function")))
    } else {
      fromClo ++ fromPrim
    }
  }

  protected def evalValue(v: Value): Option[V] = v match {
    case ValueString(s) => Some(IsSchemeLattice[V].inject(s))
    case ValueInteger(n) => Some(IsSchemeLattice[V].inject(n))
    case ValueReal(n) => Some(IsSchemeLattice[V].inject(n))
    case ValueBoolean(b) => Some(IsSchemeLattice[V].inject(b))
    case ValueCharacter(c) => Some(IsSchemeLattice[V].inject(c))
    case _ => None
  }

  protected def funcallArgs(f: Abs,
                            currentFrame: Frame,
                            fexp: SchemeExp,
                            args: List[(SchemeExp, Abs)],
                            toeval: List[SchemeExp],
                            env: Environment[Addr],
                            store: Store[Addr, Abs],
                            t: Time,
                            currentValue: Abs,
                            frameGeneratorGenerator: (SchemeExp, List[SchemeExp]) => FrameGenerator[Abs]): Set[EdgeInformation[SchemeExp, Abs, Addr]] =
    toeval match {
      case Nil =>
        evalCall(f, fexp, args.reverse, store, t)
      case e :: rest =>
        // FrameFuncallOperands(f, fexp, e, args, rest, env)
        val frameGenerator = frameGeneratorGenerator(e, rest)
        val actionGenerator = (frame: Frame) => ActionPush(frame, e, env, store)
        addPushDataActionT(currentValue, currentFrame, frameGenerator, actionGenerator)
    }

  case class PlaceOperatorFuncallOperandsFrameGenerator(fexp: SchemeExp,
                                                        e: SchemeExp,
                                                        args: List[(SchemeExp, Abs)],
                                                        rest: List[SchemeExp],
                                                        env: Environment[Addr])
    extends FrameGenerator[Abs] {
    def apply(value: Abs, frame: Frame): Frame = {
      FrameFuncallOperands(value, fexp, e, args, rest, env)
    }
  }

  case class LetFrameGenerator(variable: String,
                               frameVariable: String,
                               bindings: List[(String, Abs)],
                               toeval: List[(String, SchemeExp)],
                               body: List[SchemeExp],
                               env: Environment[Addr],
                               optInputVariable: Option[String])
    extends FrameGenerator[Abs] {
    def apply(value: Abs, other: Frame): Frame = {
      FrameLet(variable, (frameVariable, value) :: other.asInstanceOf[FrameLet[Abs, Addr, Time]].bindings, toeval, body, env, optInputVariable)
    }
  }

  case class PlaceOperandFuncallOperandsFrameGenerator(f: Abs,
                                                       fexp: SchemeExp,
                                                       e: SchemeExp,
                                                       cur: SchemeExp,
                                                       args: List[(SchemeExp, Abs)],
                                                       rest: List[SchemeExp],
                                                       env: Environment[Addr])
    extends FrameGenerator[Abs] {
    def apply(value: Abs, other: Frame): Frame = {
      FrameFuncallOperands(f, fexp, e, (cur, value) :: other.asInstanceOf[FrameFuncallOperands[Abs, Addr, Time]].args, rest, env)
    }
  }

  /*
   * To be called after popping a FrameFuncallOperator continuation.
   */
  protected def funcallArgs(f: Abs,
                            currentFrame: Frame,
                            fexp: SchemeExp,
                            args: List[SchemeExp],
                            env: Environment[Addr],
                            store: Store[Addr, Abs],
                            t: Time): Set[EdgeInformation[SchemeExp, Abs, Addr]] = {
    /*
     * As this function is called after popping a FrameFuncallOperator, the value that has just been
     * evaluated equals the operator-value.
     */
    val currentValue = f
//    funcallArgs(f, fexp, List(), args, env, store, t)
    args match {
      case Nil =>
        evalCall(f, fexp, List(), store, t)
      case e :: rest =>
        val frameGenerator = PlaceOperatorFuncallOperandsFrameGenerator(fexp, e, Nil, rest, env)
        val frame = frameGenerator(currentValue, currentFrame)
        val actionGenerator = (frame: Frame) => ActionPush(frame, e, env, store)
        addPushDataActionT(currentValue, currentFrame, frameGenerator, actionGenerator)
    }
  }

  protected def evalQuoted(exp: SExp,
                           store: Store[Addr, Abs],
                           t: Time): (Abs, Store[Addr, Abs], List[StoreChangeSemantics[Abs, Addr]]) = exp
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
       List(StoreExtendSemantics[Abs, Addr](cara, carv), StoreExtendSemantics[Abs, Addr](cdra, cdrv)) ++
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
               store: Store[Addr, Abs],
               t: Time) = e match { // Cases in stepEval shouldn't generate any splits in abstract graph
    case SchemePopSymEnv(_) =>
      ???
    case λ: SchemeLambda =>
      val action = ActionReachedValue[SchemeExp, Abs, Addr](sabs.inject[SchemeExp, Addr]((λ, env)), store)
      val actionEdge = List(ActionCreateClosureT[SchemeExp, Abs, Addr](λ, Some(env)))
      noEdgeInfosSet(action, actionEdge)
    case SchemeFuncall(f, args, _) =>
      addPushActionRSet(ActionPush[SchemeExp, Abs, Addr](FrameFuncallOperator(f, args, env), f, env, store))
    case e @ SchemeIf(cond, cons, alt, _) =>
      addPushActionRSet(ActionPush(FrameIf(cons, alt, env, e), cond, env, store))
    case SchemeLet(Nil, body, _) =>
      Set(evalBody(body, env, store))
    case SchemeLet((v, exp) :: bindings, body, _) =>
      val optInputVariable = SemanticsConcolicHelper.handleDefine(v, exp)
      addPushActionRSet(ActionPush(FrameLet(v, List(), bindings, body, env, optInputVariable), exp, env, store))
    case SchemeLetStar(Nil, body, _) =>
      Set(evalBody(body, env, store))
    case SchemeLetStar((v, exp) :: bindings, body, _) =>
      val optInputVariable = SemanticsConcolicHelper.handleDefine(v, exp)
      addPushActionRSet(ActionPush(FrameLetStar(v, bindings, body, env, optInputVariable), exp, env, store))
    case SchemeLetrec(Nil, body, _) =>
      Set(evalBody(body, env, store))
    case SchemeLetrec((v, exp) :: bindings, body, _) =>
      val optInputVariable = SemanticsConcolicHelper.handleDefine(v, exp)
      case class ValuesToUpdate(env: Environment[Addr],
                                store: Store[Addr, Abs],
                                stateChanges: List[StoreChangeSemantics[Abs, Addr]])
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
                         StoreExtendSemantics[Abs, Addr](a, abs.bottom) :: storeChanges)
      })
      val action = ActionPush(FrameLetrec(addresses.head, addressesVariables.tail.zip(bindings.map(_._2)).map( (binding) => (binding._1._1, binding._1._2, binding._2) ), body, env1, optInputVariable),
                              exp, env1, store1)
      val actionEdge = ActionAllocAddressesR[SchemeExp, Abs, Addr](addresses)
      noEdgeInfosSet(action, List(actionEdge, ActionEvalPushR(exp, action.env, action.frame)))
    case SchemeSet(variable, exp, _) =>
      addPushActionRSet(ActionPush(FrameSet(variable, env), exp, env, store))
    case SchemeBegin(body, _) =>
      Set(evalBody(body, env, store))
    case SchemeCond(Nil, _) =>
      simpleAction(ActionError[SchemeExp, Abs, Addr](NotSupported("cond without clauses")))
    case SchemeCond((cond, cons) :: clauses, _) =>
      addPushActionRSet(ActionPush(FrameCond(cons, clauses, env), cond, env, store))
    case SchemeCase(key, clauses, default, _) =>
      addPushActionRSet(ActionPush(FrameCase(clauses, default, env), key, env, store))
    case SchemeAnd(Nil, _) =>
      val valueTrue = sabs.inject(true)
      noEdgeInfosSet(ActionReachedValue[SchemeExp, Abs, Addr](valueTrue, store),
                     List(ActionReachedValueT[SchemeExp, Abs, Addr](valueTrue)))
    case SchemeAnd(exp :: exps, _) =>
      addPushActionRSet(ActionPush(FrameAnd(exps, env), exp, env, store))
    case SchemeOr(Nil, _) =>
      val valueFalse = sabs.inject(false)
      noEdgeInfosSet(ActionReachedValue[SchemeExp, Abs, Addr](valueFalse, store),
                     List(ActionReachedValueT[SchemeExp, Abs, Addr](valueFalse)))
    case SchemeOr(exp :: exps, _) =>
      addPushActionRSet(ActionPush(FrameOr(exps, env), exp, env, store))
    case SchemeDefineVariable(name, exp, _) =>
      val optInputVariable = SemanticsConcolicHelper.handleDefine(name, exp)
      addPushActionRSet(ActionPush(FrameDefine(name, env, optInputVariable), exp, env, store))
    case SchemeDefineFunction(name, args, body, pos) =>
      val a = addr.variable(name, abs.bottom, t)
      val lambda = SchemeLambda(args, body, pos)
      val v = sabs.inject[SchemeExp, Addr]((lambda, env))
      val action = ActionReachedValue[SchemeExp, Abs, Addr](v, store)
      val actionEdges = List(ActionCreateClosureT[SchemeExp, Abs, Addr](lambda, Some(env)),
                             ActionDefineAddressesR[SchemeExp, Abs, Addr](List(a)))
      noEdgeInfosSet(action, actionEdges)
    case SchemeIdentifier(name, _) =>
      env.lookup(name) match {
        case Some(a) =>
          store.lookup(a) match {
            case Some(v) =>
              val action = ActionReachedValue[SchemeExp, Abs, Addr](v, store, Set(EffectReadVariable(a)))
              noEdgeInfosSet(action, List(ActionLookupAddressR[SchemeExp, Abs, Addr](a)))
            case None =>
              simpleAction(ActionError[SchemeExp, Abs, Addr](UnboundAddress(a.toString)))
          }
        case None =>
          simpleAction(ActionError[SchemeExp, Abs, Addr](UnboundVariable(name)))
      }
    case SchemeQuoted(quoted, _) =>
      evalQuoted(quoted, store, t) match {
        case (value, store2, storeChanges) =>
          val action = ActionReachedValue[SchemeExp, Abs, Addr](value, store2)
          val actionEdges = List(ActionReachedValueT[SchemeExp, Abs, Addr](value, storeChanges = storeChanges))
          noEdgeInfosSet(action, actionEdges)
      }
    /* The start-analysis expression only has an effect in the SchemeSemanticsTraced; in these semantics, it just
     * evaluates to #f. */
    case SchemeStartAnalysis(_) =>
      val falseValue = sabs.inject(false)
      noEdgeInfosSet(ActionReachedValue[SchemeExp, Abs, Addr](falseValue, store),
                     List(ActionReachedValueT[SchemeExp, Abs, Addr](falseValue)))
    case SchemeValue(v, _) =>
      evalValue(v) match {
        case Some(v) => noEdgeInfosSet(ActionReachedValue[SchemeExp, Abs, Addr](v, store),
                                       List(ActionReachedValueT[SchemeExp, Abs, Addr](v)))
        case None => simpleAction(ActionError[SchemeExp, Abs, Addr](NotSupported(s"Unhandled value: $v")))
      }
  }

  private def lookupInputVariable(inputVariable: String, defaultValue: Abs): Abs = ConcolicSolver.getInput(inputVariable) match {
    case Some(concolicInt) =>
      sabs.inject(concolicInt)
    case None =>
      // No concolic value defined for this input variable
      defaultValue
  }

  private def getConcolicValue(frameLet: FrameLetVariant, defaultValue: Abs): Abs = frameLet.optInputVariable match {
    case Some(inputVariable) =>
      lookupInputVariable(inputVariable, defaultValue)
    case None =>
      // No input variable defined: frameLet was not binding an input variable
      defaultValue
  }

  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time) =
    frame match {
      case frame: FrameFuncallOperator[Abs, Addr, Time] =>
        funcallArgs(v, frame, frame.fexp, frame.args, frame.env, store, t)
      case frame: FrameFuncallOperands[Abs, Addr, Time] =>
        val frameGeneratorGenerator =
          (e: SchemeExp, rest: List[SchemeExp]) =>
            PlaceOperandFuncallOperandsFrameGenerator(frame.f, frame.fexp, e, frame.cur, frame.args, rest, frame.env)
        funcallArgs(frame.f, frame, frame.fexp, (frame.cur, v) :: frame.args, frame.toeval, frame.env, store, t, v,
          frameGeneratorGenerator)
      case frame: FrameIf[Abs, Addr, Time] =>
        SemanticsConcolicHelper.handleIf(frame.ifExp, sabs.isTrue(v))
        conditional(v, addEvalActionT(ActionEval(frame.cons, frame.env, store)), addEvalActionT(ActionEval(frame.alt, frame.env, store)))
      case frame: FrameLet[Abs, Addr, Time] => val concolicV = getConcolicValue(frame, v); frame.toeval match {
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
          Set(EdgeInformation(action, ActionDefineAddressesPopR[SchemeExp, Abs, Addr](addresses) :: actionEdges, filters))
        case (variable, e) :: toeval =>
          val optInputVariable = SemanticsConcolicHelper.handleDefine(variable, e)
          val newFrameGenerator = LetFrameGenerator(variable, frame.variable, frame.bindings, toeval, frame.body, frame.env, optInputVariable)
          val actionGenerator = (currentFrame: Frame) => ActionPush(currentFrame, e, frame.env, store)
          addPushDataActionT(concolicV, frame, newFrameGenerator, actionGenerator)
      }
      case frame: FrameLetStar[Abs, Addr, Time] =>
        val concolicV = getConcolicValue(frame, v)
        val a = addr.variable(frame.variable, abs.bottom, t)
        val env1 = frame.env.extend(frame.variable, a)
        val store1 = store.extend(a, concolicV)
        val actionEdges = List(ActionDefineAddressesPopR[SchemeExp, Abs, Addr](List(a)))
        frame.bindings match {
          case Nil =>
            val EdgeInformation(actions, actionEdges2, edgeInfos) = evalBody(frame.body, env1, store1)
            Set(EdgeInformation(actions, actionEdges ++ actionEdges2, edgeInfos))
          case (variable, exp) :: rest =>
            val optInputVariable = SemanticsConcolicHelper.handleDefine(variable, exp)
            val action = ActionPush(FrameLetStar(variable, rest, frame.body, env1, optInputVariable), exp, env1, store1)
            noEdgeInfosSet(action, actionEdges :+ ActionEvalPushR(exp, action.env, action.frame))
        }
      case frame: FrameLetrec[Abs, Addr, Time] => val concolicV = getConcolicValue(frame, v); frame.bindings match {
        case Nil =>
          // If frame defines an input variable, and if a value is bound to this input variable, use that bound value
          val EdgeInformation(action, actionEdges, edgeInfos) = evalBody(frame.body, frame.env, store.update(frame.addr, concolicV))
          Set(EdgeInformation(action, ActionSetAddressR[SchemeExp, Abs, Addr](frame.addr) :: actionEdges, edgeInfos))
        case (a1, varName, exp) :: rest =>
          val optInputVariable = SemanticsConcolicHelper.handleDefine(varName, exp)
          val action = ActionPush(FrameLetrec(a1, rest, frame.body, frame.env, optInputVariable), exp, frame.env, store.update(frame.addr, concolicV))
          val actionEdges = List[ActionReplay[SchemeExp, Abs, Addr]](ActionSetAddressR(frame.addr),
                                                                     ActionEvalPushR(exp, action.env, action.frame))
          noEdgeInfosSet(action, actionEdges)
      }
      case frame: FrameSet[Abs, Addr, Time] =>
        frame.env.lookup(frame.variable) match {
          case Some(a) =>
            val valueFalse = sabs.inject(false)
            val actionEdges = List[ActionReplay[SchemeExp, Abs, Addr]](ActionSetAddressR(a),
                                                                       ActionReachedValueT(valueFalse))
            noEdgeInfosSet(ActionReachedValue[SchemeExp, Abs, Addr](valueFalse, store.update(a, v), Set(EffectWriteVariable(a))),
                           actionEdges)
          case None =>
            simpleAction(ActionError[SchemeExp, Abs, Addr](UnboundVariable(frame.variable)))
        }
      case frame: FrameBegin[Abs, Addr, Time] => frame.rest match {
        case List(SchemePopSymEnv(_)) =>
          Reporter.popEnvironment()
          val action = ActionReachedValue[SchemeExp, Abs, Addr](v, store)
          val actionR = ActionReachedValueT[SchemeExp, Abs, Addr](v)
          noEdgeInfosSet(action, actionR)
        case _ =>
          Set(evalBody(frame.rest, frame.env, store))
      }
      case frame: FrameCond[Abs, Addr, Time] =>
        val falseValue = sabs.inject(false)
        conditional(v,
                    if (frame.cons.isEmpty) {
                      noEdgeInfosSet(ActionReachedValue[SchemeExp, Abs, Addr](v, store),
                                     List(ActionReachedValueT[SchemeExp, Abs, Addr](v)))
                    } else {
                      Set(evalBody(frame.cons, frame.env, store))
                    },
                    frame.clauses match {
                      case Nil =>
                        noEdgeInfosSet(ActionReachedValue[SchemeExp, Abs, Addr](falseValue, store),
                                       List(ActionReachedValueT[SchemeExp, Abs, Addr](falseValue)))
                      case (exp, cons2) :: rest =>
                        addPushActionRSet(ActionPush(FrameCond(cons2, rest, frame.env), exp, frame.env, store))
                    })
      case frame: FrameCase[Abs, Addr, Time] =>
        val fromClauses = frame.clauses.flatMap({
          case (values, body) =>
            if (values.exists(v2 =>
              evalValue(v2.value) match {
                case None => false
                case Some(v2) => sabs.subsumes(v, v2)
              }))
            /* TODO: precision could be improved by restricting v to v2 */
              Set[EdgeInformation[SchemeExp, Abs, Addr]](evalBody(body, frame.env, store))
            else
              Set[EdgeInformation[SchemeExp, Abs, Addr]]()
        })
        /* TODO: precision could be improved in cases where we know that default is not reachable */
        fromClauses.toSet + evalBody(frame.default, frame.env, store)
      case frame: FrameAnd[Abs, Addr, Time] => frame.rest match {
        case Nil =>
          val falseValue = sabs.inject(false)
          conditional(v,
                      noEdgeInfosSet(ActionReachedValue[SchemeExp, Abs, Addr](v, store),
                                     List(ActionReachedValueT[SchemeExp, Abs, Addr](v))),
                      noEdgeInfosSet(ActionReachedValue[SchemeExp, Abs, Addr](falseValue, store),
                                     List(ActionReachedValueT[SchemeExp, Abs, Addr](falseValue))))
        case e :: rest =>
          val falseValue = sabs.inject(false)
          conditional(v,
                      addPushActionRSet(ActionPush(FrameAnd(rest, frame.env), e, frame.env, store)),
                      noEdgeInfosSet(ActionReachedValue[SchemeExp, Abs, Addr](falseValue, store),
                                     List(ActionReachedValueT[SchemeExp, Abs, Addr](falseValue))))
      }
      case frame: FrameOr[Abs, Addr, Time] => frame.rest match {
        case Nil =>
          val falseValue = sabs.inject(false)
          conditional(v,
            noEdgeInfosSet(ActionReachedValue[SchemeExp, Abs, Addr](v, store),
                           List(ActionReachedValueT[SchemeExp, Abs, Addr](v))),
            noEdgeInfosSet(ActionReachedValue[SchemeExp, Abs, Addr](falseValue, store),
                           List(ActionReachedValueT[SchemeExp, Abs, Addr](falseValue))))
        case e :: rest =>
          conditional(v,
            noEdgeInfosSet(ActionReachedValue[SchemeExp, Abs, Addr](v, store),
                           List(ActionReachedValueT[SchemeExp, Abs, Addr](v))),
            addPushActionRSet(ActionPush(FrameOr(rest, frame.env), e, frame.env, store)))
      }
      case frame: FrameDefine[Abs, Addr, Time] =>
        throw new Exception("TODO: define not handled (no global environment)")
    }

  def parse(program: String): SchemeExp = Scheme.parse(program)
  override def initialBindings = primitives.bindings
}

/**
  * Extend base Scheme semantics with:
  *   - atomic evaluation: parts of some constructs can be evaluated atomically
  *     without needing to introduce more states in the state graph. For example,
  *     (+ 1 1) can directly be evaluated to 2 without modifying the store. Also,
  *     to evaluate (+ 1 (f)), we can directly push the continuation and jump to
  *     the evaluation of (f), instead of evaluating +, and 1 in separate states.
  */
/**
  * TODO Check whether atomic optimisations don't mess up the computed StateChangeEdges
  */
class ConcolicSchemeSemantics[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    primitives: SchemePrimitives[Addr, Abs])
    extends ConcolicBaseSchemeSemantics[Abs, Addr, Time](primitives) {

  /** Tries to perform atomic evaluation of an expression. Returns the result of
    * the evaluation if it succeeded, otherwise returns None */
  protected def atomicEval(
      e: SchemeExp,
      env: Environment[Addr],
      store: Store[Addr, Abs]): Option[(Abs, Set[Effect[Addr]], ActionReplay[SchemeExp, Abs, Addr])] = e match {
    case λ: SchemeLambda =>
      Some((sabs.inject[SchemeExp, Addr]((λ, env)), Set[Effect[Addr]](), ActionCreateClosureT[SchemeExp, Abs, Addr](λ, Some(env))))
    case SchemeIdentifier(name, _) =>
      env.lookup(name).flatMap( (a) =>
        store.lookup(a).map(v => (v, Set(EffectReadVariable(a)), ActionLookupAddressR[SchemeExp, Abs, Addr](a)) ))
    case SchemeValue(v, _) =>
      evalValue(v).map(value => (value, Set[Effect[Addr]](), ActionReachedValueT[SchemeExp, Abs, Addr](value)))
    case _ =>
      None
  }

  protected def addEffects(
      action: Action[SchemeExp, Abs, Addr],
      effects: Set[Effect[Addr]]): Action[SchemeExp, Abs, Addr] =
    action match {
      case ActionReachedValue(v, store, effs) =>
        ActionReachedValue(v, store, effs ++ effects)
      case ActionPush(frame, e, env, store, effs) =>
        ActionPush(frame, e, env, store, effs ++ effects)
      case ActionEval(e, env, store, effs) =>
        ActionEval(e, env, store, effs ++ effects)
      case ActionStepIn(fexp, clo, e, env, store, argsv, effs) =>
        ActionStepIn(fexp, clo, e, env, store, argsv, effs ++ effects)
      case ActionError(err) => action
    }

  protected def funcallArgs(
      f: Abs,
      fexp: SchemeExp,
      args: List[(SchemeExp, Abs)],
      toeval: List[SchemeExp],
      env: Environment[Addr],
      store: Store[Addr, Abs],
      t: Time): Set[EdgeInformation[SchemeExp, Abs, Addr]] = toeval match {
    case Nil =>
      evalCall(f, fexp, args.reverse, store, t)
    case e :: rest =>
      atomicEval(e, env, store) match {
        case Some((v, effs, actionR)) =>
          funcallArgs(f, fexp, (e, v) :: args, rest, env, store, t).map( (edgeInfo: EdgeInformation[SchemeExp, Abs, Addr]) =>
              noEdgeInfos(addEffects(edgeInfo.action, effs), actionR :: edgeInfo.actions) )
        case None =>
          addPushActionRSet(ActionPush(FrameFuncallOperands(f, fexp, e, args, rest, env), e, env, store))
      }
  }

  /**
    * Optimize the following pattern: when we see an ActionPush(frame, exp, env, store)
    * where exp is an atomic expression, we can atomically evaluate exp to get v,
    * and call stepKont(v, store, frame).
    */
  protected def optimizeAtomic(edgeInfos: Set[EdgeInformation[SchemeExp, Abs, Addr]],
                               t: Time): Set[EdgeInformation[SchemeExp, Abs, Addr]] = {
    edgeInfos.flatMap({
      case EdgeInformation(ActionPush(frame, exp, env, store, effects), actions, semFilters) =>
        atomicEval(exp, env, store) match {
          case Some((v, effs, actionR)) =>
            stepKont(v, frame, store, t).map( (edgeInfo: EdgeInformation[SchemeExp, Abs, Addr]) =>
              EdgeInformation(addEffects(edgeInfo.action, effs ++ effects), actionR :: edgeInfo.actions, semFilters) )
          case None =>
            Set(EdgeInformation(ActionPush[SchemeExp, Abs, Addr](frame, exp, env, store, effects), actions, semFilters))
        }
      case edgeInfo =>
        Set[EdgeInformation[SchemeExp, Abs, Addr]](edgeInfo)
    })
  }

  override def stepEval(e: SchemeExp,
                        env: Environment[Addr],
                        store: Store[Addr, Abs],
                        t: Time) =
    optimizeAtomic(super.stepEval(e, env, store, t), t)

  override def stepKont(v: Abs,
                        frame: Frame,
                        store: Store[Addr, Abs],
                        t: Time) =
    optimizeAtomic(super.stepKont(v, frame, store, t), t)
}
