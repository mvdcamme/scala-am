import SchemeOps._

/**
  * Basic Scheme semantics, without any optimization
  */
class BaseSchemeSemantics[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
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
                               frameGenerator: Abs => Frame,
                               actionGenerator: Frame => ActionPush[SchemeExp, Abs, Addr]):
  Set[EdgeInformation[SchemeExp, Abs, Addr]] = {
    val currentFrame = frameGenerator(currentValue)
    val currentAction = actionGenerator(currentFrame)
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
          val cloCall =  ActionClosureCallR[SchemeExp, Abs, Addr](fexp, function, lambda)
          if (args.length == argsv.length) {
            bindArgs(args.zip(argsv), env1, store, t) match {
              case (env2, store, boundAddresses) =>
                val defAddr = ActionDefineAddressesPopR[SchemeExp, Abs, Addr](boundAddresses.map(_._1))
                val timeTick = ActionTimeTickExpR[SchemeExp, Abs, Addr](fexp)
                val makeActionRs = (edgeAnnotation: ActionReplay[SchemeExp, Abs, Addr]) => List(defAddr, edgeAnnotation, cloCall, timeTick)
                if (body.length == 1) {
                  val action = ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body, pos), env1), body.head, env2, store, argsv)
                  noEdgeInfos(action, makeActionRs(ActionEvalR(body.head, env2)))
                }
                else {
                  val action = ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body, pos), env1), SchemeBegin(body, pos), env2, store, argsv)
                  noEdgeInfos(action, makeActionRs(ActionEvalR[SchemeExp, Abs, Addr](SchemeBegin(body, pos), env2)))
                }
            }
          } else {
            val error = ArityError(fexp.toString, args.length, argsv.length)
            val actionError = ActionErrorT[SchemeExp, Abs, Addr](error)
            noEdgeInfos(ActionError[SchemeExp, Abs, Addr](error), List(actionError, cloCall))
          }
        case (lambda, _) =>
          val error = TypeError(lambda.toString, "operator", "closure", "not a closure")
          val actionError = ActionErrorT[SchemeExp, Abs, Addr](error)
          noEdgeInfos(ActionError[SchemeExp, Abs, Addr](error), List(actionError))
      })
    /* TODO take into account store changes made by the application of the primitives */
    val fromPrim = sabs
      .getPrimitives[Addr, Abs](function)
      .flatMap(
        prim =>
          prim
            .call(fexp, argsv, store, t)
            .collect({
                       case (res, store2, effects) =>
                         val n = argsv.size + 1 // Number of values to pop: all arguments + the operator
                         val applyPrim = ActionPrimCallT[SchemeExp, Abs, Addr](argsv.size + 1, fexp, argsv.map(_._1))
                         val action = ActionReachedValue[SchemeExp, Abs, Addr](res, store2, effects)
                         noEdgeInfosSet(action, List(applyPrim))
                     },
                     err =>
                       simpleAction(ActionError[SchemeExp, Abs, Addr](err))))
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      simpleAction(ActionError[SchemeExp, Abs, Addr](TypeError(function.toString, "operator", "function", "not a function")))
    } else {
      fromClo ++ fromPrim
    }
  }

  protected def evalValue(v: Value): Option[Abs] = v match {
    case ValueString(s) => Some(sabs.inject(s))
    case ValueInteger(n) => Some(sabs.inject(n))
    case ValueFloat(n) => Some(sabs.inject(n))
    case ValueBoolean(b) => Some(sabs.inject(b))
    case _ => None
  }

  protected def funcallArgs(f: Abs,
                            fexp: SchemeExp,
                            args: List[(SchemeExp, Abs)],
                            toeval: List[SchemeExp],
                            env: Environment[Addr],
                            store: Store[Addr, Abs],
                            t: Time,
                            currentValue: Abs,
                            frameGeneratorGenerator: (SchemeExp, List[SchemeExp]) => Abs => Frame): Set[EdgeInformation[SchemeExp, Abs, Addr]] =
    toeval match {
      case Nil =>
        evalCall(f, fexp, args.reverse, store, t)
      case e :: rest =>
        // FrameFuncallOperands(f, fexp, e, args, rest, env)
        val frameGenerator = frameGeneratorGenerator(e, rest)
        val actionGenerator = (frame: Frame) => ActionPush(frame, e, env, store)
        addPushDataActionT(currentValue, frameGenerator, actionGenerator)
    }

  /*
   * To be called after popping a FrameFuncallOperator continuation.
   */
  protected def funcallArgs(f: Abs,
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
        val frameGenerator = (value: Abs) => FrameFuncallOperands(value, fexp, e, Nil, rest, env)
        val frame = frameGenerator(currentValue)
        val actionGenerator = (frame: Frame) => ActionPush(frame, e, env, store)
        addPushDataActionT(currentValue, frameGenerator, actionGenerator)
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
    case λ: SchemeLambda =>
      val action = ActionReachedValue[SchemeExp, Abs, Addr](sabs.inject[SchemeExp, Addr]((λ, env)), store)
      val actionEdge = List(ActionCreateClosureT[SchemeExp, Abs, Addr](λ, Some(env)))
      noEdgeInfosSet(action, actionEdge)
    case SchemeFuncall(f, args, _) =>
      addPushActionRSet(ActionPush[SchemeExp, Abs, Addr](FrameFuncallOperator(f, args, env), f, env, store))
    case SchemeIf(cond, cons, alt, _) =>
      addPushActionRSet(ActionPush(FrameIf(cons, alt, env), cond, env, store))
    case SchemeLet(Nil, body, _) =>
      Set(evalBody(body, env, store))
    case SchemeLet((v, exp) :: bindings, body, _) =>
      addPushActionRSet(ActionPush(FrameLet(v, List(), bindings, body, env), exp, env, store))
    case SchemeLetStar(Nil, body, _) =>
      Set(evalBody(body, env, store))
    case SchemeLetStar((v, exp) :: bindings, body, _) =>
      addPushActionRSet(ActionPush(FrameLetStar(v, bindings, body, env), exp, env, store))
    case SchemeLetrec(Nil, body, _) =>
      Set(evalBody(body, env, store))
    case SchemeLetrec((v, exp) :: bindings, body, _) =>
      case class ValuesToUpdate(env: Environment[Addr],
                                store: Store[Addr, Abs],
                                stateChanges: List[StoreChangeSemantics[Abs, Addr]])
      type X = (ValuesToUpdate, (String, Addr))
      val variables = v :: bindings.map(_._1)
      val addresses = variables.map(v => addr.variable(v, abs.bottom, t))
      val initial = ValuesToUpdate(env, store, Nil)
      val ValuesToUpdate(env1, store1, stateChanges) = variables
        .zip(addresses)
        .foldLeft[ValuesToUpdate](initial)({
        case (ValuesToUpdate(env, store, storeChanges), (v, a)) =>
          ValuesToUpdate(env.extend(v, a),
                         store.extend(a, abs.bottom),
                         StoreExtendSemantics[Abs, Addr](a, abs.bottom) :: storeChanges)
      })
      val action = ActionPush(FrameLetrec(addresses.head, addresses.tail.zip(bindings.map(_._2)), body, env1),
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
      addPushActionRSet(ActionPush(FrameDefine(name, env), exp, env, store))
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
            case None => simpleAction(ActionError[SchemeExp, Abs, Addr](UnboundAddress(a.toString)))
          }
        case None => simpleAction(ActionError[SchemeExp, Abs, Addr](UnboundVariable(name)))
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

  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time) =
    frame match {
      case frame: FrameFuncallOperator[Abs, Addr, Time] =>
        funcallArgs(v, frame.fexp, frame.args, frame.env, store, t)
      case frame: FrameFuncallOperands[Abs, Addr, Time] =>
        val frameGeneratorGenerator =
          (e: SchemeExp, rest: List[SchemeExp]) =>
            (value: Abs) =>
              FrameFuncallOperands(frame.f, frame.fexp, e, (frame.cur, value) :: frame.args, rest, frame.env)
        funcallArgs(frame.f, frame.fexp, (frame.cur, v) :: frame.args, frame.toeval, frame.env, store, t, v, frameGeneratorGenerator)
      case frame: FrameIf[Abs, Addr, Time] =>
        conditional(v, addEvalActionT(ActionEval(frame.cons, frame.env, store)), addEvalActionT(ActionEval(frame.alt, frame.env, store)))
      case frame: FrameLet[Abs, Addr, Time] => frame.toeval match {
        case Nil =>
          val variables = frame.variable :: frame.bindings.reverse.map(_._1)
          val addresses = variables.map(variable => addr.variable(variable, v, t))
          val (env1, store1) = ((frame.variable, v) :: frame.bindings)
            .zip(addresses)
            .foldLeft((frame.env, store))({
              case ((env, store), ((variable, value), a)) =>
                (env.extend(variable, a), store.extend(a, value))
            })
          val EdgeInformation(action, actionEdges, filters) = evalBody(frame.body, env1, store1)
          Set(EdgeInformation(action, ActionDefineAddressesPopR[SchemeExp, Abs, Addr](addresses) :: actionEdges, filters))
        case (variable, e) :: toeval =>
          val newFrameGenerator =
            (value: Abs) => FrameLet(variable, (frame.variable, value) :: frame.bindings, toeval, frame.body, frame.env)
          val actionGenerator = (currentFrame: Frame) => ActionPush(currentFrame, e, frame.env, store)
          addPushDataActionT(v, newFrameGenerator, actionGenerator)
      }
      case frame: FrameLetStar[Abs, Addr, Time] =>
        val a = addr.variable(frame.variable, abs.bottom, t)
        val env1 = frame.env.extend(frame.variable, a)
        val store1 = store.extend(a, v)
        val actionEdges = List(ActionDefineAddressesPopR[SchemeExp, Abs, Addr](List(a)))
        frame.bindings match {
          case Nil =>
            val EdgeInformation(actions, actionEdges2, edgeInfos) = evalBody(frame.body, env1, store1)
            Set(EdgeInformation(actions, actionEdges ++ actionEdges2, edgeInfos))
          case (variable, exp) :: rest =>
            val action = ActionPush(FrameLetStar(variable, rest, frame.body, env1), exp, env1, store1)
            noEdgeInfosSet(action, actionEdges :+ ActionEvalPushR(exp, action.env, action.frame))
        }
      case frame: FrameLetrec[Abs, Addr, Time] => frame.bindings match {
        case Nil =>
          val EdgeInformation(action, actionEdges, edgeInfos) = evalBody(frame.body, frame.env, store.update(frame.addr, v))
          Set(EdgeInformation(action, ActionSetAddressR[SchemeExp, Abs, Addr](frame.addr) :: actionEdges, edgeInfos))
        case (a1, exp) :: rest =>
          val action = ActionPush(FrameLetrec(a1, rest, frame.body, frame.env), exp, frame.env, store.update(frame.addr, v))
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
          case None => simpleAction(ActionError[SchemeExp, Abs, Addr](UnboundVariable(frame.variable)))
        }
      case frame: FrameBegin[Abs, Addr, Time] =>
        Set(evalBody(frame.rest, frame.env, store))
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
class SchemeSemantics[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    primitives: SchemePrimitives[Addr, Abs])
    extends BaseSchemeSemantics[Abs, Addr, Time](primitives) {

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

/*
class ConcurrentSchemeSemantics[Abs : ConcurrentSchemeLattice, Addr : Address, Time : Timestamp, TID : ThreadIdentifier](primitives: Primitives[Addr, Abs])
    extends SchemeSemantics[Abs, Addr, Time](primitives: Primitives[Addr, Abs]) {
  def cabs = implicitly[ConcurrentSchemeLattice[Abs]]
  def aabs = implicitly[AbstractValue[Abs]]
  def thread = implicitly[ThreadIdentifier[TID]]

  case class FrameJoin(env: Environment[Addr]) extends SchemeFrame
  case class FrameCasIndex(variable: String, eold: SchemeExp, enew: SchemeExp, env: Environment[Addr]) extends SchemeFrame
  case class FrameCasOld(variable: String, index: Option[Abs], enew: SchemeExp, env: Environment[Addr]) extends SchemeFrame
  case class FrameCasNew(variable: String, index: Option[Abs], enew: SchemeExp, old: Abs, env: Environment[Addr]) extends SchemeFrame
  case class FrameAcquire(env: Environment[Addr]) extends SchemeFrame
  case class FrameRelease(env: Environment[Addr]) extends SchemeFrame

  override def addEffects(action: Action[SchemeExp, Abs, Addr], effects: Set[Effect[Addr]]) = action match {
    case ActionSpawn(t: TID @unchecked, e, env, act, effs) => ActionSpawn(t, e, env, act, effs ++ effects)
    case ActionJoin(tid, store, effs) => ActionJoin(tid, store, effs ++ effects)
    case _ => super.addEffects(action, effects)
  }

  override def stepEval(e: SchemeExp, env: Environment[Addr], store: Store[Addr, Abs], t: Time) = e match {
    case SchemeSpawn(exp, _) =>
      val tid = thread.thread[SchemeExp, Time](exp, t)
      Set(ActionSpawn(tid, exp, env, ActionReachedValue(cabs.injectTid(tid), store)))
    case SchemeJoin(exp, _) => optimizeAtomic(Set(ActionPush(FrameJoin(env), exp, env, store)), t)
    case SchemeCas(variable, eold, enew, _) => Set(ActionPush(FrameCasOld(variable, None, enew, env), eold, env, store))
    case SchemeCasVector(variable, index, eold, enew, _) => Set(ActionPush(FrameCasIndex(variable, eold, enew, env), index, env, store))
    case SchemeAcquire(exp, _) => Set(ActionPush(FrameAcquire(env), exp, env, store))
    case SchemeRelease(exp, _) => Set(ActionPush(FrameRelease(env), exp, env, store))
    case _ => super.stepEval(e, env, store, t)
  }

  override def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time) = frame match {
    case FrameJoin(env) =>
      val tids = cabs.getTids(v)
      if (tids.isEmpty) {
        Set(ActionError(s"join performed on a non-tid value: $v"))
      } else {
        Set(ActionJoin(v, store))
      }
    case FrameCasIndex(variable, eold, enew, env) =>
      Set(ActionPush(FrameCasOld(variable, Some(v), enew, env), eold, env, store))
    case FrameCasOld(variable, index, enew, env) =>
      Set(ActionPush(FrameCasNew(variable, index, enew, v, env), enew, env, store))
      /* TODO
    case FrameCasNew(variable, index, enew, old, env) =>
      env.lookup(variable) match {
        case Some(a) => index match {
          case Some(i) =>
            /* Compare and swap on vector element */
            aabs.getVectors(store.lookupBot(a)).flatMap(va => {
              val vec = store.lookupBot(va)
              val oldvals = aabs.vectorRef(vec, i)
              oldvals.flatMap({
                case Left(_) => /* ignoring error values */ Set[Action[SchemeExp, Abs, Addr]]()
                case Right(a) => {
                  val oldval = store.lookupBot(a)
                  val success: Action[SchemeExp, Abs, Addr] = {
                    /* Vector element matches old, success */
                    val (newvec, addrs) = aabs.vectorSet(vec, i, addr.cell(enew, t))
                    ActionReachedValue(cabs.inject(true), addrs.foldLeft(store.update(va, newvec))((acc, a) => acc.updateOrExtend(a, v)),
                      addrs.flatMap(a => Set(EffectWriteVector(a), EffectReadVector(a))))
                  }
                  val fail: Action[SchemeExp, Abs, Addr] = ActionReachedValue(cabs.inject(false), store, Set(EffectReadVector(a))) /* Vector element doesn't match, fail */
                  conditional(cabs.binaryOp(Eq)(oldval, old), success, fail)
                }})})
          case None =>
            /* Compare and swap on variable value */
            conditional(cabs.binaryOp(Eq)(store.lookupBot(a), old),
              /* Compare and swap succeeds */
              ActionReachedValue(aabs.inject(true), store.update(a, v), Set(EffectWriteVariable(a), EffectReadVariable(a))),
              /* Compare and swap fails */
              ActionReachedValue(aabs.inject(false), store, Set(EffectReadVariable(a))))
        }
        case None => Set(ActionError(s"Unbound variable: $variable"))
      }
    case FrameAcquire(env) =>
      val locks = cabs.getLocks(v)
      if (locks.isEmpty) {
        Set[Action[SchemeExp, Abs, Addr]](ActionError[SchemeExp, Abs, Addr](s"acquire performed on a non-lock value: $v"))
      } else {
        locks.flatMap(a => {
          val v = store.lookupBot(a)
          if (cabs.isTrue(cabs.unaryOp(IsLock)(v))) {
            if (cabs.isFalse(cabs.unaryOp(IsLocked)(v))) {
              Set[Action[SchemeExp, Abs, Addr]](ActionReachedValue[SchemeExp, Abs, Addr](cabs.inject(true), store.update(a, cabs.lockedValue), Set(EffectAcquire(a))))
            } else {
              Set[Action[SchemeExp, Abs, Addr]]()
            }
          } else {
            Set[Action[SchemeExp, Abs, Addr]](ActionError[SchemeExp, Abs, Addr](s"acquire performed on a non-lock value: $v"))
          }
        })
      }
    case FrameRelease(env) =>
      val locks = cabs.getLocks(v)
      if (locks.isEmpty) {
        Set[Action[SchemeExp, Abs, Addr]](ActionError[SchemeExp, Abs, Addr](s"release performed on a non-lock value: $v"))
      } else {
        cabs.getLocks(v).flatMap(a => {
          val v = store.lookupBot(a)
          if (cabs.isTrue(cabs.unaryOp(IsLock)(v))) {
            if (cabs.isTrue(cabs.unaryOp(IsLocked)(v))) {
              Set[Action[SchemeExp, Abs, Addr]](ActionReachedValue[SchemeExp, Abs, Addr](cabs.inject(true), store.update(a, cabs.unlockedValue), Set(EffectRelease(a))))
            } else {
              /* Lock is already released */
              Set[Action[SchemeExp, Abs, Addr]](ActionReachedValue[SchemeExp, Abs, Addr](cabs.inject(true), store))
            }
          } else {
            Set[Action[SchemeExp, Abs, Addr]](ActionError[SchemeExp, Abs, Addr](s"release performed on a non-lock value: $v"))
          }
        })
      }*/
    case _ => super.stepKont(v, frame, store, t)
  }
}
 */
