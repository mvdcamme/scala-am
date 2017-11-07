import backend.expression._

/**
  * Basic Scheme semantics, without any optimization
  */
class ConvertableBaseSchemeSemantics[V : IsSchemeLattice, Addr: Address, Time: Timestamp](val primitives: SchemePrimitives[Addr, V])
  extends ConvertableSemantics[SchemeExp, V, Addr, Time] {

  type Env = Environment[Addr]
  type Sto = Store[Addr, V]

  def convertToAbsSemanticsFrame(frame: Frame, ρ: Environment[Addr], vStack: List[Storable[V, Addr]], absSem: ConvertableBaseSchemeSemantics[V, Addr, Time]): (Option[Frame], List[Storable[V, Addr]], Environment[Addr]) =
    (Some(frame), vStack, ρ)

  protected def evalBody(body: List[SchemeExp], env: Environment[Addr], store: Store[Addr, V]): EdgeInfo = body match {
    case Nil => noEdgeInfos(ActionReachedValue[SchemeExp, V, Addr](IsSchemeLattice[V].inject(false), store), List(ActionReachedValueT[SchemeExp, V, Addr](IsSchemeLattice[V].inject(false))))
    case List(exp) => noEdgeInfos(ActionEval(exp, env, store), ActionEvalR[SchemeExp, V, Addr](exp, env))
    case exp :: rest => addPushActionR(ActionPush(FrameBegin(rest, env), exp, env, store))
  }

  def convertAbsInFrame[OtherAbs: IsConvertableLattice](
      frame: ConvertableSchemeFrame[V, Addr, Time],
      convertValue: (V) => OtherAbs,
      convertEnv: (Environment[Addr]) => Environment[Addr],
      abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time])
    : ConvertableSchemeFrame[OtherAbs, Addr, Time] =
    frame.convert(convertValue, convertEnv, abstSem)

  def frameReaches(frame: ConvertableSchemeFrame[V, Addr, Time],
                   valueReaches: V => Set[Addr],
                   envReaches: Environment[Addr] => Set[Addr],
                   addressReaches: Addr => Set[Addr]): Set[Addr] =
    frame.reaches(valueReaches, envReaches, addressReaches)

  def conditional(v: V, t: => EdgeInfos, f: => EdgeInfos): EdgeInfos = {
    def addFilter(x: EdgeInfos, filter: SemanticsFilterAnnotation): EdgeInfos = x.map({
      case EdgeInformation(action, actionTs, semanticsFilters) =>
        EdgeInformation(action, actionTs, semanticsFilters + filter)
    })
    (if (IsSchemeLattice[V].isTrue(v)) addFilter(t, ThenBranchFilter) else Set[EdgeInfo]()) ++
    (if (IsSchemeLattice[V].isFalse(v)) addFilter(f, ElseBranchFilter) else Set[EdgeInfo]())
  }

  protected def addPushDataActionT(currentValue: V,
                                   currentFrame: Frame,
                                   frameGenerator: FrameGenerator[V],
                                   actionGenerator: Frame => ActionPush[SchemeExp, V, Addr]): EdgeInfos = {
    val newFrame = frameGenerator(currentValue, None, currentFrame)
    val currentAction = actionGenerator(newFrame)
    noEdgeInfosSet(currentAction, ActionEvalPushDataR(currentAction.e, currentAction.env, frameGenerator))
  }

  def evalCall(function: V, fexp: SchemeExp, argsv: List[(SchemeExp, V)], store: Store[Addr, V], t: Time): EdgeInfos = {
    val fromClo: EdgeInfos = IsSchemeLattice[V]
      .getClosures[SchemeExp, Addr](function)
      .map({
        case (lambda@(SchemeLambda(args, body, pos)), env1) =>
          val cloCall = ActionClosureCallR[SchemeExp, V, Addr](fexp, lambda, env1)
          if (args.length == argsv.length) {
            val argsZipped: List[(Identifier, (SchemeExp, V))] = args.zip(argsv)
            val updatedArgsZipped: List[(Identifier, SchemeExp, V)] = argsZipped.map({
              case (varName, (exp, value)) => (varName, exp, value)
            })
            bindArgsAndReturnAddresses(updatedArgsZipped, env1, store, t) match {
              case (env2, store, boundAddresses) =>
                val defAddr = ActionDefineAddressesPopR[SchemeExp, V, Addr](boundAddresses.map(_._1))
                val timeTick = ActionTimeTickExpR[SchemeExp, V, Addr](fexp)
                val makeActionRs = (edgeAnnotation: ActionReplay[SchemeExp, V, Addr]) =>
                  List(defAddr, edgeAnnotation, timeTick, cloCall)
                if (body.length == 1) {
                  val action = ActionStepIn[SchemeExp, V, Addr](fexp, (SchemeLambda(args, body, pos), env1), body.head, env2, store, argsv)
                  EdgeInformation(action, makeActionRs(ActionEvalR[SchemeExp, V, Addr](body.head, env2)), Set())
                }
                else {
                  val action = ActionStepIn[SchemeExp, V, Addr](fexp, (SchemeLambda(args, body, pos), env1), SchemeBegin(body, pos), env2, store, argsv)
                  EdgeInformation(action, makeActionRs(ActionEvalR[SchemeExp, V, Addr](SchemeBegin(body, pos),
                    env2)), Set())
                }
            }
          } else {
            val error = ArityError(fexp.toString, args.length, argsv.length)
            val actionError = ActionErrorT[SchemeExp, V, Addr](error)
            EdgeInformation(ActionError[SchemeExp, V, Addr](error), List(actionError, cloCall), Set())
          }
        case (lambda, env) =>
          val cloCall = ActionClosureCallR[SchemeExp, V, Addr](fexp, lambda, env)
          val error = TypeError(lambda.toString, "operator", "closure", "not a closure")
          val actionError = ActionErrorT[SchemeExp, V, Addr](error)
          noEdgeInfos(ActionError[SchemeExp, V, Addr](error), List(actionError, cloCall))
      })
    val fromPrim: EdgeInfos = IsSchemeLattice[V].getPrimitives[Addr, V](function).flatMap( (prim) => {
      val n = argsv.size + 1 // Number of values to pop: all arguments + the operator
      val applyPrim = ActionPrimCallT[SchemeExp, V, Addr](n, fexp, argsv.map(_._1), IsSchemeLattice[V].inject(prim))
      prim.call(fexp, argsv, store, t).collect[EdgeInformation[SchemeExp, V, Addr]]({
        case (res, store2, effects) =>
          val action = ActionReachedValue[SchemeExp, V, Addr](res, store2, effects)
          Set(EdgeInformation[SchemeExp, V, Addr](action, List(applyPrim), Set()))
      },
        err => {
          val actionError = ActionErrorT[SchemeExp, V, Addr](err)
          Set(EdgeInformation[SchemeExp, V, Addr](ActionError[SchemeExp, V, Addr](err), List(actionError), Set()))
        })
    })
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      simpleActionSet(ActionError[SchemeExp, V, Addr](TypeError(function.toString, "operator", "function", "not a function")))
    } else {
      (fromClo ++ fromPrim)
    }
  }

  protected def evalValue(v: SExpValueType): Option[V] = v match {
    case ValueString(s) => Some(IsSchemeLattice[V].inject(s))
    case ValueInteger(n) => Some(IsSchemeLattice[V].inject(n))
    case ValueReal(n) => Some(IsSchemeLattice[V].inject(n))
    case ValueBoolean(b) => Some(IsSchemeLattice[V].inject(b))
    case _ => None
  }

  protected def funcallArgs(f: V,
                            currentFrame: Frame,
                            fexp: SchemeExp,
                            args: List[(SchemeExp, V)],
                            toeval: List[SchemeExp],
                            env: Environment[Addr],
                            store: Store[Addr, V],
                            t: Time,
                            currentValue: V,
                            frameGeneratorGenerator: (SchemeExp, List[SchemeExp]) => FrameGenerator[V]): EdgeInfos =
    toeval match {
      case Nil => evalCall(f, fexp, args.reverse, store, t)
      case e :: rest =>
        val frameGenerator = frameGeneratorGenerator(e, rest)
        val actionGenerator = (frame: Frame) => ActionPush(frame, e, env, store)
        addPushDataActionT(currentValue, currentFrame, frameGenerator, actionGenerator)
    }

  case class PlaceOperatorFuncallOperandsFrameGenerator(fexp: SchemeExp, e: SchemeExp, args: List[(SchemeExp, V)], rest: List[SchemeExp], env: Environment[Addr])
    extends FrameGenerator[V] {
    def apply(value: V, optConcolicValue: Option[ConcolicExpression], frame: Frame): Frame = {
      FrameFuncallOperands(value, fexp, e, args, rest, env)
    }
  }

  case class LetFrameGenerator(variable: Identifier,
                               frameVariable: Identifier,
                               bindings: List[(Identifier, V)],
                               toeval: List[(Identifier, SchemeExp)],
                               body: List[SchemeExp],
                               env: Environment[Addr])
    extends FrameGenerator[V] {
    def apply(value: V, optConcolicValue: Option[ConcolicExpression], other: Frame): Frame = {
      FrameLet(variable, (frameVariable, value) :: other.asInstanceOf[FrameLet[V, Addr, Time]].bindings, toeval, body, env)
    }
  }

  case class PlaceOperandFuncallOperandsFrameGenerator(f: V,
                                                       fexp: SchemeExp,
                                                       e: SchemeExp,
                                                       cur: SchemeExp,
                                                       args: List[(SchemeExp, V)],
                                                       rest: List[SchemeExp],
                                                       env: Environment[Addr])
    extends FrameGenerator[V] {
    def apply(value: V, optConcolicValue: Option[ConcolicExpression], other: Frame): Frame = {
      FrameFuncallOperands(f, fexp, e, (cur, value) :: other.asInstanceOf[FrameFuncallOperands[V, Addr, Time]].args, rest, env)
    }
  }

  /*
   * To be called after popping a FrameFuncallOperator continuation.
   */
  protected def funcallArgs(f: V, currentFrame: Frame, fexp: SchemeExp, args: List[SchemeExp], env: Environment[Addr],
                            store: Store[Addr, V], t: Time): EdgeInfos = {
    /*
     * As this function is called after popping a FrameFuncallOperator, the value that has just been
     * evaluated equals the operator-value.
     */
    val currentValue = f
    args match {
      case Nil => evalCall(f, fexp, List(), store, t)
      case e :: rest =>
        val frameGenerator = PlaceOperatorFuncallOperandsFrameGenerator(fexp, e, Nil, rest, env)
        val actionGenerator = (frame: Frame) => ActionPush(frame, e, env, store)
        addPushDataActionT(currentValue, currentFrame, frameGenerator, actionGenerator)
    }
  }

  protected def evalQuoted(exp: SExp, store: Store[Addr, V], t: Time): (V, Store[Addr, V], List[StoreChangeSemantics[V, Addr]]) = exp
  match {
    case SExpId(Identifier(sym, _)) => (IsSchemeLattice[V].injectSymbol(sym), store, Nil)
    case SExpPair(car, cdr, _) => {
      val care: SchemeExp = SchemeVar(Identifier(car.toString, car.pos))
      val cdre: SchemeExp = SchemeVar(Identifier(cdr.toString, cdr.pos))
      val cara = Address[Addr].cell(care, t)
      val (carv, store2, stateChangesCar) = evalQuoted(car, store, t)
      val cdra = Address[Addr].cell(cdre, t)
      val (cdrv, store3, stateChangesCdr) = evalQuoted(cdr, store2, t)
      (IsSchemeLattice[V].cons(cara, cdra), store3.extend(cara, carv).extend(cdra, cdrv),
       List(StoreExtendSemantics[V, Addr](cara, carv), StoreExtendSemantics[V, Addr](cdra, cdrv)) ++
         stateChangesCar ++
         stateChangesCdr)
    }
    case SExpValue(v, _) =>
      (v match {
        case ValueString(str) => IsSchemeLattice[V].inject(str)
        case ValueCharacter(c) => IsSchemeLattice[V].inject(c)
        case ValueSymbol(sym) => IsSchemeLattice[V].injectSymbol(sym) /* shouldn't happen */
        case ValueInteger(n) => IsSchemeLattice[V].inject(n)
        case ValueReal(n) => IsSchemeLattice[V].inject(n)
        case ValueBoolean(b) => IsSchemeLattice[V].inject(b)
        case ValueNil => IsSchemeLattice[V].nil
      }, store, Nil)
    case SExpQuoted(q, pos) =>
      evalQuoted(SExpPair(SExpId(Identifier("quote", pos)),
                          SExpPair(q, SExpValue(ValueNil, pos), pos),
                          pos),
                 store,
                 t)
  }

  def stepEval(e: SchemeExp,
               env: Environment[Addr],
               store: Store[Addr, V],
               t: Time): EdgeInfos = e match { // Cases in stepEval shouldn't generate any splits in abstract graph
    case λ: SchemeLambda =>
      val action = ActionReachedValue[SchemeExp, V, Addr](IsSchemeLattice[V].inject[SchemeExp, Addr]((λ, env)), store)
      val actionEdge = List(ActionCreateClosureT[SchemeExp, V, Addr](λ, Some(env)))
      noEdgeInfosSet(action, actionEdge)
    case SchemeFuncall(f, args, _) =>
      addPushActionRSet(ActionPush[SchemeExp, V, Addr](FrameFuncallOperator[V, Addr, Time](f, args, env), f, env, store))
    case e @ SchemeIf(cond, cons, alt, _) =>
      addPushActionRSet(ActionPush(FrameIf[V, Addr, Time](cons, alt, env, e), cond, env, store))
    case SchemeLet(Nil, body, _) =>
      Set(evalBody(body, env, store))
    case SchemeLet((v, exp) :: bindings, body, _) =>
      addPushActionRSet(ActionPush(FrameLet[V, Addr, Time](v, List(), bindings, body, env), exp, env, store))
    case SchemeLetStar(Nil, body, _) =>
      Set(evalBody(body, env, store))
    case SchemeLetStar((v, exp) :: bindings, body, _) =>
      addPushActionRSet(ActionPush(FrameLetStar[V, Addr, Time](v, bindings, body, env), exp, env, store))
    case SchemeLetrec(Nil, body, _) =>
      Set(evalBody(body, env, store))
    case SchemeLetrec((v, exp) :: bindings, body, _) =>
      case class ValuesToUpdate(env: Environment[Addr],
                                store: Store[Addr, V],
                                stateChanges: List[StoreChangeSemantics[V, Addr]])
      type X = (ValuesToUpdate, (String, Addr))
      val variables = v :: bindings.map(_._1)
      val addresses = variables.map(v => Address[Addr].variable(v, IsSchemeLattice[V].bottom, t))
      val addressesVariables: List[(Addr, Identifier)] = addresses.zip(variables)
      val initial = ValuesToUpdate(env, store, Nil)
      val ValuesToUpdate(env1, store1, stateChanges) = variables.zip(addresses).foldLeft[ValuesToUpdate](initial)({
        case (ValuesToUpdate(env, store, storeChanges), (v, a)) =>
          ValuesToUpdate(env.extend(v.name, a), store.extend(a, IsSchemeLattice[V].bottom), StoreExtendSemantics[V, Addr](a, IsSchemeLattice[V].bottom) :: storeChanges)
      })
      val action = ActionPush(FrameLetrec(addresses.head, variables.head, addressesVariables.tail.zip(bindings.map(_._2)).map( (binding) => (binding._1._1, binding._1._2, binding._2) ), body, env1),
                              exp, env1, store1)
      val actionEdge = ActionAllocAddressesR[SchemeExp, V, Addr](addresses)
      noEdgeInfosSet(action, List(actionEdge, ActionEvalPushR[SchemeExp, V, Addr](exp, action.env, action.frame)))
    case SchemeSet(variable, exp, _) =>
      addPushActionRSet(ActionPush(FrameSet(variable, env), exp, env, store))
    case SchemeBegin(body, _) =>
      Set(evalBody(body, env, store))
    case SchemeCond(Nil, _) =>
      simpleActionSet(ActionError[SchemeExp, V, Addr](NotSupported("cond without clauses")))
    case SchemeCond((cond, cons) :: clauses, _) =>
      addPushActionRSet(ActionPush(FrameCond(cons, clauses, env), cond, env, store))
    case SchemeCase(key, clauses, default, _) =>
      addPushActionRSet(ActionPush(FrameCase(clauses, default, env), key, env, store))
    case SchemeAnd(Nil, _) =>
      val valueTrue = IsSchemeLattice[V].inject(true)
      noEdgeInfosSet(ActionReachedValue[SchemeExp, V, Addr](valueTrue, store), List(ActionReachedValueT[SchemeExp, V, Addr](valueTrue)))
    case SchemeAnd(exp :: exps, _) =>
      addPushActionRSet(ActionPush(FrameAnd(exps, env), exp, env, store))
    case SchemeOr(Nil, _) =>
      val valueFalse = IsSchemeLattice[V].inject(false)
      noEdgeInfosSet(ActionReachedValue[SchemeExp, V, Addr](valueFalse, store), List(ActionReachedValueT[SchemeExp, V, Addr](valueFalse)))
    case SchemeOr(exp :: exps, _) =>
      addPushActionRSet(ActionPush(FrameOr(exps, env), exp, env, store))
    case SchemeDefineVariable(name, exp, _) =>
      addPushActionRSet(ActionPush(FrameDefine(name, env), exp, env, store))
    case SchemeDefineFunction(name, args, body, pos) =>
      val a = Address[Addr].variable(name, IsSchemeLattice[V].bottom, t)
      val lambda = SchemeLambda(args, body, pos)
      val v = IsSchemeLattice[V].inject[SchemeExp, Addr]((lambda, env))
      val action = ActionReachedValue[SchemeExp, V, Addr](v, store)
      val actionEdges = List(ActionCreateClosureT[SchemeExp, V, Addr](lambda, Some(env)),
                             ActionDefineAddressesR[SchemeExp, V, Addr](List(a)))
      noEdgeInfosSet(action, actionEdges)
    case SchemeVar(variable) =>
      env.lookup(variable.name) match {
        case Some(a) =>
          store.lookup(a) match {
            case Some(v) =>
              val action = ActionReachedValue[SchemeExp, V, Addr](v, store, Set(EffectReadVariable(a)))
              noEdgeInfosSet(action, List(ActionLookupAddressR[SchemeExp, V, Addr](a)))
            case None =>
              simpleActionSet(ActionError[SchemeExp, V, Addr](UnboundAddress(a.toString)))
          }
        case None => simpleActionSet(ActionError[SchemeExp, V, Addr](UnboundVariable(variable)))
      }
    case SchemeQuoted(quoted, _) =>
      evalQuoted(quoted, store, t) match {
        case (value, store2, storeChanges) =>
          val action = ActionReachedValue[SchemeExp, V, Addr](value, store2)
          val actionEdges = List(ActionReachedValueT[SchemeExp, V, Addr](value, storeChanges = storeChanges))
          noEdgeInfosSet(action, actionEdges)
      }
    case SchemeValue(v, _) =>
      evalValue(v) match {
        case Some(v) =>
          noEdgeInfosSet(ActionReachedValue[SchemeExp, V, Addr](v, store), List(ActionReachedValueT[SchemeExp, V, Addr](v)))
        case None => simpleActionSet(ActionError[SchemeExp, V, Addr](NotSupported(s"Unhandled value: $v")))
      }
  }

  def stepKont(v: V, frame: Frame, store: Store[Addr, V], t: Time): EdgeInfos =
    frame match {
      case frame: FrameFuncallOperator[V, Addr, Time] =>
        funcallArgs(v, frame, frame.fexp, frame.args, frame.env, store, t)
      case frame: FrameFuncallOperands[V, Addr, Time] =>
        val frameGeneratorGenerator =
          (e: SchemeExp, rest: List[SchemeExp]) =>
            PlaceOperandFuncallOperandsFrameGenerator(frame.f, frame.fexp, e, frame.cur, frame.args, rest, frame.env)
        funcallArgs(frame.f, frame, frame.fexp, (frame.cur, v) :: frame.args, frame.toeval, frame.env, store, t, v, frameGeneratorGenerator)
      case frame: FrameIf[V, Addr, Time] =>
        conditional(v, addEvalActionTSet(ActionEval(frame.cons, frame.env, store)), addEvalActionTSet(ActionEval(frame.alt, frame.env, store)))
      case frame: FrameLet[V, Addr, Time] =>
        frame.toeval match {
          case Nil =>
            val variables = frame.variable :: frame.bindings.reverse.map(_._1)
            val addresses = variables.map(variable => Address[Addr].variable(variable, v, t))
            val (env1, store1) = ((frame.variable, v) :: frame.bindings).zip(addresses).foldLeft((frame.env, store))({
                case ((env, store), ((variable, value), a)) => (env.extend(variable.name, a), store.extend(a, value))
            })
            val EdgeInformation(action, actionEdges, filters) = evalBody(frame.body, env1, store1)
            Set(EdgeInformation(action, ActionDefineAddressesPopR[SchemeExp, V, Addr](addresses) :: actionEdges, filters))
          case (variable, e) :: toeval =>
            val newFrameGenerator = LetFrameGenerator(variable, frame.variable, frame.bindings, toeval, frame.body, frame.env)
            val actionGenerator = (currentFrame: Frame) => ActionPush(currentFrame, e, frame.env, store)
            addPushDataActionT(v, frame, newFrameGenerator, actionGenerator)
        }
      case frame: FrameLetStar[V, Addr, Time] =>
        val a = Address[Addr].variable(frame.variable, IsSchemeLattice[V].bottom, t)
        val env1 = frame.env.extend(frame.variable.name, a)
        val store1 = store.extend(a, v)
        val actionEdges = List(ActionDefineAddressesPopR[SchemeExp, V, Addr](List(a)))
        val variable = frame.variable
        frame.bindings match {
          case Nil =>
            val EdgeInformation(actions, actionEdges2, edgeInfos) = evalBody(frame.body, env1, store1)
            Set[EdgeInfo](EdgeInformation(actions, actionEdges ++ actionEdges2, edgeInfos))
          case (variable, exp) :: rest =>
            val action = ActionPush[SchemeExp, V, Addr](FrameLetStar(variable, rest, frame.body, env1), exp, env1, store1)
            noEdgeInfosSet(action, actionEdges :+ ActionEvalPushR[SchemeExp, V, Addr](exp, action.env, action.frame))
        }
      case frame: FrameLetrec[V, Addr, Time] =>
        val variable = frame.variable
        frame.bindings match {
          case Nil =>
            // If frame defines an input variable, and if a value is bound to this input variable, use that bound value
            val EdgeInformation(action, actionEdges, edgeInfos) = evalBody(frame.body, frame.env, store.update(frame.addr, v))
            Set(EdgeInformation(action, ActionSetAddressR[SchemeExp, V, Addr](frame.addr) :: actionEdges, edgeInfos))
          case (a1, varName, exp) :: rest =>
            val action = ActionPush(FrameLetrec(a1, varName, rest, frame.body, frame.env), exp, frame.env, store.update(frame.addr, v))
            val actionEdges = List[ActionReplay[SchemeExp, V, Addr]](ActionSetAddressR[SchemeExp, V, Addr](frame.addr),
                                   ActionEvalPushR[SchemeExp, V, Addr](exp, action.env, action.frame))
            noEdgeInfosSet(action, actionEdges)
        }
      case frame: FrameSet[V, Addr, Time] =>
        val variable = frame.variable
        frame.env.lookup(frame.variable.name) match {
          case Some(a) =>
            val valueFalse = IsSchemeLattice[V].inject(false)
            val actionEdges = List(ActionSetAddressR[SchemeExp, V, Addr](a),
                                   ActionReachedValueT[SchemeExp, V, Addr](valueFalse))
            noEdgeInfosSet(ActionReachedValue[SchemeExp, V, Addr](valueFalse, store.update(a, v), Set(EffectWriteVariable(a))),
                           actionEdges)
          case None =>
            simpleActionSet(ActionError[SchemeExp, V, Addr](UnboundVariable(frame.variable)))
        }
      case frame: FrameBegin[V, Addr, Time] => frame.rest match {
        case List(SchemePopSymEnv(_)) =>
          ScalaAMReporter.popEnvironment()
          val action = ActionReachedValue[SchemeExp, V, Addr](v, store)
          val actionR = ActionReachedValueT[SchemeExp, V, Addr](v)
          noEdgeInfosSet(action, actionR)
        case _ => Set(evalBody(frame.rest, frame.env, store))
      }
      case frame: FrameCond[V, Addr, Time] =>
        val falseValue = IsSchemeLattice[V].inject(false)
        conditional(v,
                    if (frame.cons.isEmpty) {
                      noEdgeInfosSet(ActionReachedValue[SchemeExp, V, Addr](v, store),
                                  List(ActionReachedValueT[SchemeExp, V, Addr](v)))
                    } else {
                      Set(evalBody(frame.cons, frame.env, store))
                    },
                    frame.clauses match {
                      case Nil =>
                        noEdgeInfosSet(ActionReachedValue[SchemeExp, V, Addr](falseValue, store),
                                    List(ActionReachedValueT[SchemeExp, V, Addr](falseValue)))
                      case (exp, cons2) :: rest =>
                        addPushActionRSet(ActionPush(FrameCond(cons2, rest, frame.env), exp, frame.env, store))
                    })
      case frame: FrameCase[V, Addr, Time] =>
        val fromClauses = frame.clauses.flatMap({
          case (values, body) =>
            if (values.exists(v2 =>
              evalValue(v2.value) match {
                case None => false
                case Some(v2) => IsSchemeLattice[V].subsumes(v, v2)
              }))
            /* TODO: precision could be improved by restricting v to v2 */
              Set(evalBody(body, frame.env, store))
            else
              Set[EdgeInformation[SchemeExp, V, Addr]]()
        })
        /* TODO: precision could be improved in cases where we know that default is not reachable */
        fromClauses.toSet + evalBody(frame.default, frame.env, store)
      case frame: FrameAnd[V, Addr, Time] => frame.rest match {
        case Nil =>
          val falseValue = IsSchemeLattice[V].inject(false)
          conditional(v,
                      noEdgeInfosSet(ActionReachedValue[SchemeExp, V, Addr](v, store), List(ActionReachedValueT[SchemeExp, V, Addr](v))),
                      noEdgeInfosSet(ActionReachedValue[SchemeExp, V, Addr](falseValue, store), List(ActionReachedValueT[SchemeExp, V, Addr](falseValue))))
        case e :: rest =>
          val falseValue = IsSchemeLattice[V].inject(false)
          conditional(v,
                      addPushActionRSet(ActionPush(FrameAnd(rest, frame.env), e, frame.env, store)),
                      noEdgeInfosSet(ActionReachedValue[SchemeExp, V, Addr](falseValue, store), List(ActionReachedValueT[SchemeExp, V, Addr](falseValue))))
      }
      case frame: FrameOr[V, Addr, Time] => frame.rest match {
        case Nil =>
          val falseValue = IsSchemeLattice[V].inject(false)
          conditional(v,
            noEdgeInfosSet(ActionReachedValue[SchemeExp, V, Addr](v, store), List(ActionReachedValueT[SchemeExp, V, Addr](v))),
            noEdgeInfosSet(ActionReachedValue[SchemeExp, V, Addr](falseValue, store), List(ActionReachedValueT[SchemeExp, V, Addr](falseValue))))
        case e :: rest =>
          conditional(v,
            noEdgeInfosSet(ActionReachedValue[SchemeExp, V, Addr](v, store), List(ActionReachedValueT[SchemeExp, V, Addr](v))),
            addPushActionRSet(ActionPush(FrameOr(rest, frame.env), e, frame.env, store)))
      }
      case frame: FrameDefine[V, Addr, Time] =>
        throw new Exception("TODO: define not handled (no global environment)")
    }

  def parse(program: String): SchemeExp = Scheme.parse(program)
  override def initialBindings = primitives.bindings
}