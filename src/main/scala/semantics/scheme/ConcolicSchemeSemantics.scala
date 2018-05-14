import backend.expression._
import ConcreteConcreteLattice.{L => ConcreteValue}

import concolic.SymbolicEnvironment

/**
  * Basic Scheme semantics, without any optimization
  */
class ConcolicBaseSchemeSemantics[Addr : Address, Time : Timestamp: CompareTimestampsWithMapping, PCElementUsed](val primitives: Primitives[Addr, ConcreteValue])
  extends ConvertableSemantics[SchemeExp, ConcreteValue, Addr, Time]()(Expression[SchemeExp], ConcreteConcreteLattice.isSchemeLattice, Address[Addr], Timestamp[Time]) {

  implicit val abs: JoinLattice[ConcreteValue] = ConcreteConcreteLattice.isSchemeLattice
  implicit def sabs: IsSchemeLattice[ConcreteValue] = ConcreteConcreteLattice.isSchemeLattice

  override def stepEval(e: SchemeExp,
    env: Environment[Addr],
    store: Store[Addr, ConcreteValue],
    t: Time): Set[EdgeInformation[SchemeExp, ConcreteValue, Addr]] = Set()

  override def stepKont(v: ConcreteValue,
    frame: Frame,
    store: Store[Addr, ConcreteValue],
    t: Time): Set[EdgeInformation[SchemeExp, ConcreteValue, Addr]] = Set()

  protected def evalBody(body: List[SchemeExp], env: Environment[Addr], symEnv: SymbolicEnvironment, store: Store[Addr, ConcreteValue]): EdgeInformation[SchemeExp, ConcreteValue, Addr] = body match {
    case Nil => noEdgeInfos(ActionConcolicReachedValue(ActionReachedValue[SchemeExp, ConcreteValue, Addr](sabs.inject(false), store), None), List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](sabs.inject(false))))
    case List(exp) => noEdgeInfos(ActionConcolicEval(ActionEval(exp, env, store), symEnv), ActionEvalR[SchemeExp, ConcreteValue, Addr](exp, env))
    case exp :: rest =>
      val frame = FrameConcolicBegin[ConcreteValue, Addr, Time](rest, env, symEnv)
      addPushActionR(ActionConcolicPush[SchemeExp, ConcreteValue, Addr](ActionPush[SchemeExp, ConcreteValue, Addr](frame, exp, env, store), frame, symEnv))
  }

  def frameReaches(frame: ConvertableSchemeFrame[ConcreteValue, Addr, Time],
                   valueReaches: ConcreteValue => Reached[Addr],
                   envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
                   addressReaches: Addr => Reached[Addr]): Reached[Addr] =
    frame.reaches(valueReaches, envReaches, addressReaches)

  def conditional(v: ConcreteValue,
                  t: => EdgeInformation[SchemeExp, ConcreteValue, Addr],
                  f: => EdgeInformation[SchemeExp, ConcreteValue, Addr]): EdgeInformation[SchemeExp, ConcreteValue, Addr] = {
    def addFilter(x: EdgeInformation[SchemeExp, ConcreteValue, Addr],
                  filter: SemanticsFilterAnnotation): EdgeInformation[SchemeExp, ConcreteValue, Addr] = x match {
      case EdgeInformation(action, actionTs, semanticsFilters) =>
        EdgeInformation(action, actionTs, semanticsFilters + filter)
    }

    if (sabs.isTrue(v)) addFilter(t, ThenBranchFilter) else addFilter(f, ElseBranchFilter)
  }

  protected def addPushDataActionT(currentValue: ConcreteValue,
                                   optConcolicValue: Option[ConcolicExpression],
                                   currentFrame: SchemeConcolicFrame[ConcreteValue, Addr, Time],
                                   frameGenerator: FrameGenerator[ConcreteValue],
                                   actionGenerator: SchemeConcolicFrame[ConcreteValue, Addr, Time] => ActionConcolicPush[SchemeExp, ConcreteValue, Addr]): EdgeInformation[SchemeExp, ConcreteValue, Addr] = {
    val newFrame = frameGenerator(currentValue, optConcolicValue, currentFrame)
    val currentAction = actionGenerator(newFrame.asInstanceOf[SchemeConcolicFrame[ConcreteValue, Addr, Time]])
    noEdgeInfos(currentAction, ActionEvalPushDataR(currentAction.normalAction.e, currentAction.normalAction.env, frameGenerator))
  }

  def evalCall(function: ConcreteValue,
               fexp: SchemeExp,
               concolicArgsv: List[(SchemeExp, ConcreteValue, Option[ConcolicExpression])],
               store: Store[Addr, ConcreteValue],
               t: Time): EdgeInformation[SchemeExp, ConcreteValue, Addr] = {
    val argsv: List[(SchemeExp, ConcreteValue)] = concolicArgsv.map(tuple => (tuple._1, tuple._2))
    val cargsv: List[(SchemeExp, (ConcreteValue, Option[ConcolicExpression]))] = concolicArgsv.map(tuple => (tuple._1, (tuple._2, tuple._3)))
    val fromClo: Set[EdgeInformation[SchemeExp, ConcreteValue, Addr]] = sabs
      .getClosures[SchemeExp, Addr](function)
      .map({
        case (lambda@(SchemeLambda(args, body, pos)), env1, maybeSymEnv1) =>
          assert(maybeSymEnv1.isDefined)
          val symEnv1 = maybeSymEnv1.get
          val steppedInSymEnv = concolic.pushEnvironment(symEnv1)
          val cloCall = ActionClosureCallR[SchemeExp, ConcreteValue, Addr](fexp, lambda, env1)
          if (args.length == cargsv.length) {
            val argsZipped: List[(Identifier, SchemeExp, ConcreteValue)] = args.zip(argsv).map({
              case (varName, (exp, value)) => (varName, exp, value)
            })
            // Add all arguments that have a concolic expression to the symbolic environment
            args.zip(concolicArgsv).foreach({
              case (arg, (_, _, optConcolicExpression)) =>
                optConcolicExpression.foreach(concolic.addVariable(arg.name, _, steppedInSymEnv))
            })
            bindArgsAndReturnAddresses(argsZipped, env1, store, t) match {
              case (env2, store, boundAddresses) =>
                val defAddr = ActionDefineAddressesPopR[SchemeExp, ConcreteValue, Addr](boundAddresses.map(_._1))
                val timeTick = ActionTimeTickExpR[SchemeExp, ConcreteValue, Addr](fexp)
                val makeActionRs = (edgeAnnotation: ActionReplay[SchemeExp, ConcreteValue, Addr]) => List(defAddr, edgeAnnotation, timeTick, cloCall)
                if (body.length == 1) {
                  val action = ActionConcolicStepIn(ActionStepIn[SchemeExp, ConcreteValue, Addr](fexp, (SchemeLambda(args, body, pos), env1), body.head, env2, store, argsv), steppedInSymEnv)
                  EdgeInformation(action, makeActionRs(ActionEvalR[SchemeExp, ConcreteValue, Addr](body.head, env2)), Set())
                } else {
                  val action = ActionConcolicStepIn(ActionStepIn[SchemeExp, ConcreteValue, Addr](fexp, (SchemeLambda(args, body, pos), env1), SchemeBegin(body, pos), env2, store, argsv), steppedInSymEnv)
                  EdgeInformation(action, makeActionRs(ActionEvalR[SchemeExp, ConcreteValue, Addr](SchemeBegin(body, pos), env2)), Set())
                }
            }
          } else {
            val error = ArityError(fexp.toString, args.length, cargsv.length)
            val actionError = ActionErrorT[SchemeExp, ConcreteValue, Addr](error)
            EdgeInformation(ActionConcolicError(ActionError[SchemeExp, ConcreteValue, Addr](error)), List(actionError, cloCall), Set())
          }
        case (lambda, env, symEnv) =>
          val cloCall = ActionClosureCallR[SchemeExp, ConcreteValue, Addr](fexp, lambda, env)
          val error = TypeError(lambda.toString, "operator", "closure", "not a closure")
          val actionError = ActionErrorT[SchemeExp, ConcreteValue, Addr](error)
          noEdgeInfos(ActionConcolicError(ActionError[SchemeExp, ConcreteValue, Addr](error)), List(actionError, cloCall))
      })
    val fromPrim: EdgeInfos = sabs.getPrimitives[Addr, ConcreteValue](function).flatMap( (prim) => {
      val n = cargsv.size + 1 // Number of values to pop: all arguments + the operator
      val applyPrim = ActionPrimCallT[SchemeExp, ConcreteValue, Addr](n, fexp, cargsv.map(_._1), sabs.inject(prim))
      val (result, symbolicValue) = prim.call(fexp, cargsv, store, t)
      result.collect[EdgeInformation[SchemeExp, ConcreteValue, Addr]]({
        case (res, store2, effects) =>
          val action = ActionConcolicReachedValue(ActionReachedValue[SchemeExp, ConcreteValue, Addr](res, store2, effects), symbolicValue)
          Set(EdgeInformation[SchemeExp, ConcreteValue, Addr](action, List(applyPrim), Set()))
      },
        err => {
          val actionError = ActionErrorT[SchemeExp, ConcreteValue, Addr](err)
          Set(EdgeInformation[SchemeExp, ConcreteValue, Addr](ActionConcolicError(ActionError[SchemeExp, ConcreteValue, Addr](err)), List(actionError), Set()))
        })
    })
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      simpleAction(ActionConcolicError(ActionError[SchemeExp, ConcreteValue, Addr](TypeError(function.toString, "operator", "function", "not a function"))))
    } else {
      val combined = fromClo ++ fromPrim
      assert(combined.size == 1, s"There should only be one result, got $combined instead")
      combined.head
    }
  }

  protected def evalValue(v: SExpValueType): Option[ConcreteValue] = v match {
    case ValueString(s) => Some(sabs.inject(s))
    case ValueInteger(n) => Some(sabs.inject(n))
    case ValueReal(n) => Some(sabs.inject(n))
    case ValueBoolean(b) => Some(sabs.inject(b))
    case _ => None
  }

  protected def funcallArgs(f: ConcreteValue,
                            currentFrame: SchemeConcolicFrame[ConcreteValue, Addr, Time],
                            fexp: SchemeExp,
                            args: List[(SchemeExp, ConcreteValue, Option[ConcolicExpression])],
                            toeval: List[SchemeExp],
                            env: Environment[Addr],
                            symEnv: SymbolicEnvironment,
                            store: Store[Addr, ConcreteValue],
                            t: Time,
                            currentValue: ConcreteValue,
                            optConcolicValue: Option[ConcolicExpression]): EdgeInformation[SchemeExp, ConcreteValue, Addr] =
    toeval match {
      case Nil =>
        evalCall(f, fexp, args.reverse, store, t)
      case e :: rest =>
        val frame = FrameConcolicFuncallOperands(f, fexp, e, args, rest, env, symEnv)
        simpleAction(ActionConcolicPush(ActionPush(frame, e, env, store), frame, symEnv))
    }

  case class PlaceOperatorFuncallOperandsConcolicFrameGenerator(fexp: SchemeExp,
                                                                e: SchemeExp,
                                                                args: List[(SchemeExp, ConcreteValue, Option[ConcolicExpression])],
                                                                rest: List[SchemeExp],
                                                                env: Environment[Addr],
                                                                symEnv: SymbolicEnvironment)
    extends FrameGenerator[ConcreteValue] {
    def apply(value: ConcreteValue, optConcolicValue: Option[ConcolicExpression], frame: Frame): SchemeConcolicFrame[ConcreteValue, Addr, Time] = {
      FrameConcolicFuncallOperands(value, fexp, e, args, rest, env, symEnv)
    }
  }

  case class LetConcolicFrameGenerator(variable: Identifier,
                                       frameVariable: Identifier,
                                       bindings: List[(Identifier, ConcreteValue, Option[ConcolicExpression])],
                                       toeval: List[(Identifier, SchemeExp)],
                                       body: List[SchemeExp],
                                       env: Environment[Addr],
                                       symEnv: SymbolicEnvironment)
    extends FrameGenerator[ConcreteValue] {
    def apply(value: ConcreteValue, optConcolicValue: Option[ConcolicExpression], other: Frame): SchemeConcolicFrame[ConcreteValue, Addr, Time] = other match {
      case other: FrameConcolicLet[ConcreteValue, Addr, Time] =>
        FrameConcolicLet(variable, (frameVariable, value, optConcolicValue) :: other.bindings, toeval, body, env, symEnv)
    }
  }

  /*
   * To be called after popping a FrameConcolicFuncallOperator continuation.
   */
  protected def funcallArgs(f: ConcreteValue,
                            optConcolicValue: Option[ConcolicExpression],
                            currentFrame: SchemeConcolicFrame[ConcreteValue, Addr, Time],
                            fexp: SchemeExp,
                            args: List[SchemeExp],
                            env: Environment[Addr],
                            symEnv: SymbolicEnvironment,
                            store: Store[Addr, ConcreteValue],
                            t: Time): EdgeInformation[SchemeExp, ConcreteValue, Addr] = {
    /*
     * As this function is called after popping a FrameConcolicFuncallOperator, the value that has just been
     * evaluated equals the operator-value.
     */
    val currentValue = f
    args match {
      case Nil => evalCall(f, fexp, List(), store, t)
      case e :: rest =>
        val frameGenerator = PlaceOperatorFuncallOperandsConcolicFrameGenerator(fexp, e, Nil, rest, env, symEnv)
        val actionGenerator = (frame: SchemeConcolicFrame[ConcreteValue, Addr, Time]) => ActionConcolicPush(ActionPush(frame, e, env, store), frame, symEnv)
        addPushDataActionT(currentValue, optConcolicValue, currentFrame, frameGenerator, actionGenerator)
    }
  }

  protected def evalQuoted(exp: SExp,
                           store: Store[Addr, ConcreteValue],
                           t: Time): (ConcreteValue, Store[Addr, ConcreteValue], List[StoreChangeSemantics[ConcreteValue, Addr]]) = exp
  match {
    case SExpId(Identifier(sym, _)) => (sabs.injectSymbol(sym), store, Nil)
    case SExpPair(car, cdr, _) => {
      val care: SchemeExp = SchemeVar(Identifier(car.toString, car.pos))
      val cdre: SchemeExp = SchemeVar(Identifier(cdr.toString, cdr.pos))
      val cara = Address[Addr].cell(care, t)
      val (carv, store2, stateChangesCar) = evalQuoted(car, store, t)
      val cdra = Address[Addr].cell(cdre, t)
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
        case ValueReal(n) => sabs.inject(n)
        case ValueBoolean(b) => sabs.inject(b)
        case ValueNil => sabs.nil
      }, store, Nil)
    case SExpQuoted(q, pos) =>
      evalQuoted(SExpPair(SExpId(Identifier("quote", pos)),
                          SExpPair(q, SExpValue(ValueNil, pos), pos),
                          pos),
                 store,
                 t)
  }

  def stepConcolicEval(e: SchemeExp, env: Environment[Addr], symEnv: SymbolicEnvironment,
                       store: Store[Addr, ConcreteValue], t: Time): EdgeInformation[SchemeExp, ConcreteValue, Addr] = e match { // Cases in stepEval shouldn't generate any splits in abstract graph
    case λ: SchemeLambda =>
      val action = ActionConcolicReachedValue(ActionReachedValue[SchemeExp, ConcreteValue, Addr](sabs.inject[SchemeExp, Addr]((λ, env), Some(symEnv)), store), None)
      val actionEdge = List(ActionCreateClosureT[SchemeExp, ConcreteValue, Addr](λ, Some(env)))
      noEdgeInfos(action, actionEdge)
    case SchemeFuncall(f, args, _) =>
      val frame = FrameConcolicFuncallOperator[ConcreteValue, Addr, Time](f, args, env, symEnv)
      addPushActionR(ActionConcolicPush(ActionPush[SchemeExp, ConcreteValue, Addr](frame, f, env, store), frame, symEnv))
    case e @ SchemeIf(cond, cons, alt, _) =>
      val frame = FrameConcolicIf[ConcreteValue, Addr, Time](cons, alt, env, symEnv, e)
      addPushActionR(ActionConcolicPush(ActionPush(frame, cond, env, store), frame, symEnv))
    case SchemeLet(Nil, body, _) =>
      val newScopedSymEnv = concolic.pushEnvironment(symEnv)
      evalBody(body, env, newScopedSymEnv, store)
    case SchemeLet((v, exp) :: bindings, body, _) =>
      val newScopedSymEnv = concolic.pushEnvironment(symEnv)
      val frame = FrameConcolicLet[ConcreteValue, Addr, Time](v, List(), bindings, body, env, newScopedSymEnv)
      addPushActionR(ActionConcolicPush(ActionPush(frame, exp, env, store), frame, symEnv))
    case SchemeLetStar(Nil, body, _) =>
      val newScopedSymEnv = concolic.pushEnvironment(symEnv)
      evalBody(body, env, newScopedSymEnv, store)
    case SchemeLetStar((v, exp) :: bindings, body, _) =>
      val newScopedSymEnv = concolic.pushEnvironment(symEnv)
      val frame = FrameConcolicLetStar[ConcreteValue, Addr, Time](v, bindings, body, env, newScopedSymEnv)
      addPushActionR(ActionConcolicPush(ActionPush(frame, exp, env, store), frame, symEnv))
    case SchemeLetrec(Nil, body, _) =>
      val newScopedSymEnv = concolic.pushEnvironment(symEnv)
      evalBody(body, env, newScopedSymEnv, store)
    case SchemeLetrec((v, exp) :: bindings, body, _) =>
      val newScopedSymEnv = concolic.pushEnvironment(symEnv)
      case class ValuesToUpdate(env: Environment[Addr],
                                store: Store[Addr, ConcreteValue],
                                stateChanges: List[StoreChangeSemantics[ConcreteValue, Addr]])
      type X = (ValuesToUpdate, (String, Addr))
      val variables = v :: bindings.map(_._1)
      val addresses = variables.map(v => Address[Addr].variable(v, abs.bottom, t))
      val addressesVariables: List[(Addr, Identifier)] = addresses.zip(variables)
      val initial = ValuesToUpdate(env, store, Nil)
      val ValuesToUpdate(env1, store1, stateChanges) = variables.zip(addresses).foldLeft[ValuesToUpdate](initial)({
        case (ValuesToUpdate(env, store, storeChanges), (v, a)) =>
          ValuesToUpdate(env.extend(v.name, a), store.extend(a, abs.bottom), StoreExtendSemantics[ConcreteValue, Addr](a, abs.bottom) :: storeChanges)
      })
      val frame = FrameConcolicLetrec[ConcreteValue, Addr, Time](
        addresses.head, variables.head,
        addressesVariables.tail.zip(bindings.map(_._2)).map((binding) => (binding._1._1, binding._1._2, binding._2)),
        body, env1, newScopedSymEnv)
      val action = ActionConcolicPush(ActionPush(frame, exp, env1, store1), frame, symEnv)
      val actionEdge = ActionAllocAddressesR[SchemeExp, ConcreteValue, Addr](addresses)
      noEdgeInfos(action, List(actionEdge, ActionEvalPushR[SchemeExp, ConcreteValue, Addr](exp, action.normalAction.env, action.frame)))
    case SchemeSet(variable, exp, _) =>
      val frame = FrameConcolicSet(variable, env, symEnv)
      addPushActionR(ActionConcolicPush(ActionPush(frame, exp, env, store), frame, symEnv))
    case SchemeBegin(body, _) =>
      evalBody(body, env, symEnv, store)
    case SchemeCond(Nil, _) =>
      throw new Exception("TODO: cond not handled by semantics (desugared to if)")
    case SchemeCond((cond, cons) :: clauses, _) =>
      throw new Exception("TODO: cond not handled by semantics (desugared to if)")
    case SchemeCase(key, clauses, default, _) =>
      val frame = FrameConcolicCase(clauses, default, env, symEnv)
      addPushActionR(ActionConcolicPush(ActionPush(frame, key, env, store), frame, symEnv))
    case SchemeAnd(Nil, _) =>
      throw new Exception("TODO: and not handled by semantics (desugared to if)")
    case SchemeAnd(exp :: exps, _) =>
      throw new Exception("TODO: and not handled by semantics (desugared to if)")
    case SchemeOr(Nil, _) =>
      throw new Exception("TODO: or not handled by semantics (desugared to if)")
    case SchemeOr(exp :: exps, _) =>
      throw new Exception("TODO: or not handled by semantics (desugared to if)")
    case SchemeDefineVariable(name, exp, _) =>
      val frame = FrameConcolicDefine(name, env, symEnv)
      addPushActionR(ActionConcolicPush(ActionPush(frame, exp, env, store), frame, symEnv))
    case SchemeDefineFunction(name, args, body, pos) =>
      val a = Address[Addr].variable(name, abs.bottom, t)
      val lambda = SchemeLambda(args, body, pos)
      val v = sabs.inject[SchemeExp, Addr]((lambda, env), Some(symEnv))
      val action = ActionConcolicReachedValue(ActionReachedValue[SchemeExp, ConcreteValue, Addr](v, store), None)
      val actionEdges = List(ActionCreateClosureT[SchemeExp, ConcreteValue, Addr](lambda, Some(env)),
                             ActionDefineAddressesR[SchemeExp, ConcreteValue, Addr](List(a)))
      noEdgeInfos(action, actionEdges)
    case SchemeVar(variable) =>
      val concolicValue = concolic.lookupVariable(variable.name, symEnv)
      env.lookup(variable.name) match {
        case Some(a) =>
          store.lookup(a) match {
            case Some(v) =>
              val action = ActionConcolicReachedValue(ActionReachedValue[SchemeExp, ConcreteValue, Addr](v, store, Set(EffectReadVariable(a))), concolicValue)
              noEdgeInfos(action, List(ActionLookupAddressR[SchemeExp, ConcreteValue, Addr](a)))
            case None =>
              simpleAction(ActionConcolicError(ActionError[SchemeExp, ConcreteValue, Addr](UnboundAddress(a.toString))))
          }
        case None =>
          simpleAction(ActionConcolicError(ActionError[SchemeExp, ConcreteValue, Addr](UnboundVariable(variable))))
      }
    case SchemeQuoted(quoted, _) =>
      evalQuoted(quoted, store, t) match {
        case (value, store2, storeChanges) =>
          val action = ActionConcolicReachedValue(ActionReachedValue[SchemeExp, ConcreteValue, Addr](value, store2), None)
          val actionEdges = List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](value, storeChanges = storeChanges))
          noEdgeInfos(action, actionEdges)
      }
    case SchemeValue(v, _) =>
      evalValue(v) match {
        case Some(v) =>
          val optionConcolicValue: Option[ConcolicExpression] = ConcreteConcreteLattice.simplify(v) match {
            case Some(ValueInteger(i)) => Some(ConcolicInt(i))
            case Some(ValueBoolean(b)) => Some(ConcolicBool(b))
            case _ =>
              None
          }
          noEdgeInfos(ActionConcolicReachedValue(ActionReachedValue[SchemeExp, ConcreteValue, Addr](v, store), optionConcolicValue),
                      List(ActionReachedValueT[SchemeExp, ConcreteValue, Addr](v)))
        case None => simpleAction(ActionConcolicError(ActionError[SchemeExp, ConcreteValue, Addr](NotSupported(s"Unhandled value: $v"))))
      }
  }

  def stepConcolicKont(v: ConcreteValue, concolicValue: Option[ConcolicExpression],
                       frame: SchemeConcolicFrame[ConcreteValue, Addr, Time], store: Store[Addr, ConcreteValue],
                       t: Time, concolicHelper: SemanticsConcolicHelper[PCElementUsed, _]) = frame match {
      case frame: FrameConcolicFuncallOperator[ConcreteValue, Addr, Time] =>
        funcallArgs(v, concolicValue, frame, frame.fexp, frame.args, frame.env, frame.symEnv, store, t)
      case frame: FrameConcolicFuncallOperands[ConcreteValue, Addr, Time] =>
        funcallArgs(frame.f, frame, frame.fexp, (frame.cur, v, concolicValue) :: frame.args,
                    frame.toeval, frame.env, frame.symEnv, store, t, v, concolicValue)
      case frame: FrameConcolicIf[ConcreteValue, Addr, Time] =>
        concolicHelper.handleIf(concolicValue, sabs.isTrue(v))
        conditional(v,
                    addEvalActionT(ActionConcolicEval(ActionEval(frame.cons, frame.env, store), frame.symEnv)),
                    addEvalActionT(ActionConcolicEval(ActionEval(frame.alt, frame.env, store), frame.symEnv)))
      case frame: FrameConcolicLet[ConcreteValue, Addr, Time] =>
        frame.toeval match {
          case Nil =>
            val variables = frame.variable :: frame.bindings.reverse.map(_._1)
            val addresses = variables.map(variable => Address[Addr].variable(variable, v, t))
            val (env1, symEnv1, store1) = ((frame.variable, v, concolicValue) :: frame.bindings)
              .zip(addresses)
              .foldLeft((frame.env, frame.symEnv, store))({
                case ((env, symEnv, store), ((variable, value, optConcolicValue), a)) =>
                  optConcolicValue.foreach(concolic.addVariable(variable.name, _, symEnv))
                  (env.extend(variable.name, a), symEnv, store.extend(a, value))
              })
            val EdgeInformation(action, actionEdges, filters) = evalBody(frame.body, env1, symEnv1, store1)
            EdgeInformation(action, ActionDefineAddressesPopR[SchemeExp, ConcreteValue, Addr](addresses) :: actionEdges, filters)
          case (variable, e) :: toeval =>
            val newFrameGenerator = LetConcolicFrameGenerator(variable, frame.variable, frame.bindings, toeval, frame.body, frame.env, frame.symEnv)
            val actionGenerator = (currentFrame: SchemeConcolicFrame[ConcreteValue, Addr, Time]) => ActionConcolicPush(ActionPush(currentFrame, e, frame.env, store), currentFrame, frame.symEnv)
            addPushDataActionT(v, concolicValue, frame, newFrameGenerator, actionGenerator)
        }
      case frame: FrameConcolicLetStar[ConcreteValue, Addr, Time] =>
        val a = Address[Addr].variable(frame.variable, abs.bottom, t)
        val env1 = frame.env.extend(frame.variable.name, a)
        val symEnv1 = frame.symEnv
        concolicValue.foreach(concolic.addVariable(frame.variable.name, _, symEnv1))
        val store1 = store.extend(a, v)
        val actionEdges = List(ActionDefineAddressesPopR[SchemeExp, ConcreteValue, Addr](List(a)))
        frame.bindings match {
          case Nil =>
            val EdgeInformation(actions, actionEdges2, edgeInfos) = evalBody(frame.body, env1, symEnv1, store1)
            EdgeInformation(actions, actionEdges ++ actionEdges2, edgeInfos)
          case (variable, exp) :: rest =>
            val newLetStarFrame = FrameConcolicLetStar(variable, rest, frame.body, env1, symEnv1)
            val action = ActionConcolicPush(ActionPush[SchemeExp, ConcreteValue, Addr](newLetStarFrame, exp, env1, store1), newLetStarFrame, symEnv1)
            noEdgeInfos(action, actionEdges :+ ActionEvalPushR[SchemeExp, ConcreteValue, Addr](exp, action.normalAction.env, action.frame))
        }
      case frame: FrameConcolicLetrec[ConcreteValue, Addr, Time] =>
        concolicValue.foreach(concolic.addVariable(frame.variable.name, _, frame.symEnv))
        frame.bindings match {
          case Nil =>
            // If frame defines an input variable, and if a value is bound to this input variable, use that bound value
            val EdgeInformation(action, actionEdges, edgeInfos) = evalBody(frame.body, frame.env, frame.symEnv, store.update(frame.addr, v))
            EdgeInformation(action, ActionSetAddressR[SchemeExp, ConcreteValue, Addr](frame.addr) :: actionEdges, edgeInfos)
          case (a1, varName, exp) :: rest =>
            val newLetrecFrame = FrameConcolicLetrec(a1, varName, rest, frame.body, frame.env, frame.symEnv)
            val action = ActionConcolicPush(ActionPush(newLetrecFrame, exp, frame.env, store.update(frame.addr, v)), newLetrecFrame, frame.symEnv)
            val actionEdges = List[ActionReplay[SchemeExp, ConcreteValue, Addr]](ActionSetAddressR[SchemeExp, ConcreteValue, Addr](frame.addr),
                                   ActionEvalPushR[SchemeExp, ConcreteValue, Addr](exp, action.normalAction.env, action.frame))
            noEdgeInfos(action, actionEdges)
        }
      case frame: FrameConcolicSet[ConcreteValue, Addr, Time] =>
        concolicValue.foreach(concolic.setVariable(frame.variable.name, _, frame.symEnv))
        frame.env.lookup(frame.variable.name) match {
          case Some(a) =>
            val valueFalse = sabs.inject(false)
            val actionEdges = List(ActionSetAddressR[SchemeExp, ConcreteValue, Addr](a),
                                   ActionReachedValueT[SchemeExp, ConcreteValue, Addr](valueFalse))
            noEdgeInfos(ActionConcolicReachedValue(ActionReachedValue[SchemeExp, ConcreteValue, Addr](valueFalse, store.update(a, v), Set(EffectWriteVariable(a))), None),
                        actionEdges)
          case None => simpleAction(ActionConcolicError(ActionError[SchemeExp, ConcreteValue, Addr](UnboundVariable(frame.variable))))
        }
      case frame: FrameConcolicBegin[ConcreteValue, Addr, Time] => evalBody(frame.rest, frame.env, frame.symEnv, store)
      case frame: FrameConcolicCond[ConcreteValue, Addr, Time] =>
        throw new Exception("TODO: cond not handled by semantics (desugared to if)")
      case frame: FrameConcolicCase[ConcreteValue, Addr, Time] =>
        val fromClauses = frame.clauses.flatMap({
          case (values, body) =>
            if (values.exists(v2 =>
              evalValue(v2.value) match {
                case None => false
                case Some(v2) => sabs.subsumes(v, v2)
              }))
            /* TODO: precision could be improved by restricting v to v2 */
              Set[EdgeInformation[SchemeExp, ConcreteValue, Addr]](evalBody(body, frame.env, frame.symEnv, store))
            else
              Set[EdgeInformation[SchemeExp, ConcreteValue, Addr]]()
        })
        /* TODO: precision could be improved in cases where we know that default is not reachable */
        if (fromClauses.nonEmpty) {
          fromClauses.head
        } else {
          evalBody(frame.default, frame.env, frame.symEnv, store)
        }
      case frame: FrameAnd[ConcreteValue, Addr, Time] =>
        throw new Exception("TODO: and not handled by semantics (desugared to if)")
      case frame: FrameOr[ConcreteValue, Addr, Time] =>
        throw new Exception("TODO: or not handled by semantics (desugared to if)")
      case frame: FrameConcolicDefine[ConcreteValue, Addr, Time] =>
        throw new Exception("TODO: define not handled (no global environment)")
    }

  override def parse(program: String): SchemeExp = Scheme.parse(program)
  override def initialBindings = primitives.bindings
}