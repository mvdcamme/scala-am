import backend.expression._

/**
  * Basic Scheme semantics, without any optimization
  */
class ConvertableBaseSchemeSemantics[V : IsSchemeLattice, Addr: Address, Time: Timestamp](val primitives: SchemePrimitives[Addr, V])
  extends ConvertableSemantics[SchemeExp, V, Addr, Time] {

  type Env = Environment[Addr]
  type Sto = Store[Addr, V]

  protected def evalBody(body: List[SchemeExp], env: Environment[Addr], store: Store[Addr, V]): EdgeInfo = body match {
    case Nil => simpleAction(ActionReachedValue[SchemeExp, V, Addr](IsSchemeLattice[V].inject(false), store))
    case List(exp) => simpleAction(ActionEval(exp, env, store))
    case exp :: rest => simpleAction(ActionPush(FrameBegin(rest, env), exp, env, store))
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
      .map(clo => {
        val action: Action[SchemeExp, V, Addr] = clo match {
          case (lambda@(SchemeLambda(args, body, pos)), env1, _) =>
            if (args.length == argsv.length) {
              val argsZipped: List[(Identifier, (SchemeExp, V))] = args.zip(argsv)
              val updatedArgsZipped: List[(Identifier, SchemeExp, V)] = argsZipped.map({
                case (varName, (exp, value)) => (varName, exp, value)
              })
              bindArgsAndReturnAddresses(updatedArgsZipped, env1, store, t) match {
                case (env2, store, boundAddresses) =>
                  if (body.length == 1) {
                    val action = ActionStepIn[SchemeExp, V, Addr](fexp, (SchemeLambda(args, body, pos), env1), body.head, env2, store, argsv)
                    action
                  }
                  else {
                    val action = ActionStepIn[SchemeExp, V, Addr](fexp, (SchemeLambda(args, body, pos), env1), SchemeBegin(body, pos), env2, store, argsv)
                    action
                  }
              }
            } else {
              val error = ArityError(fexp.toString, args.length, argsv.length)
              ActionError[SchemeExp, V, Addr](error)
            }
          case (lambda, env, _) =>
            val cloCall = ActionClosureCallR[SchemeExp, V, Addr](fexp, lambda, env)
            val error = TypeError(lambda.toString, "operator", "closure", "not a closure")
            ActionError[SchemeExp, V, Addr](error)
        }
        simpleAction(action)
      })
    val fromPrim: EdgeInfos = IsSchemeLattice[V].getPrimitives[Addr, V](function).flatMap( (prim) => {
      val n = argsv.size + 1 // Number of values to pop: all arguments + the operator
      prim.call(fexp, argsv.map(x => (x._1, (x._2, None))), store, t)._1.collect[EdgeInformation[SchemeExp, V, Addr]]({
        case (res, store2, effects) =>
          val action = ActionReachedValue[SchemeExp, V, Addr](res, store2, effects)
          simpleActionSet(action)
      },
        err => {
          simpleActionSet(ActionError[SchemeExp, V, Addr](err))
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

  protected def funcallArgs(f: V, fexp: SchemeExp, args: List[(SchemeExp, V)], toeval: List[SchemeExp],
                            env: Environment[Addr], store: Store[Addr, V], t: Time): EdgeInfos = toeval match {
      case Nil => evalCall(f, fexp, args.reverse, store, t)
      case e :: rest =>
        simpleActionSet(ActionPush(FrameFuncallOperands(f, fexp, e, args, rest, env), e, env, store))
    }

  protected def funcallArgs(f: V, fexp: SchemeExp, args: List[SchemeExp], env: Environment[Addr], store: Store[Addr, V], t: Time): EdgeInfos = {
    funcallArgs(f, fexp, List(), args, env, store, t)
  }

  protected def evalQuoted(exp: SExp, store: Store[Addr, V], t: Time): (V, Store[Addr, V], List[StoreChangeSemantics[V, Addr]]) = exp match {
    case SExpId(Identifier(sym, _)) => (IsSchemeLattice[V].injectSymbol(sym), store, Nil)
    case SExpPair(car, cdr, _) => {
      val care: SchemeExp = SchemeVar(Identifier(car.toString, car.pos))
      val cdre: SchemeExp = SchemeVar(Identifier(cdr.toString, cdr.pos))
      val cara = Address[Addr].cell(care, t)
      val (carv, store2, stateChangesCar) = evalQuoted(car, store, t)
      val cdra = Address[Addr].cell(cdre, t)
      val (cdrv, store3, stateChangesCdr) = evalQuoted(cdr, store2, t)
      val storeChangeSemantics = List(StoreExtendSemantics[V, Addr](cara, carv), StoreExtendSemantics[V, Addr](cdra, cdrv)) ++ stateChangesCar ++ stateChangesCdr
      (IsSchemeLattice[V].cons(cara, cdra), store3.extend(cara, carv).extend(cdra, cdrv), storeChangeSemantics)
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
      evalQuoted(SExpPair(SExpId(Identifier("quote", pos)), SExpPair(q, SExpValue(ValueNil, pos), pos), pos), store, t)
  }

  override def stepEval(e: SchemeExp, env: Environment[Addr], store: Store[Addr, V], t: Time): EdgeInfos = e match { // Cases in stepEval shouldn't generate any splits in abstract graph
    case λ: SchemeLambda =>
      simpleActionSet(ActionReachedValue[SchemeExp, V, Addr](IsSchemeLattice[V].inject[SchemeExp, Addr]((λ, env), None), store))
    case SchemeFuncall(f, args, _) =>
      simpleActionSet(Action.push(FrameFuncallOperator(f, args, env), f, env, store))
    case e @ SchemeIf(cond, cons, alt, _) =>
      simpleActionSet(Action.push(FrameIf(cons, alt, env, e), cond, env, store))
    case SchemeLet(Nil, body, _) =>
      Set(evalBody(body, env, store))
    case SchemeLet((v, exp) :: bindings, body, _) =>
      simpleActionSet(ActionPush(FrameLet(v, List(), bindings, body, env), exp, env, store))
    case SchemeLetStar(Nil, body, _) => Set(evalBody(body, env, store))
    case SchemeLetStar((v, exp) :: bindings, body, _) =>
      simpleActionSet(ActionPush(FrameLetStar(v, bindings, body, env), exp, env, store))
    case SchemeLetrec(Nil, body, _) => Set(evalBody(body, env, store))
    case SchemeLetrec(bindings, body, _) =>
      val variables = bindings.map(_._1)
      val addresses = variables.map(v => Address[Addr].variable(v, JoinLattice[V].bottom, t))
      val (env1, store1) = variables.zip(addresses).foldLeft((env, store))({
        case ((env, store), (v, a)) =>
          (env.extend(v.name, a), store.extend(a, JoinLattice[V].bottom))
      })
      val exp = bindings.head._2
      simpleActionSet(ActionPush(FrameLetrec(addresses.head, addresses.zip(bindings.map(_._2)).tail, body, env1), exp, env1, store1))
    case SchemeSet(variable, exp, _) => simpleActionSet(ActionPush(FrameSet(variable, env), exp, env, store))
    case SchemeBegin(body, _) => Set(evalBody(body, env, store))
    /* Should not be used: or-expressions should have been desugared into if-expressions */
    case SchemeCond(Nil, _) => ???
    /* Should not be used: or-expressions should have been desugared into if-expressions */
    case SchemeCond((cond, cons) :: clauses, _) => ???
    case SchemeCase(key, clauses, default, _) =>
      simpleActionSet(ActionPush(FrameCase(clauses, default, env), key, env, store))
    /* Should not be used: or-expressions should have been desugared into if-expressions */
    case SchemeAnd(Nil, _) => ???
    /* Should not be used: or-expressions should have been desugared into if-expressions */
    case SchemeAnd(exp :: exps, _) => ???
    /* Should not be used: or-expressions should have been desugared into if-expressions */
    case SchemeOr(Nil, _) => ???
    /* Should not be used: or-expressions should have been desugared into if-expressions */
    case SchemeOr(exp :: exps, _) => ???
    case SchemeDefineVariable(name, exp, _) =>
      simpleActionSet(ActionPush(FrameDefine(name, env), exp, env, store))
    case SchemeDefineFunction(f, args, body, pos) =>
      val a = Address[Addr].variable(f, JoinLattice[V].bottom, t)
      val v = IsSchemeLattice[V].inject[SchemeExp, Addr]((SchemeLambda(args, body, pos), env), None)
      val env1 = env.extend(f.name, a)
      val store1 = store.extend(a, v)
      simpleActionSet(Action.value(v, store))
    case SchemeVar(variable) => env.lookup(variable.name) match {
      case Some(a) => store.lookup(a) match {
        case Some(v) => simpleActionSet(Action.value(v, store, Set(EffectReadVariable(a))))
        case None => simpleActionSet(Action.error(UnboundAddress(a.toString)))
      }
      case None => simpleActionSet(Action.error(UnboundVariable(variable)))
    }
    case SchemeQuoted(quoted, _) => evalQuoted(quoted, store, t) match {
      case (value, store2, _) => simpleActionSet(Action.value(value, store2))
    }
    case SchemeValue(v, _) => evalValue(v) match {
      case Some(v) => simpleActionSet(Action.value(v, store))
      case None => simpleActionSet(Action.error(NotSupported(s"Unhandled value: $v")))
    }
  }

  override def stepKont(v: V, frame: Frame, store: Store[Addr, V], t: Time): EdgeInfos = frame match {
    /* We use two-step matching  */
    case  frame: FrameFuncallOperator[V, Addr, Time] =>
      val FrameFuncallOperator(fexp, args, env) = frame
      funcallArgs(v, fexp, args, env, store, t)
    case frame: FrameFuncallOperands[V, Addr, Time] =>
      val FrameFuncallOperands(f, fexp, exp, args, toeval, env) = frame
      funcallArgs(f, fexp, (exp, v) :: args, toeval, env, store, t)
    case frame: FrameIf[V, Addr, Time] =>
      val FrameIf(cons, alt, env, _) = frame
      conditional(v, simpleActionSet(ActionEval(cons, env, store)), simpleActionSet(ActionEval(alt, env, store)))
    case frame: FrameLet[V, Addr, Time] =>
      val FrameLet(name, bindings, toeval, body, env) = frame
      toeval match {
        case Nil =>
          val variables = name :: bindings.reverseMap(_._1)
          val addresses = variables.map(variable => Address[Addr].variable(variable, v, t))
          val (env1, store1) = ((name, v) :: bindings).zip(addresses).foldLeft((env, store))({
            case ((env, store), ((variable, value), a)) => (env.extend(variable.name, a), store.extend(a, value))
          })
          Set(evalBody(body, env1, store1))
        case (variable, e) :: toeval =>
          simpleActionSet(Action.push(FrameLet(variable, (name, v) :: bindings, toeval, body, env), e, env, store))
      }
    case frame: FrameLetStar[V, Addr, Time] =>
      val FrameLetStar(variable, bindings, body, env) = frame
      val a = Address[Addr].variable(variable, JoinLattice[V].bottom, t)
      val env1 = env.extend(variable.name, a)
      val store1 = store.extend(a, v)
      bindings match {
        case Nil =>
          Set(evalBody(body, env1, store1))
        case (variable, exp) :: rest =>
          simpleActionSet(Action.push(FrameLetStar(variable, rest, body, env1), exp, env1, store1))
      }
    case frame: FrameLetrec[V, Addr, Time] =>
      val FrameLetrec(a, bindings, body, env) = frame
      bindings match {
        case Nil => Set(evalBody(body, env, store.update(a, v)))
        case (a1, exp) :: rest =>
          simpleActionSet(Action.push(FrameLetrec(a1, rest, body, env), exp, env, store.update(a, v)))
      }
    case frame: FrameSet[V, Addr, Time] =>
      val FrameSet(variable, env) = frame
      env.lookup(variable.name) match {
        case Some(a) => simpleActionSet(Action.value(IsSchemeLattice[V].inject(false), store.update(a, v), Set(EffectWriteVariable(a))))
        case None => simpleActionSet(Action.error(UnboundVariable(variable)))
      }
    case frame: FrameBegin[V, Addr, Time] =>
      val FrameBegin(body, env) = frame
      Set(evalBody(body, env, store))
    /* Should not be used: cond-expressions should have been desugared into if-expressions */
    case frame: FrameCond[V, Addr, Time] => ???
    case frame: FrameCase[V, Addr, Time] =>
      val FrameCase(clauses, default, env) = frame
      val fromClauses = clauses.flatMap({ case (values, body) =>
        if (values.exists({
          case SchemeValue(ValueSymbol(s), _) =>
            IsSchemeLattice[V].subsumes(v, IsSchemeLattice[V].injectSymbol(s))
          case v2 => evalValue(v2.value).exists(v2 => IsSchemeLattice[V].subsumes(v, v2)) })) {
          /* TODO: precision could be improved by restricting v to v2 */
          Set(evalBody(body, env, store))
        } else {
          Set[EdgeInfo]()
        }
      })
      /* TODO: precision could be improved in cases where we know that default is not reachable */
      fromClauses.toSet + evalBody(default, env, store)
    /* Should not be used: and-expressions should have been desugared into if-expressions */
    case frame: FrameAnd[V, Addr, Time] => ???
    /* Should not be used: or-expressions should have been desugared into if-expressions */
    case frame: FrameOr[V, Addr, Time] => ???
    case frame: FrameDefine[V, Addr, Time] => throw new Exception("TODO: define not handled (no global environment)")
  }

  def parse(program: String): SchemeExp = Scheme.parse(program)
  override def initialBindings = primitives.bindings
}

class ConvertableSchemeSemantics[V : IsSchemeLattice, Addr : Address, Time : Timestamp](primitives: SchemePrimitives[Addr, V])
  extends ConvertableBaseSchemeSemantics[V, Addr, Time](primitives) {
  /** Tries to perform atomic evaluation of an expression. Returns the result of
    * the evaluation if it succeeded, otherwise returns None */
  protected def atomicEval(e: SchemeExp, env: Env, store: Sto): Option[(V, Set[Effect[Addr]])] = e match {
    case λ: SchemeLambda => Some((IsSchemeLattice[V].inject[SchemeExp, Addr]((λ, env), None), Set()))
    case SchemeVar(variable) => env.lookup(variable.name).flatMap(a => store.lookup(a).map(v => (v, Set(EffectReadVariable(a)))))
    case SchemeValue(v, _) => evalValue(v).map(value => (value, Set()))
    case _ => None
  }

  /**
    * Optimize the following pattern: when we see an ActionPush(frame, exp, env, store)
    * where exp is an atomic expression, we can atomically evaluate exp to get v,
    * and call stepKont(v, store, frame).
    */
  protected def optimizeAtomic(edgeInfos: EdgeInfos, t: Time): EdgeInfos = edgeInfos.flatMap(edgeInfo => {
    edgeInfo.action match {
      case ActionPush(frame, exp, env, store, effects) => atomicEval(exp, env, store) match {
        case Some((v, effs)) =>
          val newEdgeInfos = stepKont(v, frame, store, t).map(edgeInfo => edgeInfo.copy(action = edgeInfo.action.addEffects(effs ++ effects)))
          newEdgeInfos.map(edgeInfo.merge)
        case None =>
          val newPushAction = Action.push(frame, exp, env, store, effects)
          val newPushEdgeAnnotation = simpleAction(newPushAction)
          Set(edgeInfo.merge(newPushEdgeAnnotation))
      }
      case _ => Set(edgeInfo)
    }
  })

  override protected def funcallArgs(f: V, fexp: SchemeExp, args: List[(SchemeExp, V)], toeval: List[SchemeExp],
                                     env: Env, store: Sto, t: Time): EdgeInfos = toeval match {
    case Nil => evalCall(f, fexp, args.reverse, store, t)
    case e :: rest => atomicEval(e, env, store) match {
      case Some((v, effs)) =>
        val edgeInfos = funcallArgs(f, fexp, (e, v) :: args, rest, env, store, t)
        edgeInfos.map(edgeInfo => edgeInfo.copy(action = edgeInfo.action.addEffects(effs)))
      case None =>
        val action = Action.push(FrameFuncallOperands(f, fexp, e, args, rest, env), e, env, store)
        simpleActionSet(action)
    }
  }

  override def stepEval(e: SchemeExp, env: Env, store: Sto, t: Time): EdgeInfos =
    optimizeAtomic(super.stepEval(e, env, store, t), t)

  override def stepKont(v: V, frame: Frame, store: Sto, t: Time): EdgeInfos =
    optimizeAtomic(super.stepKont(v, frame, store, t), t)
}