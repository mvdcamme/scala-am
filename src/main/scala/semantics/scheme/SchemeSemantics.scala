import SchemeOps._

trait SchemeFrame[Abs, Addr, Time] extends Frame {
  type Address = Addr

  def subsumes(that: Frame) = that.equals(this)
  override def toString = s"${this.getClass.getSimpleName}"

  def convert[OtherAbs: IsSchemeLattice](
      convertValue: (Abs) => OtherAbs,
      convertEnv: Environment[Addr] => Environment[Addr],
      abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time])
    : SchemeFrame[OtherAbs, Addr, Time]

  def reaches(valueReaches: Abs => Set[Addr],
              envReaches: Environment[Addr] => Set[Addr],
              addressReaches: Addr => Set[Addr]): Set[Addr]

}

/**
  * Basic Scheme semantics, without any optimization
  */
class BaseSchemeSemantics[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    val primitives: SchemePrimitives[Addr, Abs])
    extends BaseSemantics[SchemeExp, Abs, Addr, Time]
    with ConvertableSemantics[SchemeExp, Abs, Addr, Time] {

  val actionPushVal = ActionPushValT[SchemeExp, Abs, Addr]()

  def sabs = implicitly[IsSchemeLattice[Abs]]
  case class FrameFuncallOperator(fexp: SchemeExp,
                                  args: List[SchemeExp],
                                  env: Environment[Addr])
      extends SchemeFrame[Abs, Addr, Time] {
    override def savesEnv(): Option[Environment[Address]] = Some(env)

    def convert[OtherAbs: IsSchemeLattice](
        convertValue: (Abs) => OtherAbs,
        convertEnv: Environment[Addr] => Environment[Addr],
        abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
      abstSem.FrameFuncallOperator(fexp, args, convertEnv(env))

    def reaches(valueReaches: Abs => Set[Addr],
                envReaches: Environment[Addr] => Set[Addr],
                addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)

  }
  case class FrameFuncallOperands(f: Abs,
                                  fexp: SchemeExp,
                                  cur: SchemeExp,
                                  args: List[(SchemeExp, Abs)],
                                  toeval: List[SchemeExp],
                                  env: Environment[Addr])
      extends SchemeFrame[Abs, Addr, Time] {
    override def savesEnv(): Option[Environment[Address]] = Some(env)

    override def toString: String = s"FrameFuncallOperands($f, $args, $env)"

    override def meaningfullySubsumes = true
    override def subsumes(that: Frame): Boolean = that match {
      case that: FrameFuncallOperands =>
        fexp == that.fexp &&
        cur == that.cur &&
        toeval == that.toeval &&
        sabs.subsumes(f, that.f) &&
        args.zip(that.args).forall( (zipped) =>
          /* Check whether they have evaluated the same argument expression */
          zipped._1._1 == zipped._2._1 &&
          /* and whether the results of this subsume those of that. */
          sabs.subsumes(zipped._1._2, zipped._2._2)) &&
        env.subsumes(that.env)
      case _ =>
        false
    }

    def convert[OtherAbs: IsSchemeLattice](
        convertValue: (Abs) => OtherAbs,
        convertEnv: Environment[Addr] => Environment[Addr],
        abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
      abstSem.FrameFuncallOperands(
        convertValue(f),
        fexp,
        cur,
        args.map((arg) => (arg._1, convertValue(arg._2))),
        toeval,
        convertEnv(env))

    def reaches(valueReaches: Abs => Set[Addr],
                envReaches: Environment[Addr] => Set[Addr],
                addressReaches: Addr => Set[Addr]): Set[Addr] =
      valueReaches(f) ++ args.foldLeft[Set[Addr]](Set[Addr]())((acc, arg) =>
        acc ++ valueReaches(arg._2)) ++
        envReaches(env)
  }
  case class FrameIf(cons: SchemeExp, alt: SchemeExp, env: Environment[Addr])
      extends SchemeFrame[Abs, Addr, Time] {
    override def savesEnv(): Option[Environment[Address]] = Some(env)

    override def meaningfullySubsumes = true
    override def subsumes(that: Frame): Boolean = that match {
      case FrameIf(thatCons, thatAlt, thatEnv) =>
        cons == thatCons &&
        alt == thatAlt &&
        env.subsumes(thatEnv)
      case _ => false

    }

    def convert[OtherAbs: IsSchemeLattice](
        convertValue: (Abs) => OtherAbs,
        convertEnv: Environment[Addr] => Environment[Addr],
        abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
      abstSem.FrameIf(cons, alt, convertEnv(env))

    def reaches(valueReaches: Abs => Set[Addr],
                envReaches: Environment[Addr] => Set[Addr],
                addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
  }
  case class FrameLet(variable: String,
                      bindings: List[(String, Abs)],
                      toeval: List[(String, SchemeExp)],
                      body: List[SchemeExp],
                      env: Environment[Addr])
      extends SchemeFrame[Abs, Addr, Time] {
    override def savesEnv(): Option[Environment[Address]] = Some(env)

    def convert[OtherAbs: IsSchemeLattice](
        convertValue: (Abs) => OtherAbs,
        convertEnv: Environment[Addr] => Environment[Addr],
        abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
      abstSem.FrameLet(
        variable,
        bindings.map((binding) => (binding._1, convertValue(binding._2))),
        toeval,
        body,
        convertEnv(env))

    def reaches(valueReaches: Abs => Set[Addr],
                envReaches: Environment[Addr] => Set[Addr],
                addressReaches: Addr => Set[Addr]): Set[Addr] =
      bindings.foldLeft[Set[Addr]](Set[Addr]())((acc, binding) =>
        acc ++ valueReaches(binding._2)) ++
        envReaches(env)
  }
  case class FrameLetStar(variable: String,
                          bindings: List[(String, SchemeExp)],
                          body: List[SchemeExp],
                          env: Environment[Addr])
      extends SchemeFrame[Abs, Addr, Time] {
    override def savesEnv(): Option[Environment[Address]] = Some(env)

    def convert[OtherAbs: IsSchemeLattice](
        convertValue: (Abs) => OtherAbs,
        convertEnv: Environment[Addr] => Environment[Addr],
        abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
      abstSem.FrameLetStar(variable, bindings, body, convertEnv(env))

    def reaches(valueReaches: Abs => Set[Addr],
                envReaches: Environment[Addr] => Set[Addr],
                addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
  }
  case class FrameLetrec(addr: Addr,
                         bindings: List[(Addr, SchemeExp)],
                         body: List[SchemeExp],
                         env: Environment[Addr])
      extends SchemeFrame[Abs, Addr, Time] {
    override def savesEnv(): Option[Environment[Address]] = Some(env)

    def convert[OtherAbs: IsSchemeLattice](
        convertValue: (Abs) => OtherAbs,
        convertEnv: Environment[Addr] => Environment[Addr],
        abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) = {
      val addressConverter = new DefaultHybridAddressConverter[SchemeExp]()
      abstSem.FrameLetrec(
        addressConverter.convertAddress(addr.asInstanceOf[HybridAddress.A]).asInstanceOf[Addr],
        bindings.map(
          (binding) =>
            (addressConverter.convertAddress(binding._1.asInstanceOf[HybridAddress.A]).asInstanceOf[Addr], binding._2)),
        body,
        convertEnv(env))
    }

    def reaches(valueReaches: Abs => Set[Addr],
                envReaches: Environment[Addr] => Set[Addr],
                addressReaches: Addr => Set[Addr]): Set[Addr] =
      addressReaches(addr) ++ envReaches(env)
  }
  case class FrameSet(variable: String, env: Environment[Addr])
      extends SchemeFrame[Abs, Addr, Time] {

    override def writeEffectsFor(): Set[Address] = env.lookup(variable) match {
      case Some(a) => Set(a)
      case None => Set()
    }

    override def savesEnv(): Option[Environment[Address]] = Some(env)

    def convert[OtherAbs: IsSchemeLattice](
        convertValue: (Abs) => OtherAbs,
        convertEnv: Environment[Addr] => Environment[Addr],
        abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
      abstSem.FrameSet(variable, convertEnv(env))

    def reaches(valueReaches: Abs => Set[Addr],
                envReaches: Environment[Addr] => Set[Addr],
                addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
  }
  case class FrameBegin(rest: List[SchemeExp], env: Environment[Addr])
      extends SchemeFrame[Abs, Addr, Time] {
    override def savesEnv(): Option[Environment[Address]] = Some(env)

    def convert[OtherAbs: IsSchemeLattice](
        convertValue: (Abs) => OtherAbs,
        convertEnv: Environment[Addr] => Environment[Addr],
        abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
      abstSem.FrameBegin(rest, convertEnv(env))

    def reaches(valueReaches: Abs => Set[Addr],
                envReaches: Environment[Addr] => Set[Addr],
                addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
  }
  case class FrameCond(cons: List[SchemeExp],
                       clauses: List[(SchemeExp, List[SchemeExp])],
                       env: Environment[Addr])
      extends SchemeFrame[Abs, Addr, Time] {
    override def savesEnv(): Option[Environment[Address]] = Some(env)

    def convert[OtherAbs: IsSchemeLattice](
        convertValue: (Abs) => OtherAbs,
        convertEnv: Environment[Addr] => Environment[Addr],
        abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
      abstSem.FrameCond(cons, clauses, convertEnv(env))

    def reaches(valueReaches: Abs => Set[Addr],
                envReaches: Environment[Addr] => Set[Addr],
                addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
  }
  case class FrameCase(clauses: List[(List[SchemeValue], List[SchemeExp])],
                       default: List[SchemeExp],
                       env: Environment[Addr])
      extends SchemeFrame[Abs, Addr, Time] {
    override def savesEnv(): Option[Environment[Address]] = Some(env)

    def convert[OtherAbs: IsSchemeLattice](
        convertValue: (Abs) => OtherAbs,
        convertEnv: Environment[Addr] => Environment[Addr],
        abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
      abstSem.FrameCase(clauses, default, convertEnv(env))

    def reaches(valueReaches: Abs => Set[Addr],
                envReaches: Environment[Addr] => Set[Addr],
                addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
  }
  case class FrameAnd(rest: List[SchemeExp], env: Environment[Addr])
      extends SchemeFrame[Abs, Addr, Time] {
    override def savesEnv(): Option[Environment[Address]] = Some(env)

    def convert[OtherAbs: IsSchemeLattice](
        convertValue: (Abs) => OtherAbs,
        convertEnv: Environment[Addr] => Environment[Addr],
        abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
      abstSem.FrameAnd(rest, convertEnv(env))

    def reaches(valueReaches: Abs => Set[Addr],
                envReaches: Environment[Addr] => Set[Addr],
                addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
  }
  case class FrameOr(rest: List[SchemeExp], env: Environment[Addr])
      extends SchemeFrame[Abs, Addr, Time] {
    override def savesEnv(): Option[Environment[Address]] = Some(env)

    def convert[OtherAbs: IsSchemeLattice](
        convertValue: (Abs) => OtherAbs,
        convertEnv: Environment[Addr] => Environment[Addr],
        abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
      abstSem.FrameOr(rest, convertEnv(env))

    def reaches(valueReaches: Abs => Set[Addr],
                envReaches: Environment[Addr] => Set[Addr],
                addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
  }
  case class FrameDefine(variable: String, env: Environment[Addr])
      extends SchemeFrame[Abs, Addr, Time] {
    override def savesEnv(): Option[Environment[Address]] = Some(env)

    def convert[OtherAbs: IsSchemeLattice](
        convertValue: (Abs) => OtherAbs,
        convertEnv: Environment[Addr] => Environment[Addr],
        abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
      abstSem.FrameDefine(variable, convertEnv(env))

    def reaches(valueReaches: Abs => Set[Addr],
                envReaches: Environment[Addr] => Set[Addr],
                addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
  }

  def convertToAbsSemanticsFrame(frame: Frame,
                                 ρ: Environment[Addr],
                                 vStack: List[Storable[Abs, Addr]],
                                 absSem: BaseSchemeSemantics[Abs, Addr, Time])
  : (Option[Frame], List[Storable[Abs, Addr]], Environment[Addr]) = (Some(frame), vStack, ρ)

  protected def evalBody(
      body: List[SchemeExp],
      env: Environment[Addr],
      store: Store[Addr, Abs]): ActionChange[SchemeExp, Abs, Addr] = body match {
    case Nil => ActionChange(ActionReachedValue(sabs.inject(false), store), Nil)
    case List(exp) => ActionChange(ActionEval(exp, env, store), Nil)
    case exp :: rest => ActionChange(ActionPush(FrameBegin(rest, env), exp, env, store), Nil)
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
                  t: => Set[ActionChange[SchemeExp, Abs, Addr]],
                  f: => Set[ActionChange[SchemeExp, Abs, Addr]]): Set[ActionChange[SchemeExp, Abs, Addr]] =
    (if (sabs.isTrue(v)) t else Set[ActionChange[SchemeExp, Abs, Addr]]()) ++
    (if (sabs.isFalse(v)) f else Set[ActionChange[SchemeExp, Abs, Addr]]())

  def evalCall(function: Abs,
               fexp: SchemeExp,
               argsv: List[(SchemeExp, Abs)],
               store: Store[Addr, Abs],
               t: Time): Set[ActionChange[SchemeExp, Abs, Addr]] = {
    val fromClo: Set[ActionChange[SchemeExp, Abs, Addr]] = sabs
      .getClosures[SchemeExp, Addr](function)
      .map({
        case (SchemeLambda(args, body, pos), env1) =>
          if (args.length == argsv.length) {
            bindArgs(args.zip(argsv), env1, store, t) match {
              case (env2, store, boundAddresses) =>
                val actionEdge: ActionT[SchemeExp, Abs, Addr] = ActionDefineAddressesT[SchemeExp, Abs, Addr](boundAddresses.map(_._1))
                if (body.length == 1) {
                  val action = ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body, pos), env1), body.head, env2, store, argsv)
                  ActionChange[SchemeExp, Abs, Addr](action, List(actionEdge))
                }
                else {
                  val action = ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body, pos), env1), SchemeBegin(body, pos), env2, store, argsv)
                  ActionChange[SchemeExp, Abs, Addr](action, List(actionEdge))
                }
            }
          } else {
            ActionChange(ActionError[SchemeExp, Abs, Addr](ArityError(fexp.toString, args.length, argsv.length)), Nil)
          }
        case (lambda, _) =>
          ActionChange[SchemeExp, Abs, Addr](ActionError[SchemeExp, Abs, Addr](TypeError(lambda.toString, "operator", "closure", "not a closure")), Nil)
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
                         val n = argsv.size + 1 // Numbr of values to pop: all arguments + the operator
                         val actionEdge = ActionPrimCallT[SchemeExp, Abs, Addr](argsv.size + 1, fexp, argsv.map(_._1))
                         val action = ActionReachedValue[SchemeExp, Abs, Addr](res, store2, effects)
                         Set(ActionChange[SchemeExp, Abs, Addr](action, List(actionEdge)))
                     },
                     err =>
                       addNilStateChangeEdges(ActionError[SchemeExp, Abs, Addr](err))))
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      addNilStateChangeEdges(ActionError[SchemeExp, Abs, Addr](TypeError(function.toString, "operator", "function", "not a function")))
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
                            t: Time): Set[ActionChange[SchemeExp, Abs, Addr]] =
    toeval match {
      case Nil => evalCall(f, fexp, args.reverse, store, t)
      case e :: rest =>
        Set(
          ActionChange(ActionPush(FrameFuncallOperands(f, fexp, e, args, rest, env),
                     e,
                     env,
                     store), Nil))
    }
  protected def funcallArgs(f: Abs,
                            fexp: SchemeExp,
                            args: List[SchemeExp],
                            env: Environment[Addr],
                            store: Store[Addr, Abs],
                            t: Time): Set[ActionChange[SchemeExp, Abs, Addr]] =
    funcallArgs(f, fexp, List(), args, env, store, t)

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
      val actionEdge = ActionCreateClosureT[SchemeExp, Abs, Addr](λ)
      Set(ActionChange(action, List(actionEdge)))
    case SchemeFuncall(f, args, _) =>
      addNilStateChangeEdges(ActionPush[SchemeExp, Abs, Addr](FrameFuncallOperator(f, args, env), f, env, store))
    case SchemeIf(cond, cons, alt, _) =>
      addNilStateChangeEdges(ActionPush(FrameIf(cons, alt, env), cond, env, store))
    case SchemeLet(Nil, body, _) => Set(evalBody(body, env, store))
    case SchemeLet((v, exp) :: bindings, body, _) =>
      addNilStateChangeEdges(ActionPush(FrameLet(v, List(), bindings, body, env), exp, env, store))
    case SchemeLetStar(Nil, body, _) =>
      Set(evalBody(body, env, store))
    case SchemeLetStar((v, exp) :: bindings, body, _) =>
      addNilStateChangeEdges(ActionPush(FrameLetStar(v, bindings, body, env), exp, env, store))
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
      val actionEdge = ActionAllocAddressesT[SchemeExp, Abs, Addr](addresses)
      Set(ActionChange(action, List(actionEdge)))
    case SchemeSet(variable, exp, _) =>
      addNilStateChangeEdges(ActionPush(FrameSet(variable, env), exp, env, store))
    case SchemeBegin(body, _) =>
      Set(evalBody(body, env, store))
    case SchemeCond(Nil, _) =>
      addNilStateChangeEdges(ActionError[SchemeExp, Abs, Addr](NotSupported("cond without clauses")))
    case SchemeCond((cond, cons) :: clauses, _) =>
      addNilStateChangeEdges(ActionPush(FrameCond(cons, clauses, env), cond, env, store))
    case SchemeCase(key, clauses, default, _) =>
      addNilStateChangeEdges(ActionPush(FrameCase(clauses, default, env), key, env, store))
    case SchemeAnd(Nil, _) =>
      addNilStateChangeEdges(ActionReachedValue[SchemeExp, Abs, Addr](sabs.inject(true), store))
    case SchemeAnd(exp :: exps, _) =>
      addNilStateChangeEdges(ActionPush(FrameAnd(exps, env), exp, env, store))
    case SchemeOr(Nil, _) =>
      addNilStateChangeEdges(ActionReachedValue[SchemeExp, Abs, Addr](sabs.inject(false), store))
    case SchemeOr(exp :: exps, _) =>
      addNilStateChangeEdges(ActionPush(FrameOr(exps, env), exp, env, store))
    case SchemeDefineVariable(name, exp, _) =>
      addNilStateChangeEdges(ActionPush(FrameDefine(name, env), exp, env, store))
    case SchemeDefineFunction(name, args, body, pos) =>
      val a = addr.variable(name, abs.bottom, t)
      val lambda = SchemeLambda(args, body, pos)
      val v = sabs.inject[SchemeExp, Addr]((lambda, env))
      val action = ActionReachedValue[SchemeExp, Abs, Addr](v, store)
      val actionEdges = List(ActionCreateClosureT[SchemeExp, Abs, Addr](lambda),
                             ActionDefineAddressesT[SchemeExp, Abs, Addr](List(a)))
      Set(ActionChange(action, actionEdges))
    case SchemeIdentifier(name, _) =>
      env.lookup(name) match {
        case Some(a) =>
          store.lookup(a) match {
            case Some(v) =>
              val action = ActionReachedValue[SchemeExp, Abs, Addr](v, store, Set(EffectReadVariable(a)))
              Set(ActionChange(action, List(ActionLookupVariableT(name))))
            case None => addNilStateChangeEdges(ActionError[SchemeExp, Abs, Addr](UnboundAddress(a.toString)))
          }
        case None => addNilStateChangeEdges(ActionError[SchemeExp, Abs, Addr](UnboundVariable(name)))
      }
    case SchemeQuoted(quoted, _) =>
      evalQuoted(quoted, store, t) match {
        case (value, store2, stateChanges) =>
          val action = ActionReachedValue[SchemeExp, Abs, Addr](value, store2)
          val actionEdge = ActionReachedValueT[SchemeExp, Abs, Addr](value)
          Set(ActionChange(action, List(actionEdge)))
      }
    /* The start-analysis expression only has an effect in the SchemeSemanticsTraced; in these semantics, it just
     * evaluates to #f. */
    case SchemeStartAnalysis(_) =>
      addNilStateChangeEdges(ActionReachedValue[SchemeExp, Abs, Addr](sabs.inject(false), store))
    case SchemeValue(v, _) =>
      evalValue(v) match {
        case Some(v) => Set(ActionChange(ActionReachedValue(v, store), List(ActionReachedValueT(v))))
        case None => Set(ActionChange(ActionError(NotSupported(s"Unhandled value: $v")), Nil))
      }
  }

  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time) =
    frame match {
      case FrameFuncallOperator(fexp, args, env) =>
        funcallArgs(v, fexp, args, env, store, t)
      case FrameFuncallOperands(f, fexp, exp, args, toeval, env) =>
        funcallArgs(f, fexp, (exp, v) :: args, toeval, env, store, t)
      case FrameIf(cons, alt, env) =>
        conditional(v,
                    Set(ActionChange(ActionEval(cons, env, store), Nil)),
                    Set(ActionChange(ActionEval(alt, env, store), Nil)))
      case FrameLet(name, bindings, Nil, body, env) =>
        val variables = name :: bindings.reverse.map(_._1)
        val addresses = variables.map(variable => addr.variable(variable, v, t))
        val (env1, store1) = ((name, v) :: bindings)
          .zip(addresses)
          .foldLeft((env, store))({
            case ((env, store), ((variable, value), a)) =>
              (env.extend(variable, a), store.extend(a, value))
          })
        val ActionChange(action, actionEdges) = evalBody(body, env1, store1)
        Set(ActionChange(action, actionPushVal :: ActionDefineAddressesT[SchemeExp, Abs, Addr](addresses) :: actionEdges))
      case FrameLet(name, bindings, (variable, e) :: toeval, body, env) =>
        val action = ActionPush(FrameLet(variable, (name, v) :: bindings, toeval, body, env),
                                e, env, store)
        Set(ActionChange(action, List(actionPushVal)))
      case FrameLetStar(name, bindings, body, env) =>
        val a = addr.variable(name, abs.bottom, t)
        val env1 = env.extend(name, a)
        val store1 = store.extend(a, v)
        val actionEdges = List(actionPushVal, ActionAllocAddressesT[SchemeExp, Abs, Addr](List(a)))
        bindings match {
          case Nil =>
            val ActionChange(actions, actionEdges2) = evalBody(body, env1, store1)
            Set(ActionChange(actions, actionEdges ++ actionEdges2))
          case (variable, exp) :: rest =>
            val action = ActionPush(FrameLetStar(variable, rest, body, env1), exp, env1, store1)
            Set(ActionChange(action, actionEdges))
        }
      case FrameLetrec(a, Nil, body, env) =>
        val ActionChange(action, actionEdges) = evalBody(body, env, store.update(a, v))
        Set(ActionChange(action, ActionSetAddressT[SchemeExp, Abs, Addr](a) :: actionEdges))
      case FrameLetrec(a, (a1, exp) :: rest, body, env) =>
        Set(ActionChange(ActionPush(FrameLetrec(a1, rest, body, env), exp, env, store.update(a, v)),
                         List(ActionSetAddressT(a))))
      case FrameSet(name, env) =>
        env.lookup(name) match {
          case Some(a) =>
            Set(ActionChange(ActionReachedValue(sabs.inject(false), store.update(a, v), Set(EffectWriteVariable(a))),
                             List(ActionSetAddressT(a))))
          case None => addNilStateChangeEdges(ActionError[SchemeExp, Abs, Addr](UnboundVariable(name)))
        }
      case FrameBegin(body, env) =>
        Set(evalBody(body, env, store))
      case FrameCond(cons, clauses, env) =>
        val falseValue = sabs.inject(false)
        conditional(
          v,
          if (cons.isEmpty) { Set(ActionChange(ActionReachedValue(v, store), List(ActionReachedValueT(v)))) }
          else { Set(evalBody(cons, env, store)) },
          clauses match {
            case Nil =>
              Set(ActionChange(ActionReachedValue(falseValue, store), List(ActionReachedValueT(falseValue))))
            case (exp, cons2) :: rest =>
              addNilStateChangeEdges(ActionPush(FrameCond(cons2, rest, env), exp, env, store))
          })
      case FrameCase(clauses, default, env) =>
        val fromClauses = clauses.flatMap({
          case (values, body) =>
            if (values.exists(v2 =>
                  evalValue(v2.value) match {
                    case None => false
                    case Some(v2) => sabs.subsumes(v, v2)
                }))
              /* TODO: precision could be improved by restricting v to v2 */
              Set[ActionChange[SchemeExp, Abs, Addr]](evalBody(body, env, store))
            else
              Set[ActionChange[SchemeExp, Abs, Addr]]()
        })
        /* TODO: precision could be improved in cases where we know that default is not
         * reachable */
        fromClauses.toSet + evalBody(default, env, store)
      case FrameAnd(Nil, env) =>
        val falseValue = sabs.inject(false)
        conditional(v,
                    Set(ActionChange(ActionReachedValue(v, store), List(ActionReachedValueT(v)))),
                    Set(ActionChange(ActionReachedValue(falseValue, store), List(ActionReachedValueT(falseValue)))))
      case FrameAnd(e :: rest, env) =>
        val falseValue = sabs.inject(false)
        conditional(v,
                    addNilStateChangeEdges(ActionPush(FrameAnd(rest, env), e, env, store)),
                    Set(ActionChange(ActionReachedValue(falseValue, store), List(ActionReachedValueT(falseValue)))))
      case FrameOr(Nil, env) =>
        val falseValue = sabs.inject(false)
        conditional(v,
                    Set(ActionChange(ActionReachedValue(v, store), List(ActionReachedValueT(v)))),
                    Set(ActionChange(ActionReachedValue(falseValue, store), List(ActionReachedValueT(falseValue)))))
      case FrameOr(e :: rest, env) =>
        conditional(v,
                    Set(ActionChange(ActionReachedValue(v, store), List(ActionReachedValueT(v)))),
                    addNilStateChangeEdges(ActionPush(FrameOr(rest, env), e, env, store)))
      case FrameDefine(name, env) =>
        throw new Exception(
          s"TODO: define not handled (no global environment)")
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
      store: Store[Addr, Abs]): Option[(Abs, Set[Effect[Addr]])] = e match {
    case λ: SchemeLambda =>
      Some((sabs.inject[SchemeExp, Addr]((λ, env)), Set()))
    case SchemeIdentifier(name, _) =>
      env
        .lookup(name)
        .flatMap(a =>
          store.lookup(a).map(v => (v, Set(EffectReadVariable(a)))))
    case SchemeValue(v, _) => evalValue(v).map(value => (value, Set()))
    case _ => None
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

  override protected def funcallArgs(
      f: Abs,
      fexp: SchemeExp,
      args: List[(SchemeExp, Abs)],
      toeval: List[SchemeExp],
      env: Environment[Addr],
      store: Store[Addr, Abs],
      t: Time): Set[ActionChange[SchemeExp, Abs, Addr]] = toeval match {
    case Nil => evalCall(f, fexp, args.reverse, store, t)
    case e :: rest =>
      atomicEval(e, env, store) match {
        case Some((v, effs)) =>
          funcallArgs(f, fexp, (e, v) :: args, rest, env, store, t)
            .map( (actionChange: ActionChange[SchemeExp, Abs, Addr]) =>
              ActionChange(addEffects(actionChange.action, effs), actionChange.actionEdge) )
        case None =>
          addNilStateChangeEdges(ActionPush(FrameFuncallOperands(f, fexp, e, args, rest, env), e, env, store))
      }
  }

  /**
    * Optimize the following pattern: when we see an ActionPush(frame, exp, env, store)
    * where exp is an atomic expression, we can atomically evaluate exp to get v,
    * and call stepKont(v, store, frame).
    */
  protected def optimizeAtomic(actionChanges: Set[ActionChange[SchemeExp, Abs, Addr]],
                               t: Time): Set[ActionChange[SchemeExp, Abs, Addr]] = {
    actionChanges.flatMap( (actionChange) => actionChange match {
      case ActionChange(ActionPush(frame, exp, env, store, effects), stateChanges) =>
        atomicEval(exp, env, store) match {
          case Some((v, effs)) =>
            stepKont(v, frame, store, t).map( (actionChange: ActionChange[SchemeExp, Abs, Addr]) =>
              ActionChange(addEffects(actionChange.action, effs ++ effects), actionChange.actionEdge) )
          case None =>
            Set[ActionChange[SchemeExp, Abs, Addr]](ActionChange(ActionPush(frame, exp, env, store, effects), stateChanges))
        }
      case actionChange => Set[ActionChange[SchemeExp, Abs, Addr]](actionChange)
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
