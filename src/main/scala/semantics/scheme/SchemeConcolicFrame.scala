import backend.expression._

trait FrameStoresConcolicExpression {
  def optConcolicExpression: Option[ConcolicExpression]
}

case class FrameConcolicAnd[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  rest: List[SchemeExp],
  evaluatedSymbolicValues: List[Option[ConcolicExpression]],
  env: Environment[Addr])
  extends ConvertableSchemeFrame[Abs, Addr, Time] {

  def generateConcolicExpression(concreteValue: Boolean): Option[ConcolicExpression] = evaluatedSymbolicValues match {
    case Nil =>
      Some(ConcolicBool(concreteValue))
    case (head : Option[BooleanConcolicExpression]) :: rest =>
      val optLeftOperand = rest.foldLeft[Option[BooleanConcolicExpression]](head)((optAcc, optConcolicValue) => optAcc.flatMap(acc => optConcolicValue.flatMap(concolicValue => (acc, concolicValue) match {
        case (acc: BooleanConcolicExpression, concolicValue : BooleanConcolicExpression) =>
          Some(LogicalConcolicExpression(acc, LogicalAnd, concolicValue))
        case _ => None
      })))
      optLeftOperand.map(LogicalConcolicExpression(_, LogicalAnd, ConcolicBool(concreteValue)))
    case _ =>
      None
  }

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameAnd(rest, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr], envReaches: Environment[Addr] => Set[Addr], addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)

}

case class FrameConcolicOr[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  rest: List[SchemeExp],
  evaluatedSymbolicValues: List[Option[ConcolicExpression]],
  env: Environment[Addr])
  extends ConvertableSchemeFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameOr(rest, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr], envReaches: Environment[Addr] => Set[Addr], addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)

}

case class FrameConcolicFuncallOperands[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  f: Abs,
  fexp: SchemeExp,
  cur: SchemeExp,
  args: List[(SchemeExp, Abs, Option[ConcolicExpression])],
  toeval: List[SchemeExp],
  env: Environment[Addr])
  extends ConvertableSchemeFrame[Abs, Addr, Time] {

  override def toString: String = s"FrameConcolicFuncallOperands($f, $args, $env)"

  override def meaningfullySubsumes = true
  override def subsumes(that: Frame): Boolean = that match {
    case that: FrameConcolicFuncallOperands[Abs, Addr, Time] =>
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

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameFuncallOperands(convertValue(f),
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



case class FrameConcolicLet[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  variable: Identifier,
  bindings: List[(Identifier, Abs)],
  toeval: List[(Identifier, SchemeExp)],
  body: List[SchemeExp],
  env: Environment[Addr],
  optConcolicExpression: Option[ConcolicExpression])
  extends ConvertableSchemeFrame[Abs, Addr, Time] with FrameStoresConcolicExpression {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameLet(variable,
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

case class FrameConcolicLetStar[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  variable: Identifier,
  bindings: List[(Identifier, SchemeExp)],
  body: List[SchemeExp],
  env: Environment[Addr],
  optConcolicExpression: Option[ConcolicExpression])
  extends ConvertableSchemeFrame[Abs, Addr, Time] with FrameStoresConcolicExpression {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameLetStar(variable, bindings, body, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr],
    envReaches: Environment[Addr] => Set[Addr],
    addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameConcolicSet[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  variable: Identifier,
  env: Environment[Addr],
  optConcolicExpression: Option[ConcolicExpression])
  extends ConvertableSchemeFrame[Abs, Addr, Time] with FrameStoresConcolicExpression {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameSet(variable, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr],
    envReaches: Environment[Addr] => Set[Addr],
    addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}