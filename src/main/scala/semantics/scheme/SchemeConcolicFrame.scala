import backend.expression._

import concolic.SymbolicEnvironment

sealed trait ConcolicFrame extends Frame {
  /*
   * Don't intend to actually use this method;
   * this is just a check to make sure we didn't forget to include a SymbolicEnvironment in the frame.
   */
  protected def symEnv: SymbolicEnvironment
}
sealed trait SchemeConcolicFrame extends ConcolicFrame

case class FrameConcolicFuncallOperator[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  fexp: SchemeExp,
  args: List[SchemeExp],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame {
  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameFuncallOperator(fexp, args, convertEnv(env))
  def reaches(valueReaches: Abs => Set[Addr], envReaches: Environment[Addr] => Set[Addr], addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameConcolicFuncallOperands[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  f: Abs,
  fexp: SchemeExp,
  cur: SchemeExp,
  args: List[(SchemeExp, Abs, Option[ConcolicExpression])],
  toeval: List[SchemeExp],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame {
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

case class FrameConcolicIf[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  cons: SchemeExp,
  alt: SchemeExp,
  env: Environment[Addr],
  symEnv: SymbolicEnvironment,
  ifExp: SchemeIf)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame {

  override def meaningfullySubsumes = true
  override def subsumes(that: Frame): Boolean = that match {
    case that: FrameIf[Abs, Addr, Time] => cons == that.cons && alt == that.alt && env.subsumes(that.env)
    case _ => false
  }

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameIf(cons, alt, convertEnv(env), ifExp)

  def reaches(valueReaches: Abs => Set[Addr], envReaches: Environment[Addr] => Set[Addr], addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameConcolicLet[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  variable: Identifier,
  bindings: List[(Identifier, Abs, Option[ConcolicExpression])],
  toeval: List[(Identifier, SchemeExp)],
  body: List[SchemeExp],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame {

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
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameLetStar(variable, bindings, body, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr],
    envReaches: Environment[Addr] => Set[Addr],
    addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameConcolicLetrec[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  addr: Addr,
  variable: Identifier,
  bindings: List[(Addr, Identifier, SchemeExp)],
  body: List[SchemeExp],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) = {
    val addressConverter = new DefaultHybridAddressConverter[SchemeExp]()
    FrameLetrec(addressConverter.convertAddress(addr.asInstanceOf[HybridAddress.A]).asInstanceOf[Addr],
      variable,
      bindings.map( (binding) =>
        (addressConverter.convertAddress(binding._1.asInstanceOf[HybridAddress.A]).asInstanceOf[Addr], binding._2, binding._3)),
      body,
      convertEnv(env))
  }

  def reaches(valueReaches: Abs => Set[Addr], envReaches: Environment[Addr] => Set[Addr], addressReaches: Addr => Set[Addr]): Set[Addr] =
    addressReaches(addr) ++ envReaches(env)
}

case class FrameConcolicSet[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  variable: Identifier,
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameSet(variable, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr], envReaches: Environment[Addr] => Set[Addr], addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameConcolicBegin[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  rest: List[SchemeExp],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameBegin(rest, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr], envReaches: Environment[Addr] => Set[Addr], addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameConcolicCond[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  cons: List[SchemeExp],
  clauses: List[(SchemeExp, List[SchemeExp])],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameCond(cons, clauses, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr], envReaches: Environment[Addr] => Set[Addr], addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameConcolicCase[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  clauses: List[(List[SchemeValue], List[SchemeExp])],
  default: List[SchemeExp],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameCase(clauses, default, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr], envReaches: Environment[Addr] => Set[Addr], addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

//case class FrameConcolicAnd[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
//  rest: List[SchemeExp],
//  evaluatedSymbolicValues: List[Option[ConcolicExpression]],
//  env: Environment[Addr],
//  symEnv: SymbolicEnvironment)
//  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame {
//
//  def generateConcolicExpression(concreteValue: Boolean): Option[ConcolicExpression] = evaluatedSymbolicValues match {
//    case Nil =>
//      Some(ConcolicBool(concreteValue))
//    case (head : Option[BooleanConcolicExpression]) :: rest =>
//      val optLeftOperand = rest.foldLeft[Option[BooleanConcolicExpression]](head)((optAcc, optConcolicValue) => optAcc.flatMap(acc => optConcolicValue.flatMap(concolicValue => (acc, concolicValue) match {
//        case (acc: BooleanConcolicExpression, concolicValue : BooleanConcolicExpression) =>
//          Some(LogicalBinaryConcolicExpression(acc, LogicalAnd, concolicValue))
//        case _ => None
//      })))
//      optLeftOperand.map(LogicalBinaryConcolicExpression(_, LogicalAnd, ConcolicBool(concreteValue)))
//    case _ =>
//      None
//  }
//
//  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
//    FrameAnd(rest, convertEnv(env))
//
//  def reaches(valueReaches: Abs => Set[Addr], envReaches: Environment[Addr] => Set[Addr], addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
//
//}
//
//case class FrameConcolicOr[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
//  rest: List[SchemeExp],
//  evaluatedSymbolicValues: List[Option[ConcolicExpression]],
//  env: Environment[Addr],
//  symEnv: SymbolicEnvironment)
//  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame {
//
//  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
//    FrameOr(rest, convertEnv(env))
//
//  def reaches(valueReaches: Abs => Set[Addr], envReaches: Environment[Addr] => Set[Addr], addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
//
//}

case class FrameConcolicDefine[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
  variable: Identifier,
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameDefine(variable, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr], envReaches: Environment[Addr] => Set[Addr], addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}