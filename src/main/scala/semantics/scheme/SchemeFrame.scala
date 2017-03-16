import PrimitiveDefinitions._

abstract class SchemeFrame[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp] extends Frame {
  type Address = Addr

  val sabs = implicitly[IsSchemeLattice[Abs]]

  def subsumes(that: Frame) = that.equals(this)
  override def toString = s"${this.getClass.getSimpleName}"

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                              convertEnv: Environment[Addr] => Environment[Addr],
                                              abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time])
  : SchemeFrame[OtherAbs, Addr, Time]

  def reaches(valueReaches: Abs => Set[Addr],
              envReaches: Environment[Addr] => Set[Addr],
              addressReaches: Addr => Set[Addr]): Set[Addr]

}

case class FrameFuncallOperator[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    fexp: SchemeExp,
    args: List[SchemeExp],
    env: Environment[Addr])
    extends SchemeFrame[Abs, Addr, Time] {

  override def savesEnv: Option[Environment[Address]] = Some(env)
  override def savedValues[Abs] = Nil

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameFuncallOperator(fexp, args, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr],
              envReaches: Environment[Addr] => Set[Addr],
              addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)

}

case class FrameFuncallOperands[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    f: Abs,
    fexp: SchemeExp,
    cur: SchemeExp,
    args: List[(SchemeExp, Abs)],
    toeval: List[SchemeExp],
    env: Environment[Addr])
    extends SchemeFrame[Abs, Addr, Time] {

  override def savesEnv: Option[Environment[Address]] = Some(env)
  override def savedValues[Abs] = args.map(_._2.asInstanceOf[Abs]) :+ f.asInstanceOf[Abs]

  override def toString: String = s"FrameFuncallOperands($f, $args, $env)"

  override def meaningfullySubsumes = true
  override def subsumes(that: Frame): Boolean = that match {
    case that: FrameFuncallOperands[Abs, Addr, Time] =>
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

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
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

case class FrameIf[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    cons: SchemeExp, alt: SchemeExp, env: Environment[Addr])
    extends SchemeFrame[Abs, Addr, Time] {

  override def savesEnv: Option[Environment[Address]] = Some(env)

  override def meaningfullySubsumes = true
  override def subsumes(that: Frame): Boolean = that match {
    case that: FrameIf[Abs, Addr, Time] =>
      cons == that.cons &&
      alt == that.alt &&
      env.subsumes(that.env)
    case _ => false

  }

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameIf(cons, alt, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr],
              envReaches: Environment[Addr] => Set[Addr],
              addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameLet[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    variable: String,
    bindings: List[(String, Abs)],
    toeval: List[(String, SchemeExp)],
    body: List[SchemeExp],
    env: Environment[Addr])
    extends SchemeFrame[Abs, Addr, Time] {

  override def savesEnv: Option[Environment[Address]] = Some(env)
  override def savedValues[Abs] = bindings.map(_._2.asInstanceOf[Abs])

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
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

case class FrameLetStar[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    variable: String,
    bindings: List[(String, SchemeExp)],
    body: List[SchemeExp],
    env: Environment[Addr])
    extends SchemeFrame[Abs, Addr, Time] {

  override def savesEnv: Option[Environment[Address]] = Some(env)

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameLetStar(variable, bindings, body, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr],
              envReaches: Environment[Addr] => Set[Addr],
              addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameLetrec[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    addr: Addr,
    bindings: List[(Addr, SchemeExp)],
    body: List[SchemeExp],
    env: Environment[Addr])
    extends SchemeFrame[Abs, Addr, Time] {

  override def savesEnv: Option[Environment[Address]] = Some(env)

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) = {
    val addressConverter = new DefaultHybridAddressConverter[SchemeExp]()
    FrameLetrec(addressConverter.convertAddress(addr.asInstanceOf[HybridAddress.A]).asInstanceOf[Addr],
                bindings.map( (binding) =>
                  (addressConverter.convertAddress(binding._1.asInstanceOf[HybridAddress.A]).asInstanceOf[Addr], binding._2)),
                body,
                convertEnv(env))
  }

  def reaches(valueReaches: Abs => Set[Addr],
              envReaches: Environment[Addr] => Set[Addr],
              addressReaches: Addr => Set[Addr]): Set[Addr] =
    addressReaches(addr) ++ envReaches(env)
}

case class FrameSet[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    variable: String, env: Environment[Addr])
    extends SchemeFrame[Abs, Addr, Time] {

  override def writeEffectsFor(): Set[Address] = env.lookup(variable) match {
    case Some(a) => Set(a)
    case None => Set()
  }

  override def savesEnv: Option[Environment[Address]] = Some(env)

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameSet(variable, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr],
              envReaches: Environment[Addr] => Set[Addr],
              addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameBegin[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    rest: List[SchemeExp], env: Environment[Addr])
    extends SchemeFrame[Abs, Addr, Time] {

  override def savesEnv: Option[Environment[Address]] = Some(env)

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameBegin(rest, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr],
              envReaches: Environment[Addr] => Set[Addr],
              addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameCond[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    cons: List[SchemeExp],
    clauses: List[(SchemeExp, List[SchemeExp])],
    env: Environment[Addr])
    extends SchemeFrame[Abs, Addr, Time] {

  override def savesEnv: Option[Environment[Address]] = Some(env)

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameCond(cons, clauses, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr],
              envReaches: Environment[Addr] => Set[Addr],
              addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameCase[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    clauses: List[(List[SchemeValue], List[SchemeExp])],
    default: List[SchemeExp],
    env: Environment[Addr])
    extends SchemeFrame[Abs, Addr, Time] {

  override def savesEnv: Option[Environment[Address]] = Some(env)

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameCase(clauses, default, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr],
              envReaches: Environment[Addr] => Set[Addr],
              addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameAnd[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    rest: List[SchemeExp], env: Environment[Addr])
    extends SchemeFrame[Abs, Addr, Time] {

  override def savesEnv: Option[Environment[Address]] = Some(env)

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameAnd(rest, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr],
              envReaches: Environment[Addr] => Set[Addr],
              addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameOr[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    rest: List[SchemeExp], env: Environment[Addr])
    extends SchemeFrame[Abs, Addr, Time] {

  override def savesEnv: Option[Environment[Address]] = Some(env)

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameOr(rest, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr],
              envReaches: Environment[Addr] => Set[Addr],
              addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameDefine[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    variable: String,
    env: Environment[Addr])
    extends SchemeFrame[Abs, Addr, Time] {

  override def savesEnv: Option[Environment[Address]] = Some(env)

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameDefine(variable, convertEnv(env))

  def reaches(valueReaches: Abs => Set[Addr],
              envReaches: Environment[Addr] => Set[Addr],
              addressReaches: Addr => Set[Addr]): Set[Addr] = envReaches(env)
}

case class FrameHigherOrderPrimCall[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    prim: Primitive[Addr, Abs],
    state: PrimitiveApplicationState)
    extends SchemeFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsConvertableLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: BaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameHigherOrderPrimCall(prim.convert[Addr, OtherAbs](new SchemePrimitives[Addr, OtherAbs]), state)

  def reaches(valueReaches: Abs => Set[Addr],
              envReaches: Environment[Addr] => Set[Addr],
              addressReaches: Addr => Set[Addr]): Set[Addr] = Set()
}
