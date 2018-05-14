import concolic.SymbolicEnvironment

abstract class ConvertableSchemeFrame[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp] extends Frame {
  val sabs = implicitly[IsSchemeLattice[Abs]]
  override def toString = s"${this.getClass.getSimpleName}"
  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]): ConvertableSchemeFrame[OtherAbs, Addr, Time]
  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr]

}

case class FrameFuncallOperator[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    fexp: SchemeExp,
    args: List[SchemeExp],
    env: Environment[Addr]) extends ConvertableSchemeFrame[Abs, Addr, Time] {
  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameFuncallOperator(fexp, args, convertEnv(env))
  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = envReaches(env, ???)
}

case class FrameFuncallOperands[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    f: Abs,
    fexp: SchemeExp,
    cur: SchemeExp,
    args: List[(SchemeExp, Abs)],
    toeval: List[SchemeExp],
    env: Environment[Addr])
    extends ConvertableSchemeFrame[Abs, Addr, Time] {
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

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameFuncallOperands(convertValue(f), fexp, cur, args.map((arg) => (arg._1, convertValue(arg._2))), toeval, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] =
    valueReaches(f) ++ args.foldLeft(Reached.empty[Addr])((acc, arg) => acc ++ valueReaches(arg._2)) ++ envReaches(env, ???)
}

case class FrameIf[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    cons: SchemeExp, alt: SchemeExp, env: Environment[Addr], ifExp: SchemeIf)
    extends ConvertableSchemeFrame[Abs, Addr, Time] {

  override def meaningfullySubsumes = true
  override def subsumes(that: Frame): Boolean = that match {
    case that: FrameIf[Abs, Addr, Time] => cons == that.cons && alt == that.alt && env.subsumes(that.env)
    case _ => false
  }

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameIf(cons, alt, convertEnv(env), ifExp)

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
    addressReaches: Addr => Reached[Addr]): Reached[Addr] = envReaches(env, ???)
}

case class FrameLet[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    variable: Identifier,
    bindings: List[(Identifier, Abs)],
    toeval: List[(Identifier, SchemeExp)],
    body: List[SchemeExp],
    env: Environment[Addr])
    extends ConvertableSchemeFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameLet(variable, bindings.map((binding) => (binding._1, convertValue(binding._2))), toeval, body, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] =
    bindings.foldLeft(Reached.empty[Addr])((acc, binding) => acc ++ valueReaches(binding._2)) ++ envReaches(env, ???)
}

case class FrameLetStar[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    variable: Identifier,
    bindings: List[(Identifier, SchemeExp)],
    body: List[SchemeExp],
    env: Environment[Addr])
    extends ConvertableSchemeFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameLetStar(variable, bindings, body, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = envReaches(env, ???)
}

case class FrameLetrec[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    addr: Addr,
    bindings: List[(Addr, SchemeExp)],
    body: List[SchemeExp],
    env: Environment[Addr])
    extends ConvertableSchemeFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) = {
    val addressConverter = new DefaultHybridAddressConverter[SchemeExp]()
    FrameLetrec(addressConverter.convertAddress(addr.asInstanceOf[HybridAddress.A]).asInstanceOf[Addr],
                bindings.map( (binding) =>
                  (addressConverter.convertAddress(binding._1.asInstanceOf[HybridAddress.A]).asInstanceOf[Addr], binding._2)),
                body,
                convertEnv(env))
  }

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = addressReaches(addr) ++ envReaches(env, ???)
}

case class FrameSet[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    variable: Identifier, env: Environment[Addr])
    extends ConvertableSchemeFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
                                         convertEnv: Environment[Addr] => Environment[Addr],
                                         abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameSet(variable, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = envReaches(env, ???)
}

case class FrameBegin[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    rest: List[SchemeExp], env: Environment[Addr])
    extends ConvertableSchemeFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameBegin(rest, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = envReaches(env, ???)
}

case class FrameCond[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    cons: List[SchemeExp],
    clauses: List[(SchemeExp, List[SchemeExp])],
    env: Environment[Addr])
    extends ConvertableSchemeFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameCond(cons, clauses, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = envReaches(env, ???)
}

case class FrameCase[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    clauses: List[(List[SchemeValue], List[SchemeExp])],
    default: List[SchemeExp],
    env: Environment[Addr])
    extends ConvertableSchemeFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameCase(clauses, default, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = envReaches(env, ???)
}

case class FrameAnd[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    rest: List[SchemeExp], env: Environment[Addr])
    extends ConvertableSchemeFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameAnd(rest, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = envReaches(env, ???)
}

case class FrameOr[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    rest: List[SchemeExp], env: Environment[Addr])
    extends ConvertableSchemeFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameOr(rest, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = envReaches(env, ???)
}

case class FrameDefine[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](
    variable: Identifier,
    env: Environment[Addr])
    extends ConvertableSchemeFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameDefine(variable, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = envReaches(env, ???)
}
