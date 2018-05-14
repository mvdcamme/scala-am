import backend.expression._

import concolic.SymbolicEnvironment

sealed trait ConcolicFrame extends Frame {
  /*
   * Don't intend to actually use this method;
   * this is just a check to make sure we didn't forget to include a SymbolicEnvironment in the frame.
   */
  protected def symEnv: SymbolicEnvironment
}
sealed trait SchemeConcolicFrame[Abs, Addr, Time] extends ConcolicFrame {
  def env: Environment[Addr]
  def symEnv: SymbolicEnvironment
  def equalModuloTimestamp(other: SchemeConcolicFrame[Abs, Addr, Time], mapping: Mapping[Time]): Option[Mapping[Time]]
}

case class FrameConcolicFuncallOperator[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp: CompareTimestampsWithMapping](
  fexp: SchemeExp,
  args: List[SchemeExp],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame[Abs, Addr, Time] {
  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameFuncallOperator(fexp, args, convertEnv(env))
  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = envReaches(env, symEnv)
  def equalModuloTimestamp(other: SchemeConcolicFrame[Abs, Addr, Time], mapping: Mapping[Time]): Option[Mapping[Time]] = other match {
      /* Don't compare the symbolic environments because those might also be different between two states from different concrete executions. */
    case FrameConcolicFuncallOperator(fexp2, args2, env2, _) if this.fexp == fexp2 && this.args == args2 => this.env.equalModuloTimestamp[Time](env2, mapping)
    case _ => None
  }
}

case class FrameConcolicFuncallOperands[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp: CompareTimestampsWithMapping](
  f: Abs,
  fexp: SchemeExp,
  cur: SchemeExp,
  args: List[(SchemeExp, Abs, Option[ConcolicExpression])],
  toeval: List[SchemeExp],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame[Abs, Addr, Time] {
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

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = {
    valueReaches(f) ++ args.foldLeft(Reached.empty[Addr])((acc, arg) => acc ++ valueReaches(arg._2)) ++ envReaches(env, symEnv)
  }
  def equalModuloTimestamp(other: SchemeConcolicFrame[Abs, Addr, Time], mapping: Mapping[Time]): Option[Mapping[Time]] = other match {
    /* Don't compare the symbolic environments because those might also be different between two states from different concrete executions. */
    case FrameConcolicFuncallOperands(f2, fexp2, cur2, args2, toeval2, env2, _)
         if this.fexp == fexp2 && this.cur == cur2 && this.args.map(_._1) == args2.map(_._1) && this.toeval == toeval2 =>
      for {
        fMapping <- implicitly[IsSchemeLattice[Abs]].equalModuloTimestamp[Time, Addr](this.f, f2, mapping)
        argsMapping <- this.args.zip(args2).foldLeft[Option[Mapping[Time]]](Some(fMapping))({
          case (accMapping, ((_, arg1, _), (_, arg2, _))) => accMapping.flatMap(implicitly[IsSchemeLattice[Abs]].equalModuloTimestamp[Time, Addr](arg1, arg2, _))
        })
        envMapping <- this.env.equalModuloTimestamp[Time](env2, argsMapping)
      } yield {
        envMapping
      }
    case _ => None
  }
}

case class FrameConcolicIf[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp: CompareTimestampsWithMapping](
  cons: SchemeExp,
  alt: SchemeExp,
  env: Environment[Addr],
  symEnv: SymbolicEnvironment,
  ifExp: SchemeIf)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame[Abs, Addr, Time] {

  override def meaningfullySubsumes = true
  override def subsumes(that: Frame): Boolean = that match {
    case that: FrameConcolicIf[Abs, Addr, Time] => cons == that.cons && alt == that.alt && env.subsumes(that.env)
    case _ => false
  }

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameIf(cons, alt, convertEnv(env), ifExp)

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = envReaches(env, symEnv)
  def equalModuloTimestamp(other: SchemeConcolicFrame[Abs, Addr, Time], mapping: Mapping[Time]): Option[Mapping[Time]] = other match {
    case FrameConcolicIf(cons2, alt2, env2, _, ifExp2) if this.cons == cons2 && this.alt == alt2 && this.ifExp == ifExp2 =>
      this.env.equalModuloTimestamp[Time](env2, mapping)
    case _ => None
  }
}

case class FrameConcolicLet[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp: CompareTimestampsWithMapping](
  variable: Identifier,
  bindings: List[(Identifier, Abs, Option[ConcolicExpression])],
  toeval: List[(Identifier, SchemeExp)],
  body: List[SchemeExp],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameLet(variable,
      bindings.map((binding) => (binding._1, convertValue(binding._2))),
      toeval,
      body,
      convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
    addressReaches: Addr => Reached[Addr]): Reached[Addr] = {
    bindings.foldLeft(Reached.empty[Addr])((acc, binding) => acc ++ valueReaches(binding._2)) ++ envReaches(env, symEnv)
  }
  def equalModuloTimestamp(other: SchemeConcolicFrame[Abs, Addr, Time], mapping: Mapping[Time]): Option[Mapping[Time]] = other match {
    case FrameConcolicLet(variable2, bindings2, toeval2, body2, env2, _)
         if this.variable == variable2 && this.bindings.map(_._1) == bindings2.map(_._1) && this.toeval == toeval2 && this.body == body2 =>
      for {
        bindingsMapping <- this.bindings.zip(bindings2).foldLeft[Option[Mapping[Time]]](Some(mapping))({
          case (accMapping, ((_, value1, _), (_, value2, _))) => accMapping.flatMap(implicitly[IsSchemeLattice[Abs]].equalModuloTimestamp[Time, Addr](value1, value2, _))
        })
        envMapping <- this.env.equalModuloTimestamp[Time](env2, bindingsMapping)
      } yield {
        envMapping
      }
    case _ => None
  }
}

case class FrameConcolicLetStar[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp: CompareTimestampsWithMapping](
  variable: Identifier,
  bindings: List[(Identifier, SchemeExp)],
  body: List[SchemeExp],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameLetStar(variable, bindings, body, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = {
    envReaches(env, symEnv)
  }
  def equalModuloTimestamp(other: SchemeConcolicFrame[Abs, Addr, Time], mapping: Mapping[Time]): Option[Mapping[Time]] = other match {
    case FrameConcolicLetStar(variable2, bindings2, body2, env2, _) if this.variable == variable2 && this.bindings == bindings2 && this.body == body2 =>
      this.env.equalModuloTimestamp[Time](env2, mapping)
    case _ => None
  }
}

case class FrameConcolicLetrec[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp: CompareTimestampsWithMapping](
  addr: Addr,
  variable: Identifier,
  bindings: List[(Addr, Identifier, SchemeExp)],
  body: List[SchemeExp],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) = {
    val addressConverter = new DefaultHybridAddressConverter[SchemeExp]()
    FrameLetrec(addressConverter.convertAddress(addr.asInstanceOf[HybridAddress.A]).asInstanceOf[Addr],
      bindings.map( (binding) =>
        (addressConverter.convertAddress(binding._1.asInstanceOf[HybridAddress.A]).asInstanceOf[Addr], binding._3)),
      body,
      convertEnv(env))
  }

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = {
    addressReaches(addr) ++ envReaches(env, symEnv)
  }
  def equalModuloTimestamp(other: SchemeConcolicFrame[Abs, Addr, Time], mapping: Mapping[Time]): Option[Mapping[Time]] = other match {
    case FrameConcolicLetrec(addr2, variable2, bindings2, body2, env2, _)
         if this.variable == variable2 && this.bindings == bindings2 && this.body == body2 =>
      for {
        addrMapping <- implicitly[Address[Addr]].equalModuloTimestamp(this.addr, addr2, mapping)
        envMapping <- this.env.equalModuloTimestamp[Time](env2, addrMapping)
      } yield {
        envMapping
      }
    case _ => None
  }
}

case class FrameConcolicSet[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp: CompareTimestampsWithMapping](
  variable: Identifier,
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs,
    convertEnv: Environment[Addr] => Environment[Addr],
    abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameSet(variable, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = {
    envReaches(env, symEnv)
  }
  def equalModuloTimestamp(other: SchemeConcolicFrame[Abs, Addr, Time], mapping: Mapping[Time]): Option[Mapping[Time]] = other match {
    case FrameConcolicSet(variable2, env2, _) if this.variable == variable2 => this.env.equalModuloTimestamp[Time](env2, mapping)
    case _ => None
  }
}

case class FrameConcolicBegin[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp: CompareTimestampsWithMapping](
  rest: List[SchemeExp],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameBegin(rest, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = {
    envReaches(env, symEnv)
  }
  def equalModuloTimestamp(other: SchemeConcolicFrame[Abs, Addr, Time], mapping: Mapping[Time]): Option[Mapping[Time]] = other match {
    case FrameConcolicBegin(rest2, env2, _) if this.rest == rest2 => this.env.equalModuloTimestamp[Time](env2, mapping)
    case _ => None
  }
}

case class FrameConcolicCond[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp: CompareTimestampsWithMapping](
  cons: List[SchemeExp],
  clauses: List[(SchemeExp, List[SchemeExp])],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameCond(cons, clauses, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = {
    envReaches(env, symEnv)
  }
  def equalModuloTimestamp(other: SchemeConcolicFrame[Abs, Addr, Time], mapping: Mapping[Time]): Option[Mapping[Time]] = other match {
    case FrameConcolicCond(cons2, clauses2, env2, _) if this.cons == cons2 && this.clauses == clauses2 =>
      this.env.equalModuloTimestamp[Time](env2, mapping)
    case _ => None
  }
}

case class FrameConcolicCase[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp: CompareTimestampsWithMapping](
  clauses: List[(List[SchemeValue], List[SchemeExp])],
  default: List[SchemeExp],
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameCase(clauses, default, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = {
    envReaches(env, symEnv)
  }
  def equalModuloTimestamp(other: SchemeConcolicFrame[Abs, Addr, Time], mapping: Mapping[Time]): Option[Mapping[Time]] = other match {
    case FrameConcolicCase(clauses2, default2, env2, _) if this.clauses == clauses2 && this.default == default2 =>
      this.env.equalModuloTimestamp[Time](env2, mapping)
    case _ => None
  }
}

case class FrameConcolicDefine[Abs: IsSchemeLattice, Addr: Address, Time: Timestamp: CompareTimestampsWithMapping](
  variable: Identifier,
  env: Environment[Addr],
  symEnv: SymbolicEnvironment)
  extends ConvertableSchemeFrame[Abs, Addr, Time] with SchemeConcolicFrame[Abs, Addr, Time] {

  def convert[OtherAbs: IsSchemeLattice](convertValue: (Abs) => OtherAbs, convertEnv: Environment[Addr] => Environment[Addr], abstSem: ConvertableBaseSchemeSemantics[OtherAbs, Addr, Time]) =
    FrameDefine(variable, convertEnv(env))

  def reaches(valueReaches: Abs => Reached[Addr], envReaches: (Environment[Addr], SymbolicEnvironment) => Reached[Addr],
              addressReaches: Addr => Reached[Addr]): Reached[Addr] = {
    envReaches(env, symEnv)
  }
  def equalModuloTimestamp(other: SchemeConcolicFrame[Abs, Addr, Time], mapping: Mapping[Time]): Option[Mapping[Time]] = other match {
    case FrameConcolicDefine(variable2, env2, _) if this.variable == variable2 =>
      this.env.equalModuloTimestamp[Time](env2, mapping)
    case _ => None
  }
}