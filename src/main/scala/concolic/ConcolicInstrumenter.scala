object ConcolicInstrumenter {

  private def addPopEnvExp(body: List[SchemeExp]): List[SchemeExp] = {
    val instrumentedBody = body.map(instrument)
    instrumentedBody :+ SchemePopSymEnv(instrumentedBody.last.pos)
  }

  def instrument(exp: SchemeExp): SchemeExp = exp match {
    case SchemeDefineFunction(name, args, body, pos) =>
      val instrumentedBody = addPopEnvExp(body)
      SchemeDefineFunction(name, args, instrumentedBody, pos)
    case SchemeLambda(args, body, pos) =>
      val instrumentedBody = addPopEnvExp(body)
      SchemeLambda(args, instrumentedBody, pos)
    case SchemeStartAnalysis(_) =>
      exp
    case SchemeFuncall(f, args, pos) =>
      SchemeFuncall(instrument(f), args.map(instrument), pos)
    case SchemeIf(cond, cons, alt, pos) =>
      SchemeIf(instrument(cond), instrument(cons), instrument(alt), pos)
    case SchemeLet(bindings, body, pos) =>
      SchemeLet(bindings.map( (binding) => (binding._1, instrument(binding._2))), body.map(instrument), pos)
    case SchemeLetrec(bindings, body, pos) =>
      SchemeLetrec(bindings.map( (binding) => (binding._1, instrument(binding._2))), body.map(instrument), pos)
    case SchemeLetStar(bindings, body, pos) =>
      SchemeLetStar(bindings.map( (binding) => (binding._1, instrument(binding._2))), body.map(instrument), pos)
    case SchemeSet(varName, value, pos) =>
      SchemeSet(varName, instrument(value), pos)
    case SchemeBegin(exps, pos) =>
      SchemeBegin(exps.map(instrument), pos)
    case SchemeCond(clauses, pos) =>
      SchemeCond(clauses.map( (clause) => (instrument(clause._1), clause._2.map(instrument)) ), pos)
    case SchemeCase(key, clauses, default, pos) =>
      SchemeCase(instrument(key), clauses.map( (clause) => (clause._1, clause._2.map(instrument)) ), default.map(instrument), pos)
    case SchemeAnd(exps, pos) =>
      SchemeAnd(exps.map(instrument), pos)
    case SchemeOr(exps, pos) =>
      SchemeOr(exps.map(instrument), pos)
    case SchemeDefineVariable(varName, exp, pos) =>
      SchemeDefineVariable(varName, instrument(exp), pos)
    case SchemeIdentifier(_, _) =>
      exp
    case SchemeQuoted(_, _) =>
      exp
    case SchemeValue(_, _) =>
      exp
    case SchemeCas(varName, eold, enew, pos) =>
      SchemeCas(varName, instrument(eold), instrument(enew), pos)
    case SchemeCasVector(varName, index, eold, enew, pos) =>
      SchemeCasVector(varName, instrument(index), instrument(eold), instrument(enew), pos)
    case SchemeAcquire(exp, pos) =>
      SchemeAcquire(instrument(exp), pos)
    case SchemeRelease(exp, pos) =>
      SchemeRelease(instrument(exp), pos)
    case SchemeSpawn(exp, pos) =>
      SchemeSpawn(instrument(exp), pos)
    case SchemeJoin(exp, pos) =>
      SchemeJoin(instrument(exp), pos)
  }

}
