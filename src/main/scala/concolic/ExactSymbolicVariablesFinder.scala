import backend.PathConstraint
import backend.expression._
import backend.tree.BranchConstraint
import concolic.SymbolicEnvironment

object ExactSymbolicVariablesFinder {

  private def findExactInputVariables(exp: ConcolicExpression): Option[(ConcolicInput, Int)] = exp match {
    case RelationalConcolicExpression(exp1, IntEqual, exp2) => (exp1, exp2) match {
      case (i: ConcolicInput, int: ConcolicInt) => Some((i, int.i))
      case (int: ConcolicInt, i: ConcolicInput) => Some((i, int.i))
      case _ => None
    }
    case _ => None
  }

  private def isExactSymExpressions(exp: ConcolicExpression, exactInputVariables: Set[ConcolicInput]): Boolean = exp match {
    case a: ConcolicAddress =>
      val exp = GlobalSymbolicStore.lookupAddress(a).get
      isExactSymExpressions(exp, exactInputVariables)
    case ArithmeticalConcolicExpression(_, exps) => exps.forall(isExactSymExpressions(_, exactInputVariables))
    case _: ConcolicBool => true
    case _: ConcolicInt => true
    case i: ConcolicInput =>
      val isContained = exactInputVariables.contains(i)
      Logger.log(s"FOund input variable $i, contained in exactInputVariables? $isContained", Logger.U)
      isContained
    case ConcolicObject(_, fields) => fields.values.forall(isExactSymExpressions(_, exactInputVariables))
    case LogicalUnaryConcolicExpression(_, exp) => isExactSymExpressions(exp, exactInputVariables)
    case LogicalBinaryConcolicExpression(left, _, right) =>
      isExactSymExpressions(left, exactInputVariables) && isExactSymExpressions(right, exactInputVariables)
    case RelationalConcolicExpression(left, _, right) =>
      isExactSymExpressions(left, exactInputVariables) && isExactSymExpressions(right, exactInputVariables)
  }

  private def filterExactInputVariables(report: PathConstraint): List[(ConcolicInput, Int)] = {
    report.flatMap({
      /* Only consider the expression if the constraint was actually true */
      case (bc: BranchConstraint, true, _) => findExactInputVariables(bc.exp)
      case _ => Nil
    })
  }

  /**
    * Filters all variables contained in the given environment that correspond to exact symbolic values
    * in the symbolic environment.
    * @param env
    * @param symEnv
    * @return
    */
  def findExactSymbolicVariables(env: Environment[HybridAddress.A], symEnv: SymbolicEnvironment,
                                 pathConstraint: PathConstraint): Set[String] = {
    val exactInputVariables = filterExactInputVariables(pathConstraint).map(_._1).toSet
    Logger.log(s"exactInputVariables are = $exactInputVariables", Logger.E)
    env.keys.filter(name => concolic.lookupVariable(name, symEnv) match {
      case None => false
      case Some(exp) => isExactSymExpressions(exp, exactInputVariables)
    }).toSet
  }

  /* TODO Debugging method */
  def matchSymEnvAndStateEnv(symEnv: SymbolicEnvironment, env: Environment[HybridAddress.A], store: Store[HybridAddress.A, ConcreteConcreteLattice.L]): Unit = {
    env.keys.foreach((variable) => {
      val symVal = concolic.lookupVariable(variable, symEnv)
      val optValue = env.lookup(variable).map(store.lookup)
      if (symVal.isDefined && optValue.isDefined) {
        Logger.log(s"Match for variable $variable, symVal = $symVal, optValue = $optValue", Logger.U)
      }
    })
  }

}
