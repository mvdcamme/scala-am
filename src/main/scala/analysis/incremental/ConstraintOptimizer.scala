import backend._
import backend.expression._
import backend.tree._

object ConstraintOptimizer {

  def optimizeConstraint(constraint: BranchConstraint): BranchConstraint = {
    val optimizedExp = ConcolicExpressionOptimizer.optimizeBoolExp(constraint.exp)
    BranchConstraint(optimizedExp)
  }

  def optimizeReport(report: PathConstraintWithMatchers): PathConstraintWithMatchers = {
    report.map({
      case (b: BranchConstraint, constraintWasTrue, maybePartialMatcher) =>
        /* We only optimize actual BranchConstraints */
        val optimizedConstraint = optimizeConstraint(b)
        (optimizedConstraint, constraintWasTrue, maybePartialMatcher)
      case triple => triple
    })
  }

  private def isExpAtomicConstant(exp: ConcolicExpression): Boolean = exp match {
    case _: ConcolicBool | _: ConcolicInt => true
    case _ => false
  }

  def isExpressionConstant(exp: ConcolicExpression): Boolean = exp match {
    case e if isExpAtomicConstant(e) => true
    case boolExp: BooleanConcolicExpression => ConcolicExpressionOptimizer.optimizeBoolExp(boolExp) match {
      case e if isExpAtomicConstant(e) => true
      case RelationalConcolicExpression(left, _, right) => isExpAtomicConstant(left) && isExpAtomicConstant(right)
      case _ => false
    }
    case arithExp: ArithmeticalConcolicExpression => ConcolicExpressionOptimizer.optimizeArithExp(arithExp) match {
      case e if isExpAtomicConstant(e) => true
      case ArithmeticalConcolicExpression(_, args) => args.forall(isExpAtomicConstant)
    }
    case _ => false
  }

  /* TODO finish this */
  def isConstraintConstant(constraint: BranchConstraint): Boolean = optimizeConstraint(constraint).exp match {
    case e if isExpAtomicConstant(e) => true
    case RelationalConcolicExpression(left, _, right) => (left, right) match {
      case (ConcolicInt(_), ConcolicInt(_)) => true
      case _ => false
    }
    case _ => false
  }

}
