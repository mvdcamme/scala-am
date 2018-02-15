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

  /* TODO finish this */
  def isConstraintConstant(constraint: BranchConstraint): Boolean = optimizeConstraint(constraint).exp match {
    case _: ConcolicBool => true
    case RelationalConcolicExpression(left, _, right) => (left, right) match {
      case (ConcolicInt(_), ConcolicInt(_)) => true
      case _ => false
    }
    case _ => false


  }

}
