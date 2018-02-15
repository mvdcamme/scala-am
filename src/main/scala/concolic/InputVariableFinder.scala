import backend.PathConstraint
import backend.expression._
import backend.tree.{BranchConstraint, UnusableConstraint}

class InputVariableFinder {

  def findAllInputVariablesInExp(exp: ConcolicExpression): Set[ConcolicInput] = exp match {
    case ArithmeticalConcolicExpression(_, exps) => exps.flatMap(findAllInputVariablesInExp).toSet
    case RelationalConcolicExpression(left, _, right) =>
      findAllInputVariablesInExp(left) ++ findAllInputVariablesInExp(right)
    case i: ConcolicInput => Set(i)
    case ConcolicAddress(_) | ConcolicObject(_, _) | ConcolicBool(_) | ConcolicInt(_) => Set()
  }

  def findAllInputVariables(report: PathConstraint): Set[ConcolicInput] = {
    report.foldLeft(Set[ConcolicInput]())((inputVars, tuple) => tuple._1 match {
      case UnusableConstraint => inputVars
      case BranchConstraint(exp) => inputVars ++ findAllInputVariablesInExp(exp)
    })
  }

  def pathConstraintContainsInexactInputVariables(pathConstraint: PathConstraint): Boolean = {
    val exactInputVariables = ExactSymbolicVariablesFinder.filterExactInputVariables(pathConstraint).map(_._1).toSet
    val allInputsVariables = findAllInputVariables(pathConstraint)
    exactInputVariables != allInputsVariables
  }

}
