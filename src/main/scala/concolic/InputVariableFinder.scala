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

  /**
    * Returns all exact input variables (including their value) that are defined in the given path constraint, in order
    * of their definition, as well as the set of all inexact input variables (i.e., the input variables that do not
    * have an exact definition).
    * An exact input definition is a ConcolicExpression where one input variable is checked to be equal to an integer
    * constant.
    * @param pathConstraint
    * @return A tuple of the exact and inexact input variables.
    */
  def findInputVariablesDefinitions(pathConstraint: PathConstraint): (List[(ConcolicInput, Int)], Set[ConcolicInput]) = {
    val exactInputVariables = ExactSymbolicVariablesFinder.filterExactInputVariables(pathConstraint)
    val allInputVariables = findAllInputVariables(pathConstraint)
    /* An inexact input variable is an input variable for which no exact definition (e.g., i0 = 0) exists. */
    val inexactInputVariables = allInputVariables.filter(inputVariable => exactInputVariables.forall(_._1 != inputVariable))
    (exactInputVariables, inexactInputVariables)
  }

  def containsNonDefinitionalConstraints(pathConstraint: PathConstraint): Boolean = {
    pathConstraint.forall({
      case (BranchConstraint(exp), true) => exp match {
        case RelationalConcolicExpression(_: ConcolicInput, IntEqual, _: ConcolicInt) => true
        case RelationalConcolicExpression(_: ConcolicInt, IntEqual, _: ConcolicInput) => true
        case _ => false
      }
      case _ => false
    })
  }

  def pathConstraintContainsInexactInputVariables(pathConstraint: PathConstraint): Boolean = {
    val exactInputVariables = ExactSymbolicVariablesFinder.filterExactInputVariables(pathConstraint).map(_._1).toSet
    val allInputsVariables = findAllInputVariables(pathConstraint)
    (allInputsVariables -- exactInputVariables).nonEmpty
  }

}
