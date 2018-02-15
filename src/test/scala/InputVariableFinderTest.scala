import backend.expression._
import backend.tree
import backend.tree.{BranchConstraint, Constraint}
import org.scalatest.FunSuite

import scala.language.implicitConversions

class InputVariableFinderTest extends FunSuite {

  private val inputVariableFinder = new InputVariableFinder

  /**
    * Converts numbers smaller than 0 to a ConcolicInt, and numbers greater than or equal to 0 to a ConcolicInput.
    * @param int
    * @return
    */
  implicit private def intToExp(int: Int): ConcolicExpression = if (int < 0) ConcolicInt(int) else ConcolicInput(int)
  implicit private def constraintToTuple(constraint: BranchConstraint): (BranchConstraint, Boolean) = (constraint, true)

  private def dummyBranchConstraintConstructor(exp: ConcolicExpression): (BranchConstraint, Boolean) = {
    (BranchConstraint(RelationalConcolicExpression(exp, IntEqual, ConcolicInt(1))), true)
  }

  test("Simple atomic inputs 1") {
    val input = (List(0, 10, -2, 3): List[ConcolicExpression]).map(dummyBranchConstraintConstructor)
    val expected = Set(ConcolicInput(0), ConcolicInput(3), ConcolicInput(10))
    assert(inputVariableFinder.findAllInputVariables(input) == expected)
    assert(! inputVariableFinder.pathConstraintContainsInexactInputVariables(input))
    val input2 = (BranchConstraint(RelationalConcolicExpression(99, IntGreaterThan, -1)), false) :: input
    assert(inputVariableFinder.pathConstraintContainsInexactInputVariables(input2))
  }

  test("Containts non-exact atomic inputs") {
    val input = (BranchConstraint(RelationalConcolicExpression(99, IntEqual, -1)), false) ::
                (List(0, 10, -2, 3): List[ConcolicExpression]).map(dummyBranchConstraintConstructor)
    assert(inputVariableFinder.pathConstraintContainsInexactInputVariables(input))
  }

  test("Containts exact atomic inputs, with input variable appearing more than once") {
    val input = (BranchConstraint(RelationalConcolicExpression(10, IntEqual, -10)), true) ::
                (List(0, 10, -2, 3): List[ConcolicExpression]).map(dummyBranchConstraintConstructor)
    assert(inputVariableFinder.pathConstraintContainsInexactInputVariables(input))
  }

  test("Non-atomic inputs") {
    val input: List[(BranchConstraint, Boolean)] = List(
      BranchConstraint(RelationalConcolicExpression(1, IntLessThan, ArithmeticalConcolicExpression(IntPlus, List(-1, 1, 2)))),
      BranchConstraint(RelationalConcolicExpression(ArithmeticalConcolicExpression(IntPlus, List(3, 4, 5, ArithmeticalConcolicExpression(IntMinus, List(-1, 6)))),
                                                    IntGreaterThan, 7)),
      BranchConstraint(RelationalConcolicExpression(-10, IntEqual, RelationalConcolicExpression(-11, IntNonEqual, 8))))
    val expected = Set(1, 2, 3, 4, 5, 6, 7, 8).map(ConcolicInput)
    assert(inputVariableFinder.findAllInputVariables(input) == expected)
    assert(inputVariableFinder.pathConstraintContainsInexactInputVariables(input))
  }

  test("Exact atomic inputs") {
    val input = (List(0, 1, 2) : List[ConcolicExpression]).map(dummyBranchConstraintConstructor)
    assert(! inputVariableFinder.pathConstraintContainsInexactInputVariables(input))
  }

}
