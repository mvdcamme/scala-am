import backend.PathConstraint
import backend.expression._
import backend.tree._

object ConstraintOptimizer {

  def splitReduced(exps: List[ConcolicExpression]): (List[Int], List[ConcolicExpression]) = {
    exps.foldLeft[(List[Int], List[ConcolicExpression])]((Nil, Nil))((acc, exp) => reduceToInt(exp) match {
      case Left(exp) => (acc._1, acc._2 :+ exp)
      case Right(i) => (acc._1 :+ i, acc._2)
    })
  }

  private def reduceArithExp(exp: ArithmeticalConcolicExpression, reduce: List[Int] => Int): Either[ArithmeticalConcolicExpression, Int] = {
    val (ints, args) = splitReduced(exp.exps)
    val reducedInts = reduce(ints)
    val result: Either[ArithmeticalConcolicExpression, Int] = if (args.isEmpty) {
      /* All arguments could be reduced to an integer */
      Right(reducedInts)
    } else if (ints.nonEmpty) {
      /* At least one argument that could not be reduced, but exp can still be optimized by adding together arguments */
      val newArgs = ConcolicInt(reducedInts) :: args
      Left(exp.copy(exps = newArgs))
    } else {
      Left(exp)
    }
    result
  }

  private def optimizeArithExp(exp: ArithmeticalConcolicExpression): Either[ArithmeticalConcolicExpression, Int] = exp.op match {
    case IntPlus => reduceArithExp(exp, _.sum)
    case IntMinus =>
      def subtract(list: List[Int]): Int = list match {
        case Nil => 0
        case head :: rest =>
          head - rest.sum
      }
      reduceArithExp(exp, subtract)
    case IntTimes => reduceArithExp(exp, _.product)
    case _ =>
      /* TODO Optimize other arithmetical operations as well */
      Left(exp)
  }

  private def eitherToConcolicExp(either: Either[ConcolicExpression, Int]): ConcolicExpression = either match {
    case Left(exp) => exp
    case Right(i) => ConcolicInt(i)
  }

  private def optimizeBoolExp(exp: BooleanConcolicExpression): BooleanConcolicExpression = exp match {
    case RelationalConcolicExpression(left, op, right) =>
      val optimizedLeft = left match {
        case left: ArithmeticalConcolicExpression =>
          val optimized = optimizeArithExp(left)
          eitherToConcolicExp(optimized)
        case _ => left
      }
      val optimizedRight = right match {
        case right: ArithmeticalConcolicExpression =>
          val optimized = optimizeArithExp(right)
          eitherToConcolicExp(optimized)
        case _ => right
      }
      RelationalConcolicExpression(optimizedLeft, op, optimizedRight)
    case _ => exp
  }

  private def reduceToInt(exp: ConcolicExpression): Either[ConcolicExpression, Int] = exp match {
    case ConcolicInt(i) => Right(i)
    case exp: ArithmeticalConcolicExpression => optimizeArithExp(exp)
  }

  def optimizeConstraint(constraint: BranchConstraint): BranchConstraint = {
    val optimizedExp = optimizeBoolExp(constraint.exp)
    BranchConstraint(optimizedExp)
  }

  def optimizeReport(report: PathConstraint): PathConstraint = {
    report.map(tuple => {
      val optimizedConstraint = optimizeConstraint(tuple._1)
      (optimizedConstraint, tuple._2)
    })
  }

}
