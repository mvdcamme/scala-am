import backend.PathConstraint
import backend.expression._
import backend.tree._

object ConstraintOptimizer {

  type EitherReduced = Either[ConcolicExpression, Int]

  def splitReduced(exps: List[ConcolicExpression]): (List[Int], List[ConcolicExpression]) = {
    exps.foldLeft[(List[Int], List[ConcolicExpression])]((Nil, Nil))((acc, exp) => reduceToInt(exp) match {
      case Left(exp) => (acc._1, acc._2 :+ exp)
      case Right(i) => (acc._1 :+ i, acc._2)
    })
  }

  /**
    * Attempts to reduce an ArithmeticConcolicExpression that uses a commutative operator.
    * @param exp
    * @param reduce
    * @return
    */
  private def reduceCommutativeArithExp(exp: ArithmeticalConcolicExpression, reduce: List[Int] => Int): EitherReduced = {
    val (ints, args) = splitReduced(exp.exps)
    val reducedInts = reduce(ints)
    val result: EitherReduced = if (args.isEmpty) {
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

  //TODO Finish this
//  private def reduceNonCommutativeArithExp(exp: ArithmeticalConcolicExpression, reduce: (Int, Int) => Int): EitherReduced = {
//    /* Ordering must be preserved. */
//    exp.exps match {
//      case _ :: _ :: _ =>
//
//        def reduceTwoArgs(a: EitherReduced, b: EitherReduced): Either[List[ArithmeticalConcolicExpression], Int] = (a, b) match {
//          case (Right(int1), Right(int2)) => Right(reduce(int1, int2))
//          case _ => Left(List(a, b))
//        }
//
//        def reduceAllArgs(a: EitherReduced, b: EitherReduced, list: List[EitherReduced]): Either[List[ArithmeticalConcolicExpression], Int] = list match {
//          case Nil => reduceTwoArgs(a, b)
//          case head :: rest =>
//        }
//
//        /* Need at least two arguments to reduce the expression. */
//        val individallyReducedArgs = exp.exps.map(reduceToInt)
//        val reducedArg1 = individallyReducedArgs.head
//        val reducedArg2 = individallyReducedArgs(1)
//        val reducedRest = individallyReducedArgs.tail.tail
//
//        val result: EitherReduced = reduceTwoArgs(reducedArg1, reducedArg2) match {
//          case Right(int) :: Nil =>
//            /* There were only two arguments and both were successfully reduced to integers. */
//            Right(int)
//          case _ =>
//            val reducedArgs = reduceAllArgs(reducedArg1, reducedArg2, reducedRest)
//            reducedArgs match {
//              case Right(int) :: Nil =>
//                /* There were more than two arguments, but all of them were successfully reduced. */
//                Right(int)
//              case _ =>
//                Left(exp.copy(exps = reducedArgs))
//            }
//            ???
//        }
//        result
//      case _ => Left(exp)
//    }
//  }

  private def optimizeArithExp(exp: ArithmeticalConcolicExpression): EitherReduced = exp.op match {
    case IntPlus => reduceCommutativeArithExp(exp, _.sum)
    // TODO Allow for more than three arguments and MAKE THIS GENERIC
    case IntMinus if exp.exps.size == 2 =>
      val reduced1 = reduceToInt(exp.exps.head)
      val reduced2 = reduceToInt(exp.exps(1))
      (reduced1, reduced2) match {
        case (Right(int1), Right(int2)) => Right(int1 - int2)
        case (Right(int1), Left(arg2)) => Left(exp.copy(exps = List(ConcolicInt(int1), arg2)))
        case (Left(arg1), Right(int2)) => Left(exp.copy(exps = List(arg1, ConcolicInt(int2))))
        case (Left(arg1), Left(arg2)) => Left(exp.copy(exps = List(arg1, arg2)))
      }
    case IntMinus if exp.exps.size == 3 =>
      val reduced1 = reduceToInt(exp.exps.head)
      val reduced2 = reduceToInt(exp.exps(1))
      val reduced3 = reduceToInt(exp.exps(2))
      (reduced1, reduced2, reduced3) match {
        case (Right(int1), Right(int2), Right(int3)) => Right(int1 - int2 - int3)
        case (Right(int1), Right(int2), Left(arg3)) => Left(exp.copy(exps = List(ConcolicInt(int1 - int2), arg3)))
        case (Right(int1), Left(arg2), Right(int3)) => Left(exp.copy(exps = List(ConcolicInt(int1 - int3), arg2)))
        case (Right(int1), Left(arg2), Left(arg3)) => Left(exp.copy(exps = List(ConcolicInt(int1), arg2, arg3)))
        case (Left(arg1), Right(int2), Right(int3)) => Left(exp.copy(exps = List(arg1, ConcolicInt(int2 - int3))))
        case (Left(arg1), Right(int2), Left(arg3)) => Left(exp.copy(exps = List(arg1, ConcolicInt(int2), arg3)))
        case (Left(arg1), Left(arg2), Right(int3)) => Left(exp.copy(exps = List(arg1, arg2, ConcolicInt(int3))))
        case (Left(arg1), Left(arg2), Left(arg3)) => Left(exp.copy(exps = List(arg1, arg2, arg3)))
      }
    case IntMinus => throw new Exception(s"Constraint optimization does not allow optimisation of IntMinus with more than < 2 or > 3 arguments")
    case IntTimes => reduceCommutativeArithExp(exp, _.product)
    case IntDiv if exp.exps.size == 2 =>
      /* Integer division expressions are only optimized if the divider is equal to 1. */
      val reduced1 = reduceToInt(exp.exps.head)
      val reduced2 = reduceToInt(exp.exps(1))
      (reduced1, reduced2) match {
        /* If the first argument is dividable by the second, the division can be safely optimised. */
        case (Right(int1), Right(int2)) if (int2 >= 1) && (int1 % int2 == 0) => Right(int1 / int2)
        case (Right(int1), Right(int2)) =>  Left(exp.copy(exps = List(ConcolicInt(int1), ConcolicInt(int2))))
        case (Right(int1), Left(arg2)) => Left(exp.copy(exps = List(ConcolicInt(int1), arg2)))
        case (Left(arg1), Right(1)) => Left(arg1)
        case (Left(arg1), Right(int2)) => Left(exp.copy(exps = List(arg1, ConcolicInt(int2))))
        case (Left(arg1), Left(arg2)) =>  Left(exp.copy(exps = List(arg1, arg2)))
      }
    case _ =>
      /* TODO Optimize other arithmetical operations as well */
      Left(exp)
  }

  private def eitherToConcolicExp(either: EitherReduced): ConcolicExpression = either match {
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

  private def reduceToInt(exp: ConcolicExpression): EitherReduced = exp match {
    case ConcolicInt(i) => Right(i)
    case exp: ArithmeticalConcolicExpression => optimizeArithExp(exp)
    case _ => Left(exp)
  }

  def optimizeConstraint(constraint: BranchConstraint): BranchConstraint = {
    val optimizedExp = optimizeBoolExp(constraint.exp)
    BranchConstraint(optimizedExp)
  }

  def optimizeReport(report: PathConstraint): PathConstraint = {
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
