import scalaz._
import scalaz.Scalaz._

import backend.expression._

/** This is where we define Scheme primitives */
class SchemePrimitives[Addr : Address, Abs : IsSchemeLattice] extends Primitives[Addr, Abs] {
  import SchemeOps._
  val abs = implicitly[IsSchemeLattice[Abs]]

  def isNull = abs.unaryOp(UnaryOperator.IsNull) _
  def isCons = abs.unaryOp(UnaryOperator.IsCons) _
  def isChar = abs.unaryOp(UnaryOperator.IsChar) _
  def isSymbol = abs.unaryOp(UnaryOperator.IsSymbol) _
  def isString = abs.unaryOp(UnaryOperator.IsString) _
  def isInteger = abs.unaryOp(UnaryOperator.IsInteger) _
  def isReal = abs.unaryOp(UnaryOperator.IsReal) _
  def isBoolean = abs.unaryOp(UnaryOperator.IsBoolean) _
  def isVector = abs.unaryOp(UnaryOperator.IsVector) _
  def ceiling = abs.unaryOp(UnaryOperator.Ceiling) _
  def floor = abs.unaryOp(UnaryOperator.Floor) _
  def round = abs.unaryOp(UnaryOperator.Round) _
  def log = abs.unaryOp(UnaryOperator.Log) _
  def not = abs.unaryOp(UnaryOperator.Not) _
  def random = abs.unaryOp(UnaryOperator.Random) _
  def sin = abs.unaryOp(UnaryOperator.Sin) _
  def asin = abs.unaryOp(UnaryOperator.ASin) _
  def cos = abs.unaryOp(UnaryOperator.Cos) _
  def acos = abs.unaryOp(UnaryOperator.ACos) _
  def tan = abs.unaryOp(UnaryOperator.Tan) _
  def atan = abs.unaryOp(UnaryOperator.ATan) _
  def sqrt = abs.unaryOp(UnaryOperator.Sqrt) _
  def vectorLength = abs.unaryOp(UnaryOperator.VectorLength) _
  def stringLength = abs.unaryOp(UnaryOperator.StringLength) _
  def numberToString = abs.unaryOp(UnaryOperator.NumberToString) _
  def symbolToString = abs.unaryOp(UnaryOperator.SymbolToString) _
  def stringToSymbol = abs.unaryOp(UnaryOperator.StringToSymbol) _
  def inexactToExact = abs.unaryOp(UnaryOperator.InexactToExact) _
  def exactToInexact = abs.unaryOp(UnaryOperator.ExactToInexact) _

  def plus = abs.binaryOp(BinaryOperator.Plus) _
  def minus = abs.binaryOp(BinaryOperator.Minus) _
  def times = abs.binaryOp(BinaryOperator.Times) _
  def div = abs.binaryOp(BinaryOperator.Div) _
  def quotient = abs.binaryOp(BinaryOperator.Quotient) _
  def modulo = abs.binaryOp(BinaryOperator.Modulo) _
  def remainder = abs.binaryOp(BinaryOperator.Remainder) _
  def lt = abs.binaryOp(BinaryOperator.Lt) _
  def numEq = abs.binaryOp(BinaryOperator.NumEq) _
  def eqq = abs.binaryOp(BinaryOperator.Eq) _
  def stringAppend = abs.binaryOp(BinaryOperator.StringAppend) _
  def stringLt = abs.binaryOp(BinaryOperator.StringLt) _

  abstract class NoStoreOperation(val name: String, val nargs: Option[Int] = None) extends Primitive[Addr, Abs] {

    def call(args: List[Arg]): (MayFail[Abs], Option[ConcolicExpression]) =
      MayFailError[Abs](List(ArityError(name, nargs.getOrElse(-1), args.length)))
    def call(arg1: Arg, arg2: Arg): (MayFail[Abs], Option[ConcolicExpression]) =
      call(List(arg1, arg2))
    def call[Exp : Expression](arg1: (Exp, Arg), arg2: (Exp, Arg)): (MayFail[Abs], Option[ConcolicExpression]) =
      call(arg1._2, arg2._2)
    def call[Exp : Expression](fexp: Exp, arg1: (Exp, Arg), arg2: (Exp, Arg)): (MayFail[Abs], Option[ConcolicExpression]) = call(arg1, arg2)
    def call(arg: Arg): (MayFail[Abs], Option[ConcolicExpression]) = call(List(arg))
    def call[Exp : Expression](arg: (Exp, Arg)): (MayFail[Abs], Option[ConcolicExpression]) = call(arg._2)
    def call[Exp : Expression](fexp: Exp, arg: (Exp, Arg)): (MayFail[Abs], Option[ConcolicExpression]) = call(arg)
    def call(): (MayFail[Abs], Option[ConcolicExpression]) = call(List())
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Arg)], store: Store[Addr, Abs], t: Time): (MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])], Option[ConcolicExpression]) = {
      val result = args match {
        case Nil => call()
        case x :: Nil => call(fexp, x)
        case x :: y :: Nil => call(fexp, x, y)
        case l => call(args.map({ case (_, v) => v }))
      }
      (result._1.map(v => (v, store, Set())), result._2)
    }
  }

  abstract class StoreOperation(val name: String, val nargs: Option[Int] = None) extends Primitive[Addr, Abs] {
    def call(args: List[Arg], store: Store[Addr, Abs]): (MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])], Option[ConcolicExpression]) =
      (MayFailError(List(ArityError(name, nargs.getOrElse(-1), args.length))), None)
    def call(arg: Arg, store: Store[Addr, Abs]): (MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])], Option[ConcolicExpression]) =
      call(List(arg), store)
    def call(arg1: Arg,
             arg2: Arg,
             store: Store[Addr, Abs]): (MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])], Option[ConcolicExpression]) =
      call(List(arg1, arg2), store)
    def call[Exp : Expression](fexp: Exp,
                               arg1: (Exp, Arg),
                               arg2: (Exp, Arg), store: Store[Addr, Abs]): (MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])], Option[ConcolicExpression]) =
      call(arg1._2, arg2._2, store)
    def call[Exp : Expression](fexp: Exp, arg: (Exp, Arg), store: Store[Addr, Abs]): (MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])], Option[ConcolicExpression]) =
      call(arg._2, store)
    def call(store: Store[Addr, Abs]): (MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])], Option[ConcolicExpression]) = call(List(), store)
    def call[Exp : Expression, Time : Timestamp](fexp : Exp, args: List[(Exp, Arg)], store: Store[Addr, Abs], t: Time): (MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])], Option[ConcolicExpression]) = args match {
      case Nil => call(store)
      case x :: Nil => call(fexp, x, store)
      case x :: y :: Nil => call(fexp, x, y, store)
      case l => call(args.map({ case (_, v) => v }), store)
    }
  }

  class BoolTop extends NoStoreOperation("bool-top") {
    override def call(args: List[Arg]) = absToTuple(abs.boolTop)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.BoolTop
  }
  object BoolTop extends BoolTop
  class IntTop extends NoStoreOperation("int-top") {
    override def call(args: List[Arg]) = abs.intTop
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.IntTop
  }
  object IntTop extends IntTop
  class Plus extends NoStoreOperation("+") {
    def regularCall(args: List[Arg]): (MayFail[Abs], List[Option[ConcolicExpression]]) = args match {
      case Nil => (abs.inject(0), List(Some(ConcolicInt(0))))
      case x :: rest =>
        val (regResult: MayFail[Abs], symbolic: List[Option[ConcolicExpression]]) = regularCall(rest)
        (regResult._1 >>= (plus(x._1, _)), x._2 :: symbolic)
    }
    override def call(args: List[Arg]) = args match {
      case Nil => (abs.inject(0), Some(ConcolicInt(0)))
      case _ =>
        val result = regularCall(args)
        (result._1, mapJustSymbolicArgs(result._2).map(ArithmeticalConcolicExpression(IntPlus, _)))
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Plus
  }
  object Plus extends Plus
  class Minus extends NoStoreOperation("-") {
    override def call(args: List[Arg]) = args match {
      case Nil => MayFailError[Abs](List(VariadicArityError(name, 1, 0)))
      case x :: Nil => (minus(abs.inject(0), x._1), None)
      case x :: rest =>
        val (regResult: MayFail[Abs], plusSymbolic: Option[ConcolicExpression]) = Plus.call(rest)
        val minusSymbolic = plusSymbolic.map({
          case x: ArithmeticalConcolicExpression =>
            x.copy(op = IntMinus)
        })
        (regResult >>= (minus(x._1, _)), minusSymbolic)
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Minus
  }
  object Minus extends Minus
  class Times extends NoStoreOperation("*") {
    def regularCall(args: List[Arg]): (MayFail[Abs], List[Option[ConcolicExpression]]) = args match {
      case Nil => (MayFailSuccess(abs.inject(1)), List(Some(ConcolicInt(1))))
      case x :: rest =>
        val (regResult: MayFail[Abs], symbolic: List[Option[ConcolicExpression]]) = regularCall(rest)
        (regResult._1 >>= (plus(x._1, _)), x._2 :: symbolic)
    }
    override def call(args: List[Arg]) = args match {
      case Nil => (MayFailSuccess(abs.inject(1)), Some(ConcolicInt(1)))
      case _ =>
        val result = regularCall(args)
        (result._1, mapJustSymbolicArgs(result._2).map(ArithmeticalConcolicExpression(IntTimes, _)))
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Times
  }
  object Times extends Times
  class Div extends NoStoreOperation("/") {
    override def call(args: List[Arg]) = args match {
      case Nil => MayFailError[Abs](List(VariadicArityError(name, 1, 0)))
      case x :: rest =>
        val (multrest: MayFail[Abs], timesSymbolic: Option[ConcolicExpression]) = Times.call(rest)
        (for {
          rest <- multrest
          r <- div(x._1, rest)
          fl <- floor(r)
          isexact <- eqq(r, fl)
          xisint <- isInteger(x._1)
          multrestisint <- isInteger(rest)
          convert = abs.and(isexact, abs.and(xisint, multrestisint))
          exr <- inexactToExact(r)
        } yield {
          val t = if (abs.isTrue(convert)) { exr } else { abs.bottom }
          val f = if (abs.isFalse(convert)) { r } else { abs.bottom }
          abs.join(t, f)
        }, timesSymbolic.map({
          case ArithmeticalConcolicExpression(_, symbolicArgs) =>
            ArithmeticalConcolicExpression(IntDiv, symbolicArgs)
        }))
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Div
  }
  object Div extends Div
  class Quotient extends NoStoreOperation("quotient", Some(2)) {
    override def call(x: Arg, y: Arg) = quotient(x._1, y._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Quotient
  }
  object Quotient extends Quotient
  class Expt extends NoStoreOperation("expt") {
    def expt(x: Abs, y: Abs, visited: Set[Abs]): MayFail[Abs] =
      if (visited.contains(y)) {
        abs.bottom.point[MayFail]
      } else {
        numEq(y, abs.inject(0)) >>= { yiszero =>
          val t = if (abs.isTrue(yiszero)) { abs.inject(1).point[MayFail] } else { abs.bottom.point[MayFail] }
          val f = if (abs.isFalse(yiszero)) {
            minus(y, abs.inject(1)) >>= { y1 =>
              expt(x, y1, visited + y) >>= { exptrest =>
                times(x, exptrest)
              }
            }
          } else { abs.bottom.point[MayFail] }
          MayFail.monoid[Abs].append(t, f)
        }
      }
    override def call(x: Arg, y: Arg) = expt(x._1, y._1, Set())
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Expt
  }
  object Expt extends Expt
  trait RelationalPrimitive { this: Primitive[Addr, Abs] =>
    def makeRelationalConcolicExpression(op: IntegerRelationalOperator, x: Arg, y: Arg): Option[ConcolicExpression] = {
      this.mapSymbolicArgs(List(x, y)).map(args => RelationalConcolicExpression(args.head, IntLessThan, args(1)))
    }
  }
  class LessThan extends NoStoreOperation("<", Some(2)) with RelationalPrimitive {
    override def call(x: Arg, y: Arg) = /* TODO: < should accept any number of arguments (same for <= etc.) */
      (lt(x._1, y._1), makeRelationalConcolicExpression(IntLessThan, x, y))
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.LessThan
  }
  object LessThan extends LessThan
  class LessOrEqual extends NoStoreOperation("<=", Some(2)) with RelationalPrimitive {
    override def call(x: Arg, y: Arg) = (for {
      ltres <- lt(x._1, y._1)
      eqres <- numEq(x._1, y._1)
    } yield abs.or(ltres, eqres), makeRelationalConcolicExpression(IntLessThanEqual, x, y))
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.LessOrEqual
  }
  object LessOrEqual extends LessOrEqual
  class NumEq extends NoStoreOperation("=", Some(2)) with RelationalPrimitive {
    def eq(first: Abs, l: List[Abs]): MayFail[Abs] = l match {
      case Nil => abs.inject(true)
      case x :: rest => numEq(first, x) >>= (feqx => for {
        t <- if (abs.isTrue(feqx)) { eq(first, rest) } else { abs.bottom.point[MayFail] }
        f = if (abs.isFalse(feqx)) { abs.inject(false) } else { abs.bottom }
      } yield abs.join(t, f))
    }
    override def call(args: List[Arg]) = args match {
      case Nil => (abs.inject(true), Some(ConcolicBool(true)))
      case x :: rest => (eq(x._1, rest.map(_._1)),makeRelationalConcolicExpression(IntEqual, x, rest.head))
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.NumEq
  }
  object NumEq extends NumEq
  class GreaterThan extends NoStoreOperation(">", Some(2)) with RelationalPrimitive {
    override def call(x: Arg, y: Arg) = (LessOrEqual.call(x, y)._1 >>= not, makeRelationalConcolicExpression(IntGreaterThan, x, y))
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.GreaterThan
  }
  object GreaterThan extends GreaterThan
  class GreaterOrEqual extends NoStoreOperation(">=", Some(2)) with RelationalPrimitive {
    override def call(x: Arg, y: Arg) = (LessThan.call(x, y)._1 >>= not, makeRelationalConcolicExpression(IntGreaterThanEqual, x, y))
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.GreaterOrEqual
  }
  object GreaterOrEqual extends GreaterOrEqual
  class Modulo extends NoStoreOperation("modulo", Some(2)) {
    override def call(x: Arg, y: Arg) = modulo(x._1, y._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Modulo
  }
  object Modulo extends Modulo
  class Remainder extends NoStoreOperation("remainder", Some(2)) {
    override def call(x: Arg, y: Arg) = remainder(x._1, y._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Remainder
  }
  object Remainder extends Remainder
  class Random extends NoStoreOperation("random", Some(1)) {
    override def call[Exp: Expression, Time: Timestamp](fexp: Exp, args: List[(Exp, Arg)], store: Store[Addr, Abs], t: Time): (MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])], Option[ConcolicExpression]) = {
      val value: MayFail[Abs] = if (ScalaAMReporter.isConcolicEnabled) {
        val optInput = InputVariableStore.lookupInput(fexp.asInstanceOf[SchemeExp])
        optInput match {
          case Some(input) =>
            val inputValue = ScalaAMConcolicSolver.getInput(input.toString)
            inputValue match {
              case Some(int) => MayFailSuccess(abs.inject(int))
              case None =>
                // Could be that this random-call produces an input value already encountered in a previous concolic iteration,
                // but that was not assigned a value, e.g., because it was not used in the path constraint.
                callRandom(args.head._2._1)
            }
          case None => callRandom(args.head._2._1)
        }
      } else {
        callRandom(args.head._2._1)
      }
      (value.map(v => (v, store, Set())), symbolicCall(fexp.asInstanceOf[SchemeExp]))
    }
    def callRandom(x: Abs) = random(x)
    def symbolicCall(fexp: SchemeExp): Option[ConcolicExpression] = {
      val newInputVariable = ConcolicIdGenerator.newConcolicInput
      InputVariableStore.addInput(newInputVariable, fexp)
      Some(newInputVariable)
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Random
  }
  object Random extends Random
  class Ceiling extends NoStoreOperation("ceiling", Some(1)) {
    override def call(x: Arg) = ceiling(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Ceiling
  }
  object Ceiling extends Ceiling
  class Floor extends NoStoreOperation("floor", Some(1)) {
    override def call(x: Arg) = floor(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Floor
  }
  object Floor extends Floor
  class Round extends NoStoreOperation("round", Some(1)) {
    override def call(x: Arg) = round(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Round
  }
  object Round extends Round
  class Log extends NoStoreOperation("log", Some(1)) {
    override def call(x: Arg) = log(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Log
  }
  object Log extends Log
  class Sin extends NoStoreOperation("sin", Some(1)) {
    override def call(x: Arg) = sin(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Sin
  }
  object Sin extends Sin
  class ASin extends NoStoreOperation("asin", Some(1)) {
    override def call(x: Arg) = asin(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.ASin
  }
  object ASin extends ASin
  class Cos extends NoStoreOperation("cos", Some(1)) {
    override def call(x: Arg) = cos(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Cos
  }
  object Cos extends Cos
  class ACos extends NoStoreOperation("acos", Some(1)) {
    override def call(x: Arg) = acos(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.ACos
  }
  object ACos extends ACos
  class Tan extends NoStoreOperation("tan", Some(1)) {
    override def call(x: Arg) = tan(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Tan
  }
  object Tan extends Tan
  class ATan extends NoStoreOperation("atan", Some(1)) {
    override def call(x: Arg) = atan(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.ATan
  }
  object ATan extends ATan
  class Sqrt extends NoStoreOperation("sqrt", Some(1)) {
    override def call(x: Arg) = lt(x._1, abs.inject(0)) >>= { signtest =>
      val t: MayFail[Abs] = if (abs.isFalse(signtest) /* n >= 0 */) {
        for {
          r <- sqrt(x._1)
          fl <- floor(r)
          argisexact <- isInteger(x._1)
          resisexact <- eqq(r, fl)
          convert = abs.and(argisexact, resisexact)
          exr <- inexactToExact(r)
        } yield {
          val tt = if (abs.isTrue(convert)) { exr } else { abs.bottom }
          val tf = if (abs.isFalse(convert)) { r } else { abs.bottom }
          abs.join(tt, tf)
        }
      } else { abs.bottom.point[MayFail] }
      val f: MayFail[Abs] = if (abs.isTrue(signtest) /* n < 0 */ ) { MayFailError(List(OperatorNotApplicable("sqrt", List(x.toString)))) } else { MayFailSuccess(abs.bottom) }
      MayFail.monoid[Abs].append(t, f)
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Sqrt
  }
  object Sqrt extends Sqrt
  class ExactToInexact extends NoStoreOperation("exact->inexact", Some(1)) {
    override def call(x: Arg) = exactToInexact(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.ExactToInexact
  }
  object ExactToInexact extends ExactToInexact
  class InexactToExact extends NoStoreOperation("inexact->exact", Some(1)) {
    override def call(x: Arg) = inexactToExact(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.InexactToExact
  }
  object InexactToExact extends InexactToExact
  /** (define (zero? x) (= x 0)) */
  class Zerop extends NoStoreOperation("zero?", Some(1)) {
    override def call(x: Arg) = numEq(abs.inject(0), x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Zerop
  }
  object Zerop extends Zerop
  /** (define (positive? x) (< x 0)) */
  class Positivep extends NoStoreOperation("positive?", Some(1)) {
    override def call(x: Arg) = lt(abs.inject(0), x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Positivep
  }
  object Positivep extends Positivep
  /** (define (positive? x) (< 0 x)) */
  class Negativep extends NoStoreOperation("negative?", Some(1)) {
    override def call(x: Arg) = lt(x._1, abs.inject(0))
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Negativep
  }
  object Negativep extends Negativep
  /** (define (odd? x) (= 1 (modulo x 2))) */
  class Oddp extends NoStoreOperation("odd?", Some(1)) {
    override def call(x: Arg) = modulo(x._1, abs.inject(2)) >>= (numEq(abs.inject(1), _))
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Oddp
  }
  object Oddp extends Oddp
  /** (define (even? x) (= 0 (modulo x 2))) */
  class Evenp extends NoStoreOperation("even?", Some(1)) {
    override def call(x: Arg) = modulo(x._1, abs.inject(2)) >>= (numEq(abs.inject(0), _))
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Evenp
  }
  object Evenp extends Evenp
  class Max extends NoStoreOperation("max") {
    /* TODO: In Scheme, max casts numbers to inexact as soon as one of them is inexact, but we don't support that */
    private override def call(args: List[Abs], max: Abs): MayFail[Abs] = args match {
      case Nil => MayFailSuccess(max)
      case x :: rest => for {
        test <- lt(max, x)
        t <- if (abs.isTrue(test)) { call(rest, x) } else { abs.bottom.point[MayFail] }
        f <- if (abs.isFalse(test)) { call(rest, max) } else { abs.bottom.point[MayFail] }
      } yield abs.join(t, f)
    }
    override def call(args: List[Arg]) = args match {
      case Nil => VariadicArityError(name, 1, 0)
      case x :: rest => call(rest.map(_._1), x._1)
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Max
  }
  object Max extends Max
  class Min extends NoStoreOperation("min") {
    /* TODO: same remark as max */
    private override def call(args: List[Abs], min: Abs): MayFail[Abs] = args match {
      case Nil => MayFailSuccess(min)
      case x :: rest => for {
        test <- lt(x, min)
        t <- if (abs.isTrue(test)) { call(rest, x) } else { MayFailSuccess(abs.bottom) }
        f <- if (abs.isFalse(test)) { call(rest, min) } else { MayFailSuccess(abs.bottom) }
      } yield abs.join(t, f)
    }
    override def call(args: List[Arg]) = args match {
      case Nil => VariadicArityError(name, 1, 0)
      case x :: rest => call(rest.map(_._1), x._1)
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Min
  }
  object Min extends Min
  /** (define (abs x) (if (< x 0) (- 0 x) x)) */
  class ArithAbs extends NoStoreOperation("abs", Some(1)) {
    override def call(x: Arg) = for {
      test <- lt(x._1, abs.inject(0))
      t <- if (abs.isTrue(test)) { minus(abs.inject(0), x._1) } else { abs.bottom.point[MayFail] }
      f <- if (abs.isFalse(test)) { x._1.point[MayFail] } else { abs.bottom.point[MayFail] }
    } yield abs.join(t, f)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.ArithAbs
  }
  object ArithAbs extends ArithAbs
  /** (define (gcd a b) (if (= b 0) a (gcd b (modulo a b)))) */
  class Gcd extends NoStoreOperation("gcd", Some(2)) {
    private def gcd(a: Abs, b: Abs, visited: Set[(Abs, Abs)]): MayFail[Abs] = {
      if (visited.contains((a, b))) {
        abs.bottom.point[MayFail]
      } else {
        for {
          test <- numEq(b, abs.inject(0))
          t <- if (abs.isTrue(test)) { a.point[MayFail] } else { abs.bottom.point[MayFail] }
          f <- if (abs.isFalse(test)) { modulo(a, b) >>= (amodb => gcd(b, amodb, visited + ((a, b)))) } else { abs.bottom.point[MayFail] }
        } yield abs.join(t, f)
      }
    }
    override def call(x: Arg, y: Arg) = gcd(x._1, y._1, Set())
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Gcd
  }
  object Gcd extends Gcd
  class Nullp extends NoStoreOperation("null?", Some(1)) {
    override def call(x: Arg) = isNull(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Nullp
  }
  object Nullp extends Nullp
  class Pairp extends NoStoreOperation("pair?", Some(1)) {
    override def call(x: Arg) = isCons(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Pairp
  }
  object Pairp extends Pairp
  class Charp extends NoStoreOperation("char?", Some(1)) {
    override def call(x: Arg) = isChar(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Charp
  }
  object Charp extends Charp
  class Symbolp extends NoStoreOperation("symbol?", Some(1)) {
    override def call(x: Arg) = isSymbol(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Symbolp
  }
  object Symbolp extends Symbolp
  class Stringp extends NoStoreOperation("string?", Some(1)) {
    override def call(x: Arg) = isString(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Stringp
  }
  object Stringp extends Stringp
  class Integerp extends NoStoreOperation("integer?", Some(1)) {
    override def call(x: Arg) = isInteger(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Integerp
  }
  object Integerp extends Integerp
  class Realp extends NoStoreOperation("real?", Some(1)) {
    override def call(x: Arg) = for {
      isint <- isInteger(x._1)
      isreal <- isReal(x._1)
    } yield abs.or(isint, isreal)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Realp
  }
  object Realp extends Realp
  class Numberp extends NoStoreOperation("number?", Some(1)) {
    override def call(x: Arg) = Realp.call(x) /* No support for complex number, so number? is equivalent as real? */
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Numberp
  }
  object Numberp extends Numberp
  class Booleanp extends NoStoreOperation("boolean?", Some(1)) {
    override def call(x: Arg) = isBoolean(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Booleanp
  }
  object Booleanp extends Booleanp
  class Vectorp extends NoStoreOperation("vector?", Some(1)) {
    override def call(x: Arg) = isVector(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Vectorp
  }
  object Vectorp extends Vectorp
  class Eq extends NoStoreOperation("eq?", Some(2)) {
    override def call(x: Arg, y: Arg) = eqq(x._1, y._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Eq
  }
  object Eq extends Eq
  class Not extends NoStoreOperation("not", Some(1)) {
    override def call(x: Arg) = (not(x._1), x._2.map({
      case r: RelationalConcolicExpression => r.negate
      case other => other
    }))
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Not
  }
  object Not extends Not
  class NumberToString extends NoStoreOperation("number->string", Some(1)) {
    override def call(x: Arg) = numberToString(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.NumberToString
  }
  object NumberToString extends NumberToString
  class SymbolToString extends NoStoreOperation("symbol->string", Some(1)) {
    override def call(x: Arg) = symbolToString(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.SymbolToString
  }
  object SymbolToString extends SymbolToString
  class StringToSymbol extends NoStoreOperation("string->symbol", Some(1)) {
    override def call(x: Arg) = stringToSymbol(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.StringToSymbol
  }
  object StringToSymbol extends StringToSymbol
  class StringAppend extends NoStoreOperation("string-append") {
    override def call(args: List[Arg]) = args match {
      case Nil => MayFailSuccess(abs.inject(""))
      case x :: rest => call(rest)._1 >>= (stringAppend(x._1, _))
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.StringAppend
  }
  object StringAppend extends StringAppend
  class StringLt extends NoStoreOperation("string<?", Some(2)) {
    override def call(x: Arg, y: Arg) = stringLt(x._1, y._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.StringLt
  }
  object StringLt extends StringLt
  class StringLength extends NoStoreOperation("string-length", Some(1)) {
    override def call(x: Arg) = stringLength(x._1)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.StringLength
  }
  object StringLength extends StringLength
  class Newline extends NoStoreOperation("newline", Some(0)) {
    override def call() = { println(""); MayFailSuccess(abs.inject(false)) }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Newline
  }
  object Newline extends Newline
  class Display extends NoStoreOperation("display", Some(1)) {
    override def call(x: Arg) = {
      val str = x._1.toString
      print(if (str.startsWith("\"")) { str.substring(1, str.size-1) } else { str })
      MayFailSuccess(x._1) /* Undefined behavior in R5RS */
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Display
  }
  object Display extends Display
  class Error extends NoStoreOperation("error", Some(1)) {
    override def call[Exp : Expression](fexp: Exp, x: (Exp, Arg)) =
      MayFailError[Abs](List(UserError(x._2._1.toString, Expression[Exp].pos(fexp))))
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Error
  }
  object Error extends Error

  val mfmon = MayFail.monoid[(Abs, Set[Effect[Addr]])]
  def err(e: SemanticError): MayFail[(Abs, Set[Effect[Addr]])] = e
  def success(v: Abs): MayFail[(Abs, Set[Effect[Addr]])] = (v, Set[Effect[Addr]]()).point[MayFail]
  class Cons extends Primitive[Addr, Abs] {
    val name = "cons"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Arg)], store: Store[Addr, Abs], t: Time) = args match {
      case (carexp, (car, symbolicCar)) :: (cdrexp, (cdr, symbolicCdr)) :: Nil => {
        val cara = Address[Addr].cell(carexp, t)
        val cdra = Address[Addr].cell(cdrexp, t)

        val symbolicAddress: Option[ConcolicAddress] = for {
          symbolicCarValue <- symbolicCar
          symbolicCdrValue <- symbolicCdr
          fields = Map("car" -> symbolicCarValue, "cdr" -> symbolicCdrValue)
          symbolicObject = ConcolicObject(name, fields)
          symbolicAddress = ConcolicIdGenerator.newConcolicRegularAddress
        } yield {
          GlobalSymbolicStore.extendStore(symbolicAddress, symbolicObject)
          symbolicAddress
        }

        (MayFailSuccess((abs.cons(cara, cdra), store.extend(cara, car).extend(cdra, cdr), Set())), symbolicAddress)
      }
      case l => (MayFailError(List(ArityError(name, 2, l.size))), None)
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Cons
  }
  object Cons extends Cons
  class RandomCons extends Primitive[Addr, Abs] {
    val name = "random-cons"
    def symbolicCall(fexp: SchemeExp): Option[ConcolicExpression] = {
      val symbolicAddress = ConcolicIdGenerator.newConcolicInputAddress
      val fields = Map("car" -> ConcolicInt(98), "cdr" -> ConcolicInt(99))
      val symbolicObject = ConcolicObject(name, fields)
      GlobalSymbolicStore.extendStore(symbolicAddress, symbolicObject)
      Some(symbolicAddress)
    }
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Arg)], store: Store[Addr, Abs], t: Time) = args match {
      case Nil =>
        /* Add condition signalling that input address now generated was null */
        val condition = ConcolicAddressComparison
        /* TODO Symbolic call moet op hetzelfde moment als concrete call gebeuren: moet nu een conditie genereren
         * dat de ConcolicInputAddress gelijk was aan het NullObject */
        (MayFailSuccess((abs.nil, store, Set())), symbolicCall(fexp.asInstanceOf[SchemeExp]))
      case l => (MayFailError(List(ArityError(name, 0, l.size))), None)
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.RandomCons
  }
  object RandomCons extends RandomCons
  private def car(v: Abs, store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
    val addrs = abs.car(v)
    if (addrs.isEmpty) {
      CannotAccessCar(v.toString)
    } else {
      addrs.foldLeft(mfmon.zero)((acc, a) =>
        acc |+| (store.lookup(a) match {
          case Some(v) => (v, Set[Effect[Addr]](EffectReadConsCar(a))).point[MayFail]
          case None => UnboundAddress(a.toString)
        }))
    }
  }
  private def cdr(v: Abs, store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
    val addrs = abs.cdr(v)
    if (addrs.isEmpty) {
      err(CannotAccessCdr(v.toString))
    } else {
      addrs.foldLeft(mfmon.zero)((acc, a) =>
        mfmon.append(acc, store.lookup(a) match {
          case Some(v) => MayFailSuccess((v, Set(EffectReadConsCdr(a))))
          case None => err(UnboundAddress(a.toString))
        }))
    }
  }

  class CarCdrOperation(override val name: String) extends StoreOperation(name, Some(1)) {
    trait Spec
    case object Car extends Spec
    case object Cdr extends Spec
    val spec: List[Spec] = name.drop(1).take(name.length - 2).toList.reverseMap(c =>
      if (c == 'a') { Car }
      else if (c == 'd') { Cdr }
      else { throw new Exception(s"Incorrect car/cdr operation: $name") })
    override def symbolicCall(fexp: SchemeExp, concreteArgs: List[Abs], symbolicArgs: List[ConcolicExpression]): Option[ConcolicExpression] = {
      assert(symbolicArgs.size == 1)
      /* TODO Does not work when car and cdr are chained! (as in: cadr, caar, cddr etc.) */
      assert(spec.size == 1)
      println(s"Used address ${symbolicArgs.head}")
      symbolicArgs.head match {
        case symbolicAddress: ConcolicRegularAddress =>
          /* Car/cdr operation on a regular pair object */
          val result = GlobalSymbolicStore.lookupAddress(symbolicAddress)
          result match {
            case None =>
              assert(false, s"Address $symbolicAddress not found in symbolic store")
              None
            case Some(symbolicObject) =>
              println(s"Got result $result")
              spec.head match {
                case Car => symbolicObject.fields.get("car")
                case Cdr => symbolicObject.fields.get("cdr")
              }
          }
        case symbolicAddress: ConcolicInputAddress =>
          /* Car/cdr operation on an input pair object */
          val carCdrAccess = ConcolicFieldAccess(symbolicAddress, if (spec.head == Car) "car" else "cdr")
          Some(carCdrAccess)
        case _ =>
          assert(false, "Should not happen!")
          ???
      }
    }
    override def call(v: Abs, store: Store[Addr, Abs]) =
      for { (v, effs) <- spec.foldLeft(success(v))((acc, op) => for {
        (v, effs) <- acc
        (vcxr, effs2) <- op match {
          case Car => car(v, store)
          case Cdr => cdr(v, store)
        }
      } yield (vcxr, effs ++ effs2)) } yield (v, store, effs)
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      new prims.CarCdrOperation(name)
  }
  class Car extends CarCdrOperation("car")
  object Car extends Car
  class Cdr extends CarCdrOperation("cdr")
  object Cdr extends Cdr
  class Caar extends CarCdrOperation("caar")
  object Caar extends Caar
  class Cadr extends CarCdrOperation("cadr")
  object Cadr extends Cadr
  class Cdar extends CarCdrOperation("cdar")
  object Cdar extends Cdar
  class Cddr extends CarCdrOperation("cddr")
  object Cddr extends Cddr
  class Caaar extends CarCdrOperation("caaar")
  object Caaar extends Caaar
  class Caadr extends CarCdrOperation("caadr")
  object Caadr extends Caadr
  class Cadar extends CarCdrOperation("cadar")
  object Cadar extends Cadar
  class Caddr extends CarCdrOperation("caddr")
  object Caddr extends Caddr
  class Cdaar extends CarCdrOperation("cdaar")
  object Cdaar extends Cdaar
  class Cdadr extends CarCdrOperation("cdadr")
  object Cdadr extends Cdadr
  class Cddar extends CarCdrOperation("cddar")
  object Cddar extends Cddar
  class Cdddr extends CarCdrOperation("cdddr")
  object Cdddr extends Cdddr
  class Caaaar extends CarCdrOperation("caaaar")
  object Caaaar extends Caaaar
  class Caaadr extends CarCdrOperation("caaadr")
  object Caaadr extends Caaadr
  class Caadar extends CarCdrOperation("caadar")
  object Caadar extends Caadar
  class Caaddr extends CarCdrOperation("caaddr")
  object Caaddr extends Caaddr
  class Cadaar extends CarCdrOperation("cadaar")
  object Cadaar extends Cadaar
  class Cadadr extends CarCdrOperation("cadadr")
  object Cadadr extends Cadadr
  class Caddar extends CarCdrOperation("caddar")
  object Caddar extends Caddar
  class Cadddr extends CarCdrOperation("cadddr")
  object Cadddr extends Cadddr
  class Cdaaar extends CarCdrOperation("cdaaar")
  object Cdaaar extends Cdaaar
  class Cdaadr extends CarCdrOperation("cdaadr")
  object Cdaadr extends Cdaadr
  class Cdadar extends CarCdrOperation("cdadar")
  object Cdadar extends Cdadar
  class Cdaddr extends CarCdrOperation("cdaddr")
  object Cdaddr extends Cdaddr
  class Cddaar extends CarCdrOperation("cddaar")
  object Cddaar extends Cddaar
  class Cddadr extends CarCdrOperation("cddadr")
  object Cddadr extends Cddadr
  class Cdddar extends CarCdrOperation("cdddar")
  object Cdddar extends Cdddar
  class Cddddr extends CarCdrOperation("cddddr")
  object Cddddr extends Cddddr

  class SetCar extends StoreOperation("set-car!", Some(2)) {
    override def symbolicCall(fexp: SchemeExp, concreteArgs: List[Abs], symbolicArgs: List[ConcolicExpression]): Option[ConcolicExpression] = {
      assert(symbolicArgs.size == 2)
      symbolicArgs.head match {
        case symbolicAddress: ConcolicAddress => GlobalSymbolicStore.lookupAddress(symbolicAddress) match {
          case Some(symbolicObject) =>
            val updatedSymbolicObject = symbolicObject.copy(fields = symbolicObject.fields.updated("car", symbolicArgs(1)))
            GlobalSymbolicStore.updateStore(symbolicAddress, updatedSymbolicObject)
            /* Concrete evaluation returns false */
            Some(ConcolicBool(false))
          case None =>
            assert(false, "Should not happen")
            None
        }
        case _ =>
          assert(false, "Should not happen!")
          None
      }
    }
    override def call(cell: Abs, value: Abs, store: Store[Addr, Abs]) = {
      val addrs = abs.car(cell)
      if (addrs.isEmpty) {
         MayFailError(List(CannotAccessCar(cell.toString)))
      } else {
        val (store2, effects) = addrs.foldLeft((store, Set[Effect[Addr]]()))((acc, a) =>
          (acc._1.update(a, value), acc._2 + EffectWriteConsCar(a)))
        MayFailSuccess((abs.inject(false) /* undefined */, store2, effects))
      }
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.SetCar
  }
  object SetCar extends SetCar
  class SetCdr extends StoreOperation("set-cdr!", Some(2)) {
    override def symbolicCall(fexp: SchemeExp, concreteArgs: List[Abs], symbolicArgs: List[ConcolicExpression]): Option[ConcolicExpression] = {
      assert(symbolicArgs.size == 2)
      symbolicArgs.head match {
        case symbolicAddress: ConcolicAddress => GlobalSymbolicStore.lookupAddress(symbolicAddress) match {
          case Some(symbolicObject) =>
            val updatedSymbolicObject = symbolicObject.copy(fields = symbolicObject.fields.updated("cdr", symbolicArgs(1)))
            GlobalSymbolicStore.updateStore(symbolicAddress, updatedSymbolicObject)
            /* Concrete evaluation returns false */
            Some(ConcolicBool(false))
          case None =>
            assert(false, "Should not happen")
            None
        }
        case _ =>
          assert(false, "Should not happen!")
          None
      }
    }
    override def call(cell: Abs, value: Abs, store: Store[Addr, Abs]) = {
      val addrs = abs.cdr(cell)
      if (addrs.isEmpty) {
        MayFailError(List(CannotAccessCdr(cell.toString)))
      } else {
        val (store2, effects) = addrs.foldLeft((store, Set[Effect[Addr]]()))((acc, a) =>
          (acc._1.update(a, value), acc._2 + EffectWriteConsCdr(a)))
        MayFailSuccess((abs.inject(false) /* undefined */, store2, effects))
      }
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.SetCdr
  }
  object SetCdr extends SetCdr
  class Length extends StoreOperation("length", Some(1)) {
    override def call(l: Arg, store: Store[Addr, Abs]) = {
      def length(l: Abs, visited: Set[Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
        if (visited.contains(l)) {
          MayFailSuccess((abs.bottom, Set[Effect[Addr]]()))
        } else {
          isCons(l) >>= (cond => {
            val t = if (abs.isTrue(cond)) {
              cdr(l, store) >>= ({ case (cdrl, effects1) =>
                length(cdrl, visited + l) >>= ({ case (lengthcdrl, effects2) =>
                  plus(abs.inject(1), lengthcdrl).map(lengthl =>
                    (lengthl, effects1 ++ effects2))
                })
              })
            } else { mfmon.zero }
            val f = if (abs.isFalse(cond)) {
              isNull(l) >>= (fcond => {
              val ft = if (abs.isTrue(fcond)) { MayFailSuccess((abs.inject(0), Set[Effect[Addr]]())) } else { mfmon.zero }
                val ff = if (abs.isFalse(fcond)) { err(TypeError("length", "first operand", "list", "non-list")) } else { mfmon.zero }
                mfmon.append(ft, ff)
              })
            } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            mfmon.append(t, f)
          })
        }
      }
      length(l._1, Set()).map({ case (v, effs) => (v, store, effs) })
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Length
  }
  object Length extends Length

  class ListPrim extends StoreOperation("list", None) {
    override def call[Exp: Expression, Time: Timestamp](fexp: Exp,
                                                        args: List[(Exp, Arg)],
                                                        store: Store[Addr, Abs],
                                                        t: Time): (MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])], Option[ConcolicExpression]) = {
      val pos = implicitly[Expression[Exp]].pos(fexp)

      val nilv = abs.nil
      val nila = Address[Addr].variable(Identifier("_nil_", pos), abs.bottom, t) // Hack to make sure addresses use the position of fexp
      val init: (Abs, Addr, Store[Addr, Abs]) = (nilv, nila, store)
      /*
       * If args is empty, the store should not be extended, so we allocate an address, but only forward it to
       * the next iteration, so that this next iteration (if there is one) can use it to extend the store.
       */
      val result = args.zipWithIndex.reverse.foldLeft(init)({
        case ((cdrv, cdra, store), ((argExp, argv), index)) =>
          val cara = Address[Addr].cell(argExp, t)
          val cons = abs.cons(cara, cdra)
          val newStore = store.extend(cdra, cdrv).extend(cara, argv._1)
          val paira = Address[Addr].variable(Identifier(s"_cons_${index}", pos), abs.bottom, t) // Hack to make sure addresses use the position of fexp
          (abs.cons(cara, cdra), paira, newStore)
      })
      MayFailSuccess((result._1, result._3, Set[Effect[Addr]]()))
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.ListPrim
  }
  object ListPrim extends ListPrim

  /** (define (list? l) (or (and (pair? l) (list? (cdr l))) (null? l))) */
  class Listp extends StoreOperation("list?", Some(1)) {
    override def call(l: Arg, store: Store[Addr, Abs]) = {
      def listp(l: Abs, visited: Set[Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
        if (visited.contains(l)) {
          /* R5RS: "all lists have finite length", and the cases where this is reached include circular lists */
          MayFailSuccess(abs.inject(false), Set[Effect[Addr]]())
        } else {
          isNull(l) >>= (nulltest => {
            val t = if (abs.isTrue(nulltest)) { MayFailSuccess((nulltest, Set[Effect[Addr]]())) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            val f = if (abs.isFalse(nulltest)) {
              isCons(l) >>= (constest => {
                val ft = if (abs.isTrue(constest)) {
                  cdr(l, store) >>= ({ case (cdrl, effects1) =>
                    listp(cdrl, visited + l).map({ case (listpl, effects2) =>
                      (listpl, effects1 ++ effects2)
                    })
                  })
                } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                val ff = if (abs.isFalse(constest)) {
                  MayFailSuccess((abs.inject(false), Set[Effect[Addr]]()))
                } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                MayFail.monoid[(Abs, Set[Effect[Addr]])].append(ft, ff)
              })
            } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            MayFail.monoid[(Abs, Set[Effect[Addr]])].append(t, f)
          })
        }
      }
      listp(l._1, Set()).map({ case (v, effs) => (v, store, effs) })
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Listp
  }
  object Listp extends Listp

  class MakeVector extends Primitive[Addr, Abs] {
    val name = "make-vector"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Arg)], store: Store[Addr, Abs], t: Time) = {
      def createVec(size: Abs, init: Abs, initaddr: Addr): MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])]  = {
        isInteger(size) >>= (isint =>
          if (abs.isTrue(isint)) {
            val vaddr = Address[Addr].cell(fexp, t)
            abs.vector(vaddr, size, initaddr).map({ case (va, vector) =>
              (va, store.extend(vaddr, vector).extend(initaddr, init), Set.empty)})
          } else {
            MayFailError(List(TypeError("make-vector", "first operand", "integer", size.toString)))
          })
      }
      args match {
        case (_, size) :: Nil => createVec(size._1, abs.inject(false), Address[Addr].primitive("__undef-vec-element__"))
        case (_, size) :: (initexp, init) :: Nil => createVec(size._1, init._1, Address[Addr].cell(initexp, t))
        case l => MayFailError[(Abs, Store[Addr, Abs], Set[Effect[Addr]])](List(ArityError(name, 1, l.size)))
      }
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.MakeVector
  }
  object MakeVector extends MakeVector
  class VectorSet extends Primitive[Addr, Abs] {
    val name = "vector-set!"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Arg)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, vector) :: (_, index) :: (exp, value) :: Nil => {
        val addrs = abs.getVectors(vector._1)
        if (addrs.isEmpty) {
          MayFailError(List(CannotAccessVector(vector.toString)))
        } else {
          val init: MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])] = MayFailSuccess((abs.bottom, store, Set[Effect[Addr]]()))
          addrs.foldLeft(init)((acc, va) => {
            store.lookup(va) match {
              case Some(oldvec) => {
                acc >>= ({ case (oldval, store, effects) =>
                  val targetaddr = Address[Addr].cell(exp, t)
                  abs.vectorSet(oldvec, index._1, targetaddr).map({ case (vec, addrs) =>
                    val store2 = addrs.foldLeft(store.update(va, vec))((st, a) => st.updateOrExtend(a, value._1))
                    val effects2 = addrs.map(a => EffectWriteVector(a))
                    (abs.join(oldval, vec), store2, effects ++ effects2)
                  })
                })
              }
              case None => acc.addError(UnboundAddress(va.toString))
            }
          })
        }
      }
      case l => MayFailError[(Abs, Store[Addr, Abs], Set[Effect[Addr]])](List((ArityError(name, 3, l.size))))
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.VectorSet
  }
  object VectorSet extends VectorSet
  class Vector extends Primitive[Addr, Abs] {
    val name = "vector"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Arg)], store: Store[Addr, Abs], t: Time) = {
      val a = Address[Addr].cell(fexp, t)
      val botaddr = Address[Addr].primitive("__bottom__")
      abs.vector(a, abs.inject(args.size), botaddr) >>= ({ case (va, emptyVector) =>
        /* No tracked effects because we only perform atomic updates at allocation time */
        val init: MayFail[(Abs, Store[Addr, Abs])] = MayFailSuccess((emptyVector, store))
        args.zipWithIndex.foldLeft(init)((acc, arg) => acc >>= ({ case (vec, store) =>
          arg match {
            case ((exp, value), index) =>
              val valaddr = Address[Addr].cell(exp, t)
              abs.vectorSet(vec, abs.inject(index), valaddr).map({
                case (vec, addrs) => (vec, addrs.foldLeft(store)((st, a) => st.updateOrExtend(a, value._1)))
              })
          }})).map({ case (vector, store) => (va, store.extend(a, vector), Set[Effect[Addr]]()) })
      })
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Vector
  }
  object Vector extends Vector
  class VectorLength extends StoreOperation("vector-length", Some(1)) {
    def length(v: Abs, store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
      val addrs = abs.getVectors(v)
      if (addrs.isEmpty) {
        MayFailError(List(CannotAccessVector(v.toString)))
      } else {
        addrs.foldLeft(mfmon.zero)((acc, va) =>
          MayFail.monoid[(Abs, Set[Effect[Addr]])].append(acc, /* TODO: that's basically a foldMap over a monoid... */
            store.lookup(va) match {
              case Some(v) => vectorLength(v).map(vl => (vl, Set(EffectReadVariable(va))))
              case None => MayFailError(List(UnboundAddress(va.toString)))
            }))
      }
    }
    override def call(v: Arg, store: Store[Addr, Abs]) = {
      length(v._1, store).map({ case (v, effs) => (v, store, effs) })
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.VectorLength
  }
  object VectorLength extends VectorLength
  class VectorRef extends StoreOperation("vector-ref", Some(2)) {
    def vectorRef(v: Abs, index: Abs, store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
      val addrs = abs.getVectors(v)
      if (addrs.isEmpty) {
        MayFailError(List(CannotAccessVector(v.toString)))
      } else {
        addrs.foldLeft(mfmon.zero)((acc, va) =>
          store.lookup(va) match {
            case Some(v) => abs.vectorRef(v, index) >>= (vs =>
              vs.foldLeft(acc)((acc, a) =>
                mfmon.append(acc,
                  store.lookup(a) match {
                    case Some(value) => MayFailSuccess((value, Set(EffectReadVector(a))))
                    case None => err(UnboundAddress(a.toString))
                  })))
            case None => mfmon.append(acc, MayFailError(List(UnboundAddress(va.toString))))
          })
      }
    }
    override def call(v: Arg, index: Arg, store: Store[Addr, Abs]) =
      vectorRef(v._1, index._1, store).map({ case (v, effs) => (v, store, effs) })
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.VectorRef
  }
  object VectorRef extends VectorRef

  /** (define (list-ref l index)
        (if (pair? l)
          (if (= index 0)
            (car l)
            (list-ref (cdr l) (- index 1)))
          (error "list-ref applied to a non-list"))) */
  class ListRef extends StoreOperation("list-ref", Some(2)) {
    def listRef(l: Abs, index: Abs, visited: Set[(Abs, Abs)], store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
      if (visited.contains((l, index))) {
        (abs.bottom, Set[Effect[Addr]]()).point[MayFail]
      } else {
        isCons(l) >>= { constest => {
          val t: MayFail[(Abs, Set[Effect[Addr]])] = if (abs.isTrue(constest)) {
            numEq(index, abs.inject(0)) >>= { indextest =>
              val tt = if (abs.isTrue(indextest)) {
                car(l, store)
              } else { (abs.bottom, Set.empty[Effect[Addr]]).point[MayFail] }
              val tf = if (abs.isFalse(indextest)) {
                minus(index, abs.inject(1)) >>= { index2 => cdr(l, store) >>= {
                  case (cdrl, effcdr) => listRef(cdrl, index2, visited + ((l, index)), store).map({ case (v, effs) => (v, effs ++ effcdr) }) } }
              } else { (abs.bottom, Set.empty[Effect[Addr]]).point[MayFail] }
              MayFail.monoid[(Abs, Set[Effect[Addr]])].append(tt, tf)
            }
          } else { (abs.bottom, Set.empty[Effect[Addr]]).point[MayFail] }
          val f: MayFail[(Abs, Set[Effect[Addr]])] = if (abs.isFalse(constest)) {
            MayFailError(List(OperatorNotApplicable("list-ref: first argument not a list or index out of bounds", List(l.toString, index.toString))))
          } else { (abs.bottom, Set.empty[Effect[Addr]]).point[MayFail] }
          MayFail.monoid[(Abs, Set[Effect[Addr]])].append(t, f)
        }}
      }
    }
    override def call(l: Arg, index: Arg, store: Store[Addr, Abs]) =
      listRef(l._1, index._1, Set.empty, store).map({ case (v, effs) => (v, store, effs) })
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.ListRef
  }
  object ListRef extends ListRef

  /** (define (equal? a b)
        (or (eq? a b)
          (and (null? a) (null? b))
          (and (pair? a) (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
          (and (vector? a) (vector? b)
            (let ((n (vector-length a)))
              (and (= (vector-length b) n)
                (letrec ((loop (lambda (i)
                                 (or (= i n)
                                   (and (equal? (vector-ref a i) (vector-ref b i))
                                     (loop (+ i 1)))))))
                  (loop 0)))))))
   */
  class Equal extends StoreOperation("equal?", Some(2)) {
    override def call(a: Arg, b: Arg, store: Store[Addr, Abs]) = {
      def equalVec(a: Abs, b: Abs, i: Abs, n: Abs, visitedEqual: Set[(Abs, Abs)], visited: Set[(Abs, Abs, Abs, Abs)]): MayFail[(Abs, Set[Effect[Addr]])] = {
        if (visited.contains((a, b, i, n)) || a == abs.bottom || b == abs.bottom || i == abs.bottom || n == abs.bottom) {
          MayFailSuccess((abs.bottom, Set[Effect[Addr]]()))
        } else {
          numEq(i, n) >>= (numtest => {
            val t = if (abs.isTrue(numtest)) { MayFailSuccess((abs.inject(true), Set[Effect[Addr]]())) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            val f = if (abs.isFalse(numtest)) {
              VectorRef.vectorRef(a, i, store) >>= ({ case (vai, effects1) =>
                VectorRef.vectorRef(b, i, store) >>= ({ case (vbi, effects2) =>
                  equalp(vai, vbi, visitedEqual) >>= ({ case (itemtest, effects3) => {
                    val tt = if (abs.isTrue(itemtest)) {
                      plus(i, abs.inject(1)) >>= (iplus1 =>
                        equalVec(a, b, iplus1, n, visitedEqual, visited + ((a, b, i, n))).map({ case (eqvec, effects4) =>
                        (eqvec, effects1 ++ effects2 ++ effects3 ++ effects4)}))
                    } else { MayFailSuccess((abs.bottom, effects1 ++ effects2 ++ effects3)) }
                    val tf = if (abs.isFalse(itemtest)) { MayFailSuccess((abs.inject(false), effects1 ++ effects2 ++ effects3)) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                    MayFail.monoid[(Abs, Set[Effect[Addr]])].append(tt, tf)
                }})
              })})
            } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            MayFail.monoid[(Abs, Set[Effect[Addr]])].append(t, f)
          })
        }
      }
      def equalp(a: Abs, b: Abs, visited: Set[(Abs, Abs)]): MayFail[(Abs, Set[Effect[Addr]])] = {
        if (visited.contains((a, b)) || a == abs.bottom || b == abs.bottom) {
          MayFailSuccess((abs.bottom, Set[Effect[Addr]]()))
        } else {
          val visited2 = visited + ((a, b))
          eqq(a, b) >>= (eqtest => {
            val t = if (abs.isTrue(eqtest)) { MayFailSuccess((eqtest, Set[Effect[Addr]]())) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            val f = if (abs.isFalse(eqtest)) {
              isNull(a) >>= (anull => isNull(b) >>= (bnull => {
                val nulltest = abs.and(anull, bnull)
                val ft = if (abs.isTrue(nulltest)) { MayFailSuccess((eqtest, Set[Effect[Addr]]())) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                val ff = if (abs.isFalse(nulltest)) {
                  isCons(a) >>= (acons => isCons(b) >>= (bcons => {
                    val constest = abs.and(acons, bcons)
                    val fft = if (abs.isTrue(constest)) {
                      car(a, store) >>= ({ case (acar, effects1) =>
                        car(b, store) >>= ({ case (bcar, effects2) =>
                          equalp(acar, bcar, visited2) >>= ({ case (cartest, effects3) => {
                            val fftt = if (abs.isTrue(cartest)) {
                              cdr(a, store) >>= ({ case (acdr, effects4) =>
                                cdr(b, store) >>= ({ case (bcdr, effects5) =>
                                  equalp(acdr, bcdr, visited2).map({ case (eqtest, effects6) =>
                                    (eqtest, effects1 ++ effects2 ++ effects3 ++ effects4 ++ effects5 ++ effects6)
                                  })
                                })
                              })
                            } else { MayFailSuccess((abs.bottom, effects1 ++ effects2 ++ effects3)) }
                            val fftf = if (abs.isFalse(cartest)) { MayFailSuccess((abs.inject(false)), effects1 ++ effects2 ++ effects3) } else { MayFailSuccess((abs.bottom, effects1 ++ effects2 ++ effects3)) }
                            MayFail.monoid[(Abs, Set[Effect[Addr]])].append(fftt, fftf)
                          }})})})} else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                    val fff = if (abs.isFalse(constest)) {
                      isVector(a) >>= (avec =>
                        isVector(b) >>= (bvec => {
                          val vectest = abs.and(avec, bvec)
                          val ffft = if (abs.isTrue(vectest)) {
                            VectorLength.length(a, store) >>= ({ case (alength, effects1) =>
                              VectorLength.length(b, store) >>= ({ case (blength, effects2) =>
                                numEq(alength, blength) >>= (lengthtest => {
                                  val ffftt = if (abs.isTrue(lengthtest)) {
                                    equalVec(a, b, abs.inject(0), alength, visited2, Set()).map({ case (eqvec, effects3) =>
                                      (eqvec, effects1 ++ effects2 ++ effects3)})
                                  } else  { MayFailSuccess((abs.bottom, effects1 ++ effects2)) }
                                  val ffftf = if (abs.isFalse(lengthtest)) { MayFailSuccess((abs.inject(false), effects1 ++ effects2)) } else { MayFailSuccess((abs.bottom, effects1 ++ effects2)) }
                                  MayFail.monoid[(Abs, Set[Effect[Addr]])].append(ffftt, ffftf)
                                })
                              })
                            })
                          } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                          val ffff = if (abs.isFalse(vectest)) { MayFailSuccess((abs.inject(false), Set[Effect[Addr]]())) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                          MayFail.monoid[(Abs, Set[Effect[Addr]])].append(ffft, ffff)
                        }))
                    } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                    MayFail.monoid[(Abs, Set[Effect[Addr]])].append(fft, fff)
                  }))
                } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                MayFail.monoid[(Abs, Set[Effect[Addr]])].append(ft, ff)
              }))
            } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            MayFail.monoid[(Abs, Set[Effect[Addr]])].append(t, f)
          })
        }
      }
      equalp(a._1, b._1, Set()).map({ case (v, effs) => (v, store, effs) })
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Equal
  }
  object Equal extends Equal

  /** (define (member e l) ; member, memq and memv are similar, the difference lies in the comparison function used
       (if (null? l)
         #f
         (if (equal? (car l) e)
           l
           (member e (cdr l))))) */
  abstract class MemberLike(val n: String, eqFn: ((Abs, Option[ConcolicExpression]), (Abs, Option[ConcolicExpression]), Store[Addr, Abs]) => MayFail[(Abs, Set[Effect[Addr]])]) extends StoreOperation(n, Some(2)) {
    def mem(e: Abs, l: Abs, visited: Set[Abs], store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
      if (visited.contains(l)) {
        (abs.bottom, Set[Effect[Addr]]()).point[MayFail]
      } else {
        isNull(l) >>= { nulltest => {
          val t = if (abs.isTrue(nulltest)) {
            (abs.inject(false), Set[Effect[Addr]]()).point[MayFail]
          } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
          val f = if (abs.isFalse(nulltest)) {
            car(l, store) >>= { case (carl, careff) =>
              eqFn(e, carl, store) >>= { case (equaltest, equaleff) =>
                val ft = if (abs.isTrue(equaltest)) {
                  (l, careff ++ equaleff).point[MayFail]
                } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
                val ff = if (abs.isFalse(equaltest)) {
                  cdr(l, store) >>= { case (cdrl, cdreff) =>
                    mem(e, cdrl, visited + l, store).map({ case (res, effs) => (res, effs ++ careff ++ cdreff) })
                  }
                } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
                MayFail.monoid[(Abs, Set[Effect[Addr]])].append(ft, ff)
              }
            }
          } else {
            (abs.bottom, Set[Effect[Addr]]()).point[MayFail]
          }
          MayFail.monoid[(Abs, Set[Effect[Addr]])].append(t, f)
        }}
      }
    }
    override def call(e: Abs, l: Abs, store: Store[Addr, Abs]) =
      mem(e, l, Set.empty, store).map({ case (v, effs) => (v, store, effs) })
  }

  class Member extends MemberLike("member",
    (x: (Abs, Option[ConcolicExpression]), y: (Abs, Option[ConcolicExpression]), store: Store[Addr, Abs]) => Equal.call(x, y, store).map({ case (res, _ /* unchanged store */, eff) => (res, eff) })) {
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Member
  }
  object Member extends Member
  class Memq extends MemberLike("memq",
    (x: (Abs, Option[ConcolicExpression]), y: (Abs, Option[ConcolicExpression]), store: Store[Addr, Abs]) => Eq.call(x, y).map(res => (res, Set.empty))) {
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Memq
  }
  object Memq extends Memq

  abstract class AssocLike(val n: String, eqFn: ((Abs, Option[ConcolicExpression]), (Abs, Option[ConcolicExpression]), Store[Addr, Abs]) => MayFail[(Abs, Set[Effect[Addr]])]) extends StoreOperation(n, Some(2)) {
    def assoc(e: Arg, l: Arg, visited: Set[Abs], store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
      if (visited.contains(l)) {
        (abs.bottom, Set[Effect[Addr]]()).point[MayFail]
      } else {
        isNull(l) >>= { nulltest => {
          val t = if (abs.isTrue(nulltest)) {
            (abs.inject(false), Set[Effect[Addr]]()).point[MayFail]
          } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
          val f = if (abs.isFalse(nulltest)) {
            car(l, store) >>= { case (carl, careff) =>
              isCons(carl) >>= { constest =>
                val ft = if (abs.isTrue(constest)) {
                  car(carl, store) >>= { case (caarl, caareff) =>
                    eqFn(e, caarl, store) >>= { case (equaltest, equaleff) =>
                      val ftt = if (abs.isTrue(equaltest)) {
                        (carl, careff ++ caareff ++ equaleff).point[MayFail]
                      } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
                      val ftf = if (abs.isFalse(equaltest)) {
                        cdr(l, store) >>= { case (cdrl, cdreff) =>
                          assoc(e, cdrl, visited + l, store).map({ case (res, effs) => (res, effs ++ careff ++ cdreff ++ equaleff) })
                        }
                      } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
                      MayFail.monoid[(Abs, Set[Effect[Addr]])].append(ftt, ftf)
                    }
                  }
                } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
                val ff = if (abs.isFalse(constest)) {
                  (abs.inject(false), Set[Effect[Addr]]()).point[MayFail] /* or error? because it's not an alist */
                } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
                MayFail.monoid[(Abs, Set[Effect[Addr]])].append(ft, ff)
              }
            }
          } else {
            (abs.bottom, Set[Effect[Addr]]()).point[MayFail]
          }
          MayFail.monoid[(Abs, Set[Effect[Addr]])].append(t, f)
        }}
      }
    }
    override def call(e: Arg, l: Arg, store: Store[Addr, Abs]) =
      assoc(e, l, Set.empty, store).map({ case (v, effs) => (v, store, effs) })
  }

  class Assoc extends AssocLike("assoc",
    (x: (Abs, Option[ConcolicExpression]), y: (Abs, Option[ConcolicExpression]), store: Store[Addr, Abs]) => Equal.call(x, y, store).map({ case (res, _ /* unchanged store */, eff) => (res, eff) })) {
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Assoc
  }
  object Assoc extends Assoc
  class Assq extends AssocLike("assq",
    (x: (Abs, Option[ConcolicExpression]), y: (Abs, Option[ConcolicExpression]), store: Store[Addr, Abs]) => Eq.call(x, y).map(res => (res, Set.empty))) {
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.Assq
  }
  object Assq extends Assq


  /** Bundles all the primitives together, annotated with R5RS support (v: supported, vv: supported and tested in PrimitiveTests, vx: not fully supported, x: not supported), and section in Guile manual */
  def all: List[Primitive[Addr, Abs]] = List(
    Times,          /* [vv] *: Arithmetic */
    Plus,           /* [vv] +: Arithmetic */
    Minus,          /* [vv] -: Arithmetic */
    Div,            /* [vx] /: Arithmetic (no support for fractions) */
    ArithAbs,       /* [vv] abs: Arithmetic */
    ACos,           /* [vv] acos: Scientific */
                    /* [x]  angle: Complex */
                    /* [x]  append: Append/Reverse */
                    /* [x]  apply: Fly Evaluation */
                    /* [x]  apply: Fly Evaluation */
    ASin,           /* [vv] asin: Scientific */
    Assoc,          /* [vv] assoc: Retrieving Alist Entries */
    Assq,           /* [vv] assq: Retrieving Alist Entries */
                    /* [x]  assv: Retrieving Alist Entries */
    ATan,           /* [vv] atan: Scientific [easy] */
    Booleanp,       /* [vv] boolean?: Booleans */
                    /* [x]  call-with-current-continuation: Continuations */
                    /* [x]  call-with-input-file: File Ports */
                    /* [x]  call-with-output-file: File Ports */
                    /* [x]  call-with-values: Multiple Values */
    Car,            /* [vv] car: Pairs */
    Cdr,            /* [vv] cdr: Pairs */
    Ceiling,        /* [vv] ceiling: Arithmetic */
                    /* [x]  char->integer: Characters */
                    /* [x]  char-alphabetic?: Characters */
                    /* [x]  char-ci<=?: Characters */
                    /* [x]  char-ci<?: Characters */
                    /* [x]  char-ci=?: Characters */
                    /* [x]  char-ci>=?: Characters */
                    /* [x]  char-ci>?: Characters */
                    /* [x]  char-downcase: Characters */
                    /* [x]  char-lower-case?: Characters */
                    /* [x]  char-numeric?: Characters */
                    /* [x]  char-ready?: Reading */
                    /* [x]  char-upcase: Characters */
                    /* [x]  char-upper-case?: Characters */
                    /* [x]  char-whitespace?: Characters */
                    /* [x]  char<=?: Characters */
                    /* [x]  char<?: Characters */
                    /* [x]  char=?: Characters */
                    /* [x]  char>=?: Characters */
                    /* [x]  char>?: Characters */
    Charp,          /* [vv] char?: Characters */
                    /* [x]  close-input-port: Closing */
                    /* [x]  close-output-port: Closing */
                    /* [x]  complex?: Complex Numbers */
    Cons,           /* [vv] cons: Pairs */
    Cos,            /* [vv] cos: Scientific */
                    /* [x]  current-input-port: Default Ports */
                    /* [x]  current-output-port: Default Ports */
    Display,        /* [v]  display: Writing */
                    /* [x]  dynamic-wind: Dynamic Wind */
                    /* [x]  eof-object?: Reading */
    Eq,             /* [vv] eq?: Equality */
    Equal,          /* [vv] equal?: Equality */
                    /* [x]  eqv?: Equality */
                    /* [x]  eval: Fly Evaluation */
    Evenp,          /* [v]  even?: Integer Operations */
    ExactToInexact, /* [vv] exact->inexact: Exactness */
                    /* [x]  exact?: Exactness */
                    /* [x]  exp: Scientific */
    Expt,           /* [vv] expt: Scientific */
    Floor,          /* [vv] floor: Arithmetic */
                    /* [x]  for-each: List Mapping */
                    /* [x]  force: Delayed Evaluation */
    Gcd,            /* [vx] gcd: Integer Operations */
                    /* [x]  imag-part: Complex */
    InexactToExact, /* [vv] inexact->exact: Exactness */
                    /* [x]  inexact?: Exactness */
                    /* [x]  input-port?: Ports */
                    /* [x]  integer->char: Characters */
    Integerp,       /* [vv] integer?: Integers */
                    /* [x]  interaction-environment: Fly Evaluation */
                    /* [x]  lcm: Integer Operations */
    Length,         /* [vv] length: List Selection */
    ListPrim,       /* [vv] list: List Constructors */
                    /* [x]  list->string: String Constructors */
                    /* [x]  list->vector: Vector Creation */
    ListRef,        /* [vv] list-ref: List Selection */
                    /* [x]  list-tail: List Selection */
    Listp,          /* [vv] list?: List Predicates */
                    /* [x]  load: Loading */
    Log,            /* [vv] log: Scientific */
                    /* [x]  magnitude: Complex */
                    /* [x]  make-polar: Complex */
                    /* [x]  make-rectangular: Complex */
                    /* [x]  make-string: String Constructors */
    MakeVector,     /* [vv] make-vector: Vector Creation */
                    /* [x]  map: List Mapping */
    Max,            /* [vv] max: Arithmetic */
    Member,         /* [vv] member: List Searching */
    Memq,           /* [v]  memq: List Searching */
                    /* [x]  memv: List Searching */
    Min,            /* [vv] min: Arithmetic */
    Modulo,         /* [vv] modulo: Integer Operations */
    Negativep,      /* [vv] negative?: Comparison */
    Newline,        /* [v]  newline: Writing */
    Not,            /* [vv] not: Booleans */
    Nullp,          /* [vv] null?: List Predicates */
    NumberToString, /* [vx] number->string: Conversion: does not support two arguments */
    Numberp,        /* [vv] number?: Numerical Tower */
    Oddp,           /* [vv] odd?: Integer Operations */
                    /* [x]  open-input-file: File Ports */
                    /* [x]  open-output-file: File Ports */
                    /* [x]  output-port?: Ports */
    Pairp,          /* [vv] pair?: Pairs */
                    /* [x]  peek-char?: Reading */
    Positivep,      /* [vv] positive?: Comparison */
                    /* [x]  procedure?: Procedure Properties */
    Quotient,       /* [vv] quotient: Integer Operations */
                    /* [x]  rational?: Reals and Rationals */
                    /* [x]  read: Scheme Read */
                    /* [x]  read-char?: Reading */
                    /* [x]  real-part: Complex */
    Realp,          /* [vv] real?: Reals and Rationals */
    Remainder,      /* [vv] remainder: Integer Operations */
                    /* [x]  reverse: Append/Reverse */
    Round,          /* [vv] round: Arithmetic */
    SetCar,         /* [vv] set-car!: Pairs */
    SetCdr,         /* [vv] set-cdr!: Pairs */
    Sin,            /* [vv] sin: Scientific */
    Sqrt,           /* [vv] sqrt: Scientific */
                    /* [x]  string: String Constructors */
                    /* [x]  string->list: List/String Conversion */
                    /* [x]  string->number: Conversion */
    StringToSymbol, /* [vv] string->symbol: Symbol Primitives */
    StringAppend,   /* [vx] string-append: Appending Strings: only two arguments supported */
                    /* [x]  string-ci<: String Comparison */
                    /* [x]  string-ci=?: String Comparison */
                    /* [x]  string-ci>=?: String Comparison */
                    /* [x]  string-ci>?: String Comparison */
                    /* [x]  string-copy: String Selection */
                    /* [x]  string-fill!: String Modification */
    StringLength,   /* [vv] string-length: String Selection */
                    /* [x]  string-ref: String Selection */
                    /* [x]  string-set!: String Modification */
                    /* [x]  string<=?: String Comparison */
    StringLt,       /* [vv]  string<?: String Comparison */
                    /* [x]  string=?: String Comparison */
                    /* [x]  string>=?: String Comparison */
                    /* [x]  string>?: String Comparison */
    Stringp,        /* [x]  string?: String Predicates */
                    /* [x]  substring: String Selection */
    SymbolToString, /* [vv] symbol->string: Symbol Primitives */
    Symbolp,        /* [vv] symbol?: Symbol Primitives */
    Tan,            /* [vv] tan: Scientific */
                    /* [x]  truncate: Arithmetic */
                    /* [x]  values: Multiple Values */
    Vector,         /* [vv] vector: Vector Creation */
                    /* [x]  vector->list: Vector Creation */
                    /* [x]  vector-fill!: Vector Accessors */
    VectorLength,   /* [vv] vector-length: Vector Accessors */
    VectorRef,      /* [vv] vector-ref: Vector Accessors */
    VectorSet,      /* [vv] vector-set!: Vector Accessors */
    Vectorp,        /* [vv] vector?: Vector Creation */
                    /* [x]  with-input-from-file: File Ports */
                    /* [x]  with-output-to-file: File Ports */
                    /* [x]  write-char: Writing */
    Zerop,          /* [vv] zero?: Comparison */
    LessThan,       /* [v]  < */
    LessOrEqual,    /* [v]  <= */
    NumEq,          /* [v]  = */
    GreaterThan,    /* [v]  > */
    GreaterOrEqual, /* [v]  >= */
                    /* [x]  numerator */
                    /* [x]  denominator */
                    /* [x]  rationalize-string */
                    /* [x]  scheme-report-environment */
                    /* [x]  null-environment */
                    /* [x]  write transcript-on */
                    /* [x]  transcript-off */
    Caar, Cadr,     /* [v]  caar etc. */
    Cdar, Cddr, Caaar, Caadr, Cadar, Caddr, Cdaar, Cdadr, Cddar, Cdddr, Caaaar,
    Caaadr, Caadar, Caaddr, Cadaar, Cadadr, Caddar, Cadddr, Cdaaar,
    Cdaadr, Cdadar, Cdaddr, Cddaar, Cddadr, Cdddar, Cddddr,
    /* Other primitives that are not R5RS */
    Random, Error, BoolTop, IntTop, RandomCons)

  def toVal(prim: Primitive[Addr, Abs]): Abs = abs.inject(prim)
}
