object SchemeOps {

  /** These are the unary operations that should be supported by Scheme lattices */
<<<<<<< HEAD
  trait UnaryOperator
  /* Checks the type of a value */
  case object IsNull extends UnaryOperator
  case object IsCons extends UnaryOperator
  case object IsChar extends UnaryOperator
  case object IsSymbol extends UnaryOperator
  case object IsString extends UnaryOperator
  case object IsInteger extends UnaryOperator
  case object IsFloat extends UnaryOperator
  case object IsBoolean extends UnaryOperator
  case object IsVector extends UnaryOperator
  /* Negate a value */
  case object Not extends UnaryOperator
  /* Unary arithmetic operations */
  case object Ceiling extends UnaryOperator
  case object Log extends UnaryOperator
  case object Random extends UnaryOperator
  /* Length operations */
  case object VectorLength extends UnaryOperator
  case object StringLength extends UnaryOperator
  /* Conversions */
  case object NumberToString extends UnaryOperator
  case object StringToSymbol extends UnaryOperator
  case object SymbolToString extends UnaryOperator

  /** Binary operations thatt should be supported by lattices */
  trait BinaryOperator
  /* Arithmetic operations */
  case object Plus extends BinaryOperator
  case object PlusFloat extends BinaryOperator /* Plus specialized for floating-point values */
  case object PlusInteger extends BinaryOperator /* Plus specialized for integer values */
  case object Minus extends BinaryOperator
  case object MinusFloat extends BinaryOperator /* Minus specialized for floating-point values */
  case object MinusInteger extends BinaryOperator /* Minus specialized for integer values */
  case object Times extends BinaryOperator
  case object Div extends BinaryOperator
  case object Modulo extends BinaryOperator
  /* Arithmetic comparison */
  case object Lt extends BinaryOperator
  /* Equality checking */
  case object NumEq extends BinaryOperator /* number equality */
  case object Eq extends BinaryOperator /* physical equality */
  /* String operations */
  case object StringAppend extends BinaryOperator
=======
  object UnaryOperator extends Enumeration {
    val
      /* Check the type of a value */
      IsNull, IsBoolean, IsCons, IsChar, IsSymbol, IsString, IsInteger, IsReal, IsVector,
      /* Negate a value */
      Not,
      /* Unary arithmetic operations */
      Ceiling, Floor, Round, Random, Sqrt,
      /* Transcendental functions */
      Sin, ASin, Cos, ACos, Tan, ATan, Log,
      /* Length operations */
      VectorLength, StringLength,
      /* Conversions */
      NumberToString, SymbolToString, StringToSymbol, ExactToInexact, InexactToExact
    = Value
  }
  type UnaryOperator = UnaryOperator.Value

  /** Binary operations that should be supported by lattices */
  object BinaryOperator extends Enumeration {
    val
      /* Arithmetic operations */
      Plus, Minus, Times, Div, Quotient, Modulo, Remainder,
      /* Arithmetic comparison */
      Lt,
      /* Equality checking */
      NumEq, /* number equality */
      Eq, /* physical equality */
      /* String operations */
      StringAppend, StringLt
    = Value
  }
  type BinaryOperator = BinaryOperator.Value
>>>>>>> 9de48f824fa56370876d922b957948f007216898

  /** Modulo in Scheme and Scala are different. This implements the same behavior as Scheme's modulo */
  def modulo(n1: Int, n2: Int): Int =
    if (scala.math.signum(n1) * scala.math.signum(n2) < 0) {
      /* different sign, behaviour not the same between Scheme and Scala, adjust it */
      (scala.math.abs(n2) - scala.math.abs(n1) % scala.math.abs(n2)) % scala.math
        .abs(n2) * (if (n2 < 0) -1 else 1)
    } else {
      /* same sign, same behaviour */
      n1 % n2
    }
  /** Remainder in Scheme has the same behavior of Scala's modulo. */
  def remainder(n1: Int, n2: Int): Int = n1 % n2
  def random(n: Int): Int = scala.math.abs(scala.util.Random.nextInt % n)
  def random(n: Double): Double = scala.math.abs(scala.util.Random.nextDouble % n)

  /** Round in Scheme and Scala are different. This implements the same behaviour as Scheme's round. */
  def round(n: Double): Double = {
    val frac = n % 1 /* Fractional part of n */
    /* In the case of a fraction part equaling 0.5, rounding is done towards the even number. */
    if ((scala.math.abs(frac) == 0.5) && (((n > 0) && ((scala.math.abs(n - frac) % 2) == 0)) || ((n < 0) && (((n - frac) % 2) == -1)))) {
      scala.math.round(n) - 1
    }
    else {
      scala.math.round(n)
    }
  }
}
