import org.scalacheck.{Arbitrary, Gen}
import scalaz.Scalaz._
import scalaz._

case class ISetGen[A](g: Gen[A])(implicit val order: Order[A]) {
  implicit val buildable = new org.scalacheck.util.Buildable[A, ISet[A]] {
    def builder = new scala.collection.mutable.Builder[A, ISet[A]] {
      var buff: ISet[A] = ISet.empty
      def clear() { buff = ISet.empty }
      def +=(x: A) = { buff = buff.union(ISet.singleton(x)); this }
      def result = buff
    }
  }
  implicit val toTraversable = (s: ISet[A]) => new Traversable[A] {
    def foreach[U](f: A => U): Unit = s.map({ x => f(x); () })
  }
  val gen: Gen[ISet[A]] = Gen.buildableOfN[ISet[A], A](10, g)
  def genSubset(set: ISet[A]): Gen[ISet[A]] = {
    val list = set.toList
    for { n <- Gen.choose(0, set.size) } yield ISet.fromList(scala.util.Random.shuffle(list).take(n))
  }
}

trait LatticeGenerator[L] {
  def any: Gen[L]
  def le(l: L): Gen[L]
  implicit val anyArb: Arbitrary[L] = Arbitrary(any)
}

object Generators {
  val str: Gen[String] = Gen.resize(10, Gen.oneOf(Gen.identifier, Gen.alphaStr, Gen.numStr))
  val int: Gen[Int] = Gen.choose(-1000, 1000)
  val double: Gen[Double] = Gen.choose(-1000.0, 1000.0)
  val char: Gen[Char] = Gen.choose(0.toChar, 255.toChar)
  val sym: Gen[String] = Gen.resize(10, Gen.oneOf(Gen.identifier, Gen.alphaStr))
}

object ConcreteBooleanGenerator extends LatticeGenerator[Concrete.B] {
  /** ConcreteBool is a finite lattice with four elements */
  val bot = BoolLattice[Concrete.B].bottom
  val t = BoolLattice[Concrete.B].inject(true)
  val f = BoolLattice[Concrete.B].inject(false)
  val top = BoolLattice[Concrete.B].top

  def any = Gen.oneOf(bot, t, f, top)
  def le(l: Concrete.B) =
    if (l == bot) { Gen.const(bot) }
    else if (l == top) { Gen.oneOf(bot, t, f) }
    else { Gen.oneOf(l, bot) }
}

object TypeGenerator extends LatticeGenerator[Type.T] {
  /** Type lattice is a finite lattice with two elements */
  def any = Gen.oneOf(Type.Top, Type.Bottom)
  def le(l: Type.T) = l match {
    case Type.Top => any
    case Type.Bottom => Gen.const(Type.Bottom)
  }
}

class ConcreteGenerator[T : Order](g: Gen[T])(implicit lat: LatticeElement[Concrete.L[T]]) extends LatticeGenerator[Concrete.L[T]] {
  val isetgen = ISetGen[T](g)
  val topgen: Gen[Concrete.L[T]] = lat.top
  def any = Gen.oneOf(topgen, isetgen.gen.map(x => Concrete.Values(x)))
  def le(l: Concrete.L[T]) = l match {
    case Concrete.Top => any
    case Concrete.Values(content) => isetgen.genSubset(content).map(x => Concrete.Values(x))
  }
}

object ConcreteStringGenerator extends ConcreteGenerator[String](Generators.str)(Order[String], Concrete.L.stringConcrete)
object ConcreteIntGenerator extends ConcreteGenerator[Int](Generators.int)
object ConcreteDoubleGenerator extends ConcreteGenerator[Double](Generators.double)
object ConcreteCharGenerator extends ConcreteGenerator[Char](Generators.char)(Order.fromScalaOrdering[Char], implicitly[LatticeElement[Concrete.L[Char]]])
object ConcreteSymbolGenerator extends ConcreteGenerator[String](Generators.sym)(Order[String], Concrete.L.symConcrete)

// TODO: bounded ints, constant propagation

abstract class ConstantPropagationGenerator[X : Order](gen: Gen[X])(implicit lat: LatticeElement[ConstantPropagation.L[X]]) extends LatticeGenerator[ConstantPropagation.L[X]] {
  def constgen: Gen[ConstantPropagation.L[X]] = for { x <- gen } yield ConstantPropagation.Constant(x)
  def botgen: Gen[ConstantPropagation.L[X]] = lat.bottom
  def topgen: Gen[ConstantPropagation.L[X]] = lat.top
  def any: Gen[ConstantPropagation.L[X]] = Gen.oneOf(constgen, botgen, topgen)
  def le(l: ConstantPropagation.L[X]) = if (l == lat.top) { any } else if (l == lat.bottom) { botgen } else { Gen.oneOf(l, lat.bottom) }
}

object StringConstantPropagationGenerator extends ConstantPropagationGenerator[String](Generators.str)(Order[String], ConstantPropagation.L.stringCP)
object IntegerConstantPropagationGenerator extends ConstantPropagationGenerator[Int](Generators.int)
object DoubleConstantPropagationGenerator extends ConstantPropagationGenerator[Double](Generators.double)
object CharConstantPropagationGenerator extends ConstantPropagationGenerator[Char](Generators.char)
object SymbolConstantPropagationGenerator extends ConstantPropagationGenerator[String](Generators.sym)(Order[String], ConstantPropagation.L.symCP)
