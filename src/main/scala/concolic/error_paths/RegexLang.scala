sealed trait Regex[T]
sealed trait RegexMatch[T]

case class EmptyWord[T]() extends Regex[T] with RegexMatch[T] {
  override def toString: String = "ε"
}
case class EmptySet[T]() extends Regex[T] with RegexMatch[T] {
  override def toString: String = "∅"
}
case class Atomic[T](v: T) extends Regex[T] with RegexMatch[T] {
  override def toString: String = v.toString
}
case class Concat[T](x: Regex[T], y: Regex[T]) extends Regex[T] with RegexMatch[T] {
  override def toString: String = s"($x.$y)"
}
case class Or[T](x: Regex[T], y: Regex[T]) extends Regex[T] with RegexMatch[T] {
  override def toString: String = s"($x+$y)"
}
case class Star[T](v: Regex[T]) extends Regex[T] {
  override def toString: String = s"$v*"
}
case class ConcatMatch[T](x: RegexMatch[T], y: RegexMatch[T]) extends RegexMatch[T] {
  override def toString: String = s"($x.$y)"
}
case class OrMatch[T](x: RegexMatch[T], y: RegexMatch[T]) extends RegexMatch[T] {
  override def toString: String = s"($x+$y)"
}
case class StarMatch[T](current: RegexMatch[T], original: Star[T]) extends RegexMatch[T] {
  override def toString: String = original.toString
}

object RegexToRegexMatch {
  import scala.language.implicitConversions
  implicit def convert[T](regex: Regex[T]): RegexMatch[T] = regex match {
    case EmptyWord() => EmptyWord()
    case EmptySet() => EmptySet()
    case Atomic(v) => Atomic(v)
    case Concat(x, y) => ConcatMatch(convert(x), convert(y))
    case Or(x, y) => OrMatch(convert(x), convert(y))
    case star@Star(v) => StarMatch(convert(v), star)
  }
}

class RegexLang[T] {
  var cache:scala.collection.mutable.Map[Regex[T], Boolean] = scala.collection.mutable.Map[Regex[T], Boolean]()

  def sum(regex: Regex[T]) :  Int = {
    def sumAcc(trees: Regex[T], acc: Int) : Int = trees match {
      case Or(x, y) => Math.max(sumAcc(x, acc + 1), sumAcc(y, acc + 1))
      case x => acc

    }
    sumAcc(regex, 0)
  }

  def sum2(regex: Regex[T]) :  Int = {
    def sumAcc(trees: Regex[T], acc: Int) : Int = trees match {
      case Or(x, y) => sumAcc(x, acc + 1) + sumAcc(y, acc + 1)
      case Concat(x, y) => sumAcc(x, acc + 1) + sumAcc(y, acc + 1)
      case Star(x) => sumAcc(x, acc + 1)
      case x => acc

    }
    sumAcc(regex, 0)
  }

  case class Final(done: Regex[T]) extends Regex[T]
  case class OrFrame(done: Regex[T], y: Regex[T]) extends Regex[T]
  case class OrMergeFrame(x: Regex[T], y: Regex[T]) extends Regex[T]
  case class AndFrame(done: Regex[T], y: Regex[T]) extends Regex[T]
  case class AndMergeFrame(x: Regex[T], y: Regex[T]) extends Regex[T]
  case class MulFrame(done: Regex[T]) extends Regex[T]
  case class StarFrame(x: Regex[T]) extends Regex[T]


  @scala.annotation.tailrec
  final def simplifyr(r : Regex[T], stack:List[(Regex[T] => Regex[T])] = List((a => Final(a)): (Regex[T] => Regex[T]))):Regex[T] = r match {
    case Final(x) => x
    case x if cache.contains(x) =>
      simplifyr(stack.head(x), stack.tail)
    case x: Atomic[T] => simplifyr(stack.head(x), stack.tail)
    case EmptySet() => simplifyr(stack.head(EmptySet()), stack.tail)
    case EmptyWord() => simplifyr(stack.head(EmptyWord()), stack.tail)

    case Star(x) => simplifyr(x, ((a => StarFrame(a)):(Regex[T] => Regex[T])) :: stack )
    case StarFrame(EmptySet()) => simplifyr(stack.head(EmptyWord()), stack.tail)
    case StarFrame(EmptyWord()) => simplifyr(stack.head(EmptyWord()), stack.tail)
    // anything else of STAR
    case StarFrame(x) =>
      cache += (Star(x) -> true)
      simplifyr(stack.head(Star(x)), stack.tail)

    case Or(x, y) => simplifyr(x, ((a => OrFrame(a, y)):(Regex[T] => Regex[T])) :: stack )
    case OrFrame(done, y) => simplifyr(y, ((a => OrMergeFrame(done, a)):(Regex[T] => Regex[T])) :: stack )
    case OrMergeFrame(EmptySet(), y) => simplifyr(stack.head(y), stack.tail)
    case OrMergeFrame(x, EmptySet()) => simplifyr(stack.head(x), stack.tail)
    // TODO:NOT SURE?? but should be ok, because they mimic a transition to itself without consuming symbol
    case OrMergeFrame(EmptyWord(), y) => simplifyr(stack.head(y), stack.tail)
    case OrMergeFrame(x, EmptyWord()) => simplifyr(stack.head(x), stack.tail)
    case OrMergeFrame(Atomic(x), Atomic(y)) if (x == y) => simplifyr(stack.head(Atomic(x)), stack.tail)
    // anything else of OR, we could not simplify it, so we should cache it
    case OrMergeFrame(x, y) =>
      cache += (Or(x, y) -> true)
      simplifyr(stack.head(Or(x, y)), stack.tail)

    case Concat(x, y) => simplifyr(x, ((a => { AndFrame(a, y)}):(Regex[T] => Regex[T])) :: stack )
    case AndFrame(done, y) => simplifyr(y, ((a => { AndMergeFrame(done, a)}):(Regex[T] => Regex[T])) :: stack )
    case AndMergeFrame(x, EmptySet()) => simplifyr(stack.head(EmptySet()), stack.tail)
    case AndMergeFrame(x, EmptyWord()) => simplifyr(stack.head(x), stack.tail)
    case AndMergeFrame(EmptySet(), y) => simplifyr(stack.head(EmptySet()), stack.tail)
    case AndMergeFrame(EmptyWord(), y) => simplifyr(stack.head(y), stack.tail)
    // anything else of AND
    case AndMergeFrame(x, y) =>
      cache + (Concat(x, y) -> true)
      simplifyr(stack.head(Concat(x, y)), stack.tail)

    case MulFrame(done) => simplifyr(stack.head(done), stack.tail)
  }
}