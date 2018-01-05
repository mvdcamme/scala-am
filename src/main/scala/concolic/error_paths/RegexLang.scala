object RegexLang {

  var cache:scala.collection.mutable.Map[Regex, Boolean] = scala.collection.mutable.Map[Regex, Boolean]()

  def sum(regex: Regex) :  Int = {
    def sumAcc(trees: Regex, acc: Int) : Int = trees match {
      case Or(x, y) => Math.max(sumAcc(x, acc + 1), sumAcc(y, acc + 1))
      case x => acc

    }
    sumAcc(regex, 0)
  }

  def sum2(regex: Regex) :  Int = {
    def sumAcc(trees: Regex, acc: Int) : Int = trees match {
      case Or(x, y) => sumAcc(x, acc + 1) + sumAcc(y, acc + 1)
      case Concat(x, y) => sumAcc(x, acc + 1) + sumAcc(y, acc + 1)
      case Star(x) => sumAcc(x, acc + 1)
      case x => acc

    }
    sumAcc(regex, 0)
  }

  sealed trait Regex

  case object EmptyWord extends Regex {
    override def toString: String = "ε"
  }

  case object EmptySet extends Regex {
    override def toString: String = "∅"
  }

  case class Event(v: String) extends Regex {
    override def toString: String = v.toString
  }

  case class Star(v: Regex) extends Regex {
    override def toString: String = s"$v*"
  }

  case class Concat(x: Regex, y: Regex) extends Regex {
    override def toString: String = s"($x.$y)"
  }

  case class Or(x: Regex, y: Regex) extends Regex {
    override def toString: String = s"($x+$y)"
  }

  case class Final(done: Regex) extends Regex {}
  case class OrFrame(done: Regex, y: Regex) extends Regex {}
  case class OrMergeFrame(x: Regex, y: Regex) extends Regex {}
  case class AndFrame(done: Regex, y: Regex) extends Regex {}
  case class AndMergeFrame(x: Regex, y: Regex) extends Regex {}
  case class MulFrame(done: Regex) extends Regex {}
  case class StarFrame(x: Regex) extends Regex {}


  @scala.annotation.tailrec
  final def simplifyr(r : Regex, stack:List[(Regex => Regex)] = List((a => Final(a)): (Regex => Regex))):Regex = r match {
    case Final(x) => x
    case x if cache.contains(x) =>
      simplifyr(stack.head(x), stack.tail)
    case x: Event => simplifyr(stack.head(x), stack.tail)
    case EmptySet => simplifyr(stack.head(EmptySet), stack.tail)
    case EmptyWord => simplifyr(stack.head(EmptyWord), stack.tail)

    case Star(x) => simplifyr(x, ((a => StarFrame(a)):(Regex => Regex)) :: stack )
    case StarFrame(EmptySet) => simplifyr(stack.head(EmptyWord), stack.tail)
    case StarFrame(EmptyWord) => simplifyr(stack.head(EmptyWord), stack.tail)
    // anything else of STAR
    case StarFrame(x) =>
      cache += (Star(x) -> true)
      simplifyr(stack.head(Star(x)), stack.tail)

    case Or(x, y) => simplifyr(x, ((a => OrFrame(a, y)):(Regex => Regex)) :: stack )
    case OrFrame(done, y) => simplifyr(y, ((a => OrMergeFrame(done, a)):(Regex => Regex)) :: stack )
    case OrMergeFrame(EmptySet, y) => simplifyr(stack.head(y), stack.tail)
    case OrMergeFrame(x, EmptySet) => simplifyr(stack.head(x), stack.tail)
    // TODO:NOT SURE?? but should be ok, because they mimic a transition to itself without consuming symbol
    case OrMergeFrame(EmptyWord, y) => simplifyr(stack.head(y), stack.tail)
    case OrMergeFrame(x, EmptyWord) => simplifyr(stack.head(x), stack.tail)
    case OrMergeFrame(Event(x), Event(y)) if (x == y) => simplifyr(stack.head(Event(x)), stack.tail)
    // anything else of OR, we could not simplify it, so we should cache it
    case OrMergeFrame(x, y) =>
      cache += (Or(x, y) -> true)
      simplifyr(stack.head(Or(x, y)), stack.tail)

    case Concat(x, y) => simplifyr(x, ((a => { AndFrame(a, y)}):(Regex => Regex)) :: stack )
    case AndFrame(done, y) => simplifyr(y, ((a => { AndMergeFrame(done, a)}):(Regex => Regex)) :: stack )
    case AndMergeFrame(x, EmptySet) => simplifyr(stack.head(EmptySet), stack.tail)
    case AndMergeFrame(x, EmptyWord) => simplifyr(stack.head(x), stack.tail)
    case AndMergeFrame(EmptySet, y) => simplifyr(stack.head(EmptySet), stack.tail)
    case AndMergeFrame(EmptyWord, y) => simplifyr(stack.head(y), stack.tail)
    // anything else of AND
    case AndMergeFrame(x, y) =>
      cache + (Concat(x, y) -> true)
      simplifyr(stack.head(Concat(x, y)), stack.tail)

    case MulFrame(done) => simplifyr(stack.head(done), stack.tail)
  }
}