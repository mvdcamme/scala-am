import scala.collection.mutable.Map

object RegexLang {

  var cache:scala.collection.mutable.Map[Regex, Boolean] = scala.collection.mutable.Map[Regex, Boolean]()

  def sum(regex : Regex) :  Int = {
    def sumAcc(trees : Regex, acc : Int) : Int = trees match {
      case Or(x, y) => Math.max(sumAcc(x, acc + 1), sumAcc(y, acc + 1))
      //case Concat(x, y) => Math.max(sumAcc(x, acc + 1), sumAcc(y, acc + 1))
      case x => acc

    }
    sumAcc(regex, 0)
  }

  def sum2(regex : Regex) :  Int = {
    def sumAcc(trees : Regex, acc : Int) : Int = trees match {
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

  case class Concat(x: Regex, y:Regex) extends Regex {
    override def toString: String = s"($x.$y)"
  }

  case class Or(x: Regex, y:Regex) extends Regex {
    override def toString: String = s"($x+$y)"
  }

  case class Final(done:Regex) extends Regex {}
  case class OrFrame(done:Regex, y:Regex) extends Regex {}
  case class OrMergeFrame(x:Regex, y:Regex) extends Regex {}
  case class AndFrame(done:Regex, y:Regex) extends Regex {}
  case class AndMergeFrame(x:Regex, y:Regex) extends Regex {}
  case class MulFrame(done:Regex) extends Regex {}

  case class StarFrame(x:Regex) extends Regex {}


  @scala.annotation.tailrec
  final def simplifyr(r : Regex, stack:List[(Regex => Regex)] = List((a => Final(a)): (Regex => Regex))):Regex = r match {
    case Final(x) => x
    case x if cache.contains(x) =>
      //println("cached...")
      simplifyr(stack.head(x), stack.tail)
    case x:Event => simplifyr(stack.head(x), stack.tail)
    case EmptySet => simplifyr(stack.head(EmptySet), stack.tail)
    case EmptyWord => simplifyr(stack.head(EmptyWord), stack.tail)

    case Star(x) => simplifyr(x, ((a => StarFrame(a)):(Regex => Regex)) :: stack )
    case StarFrame(EmptySet) => simplifyr(stack.head(EmptyWord), stack.tail)
    case StarFrame(EmptyWord) => simplifyr(stack.head(EmptyWord), stack.tail)
    case StarFrame(Event(x)) => simplifyr(stack.head(Event(x+"*")), stack.tail)
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
    case AndMergeFrame(x:Event, y:Event) => simplifyr(stack.head(Event(x + "," + y)), stack.tail)
    case AndMergeFrame(x, EmptySet) => simplifyr(stack.head(EmptySet), stack.tail)
    case AndMergeFrame(x, EmptyWord) => simplifyr(stack.head(x), stack.tail)
    case AndMergeFrame(EmptySet, y) => simplifyr(stack.head(EmptySet), stack.tail)
    case AndMergeFrame(EmptyWord, y) => simplifyr(stack.head(y), stack.tail)
    // a* . b => a*b
    case AndMergeFrame(Star(a:Event), Event(b)) => simplifyr(stack.head(Event(a + "*," + b)), stack.tail)
    case AndMergeFrame(Event(b), Star(a:Event)) => simplifyr(stack.head(Event(b + "," + a+"*")), stack.tail)

    // ((a+b) . (c+d)) = ( (ac + ad) + (bc + bd) )
    case AndMergeFrame(Or(a, b), Or(c, d)) =>
      simplifyr(Or(Or(Concat(a,c), Concat(a,d)), Or(Concat(b,c), Concat(b,d)) ),
        ((a => MulFrame(a)):(Regex => Regex)) :: stack )


    // c . (a + b) = ca + cb
    case AndMergeFrame(c, Or(a, b)) => simplifyr(Or(Concat(c, a), Concat(c, b)), ((a => MulFrame(a)):(Regex => Regex)) :: stack )
    // (a + b) . c = ac + bc
    case AndMergeFrame(Or(a, b), c) => simplifyr(Or(Concat(a, c), Concat(b, c)), ((a => MulFrame(a)):(Regex => Regex)) :: stack )
    case MulFrame(done) => simplifyr(stack.head(done), stack.tail)
    //case AndMergeFrame(c, Or(a, b)) => simplifyr(Concat(c, a), ((a => MulFrame(a, Concat(c,b) )):(Regex => Regex)) :: stack )

    //case AndMergeFrame(Or(a, b), c) => simplifyr(Concat(a, c), ((a => MulFrame(a, Concat(b, c) )):(Regex => Regex)) :: stack )
    //case MulFrame(done, y) => simplifyr(y, ((a => AndFrame(done, a)):(Regex => Regex)) :: stack )


    // anything else of AND
    case AndMergeFrame(x, y) =>
      cache + (Concat(x, y) -> true)
      simplifyr(stack.head(Concat(x, y)), stack.tail)

  }

  //   //star(ε)=ε, e.ε=e, ∅+e=e, ∅.e=∅
  def simplify(r : Regex):Regex = r match {
    case Or(x, EmptySet) => simplify(x)
    case Or(EmptySet, y) => simplify(y)
    case Or(EmptyWord, y) => simplify(y)
    case Or(x, EmptyWord) => simplify(x)
    case Or(Event(x), Event(y)) => if(x == y) Event(x) else Or(Event(x), Event(y))
    // added to present "niet verder splitbare ORs" as an event
    //   case Or(Event(x), Event(y)) => if(x == y) Event(x) else Event("(" + x + " + " + y + ")")
    //   case Or(Or(Event(x), Event(y)), Event(z)) => Event("(" + x + " + " + y + " + " + z + ")")
    //   case Or(Event(z), Or(Event(x), Event(y))) => Event("(" + z + " + " + x + " + " + y + ")")
    //   case Or(Or(x: Event, y: Event), Or(a: Event, b: Event)) => Event("(" + x + " + " + y + " + " + a + " + " + b + ")")

    //∅∗={ϵ}∅∗={ϵ}.
    case Star(EmptySet) => EmptyWord
    //ε* = ε
    case Star(EmptyWord) => EmptyWord
    //added to fix (1), ie a:Star to string -> Event(..), this rule removes the extra event
    case Star(Event(a)) => Event(a + "*")
    case Star(Or(EmptyWord, y)) => Star(y)
    case Star(Or(y, EmptyWord)) => Star(y)
    case Concat(x, EmptySet) => EmptySet
    case Concat(x, EmptyWord) => simplify(x)
    case Concat(EmptySet, y) => EmptySet
    case Concat(EmptyWord, y) => simplify(y)
    // (1) a* . b => a*b
    case Concat(Star(a:Event), Event(b)) => Event(a + "*," + b)
    // c* . (a + b) = c*a + c*b
    case Concat(c:Star, Or(a, b)) => simplify(Or(simplify(Concat(c, a)), simplify(Concat(c, b))))
    // (a + b) . c* = ac* + bc*
    case Concat(Or(a, b), c:Star) => simplify(Or(simplify(Concat(a, c)), simplify(Concat(b, c))))
    // a* . (a . b)
    //case Concat(c, Concat(a, b)) => simplify(Concat(simplify(Concat(c, a)), simplify(Concat(c, b))))
    //case Concat(Concat(a, b), c) => simplify(Concat(simplify(Concat(a, c)), simplify(Concat(b, c))))

    // ((a+b) . (c+d)) = ( (ac + ad) + (bc + bd) )
    case Concat(Or(a, b), Or(c, d)) =>
      simplify(Or(simplify(Or(simplify(Concat(a, c)), simplify(Concat(a, d))))
        , simplify(Or(simplify(Concat(b, c)), simplify(Concat(b, d))))))

    // x . (a + b) = x.a + x.b
    case Concat(x:Event, Or(a, b)) => simplify(Or(simplify(Concat(x, simplify(a))), simplify(Concat(simplify(x), b))))
    // (a + b) . x = a.x + b.x
    case Concat(Or(a, b), x:Event) => simplify(Or(simplify(Concat(simplify(a), x)), simplify(Concat(simplify(b), x))))
    case Concat(Event(x), Event(y)) => Event(x + "," + y)

    case _ =>
      //println("matched nothing")
      //if(r.toString().contains(".") || r.toString().contains("+") || r.toString().contains("(")) println(r)
      r
  }
}