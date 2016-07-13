trait Timestamp[T] {
  def name: String
  def initial(seed: String): T
  def tick(t: T): T
  def tick[Exp](t: T, e: Exp): T
}

trait TimestampWrapper {
  type T
  val isTimestamp: Timestamp[T]
}

case class KCFA(k: Int) extends TimestampWrapper {
  trait T
  case class Time[Exp](seed: String, history: List[Exp]) extends T

  implicit val isTimestamp = new Timestamp[T] {
    def name = "$k-CFA"
    def initial(seed: String) = Time(seed, List())
    def tick(t: T) = t
    def tick[Exp](t: T, e: Exp) = t match {
      case (t : Time[Exp] @unchecked) => Time[Exp](t.seed, (e :: t.history).take(k))
    }
  }
}

/* Similar to KCFA(0), but doesn't include the empty list */
object ZeroCFA extends TimestampWrapper {
  trait T
  case class Time(seed: String) extends T
  implicit val isTimestamp = new Timestamp[T] {
    def name = "0-CFA"
    def initial(seed: String) = Time(seed)
    def tick(t: T) = t
    def tick[Exp](t: T, e: Exp) = t
  }
}

object ConcreteTimestamp extends TimestampWrapper {
  trait T
  case class Time(seed: String, n: Int) extends T {
    override def toString = if (seed.isEmpty) { n.toString } else { s"$seed-$n" }
  }
  implicit val isTimestamp = new Timestamp[T] {
    def name = "Concrete"
    def initial(seed: String) = Time(seed, 0)
    def tick(t: T) = t match {
      case Time(seed, n) => Time(seed, n+1)
    }
    def tick[Exp](t: T, e: Exp) = tick(t)
  }
}

object HybridTimestamp extends TimestampWrapper {
  trait T

  var useConcrete = true

  def switchToAbstract() = {
    useConcrete = false
  }

  def switchToConcrete() = {
    useConcrete = true
  }

  val abstractT = ZeroCFA
  val concreteT = ConcreteTimestamp

  type AbstractT = abstractT.T
  type ConcreteT = concreteT.T

  case class AbstractTime(a: AbstractT) extends T {
    override def equals(that: Any): Boolean = that match {
      case v: AbstractTime =>
        a == v.a
      case _ => super.equals(that)
    }

    override def hashCode() = a.hashCode()
  }
  case class ConcreteTime(c: ConcreteT, a: AbstractT) extends T

  def convertTime(time: T): T = time match {
    case AbstractTime(a) => time
    case ConcreteTime(c, a) => AbstractTime(a)
  }

  implicit val isTimestamp = new Timestamp[T] {
    def name = "Hybrid"
    def initial(seed: String) =
      if (useConcrete) {
        ConcreteTime(concreteT.isTimestamp.initial(seed), abstractT.isTimestamp.initial(seed))
      } else {
        AbstractTime(abstractT.isTimestamp.initial(seed))
      }
    def tick(t: T) = t match {
      case AbstractTime(a) => AbstractTime(abstractT.isTimestamp.tick(a))
      case ConcreteTime(c, a) => ConcreteTime(concreteT.isTimestamp.tick(c), abstractT.isTimestamp.tick(a))
    }
    def tick[Exp](t: T, e: Exp) = t match {
      case AbstractTime(a) => AbstractTime(abstractT.isTimestamp.tick(a, e))
      case ConcreteTime(c, a) => ConcreteTime(concreteT.isTimestamp.tick(c, e), abstractT.isTimestamp.tick(a, e))
    }
  }
}
