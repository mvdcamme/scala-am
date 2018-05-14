trait Timestamp[T] {
  def name: String
  def initial(seed: String): T
  def tick(t: T): T
  def tick[Exp](t: T, e: Exp): T
}

object Timestamp {
  def apply[T : Timestamp]: Timestamp[T] = implicitly
}

trait TimestampWrapper {
  type T
  val isTimestamp: Timestamp[T]
}

trait CompareTimestampsWithMapping[T] {
  def compareWithMapping(t1: T, t2: T, mapping: Mapping[T]): Option[Mapping[T]]
}

case class KCFA(k: Int) extends TimestampWrapper {
  trait T
  case class Time[Exp](seed: String, history: List[Exp]) extends T

  implicit val isTimestamp = new Timestamp[T] {
    def name = "$k-CFA"
    def initial(seed: String) = Time(seed, List())
    def tick(t: T) = t
    def tick[Exp](t: T, e: Exp) = t match {
      case (t: Time[Exp] @unchecked) =>
        Time[Exp](t.seed, (e :: t.history).take(k))
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
    override def toString =
      if (seed.isEmpty) { n.toString } else { s"$seed-$n" }
  }
  implicit val isTimestamp = new Timestamp[T] {
    def name = "Concrete"
    def initial(seed: String) = Time(seed, 0)
    def tick(t: T) = t match {
      case Time(seed, n) => Time(seed, n + 1)
    }
    def tick[Exp](t: T, e: Exp) = tick(t)
  }
}

case class Mapping[T](fromTo: Map[T, T], toFrom: Map[T, T]) {
  /*
   * Either the timestamps exist in both maps AND they mapped timestamps actually equal the given timestamps, or
   * both timestamps have not yet been added to the maps.
   */
  def mightEqual(t1: T, t2: T): Boolean = (fromTo.get(t1), toFrom.get(t2)) match {
    case (Some(mappedT1), Some(mappedT2)) => t2 == mappedT1 && t1 == mappedT2
    case (None, None) => true
    case _ => false
  }
  def extend(t1: T, t2: T): Mapping[T] = Mapping(fromTo + (t1 -> t2), toFrom + (t2 -> t1))
}
object Mapping {
  def initial[T]: Mapping[T] = Mapping(Map(), Map())
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

  case class AbstractTime(a: AbstractT) extends T
  case class ConcreteTime(c: ConcreteT, a: AbstractT) extends T

  implicit val isTimestamp = new Timestamp[T] with CompareTimestampsWithMapping[T] {
    def name = "Hybrid"
    def initial(seed: String) =
      if (useConcrete) {
        ConcreteTime(concreteT.isTimestamp.initial(seed), abstractT.isTimestamp.initial(seed))
      } else {
        AbstractTime(abstractT.isTimestamp.initial(seed))
      }
    def tick(t: T) = t match {
      case AbstractTime(a) => AbstractTime(abstractT.isTimestamp.tick(a))
      case ConcreteTime(c, a) =>
        ConcreteTime(concreteT.isTimestamp.tick(c), abstractT.isTimestamp.tick(a))
    }
    def tick[Exp](t: T, e: Exp) = t match {
      case AbstractTime(a) => AbstractTime(abstractT.isTimestamp.tick(a, e))
      case ConcreteTime(c, a) =>
        ConcreteTime(concreteT.isTimestamp.tick(c, e), abstractT.isTimestamp.tick(a, e))
    }

    /**
      * Compares two timestamps and checks whether they are equal (or actually, if they might be equal) modulo the
      * (arbitrary) ids of the concrete timestamps. Both timestamps should be concrete timestamps. If they are not,
      * the function automatically assumes the timestamps are not equal. If the timestamps might be equal, the function
      * returns an updated map, mapping time1 to time2.
      * @param time1
      * @param time2
      * @param mapping The mapping that maps the concrete ids of one timestamp to the concrete ids of the other timestamp.
      * @return None if the two timestamps cannot be equal, or if either or both timestamps are abstract. Returns the
      *         wrapped, updated mapping if the two timestamps can be equal.
      */
    def compareWithMapping(time1: T, time2: T, mapping: Mapping[T]): Option[Mapping[T]] = (time1, time2) match {
      case (c1: ConcreteTime, c2: ConcreteTime) if mapping.mightEqual(c1, c2) => Some(mapping.extend(c1, c2))
      case _ => None
    }
  }
}
