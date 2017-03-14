import scalaz.Scalaz._

trait Frame {
  type AbstractValue //TODO remove?
  type Address
  type Timestamp //TODO remove?

  def meaningfullySubsumes: Boolean = false
  def subsumes(that: Frame): Boolean
  def writeEffectsFor(): Set[Address] = Set()

  def savesEnv: Option[Environment[Address]] = None
  def savedValues[Abs]: List[Abs] = Nil

//  def convert[OtherAbs: IsSchemeLattice](convertValue: (AbstractValue) => OtherAbs,
//                                          convertEnv: Environment[Address] => Environment[Address],
//                                          abstSem: BaseSchemeSemantics[OtherAbs, Address, Timestamp])
//  : Frame
}
trait KontAddress[A] {
  def descriptor: Descriptor[KontAddress[A]] = new BasicDescriptor[KontAddress[A]]
}

case class Kont[KontAddr: KontAddress](frame: Frame, next: KontAddr) {
  def subsumes(that: Kont[KontAddr]) = that match {
    case Kont(frame2, next2) => frame.subsumes(frame2) && next.equals(next2)
    case _ => false
  }
}

abstract class KontStore[KontAddr: KontAddress] {
  def lookup(a: KontAddr): Set[Kont[KontAddr]]
  def extend(a: KontAddr, kont: Kont[KontAddr]): KontStore[KontAddr]
  def join(that: KontStore[KontAddr]): KontStore[KontAddr]
  def forall(p: ((KontAddr, Set[Kont[KontAddr]])) => Boolean): Boolean
  def subsumes(that: KontStore[KontAddr]): Boolean
  def fastEq(that: KontStore[KontAddr]): Boolean = this == that

  /** Returns a KontStore containing items that differ between the two stores */
  def diff(that: KontStore[KontAddr]): KontStore[KontAddr]

  /**
    * Maps all frames AND all KontAddr variables, i.e., both the KontAddr keys as well as the KontAddr pointers to next
    * frames, in the store via the given function f.
    * @param f Maps one KontAddr value to another.
    * @param g Maps one Frame to another.
    * @return A new KontStore in which all KontAddr keys have been mapped.
    */
  def map[KAddr <: KontAddr : KontAddress](f: KontAddr => KAddr, g: Frame => Frame): KontStore[KAddr]

  /**
    * Removes the Kont value k associated with the KontAddr a.
    * @param a The KontAddr for which the Kont value must be removed.
    * @param k The Kont value to be removed.
    * @return A new KontStore in which the Kont value is removed.
    */
  def remove(a: KontAddr, k: Kont[KontAddr]): KontStore[KontAddr]

  /**
    * Removes all Kont values associated with the KontAddr a.
    * @param a The KontAddr for which the Kont value must be removed.
    * @return A new KontStore in which all Konts value that map to a have been removed.
    */
  def remove(a: KontAddr): KontStore[KontAddr]

  def descriptor: Descriptor[KontStore[KontAddr]] = new BasicDescriptor[KontStore[KontAddr]]
}

case class BasicKontStore[KontAddr: KontAddress](
    content: Map[KontAddr, Set[Kont[KontAddr]]])
    extends KontStore[KontAddr] {
  def lookup(a: KontAddr) = content.getOrElse(a, Set())
  override def toString = content.toString
  def extend(a: KontAddr, kont: Kont[KontAddr]) =
    /* Profiler.logRes(s"$this.extend($a, $kont)") */ {
      this.copy(content = content + (a -> (lookup(a) + kont)))
    } /* { x => x.toString } */
  def join(that: KontStore[KontAddr]) =
    /* Profiler.logRes(s"$this.join($that)") */ {
      if (that.isInstanceOf[BasicKontStore[KontAddr]]) {
        this.copy(
          content = content |+| that
              .asInstanceOf[BasicKontStore[KontAddr]]
              .content)
      } else {
        throw new Exception(
          s"Incompatible continuation stores: ${this.getClass.getSimpleName} and ${that.getClass.getSimpleName}")
      }
    } /* { x => x.toString } */
  def forall(p: ((KontAddr, Set[Kont[KontAddr]])) => Boolean) =
    content.forall(p)
  def subsumes(that: KontStore[KontAddr]) =
    that.forall({
      case (a, ks) =>
        ks.forall((k1) => lookup(a).exists(k2 => k2.subsumes(k1)))
    })

  def diff(that: KontStore[KontAddr]) =
    this.copy(content = content.filter({
      case (ka, konts) => that.lookup(ka) != konts
    }))

  def map[KAddr <: KontAddr : KontAddress](f: KontAddr => KAddr, g: Frame => Frame): KontStore[KAddr] = {
    content.foldLeft[KontStore[KAddr]](BasicKontStore[KAddr](Map()))( (newKstore, keyValue) => {
      val (ka, konts) = keyValue
      val convertedKA = f(ka)
      val convertedKonts = konts.map( (kont) => Kont(g(kont.frame), f(kont.next)) )
      convertedKonts.foldLeft(newKstore)( (newNewKStore, convertedKont) => newNewKStore.extend(convertedKA, convertedKont))
    })
  }

  def remove(a: KontAddr, k: Kont[KontAddr]): KontStore[KontAddr] =
    content.get(a) match {
      case Some(ks) =>
        val filteredKs = ks - k
        if (filteredKs.isEmpty) {
          BasicKontStore(content - a)
        } else {
          BasicKontStore(content + (a -> filteredKs))
        }
      case None => this
    }

  def remove(a: KontAddr): KontStore[KontAddr] = {
    this.copy(content = content - a)
  }

  override def descriptor = new BasicKontStoreDescriptor[KontAddr]
}

class BasicKontStoreDescriptor[KontAddr : KontAddress] extends Descriptor[BasicKontStore[KontAddr]] {

  val kontAddrDescriptor = implicitly[KontAddress[KontAddr]].descriptor

  private def describeKonts(konts: Set[Kont[KontAddr]]): String = {
    describeCollapsableList(konts, "Konts", (kont: Kont[KontAddr]) => {
      putIntoCollapsableList(List(kont.frame.toString, kontAddrDescriptor.describe(kont.next)), kont.toString)
    })
  }

  private def describeAddressKontTuple(tuple: (KontAddr, Set[Kont[KontAddr]])): String = tuple match {
    case (ka, konts) =>
      val kaDescription = kontAddrDescriptor.describe(ka)
      val kontsDescription = describeKonts(konts)
      val descriptions = List(kaDescription, kontsDescription)
      putIntoCollapsableList(descriptions, ka.toString)
  }

  override def describe[U >: BasicKontStore[KontAddr]](kstore: U): String = kstore match {
    case kstore: BasicKontStore[KontAddr] =>
      describeCollapsableList(kstore.content, "KontStore", describeAddressKontTuple, divClass = Some("kstore"))
    case _ =>
      kstore.toString
  }

}

case class TimestampedKontStore[KontAddr: KontAddress](
    content: Map[KontAddr, Set[Kont[KontAddr]]],
    timestamp: Int)
    extends KontStore[KontAddr] {
  def lookup(a: KontAddr) = content.getOrElse(a, Set())
  override def toString = content.toString
  def extend(a: KontAddr, kont: Kont[KontAddr]) =
    /* Profiler.logRes(s"$this.extend($a, $kont)") */ {
      content.get(a) match {
        case Some(konts) if konts.contains(kont) => this
        case Some(konts) => {
          // println(s"Joining $kont with $konts, increasing timestamp to ${timestamp + 1}")
          this.copy(content = content + (a -> (konts + kont)),
                    timestamp = timestamp + 1)
        }
        case None =>
          this.copy(content = content + (a -> Set(kont)),
                    timestamp = timestamp + 1)
      }
    } /* { x => x.toString } */
  def join(that: KontStore[KontAddr]) =
    /* Profiler.logRes(s"$this.join($that)") */ {
      if (that.isInstanceOf[TimestampedKontStore[KontAddr]]) {
        if (that
              .asInstanceOf[TimestampedKontStore[KontAddr]]
              .timestamp >= timestamp) {
          that
        } else {
          this
        }
      } else {
        throw new Exception(
          s"Incompatible continuation stores: ${this.getClass.getSimpleName} and ${that.getClass.getSimpleName}")
      }
    } /* { x => x.toString } */
  def forall(p: ((KontAddr, Set[Kont[KontAddr]])) => Boolean) =
    content.forall(p)
  def subsumes(that: KontStore[KontAddr]) =
    if (that.isInstanceOf[TimestampedKontStore[KontAddr]]) {
      timestamp >= that.asInstanceOf[TimestampedKontStore[KontAddr]].timestamp
    } else {
      that.forall({
        case (a, ks) =>
          ks.forall((k1) => lookup(a).exists(k2 => k2.subsumes(k1)))
      })
    }
  override def fastEq(that: KontStore[KontAddr]) =
    if (that.isInstanceOf[TimestampedKontStore[KontAddr]]) {
      timestamp == that.asInstanceOf[TimestampedKontStore[KontAddr]].timestamp
    } else {
      false
    }

  def diff(that: KontStore[KontAddr]) =
    this.copy(content = content.filter({
      case (ka, konts) => that.lookup(ka) != konts
    }))

  def map[KAddr <: KontAddr : KontAddress](f: KontAddr => KAddr, g: Frame => Frame): KontStore[KAddr] = {
    content.foldLeft[KontStore[KAddr]](TimestampedKontStore[KAddr](Map(), timestamp))( (newKstore, keyValue) => {
      val (ka, konts) = keyValue
      val convertedKA = f(ka)
      val convertedKonts = konts.map( (kont) => Kont(g(kont.frame), f(kont.next)) )
      convertedKonts.foldLeft(newKstore)( (newNewKStore, convertedKont) => newNewKStore.extend(convertedKA, convertedKont))
    })
  }

  def remove(a: KontAddr, k: Kont[KontAddr]): KontStore[KontAddr] =
    content.get(a) match {
      case Some(ks) =>
        val filteredKs = ks - k
        if (filteredKs.isEmpty) {
          TimestampedKontStore(content - a, timestamp)
        } else {
          TimestampedKontStore(content + (a -> filteredKs), timestamp)
        }
      case None => this
    }

  def remove(a: KontAddr): KontStore[KontAddr] = {
    this.copy(content = content - a)
  }
}

object KontStore {
  def empty[KontAddr: KontAddress]: KontStore[KontAddr] =
    new BasicKontStore[KontAddr](Map())
}
