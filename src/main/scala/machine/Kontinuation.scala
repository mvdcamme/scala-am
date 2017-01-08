import scalaz.Scalaz._

trait Frame {
  type AbstractValue
  type Address
  type Timestamp

  def meaningfullySubsumes: Boolean = false
  def subsumes(that: Frame): Boolean
  def writeEffectsFor(): Set[Address] = Set()

  def savesEnv: Option[Environment[Address]] = None

//  def convert[OtherAbs: IsSchemeLattice](convertValue: (AbstractValue) => OtherAbs,
//                                          convertEnv: Environment[Address] => Environment[Address],
//                                          abstSem: BaseSchemeSemantics[OtherAbs, Address, Timestamp])
//  : Frame
}
trait KontAddress[A]

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

  def map[KAddr <: KontAddr : KontAddress](f: KontAddr => KAddr, g: Frame => Frame): KontStore[KAddr] = {
    val mappedContent = content.foldLeft[Map[KAddr, Set[Kont[KAddr]]]](
      Map[KAddr, Set[Kont[KAddr]]]())({
      case (kstore, (a, ks)) =>
        kstore + (f(a) -> ks.map(kont =>
          kont.copy(frame = g(kont.frame), next = f(kont.next))))
    })
    BasicKontStore[KAddr](mappedContent)
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

  override def descriptor = new BasicKontStoreDescriptor[KontAddr]
}

class BasicKontStoreDescriptor[KontAddr] extends Descriptor[BasicKontStore[KontAddr]] {
  override def describe[U >: BasicKontStore[KontAddr]](kstore: U): String = kstore match {
    case kstore: BasicKontStore[KontAddr] =>
      describeCollapsableList(kstore.content, "KontStore", divClass = Some("kstore"))
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

  def map[KAddr <: KontAddr : KontAddress](f: KontAddr => KAddr, g: Frame => Frame): KontStore[KAddr] = {
    val mappedContent = content.foldLeft[Map[KAddr, Set[Kont[KAddr]]]](
      Map[KAddr, Set[Kont[KAddr]]]())({
      case (kstore, (a, ks)) =>
        kstore + (f(a) -> ks.map(kont =>
          kont.copy(frame = g(kont.frame), next = f(kont.next))))
    })
    TimestampedKontStore[KAddr](mappedContent, timestamp)
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
}

object KontStore {
  def empty[KontAddr: KontAddress]: KontStore[KontAddr] =
    new BasicKontStore[KontAddr](Map())
}
