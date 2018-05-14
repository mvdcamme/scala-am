/**
  * The store used for continuations is a KontStore (defined in
  * machine/Kontinuation.scala). It is parameterized by continuation addresses, that
  * are element of the KontAddress typeclass.
  */
trait KontAddr
case class NormalKontAddress[Exp: Expression, Time: Timestamp](exp: Exp, time: Time) extends KontAddr {
  override def toString = s"NormalKontAddress($exp, $time)"
}
case class NoExpKontAddress[Time: Timestamp](time: Time) extends KontAddr
case object HaltKontAddress extends KontAddr {
  override def toString = "HaltKontAddress"
}

object KontAddr {
  implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  def equalModuloTimestamp[T: CompareTimestampsWithMapping](ka1: KontAddr, ka2: KontAddr, mapping: Mapping[T]): Option[Mapping[T]] = (ka1, ka2) match {
    case (HaltKontAddress, HaltKontAddress) => Some(mapping)
    case (NormalKontAddress(exp1, t1: T), NormalKontAddress(exp2, t2: T)) if exp1 == exp2 =>
      implicitly[CompareTimestampsWithMapping[T]].compareWithMapping(t1, t2, mapping)
    case (NoExpKontAddress(t1: T), NoExpKontAddress(t2: T)) =>
      implicitly[CompareTimestampsWithMapping[T]].compareWithMapping(t1, t2, mapping)
    case _ => None
  }
}
