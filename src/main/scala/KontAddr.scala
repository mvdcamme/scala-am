/**
  * The store used for continuations is a KontStore (defined in
  * machine/Kontinuation.scala). It is parameterized by continuation addresses, that
  * are element of the KontAddress typeclass.
  */
trait KontAddr
case class NormalKontAddress[Exp : Expression, Time : Timestamp](exp: Exp, time: Time) extends KontAddr {
  override def toString = s"NormalKontAddress($exp)"
}
object HaltKontAddress extends KontAddr {
  override def toString = "HaltKontAddress"
}

object KontAddr {
  implicit object KontAddrKontAddress extends KontAddress[KontAddr]
}
