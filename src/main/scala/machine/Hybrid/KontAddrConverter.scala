trait KontAddrConverter[KAddr] {

  def convertKontAddr(k: KAddr): KAddr

}

class IdKontAddrConverter[KAddr <: KontAddr] extends KontAddrConverter[KAddr] {

  def convertKontAddr(k: KAddr): KAddr = k

}

class ConvertTimestampKontAddrConverter[Exp : Expression, Time : Timestamp](timeSwitcher: TimestampConverter[Time])
  extends KontAddrConverter[KontAddr] {

  def convertKontAddr(k: KontAddr): KontAddr = k match {
    case k: NormalKontAddress[Exp, Time] =>
      NormalKontAddress(k.exp, timeSwitcher.convertTimestamp(k.time))
    case k: NoExpKontAddress[Time] =>
      NoExpKontAddress(timeSwitcher.convertTimestamp(k.time))
    case HaltKontAddress =>
      HaltKontAddress
  }

}

/*
 * To be used for converting continuation addresses: delegates to the proper conversion strategy.
 */
class DefaultKontAddrConverter[Exp: Expression, KAddr <: KontAddr] extends KontAddrConverter[KAddr] {

  val timestampConverter = IdHybridTimestampConverter
  val kontAddressConverter = new IdKontAddrConverter[KAddr]

  def convertKontAddr(k: KAddr): KAddr =
    kontAddressConverter.convertKontAddr(k)

}
