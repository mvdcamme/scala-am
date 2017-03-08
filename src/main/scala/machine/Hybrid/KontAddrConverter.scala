trait KontAddrConverter[KAddr] {

  def convertKontAddr(k: KAddr): KAddr

}

class IdKontAddrConverter[KAddr] extends KontAddrConverter[KAddr] {

  def convertKontAddr(k: KAddr): KAddr = k

}

class ConvertTimestampKontAddrConverter[Exp : Expression](timeSwitcher: TimestampConverter[HybridTimestamp.T])
  extends KontAddrConverter[KontAddr] {

  /* TODO Should really make a type class for this so we have a specific conversion for each possible kont addr type */
  def convertKontAddr(ka: KontAddr): KontAddr = ka match {
    case ka: NormalKontAddress[Exp, HybridTimestamp.T] =>
      NormalKontAddress(ka.exp, timeSwitcher.convertTimestamp(ka.time))
    case ka: NoExpKontAddress[HybridTimestamp.T] =>
      NoExpKontAddress(timeSwitcher.convertTimestamp(ka.time))
    case ka: FreeNormalKontAddress[Exp, _] =>
      ka
    case HaltKontAddress =>
      HaltKontAddress
    case FreeHaltKontAddress =>
      FreeHaltKontAddress
  }

}

/*
 * To be used for converting continuation addresses: delegates to the proper conversion strategy.
 */
class DefaultKontAddrConverter[Exp: Expression] extends KontAddrConverter[KontAddr] {

  val timestampConverter = IdHybridTimestampConverter
  val kontAddressConverter = new IdKontAddrConverter[KontAddr] // new ConvertTimestampKontAddrConverter[Exp](ConvertTimeStampConverter)

  def convertKontAddr(k: KontAddr): KontAddr =
    kontAddressConverter.convertKontAddr(k)

}
