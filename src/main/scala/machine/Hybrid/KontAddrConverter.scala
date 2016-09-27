trait KontAddrConverter {

  def convertKontAddr(k: KontAddr): KontAddr

}

object IdKontAddrConverter extends KontAddrConverter {

  def convertKontAddr(k: KontAddr): KontAddr = k

}

class ConvertTimestampKontAddrConverter[Exp : Expression, Time : Timestamp](timeSwitcher: TimestampConverter[Time]) extends KontAddrConverter {

  def convertKontAddr(k: KontAddr): KontAddr = k match {
    case a: NoExpKontAddress[Time] =>
      NoExpKontAddress(timeSwitcher.convertTimestamp(a.time))
    case a: NormalKontAddress[Exp, Time] =>
      NormalKontAddress(a.exp, timeSwitcher.convertTimestamp(a.time))
    case HaltKontAddress =>
      HaltKontAddress
  }

}


/*
 * To be used for converting continuation addresses: delegates to the proper conversion strategy.
 */
class DefaultKontAddrConverter[Exp : Expression] extends KontAddrConverter {

  val timestampConverter = IdHybridTimestampConverter
  val kontAddressConverter = new ConvertTimestampKontAddrConverter[Exp, HybridTimestamp.T](timestampConverter)

  def convertKontAddr(k: KontAddr): KontAddr = kontAddressConverter.convertKontAddr(k)

}