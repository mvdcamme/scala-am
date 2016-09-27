trait KontAddrConverter {

  def convertKontAddr(k: KontAddr): KontAddr

}

object IdKontAddrConverter extends KontAddrConverter {

  def convertKontAddr(k: KontAddr): KontAddr = k

}

class ConvertTimestampKontAddrConverter[Time : Timestamp](timeSwitcher: TimestampConverter[Time]) extends KontAddrConverter {

  def convertKontAddr(k: KontAddr): KontAddr = k match {
    case a: NoExpKontAddress[Time] =>
      NoExpKontAddress(timeSwitcher.convertTimestamp(a.time))
    case a: NormalKontAddress[Any, Time] =>
      NormalKontAddress(a.exp, timeSwitcher.convertTimestamp(a.time))
    case HaltKontAddress =>
      HaltKontAddress
  }

}