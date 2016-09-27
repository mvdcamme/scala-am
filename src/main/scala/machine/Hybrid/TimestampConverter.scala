trait TimestampConverter[Time] {

  def convertTimestamp(time: Time): Time

}

object IdHybridTimestampConverter extends TimestampConverter[HybridTimestamp.T] {

  def convertTimestamp(time: HybridTimestamp.T): HybridTimestamp.T = time

}

object ConvertTimeStampConverter extends TimestampConverter[HybridTimestamp.T] {

  def convertTimestamp(time: HybridTimestamp.T): HybridTimestamp.T = time match {
    case HybridTimestamp.AbstractTime(_) =>
      time
    case HybridTimestamp.ConcreteTime(_, a) =>
      HybridTimestamp.AbstractTime(a)
  }

}
