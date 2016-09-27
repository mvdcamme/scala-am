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

/*
 * To be used for converting hybrid-timestamps: delegates to the proper conversion strategy.
 */
object DefaultHybridTimestampConverter extends TimestampConverter[HybridTimestamp.T] {

  def convertTimestamp(time: HybridTimestamp.T): HybridTimestamp.T = ConvertTimeStampConverter.convertTimestamp(time)

}
