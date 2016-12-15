trait AddressConverter[Address] {

  def convertAddress(address: Address): Address

}

object IdHybridAddressConverter extends AddressConverter[HybridAddress.A] {

  def convertAddress(address: HybridAddress.A): HybridAddress.A = address

}

class ConvertTimestampHybridAddressConverter[Exp: Expression, Time: Timestamp](
    timeSwitcher: TimestampConverter[Time])
    extends AddressConverter[HybridAddress.A] {

  def convertAddress(address: HybridAddress.A): HybridAddress.A =
    address match {
      case HybridAddress.HybridAddr(classicalAddress) =>
        classicalAddress match {
          case a: ClassicalAddress.CellAddress[Exp, Time] =>
            HybridAddress.HybridAddr(
              ClassicalAddress.CellAddress(a.exp,
                                           timeSwitcher.convertTimestamp(a.t)))
          case a: ClassicalAddress.PrimitiveAddress =>
            HybridAddress.HybridAddr(a)
          case a: ClassicalAddress.VariableAddress[Time] =>
            HybridAddress.HybridAddr(
              ClassicalAddress
                .VariableAddress(a.name, timeSwitcher.convertTimestamp(a.t)))
        }
      case HybridAddress.PrimitiveAddress(name) => address
    }

}

/*
 * To be used for converting hybrid-addresses: delegates to the proper conversion strategy.
 */
class DefaultHybridAddressConverter[Exp: Expression]
    extends AddressConverter[HybridAddress.A] {

  val timestampConverter = ConvertTimeStampConverter
  val addressConverter =
    new ConvertTimestampHybridAddressConverter[Exp, HybridTimestamp.T](
      timestampConverter)

  def convertAddress(address: HybridAddress.A): HybridAddress.A =
    addressConverter.convertAddress(address)

}
