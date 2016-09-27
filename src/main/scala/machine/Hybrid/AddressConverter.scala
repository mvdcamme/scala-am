trait AddressConverter[Address] {

  def convertAddress(address: Address): Address

}

object IdHybridAddressConverter extends AddressConverter[HybridAddress.A] {

  def convertAddress(address: HybridAddress.A): HybridAddress.A = address

}

class ConvertTimeStampHybridAddressConverter[Time : Timestamp](timeSwitcher: TimestampConverter[Time])
  extends AddressConverter[HybridAddress.A] {

  def convertAddress(address: HybridAddress.A): HybridAddress.A = address match {
    case HybridAddress.HybridAddr(classicalAddress) => classicalAddress match {
      case a: ClassicalAddress.CellAddress[Any, Time] =>
        HybridAddress.HybridAddr(ClassicalAddress.CellAddress(a.exp, timeSwitcher.convertTimestamp(a.t)))
      case a: ClassicalAddress.PrimitiveAddress =>
        HybridAddress.HybridAddr(a)
      case a: ClassicalAddress.VariableAddress[Time] =>
        HybridAddress.HybridAddr(ClassicalAddress.VariableAddress(a.name, timeSwitcher.convertTimestamp(a.t)))
    }
  }

}
