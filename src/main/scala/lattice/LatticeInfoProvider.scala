trait LatticeInfoProvider[L] {
  def simpleType(x: L): SimpleTypes.Value
  def simpleTypes(xs: List[L]): SimpleTypes.Value = {
    xs.foldLeft(SimpleTypes.Bottom)({ (previousValuesTypes, value) =>
      if (previousValuesTypes == SimpleTypes.Bottom) {
        simpleType(value)
      } else if (previousValuesTypes == simpleType(value)) {
        previousValuesTypes
      } else {
        SimpleTypes.Top
      }
    })
  }
  def reaches[Addr: Address](x: L, reachesEnv: Environment[Addr] => Set[Addr], reachesAddress: Addr => Set[Addr]): Set[Addr]
}