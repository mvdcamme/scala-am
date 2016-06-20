trait TracableLattice[L] {

  def getValueType(value: L): SimpleTypes.Value
  def getValuesTypes(values: List[L]): SimpleTypes.Value

  def asPrimitive[Addr: Address](supposedPrimitive: L): Option[Primitive[L, Addr]]

}
