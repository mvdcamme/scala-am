trait TracableLattice[L] {

  def getValueType(value: L): SimpleTypes.Value
  def getValuesTypes(values: List[L]): SimpleTypes.Value

}
