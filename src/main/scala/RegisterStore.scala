object RegisterStore {

  val MAX_REGISTERS = 10

  val registers = new Array[HybridLattice.L](MAX_REGISTERS)

  def getRegister(index: Integer) = registers(index)

  def setRegister(index: Integer, value: HybridLattice.L) =
    registers(index) = value

}