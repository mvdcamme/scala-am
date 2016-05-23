object RegisterStore {

  val MAX_REGISTERS = 10

  val registers = new Array[HybridLattice.Hybrid](MAX_REGISTERS)

  def getRegister(index: Integer) = registers(index)

  def setRegister(index: Integer, value: HybridLattice.Hybrid) =
    registers(index) = value

}