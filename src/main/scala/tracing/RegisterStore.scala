object RegisterStore {

  val MAX_REGISTERS = 10

  val registers = new Array[ConcreteConcreteLattice.L](MAX_REGISTERS)

  def getRegister(index: Integer) = registers(index)

  def setRegister(index: Integer, value: ConcreteConcreteLattice.L) =
    registers(index) = value

}