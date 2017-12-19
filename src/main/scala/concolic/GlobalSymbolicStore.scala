import backend.expression._

object GlobalSymbolicStore {

  type SymbolicStore = Map[ConcolicAddress, ConcolicObject]

  private var symbolicStore: SymbolicStore = Map()

  def reset(): Unit = {
    symbolicStore = Map()
  }

  def extendStore(address: ConcolicAddress, concolicObject: ConcolicObject): Unit = {
    symbolicStore += address -> concolicObject
  }
  def updateStore(address: ConcolicAddress, concolicObject: ConcolicObject): Unit = {
    extendStore(address, concolicObject)
  }

  def lookupAddress(address: ConcolicAddress): Option[ConcolicObject] = symbolicStore.get(address)

}
