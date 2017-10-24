object InputVariableStore {

  type Key = (SchemeExp, Int)
  private var keyStore: Map[SchemeExp, Int] = Map()
  private var store: Map[Key, ConcolicInput] = Map()

  private def makeNewKey(exp: SchemeExp): Key = {
    val id = keyStore.get(exp).map(_ + 1).getOrElse(0)
    keyStore = keyStore + (exp -> id)
    (exp, id)
  }

  def reset(): Unit = {
    keyStore = Map()
  }

  def addInput(input: ConcolicInput): Unit = {
    val newKey = makeNewKey(input.fexp)
    store = store + (newKey -> input)
  }

  def lookupInput(exp: SchemeExp): Option[ConcolicInput] = {
    val id = keyStore.getOrElse(exp, 0)
    val key = (exp, id)
    store.get(key).flatMap( (result) => {
      // If an input for the given key was found, increment the id
      makeNewKey(exp)
      Some(result)
    } )
  }

}
