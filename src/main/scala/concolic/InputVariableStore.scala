import backend.expression._

class InputVariableStore {

  private var doConcolic: Boolean = false
  def enableConcolic(): Unit = {
    doConcolic = true
  }
  def disableConcolic(): Unit = {
    doConcolic = false
  }
  def isConcolicEnabled: Boolean = doConcolic

  type Key = (SchemeExp, Int)

  private var inputs: List[(ConcolicInput, Int)] = Nil
  private var randomConsStore: List[ConcolicAddress] = Nil

  def reset(newInputs: List[(ConcolicInput, Int)]): Unit = {
    inputs = newInputs
    randomConsStore = Nil
  }

  def setInputs(newInputs: List[(ConcolicInput, Int)]): Unit = {
    inputs = newInputs
  }

  def setRandomConses(newRandomConsStore: List[ConcolicAddress]): Unit = {
    randomConsStore = newRandomConsStore
  }

  def getRandomCons(): Option[ConcolicAddress] = randomConsStore match {
    case Nil =>
      None
    case head :: rest =>
      randomConsStore = rest
      Some(head)
  }

  def getInput: Option[Int] = inputs match {
    case Nil => None
    case head :: rest =>
      inputs = rest
      Some(head._2)
  }

}
