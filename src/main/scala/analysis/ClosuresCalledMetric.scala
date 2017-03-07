object ClosuresCalledMetric {

  private var nrOfConcreteClosuresCalled = 0

  def getConcreteClosuresCalled(): Int =
    nrOfConcreteClosuresCalled
  def incConcreteClosuresCalled(): Unit = {
    nrOfConcreteClosuresCalled += 1
  }
  def resetConcreteClosuresCalled(): Unit = {
    nrOfConcreteClosuresCalled = 0
  }

}
