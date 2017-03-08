object FunctionsCalledMetric {

  private var nrOfConcreteFunctionsCalled = 0

  def getConcreteFunctionsCalled: Int =
    nrOfConcreteFunctionsCalled

  def incConcreteFunctionsCalled(): Unit = {
    nrOfConcreteFunctionsCalled += 1
  }

  def resetConcreteFunctionsCalled(): Unit = {
    nrOfConcreteFunctionsCalled = 0
  }

}
