import backend.expression._

object GlobalSymbolicEnvironment {

  type SymbolicEnvironmentScope = Map[String, ConcolicExpression]
  type SymbolicEnvironment = List[SymbolicEnvironmentScope]

  private var symbolicEnvironment: SymbolicEnvironment = List(Map())

  def reset(): Unit = {
    symbolicEnvironment = List(Map())
  }

  def pushEnvironment(): Unit = {
    if (ScalaAMReporter.isConcolicEnabled) {
      symbolicEnvironment ::= Map()
    }
  }
  def popEnvironment(): Unit = {
    if (ScalaAMReporter.isConcolicEnabled) {
      symbolicEnvironment = symbolicEnvironment.tail
    }
  }

  def addVariable(originalName: String, concolicExpression: ConcolicExpression): Unit = {
    val updatedCurrentScope: SymbolicEnvironmentScope = symbolicEnvironment.head + (originalName -> concolicExpression)
    symbolicEnvironment = updatedCurrentScope :: symbolicEnvironment.tail
  }

  def lookupVariable(name: String): Option[ConcolicExpression] = {
    def loopEnv(env: SymbolicEnvironment): Option[ConcolicExpression] = env match {
      case scope :: rest => scope.get(name) match {
        case Some(concolicExp) =>
          Some(concolicExp)
        case None =>
          loopEnv(rest)
      }
      case Nil =>
        None
    }
    loopEnv(symbolicEnvironment)
  }

  def setVariable(name: String, newConcolicExp: ConcolicExpression): Unit = {
    def loopEnv(env: SymbolicEnvironment): SymbolicEnvironment = env match {
      case scope :: rest =>
        if (scope.contains(name)) {
          scope.updated(name, newConcolicExp) :: rest
        } else {
          scope :: loopEnv(rest)
        }
      case Nil =>
        Nil
    }
    val newSymbolicEnvironment = loopEnv(symbolicEnvironment)
    symbolicEnvironment = newSymbolicEnvironment
  }

}
