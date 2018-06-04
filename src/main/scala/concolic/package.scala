import scala.collection.mutable.{Map => MutMap}
import backend.expression.{ConcolicAddress, ConcolicExpression, ConcolicIdGenerator, ConcolicObject}

package object concolic {

  type SymbolicEnvironmentScope = MutMap[String, ConcolicExpression]
  type SymbolicEnvironment = List[SymbolicEnvironmentScope]

  private def newScope: SymbolicEnvironmentScope = MutMap()
  private def getCurrentScope(env: SymbolicEnvironment): SymbolicEnvironmentScope = env.head
  private def removeCurrentScope(env: SymbolicEnvironment): SymbolicEnvironment = env.tail
  private def addVariable(name: String, value: ConcolicExpression, scope: SymbolicEnvironmentScope): Unit = scope.update(name, value)
  private def updateVariable(name: String, value: ConcolicExpression, scope: SymbolicEnvironmentScope): Unit = addVariable(name, value, scope)

  def pushEnvironment(env: SymbolicEnvironment): SymbolicEnvironment = {
    newScope :: env
  }
  def popEnvironment(env: SymbolicEnvironment): SymbolicEnvironment = {
    removeCurrentScope(env)
  }

  def initialSymEnv: SymbolicEnvironment = pushEnvironment(Nil)

  def addVariable(originalName: String, concolicExpression: ConcolicExpression, env: SymbolicEnvironment): Unit = {
    val currentScope = getCurrentScope(env)
    addVariable(originalName, concolicExpression, currentScope)
  }

  def containsVariable(name: String, env: SymbolicEnvironment): Boolean = lookupVariable(name, env).isDefined
  def lookupVariable(name: String, env: SymbolicEnvironment): Option[ConcolicExpression] = {
    @scala.annotation.tailrec
    def loopEnv(env: SymbolicEnvironment): Option[ConcolicExpression] = env match {
      case scope :: rest => scope.get(name) match {
        case Some(concolicExp) => Some(concolicExp)
        case None => loopEnv(rest)
      }
      case Nil => None
    }
    loopEnv(env)
  }

  def setVariable(name: String, newConcolicExp: ConcolicExpression, env: SymbolicEnvironment): Unit = {
    @scala.annotation.tailrec
    def loopEnv(env: SymbolicEnvironment): Unit = env match {
      case scope :: rest =>
        if (scope.contains(name)) {
          updateVariable(name, newConcolicExp, scope)
        } else {
          loopEnv(rest)
        }
      case Nil => Nil
    }
    loopEnv(env)
  }

  case object ConcolicNil extends ConcolicExpression
  def addPair(symbolicCar: ConcolicExpression, symbolicCdr: ConcolicExpression): ConcolicAddress = {
    val fields = Map("car" -> symbolicCar, "cdr" -> symbolicCdr)
    val symbolicObject = ConcolicObject("cons", fields)
    val symbolicAddress = ConcolicIdGenerator.newConcolicAddress
    GlobalSymbolicStore.extendStore(symbolicAddress, symbolicObject)
    symbolicAddress
  }

}
