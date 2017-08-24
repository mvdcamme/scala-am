object Reporter {
  type PathConstraint = List[ConcolicConstraint]

  type SymbolicMemoryScope = Map[String, String]
  type SymbolicEnvironment = List[SymbolicMemoryScope]

  private var symbolicMemory: SymbolicEnvironment = List(Map())
  private var report: PathConstraint = Nil

  private var doConcolic: Boolean = false

  def enableConcolic(): Unit = {
    doConcolic = true
  }

  def disableConcolic(): Unit = {
    doConcolic = false
  }

  def clear(): Unit = {
    report = Nil
    symbolicMemory = List(Map())
  }

  def pushEnvironment(): Unit = {
    if (doConcolic) {
      symbolicMemory ::= Map()
    }
  }

  def popEnvironment(): Unit = {
    if (doConcolic) {
      symbolicMemory = symbolicMemory.tail
    }
  }

  private def addVariable(originalName: String, symbolicVariable: String): Unit = {
    val updatedCurrentScope: SymbolicMemoryScope = symbolicMemory.head + (originalName -> symbolicVariable)
    symbolicMemory = updatedCurrentScope :: symbolicMemory.tail
  }

  def addConstraint(exp: ConcolicConstraint): Unit = {
    if (doConcolic) {
      exp match {
        case bc: BranchConstraint =>
          report :+= bc
        case sc: StatementConstraint =>
          addVariable(sc.originalName, sc.symbolicVariable)
          report :+= sc
      }
    }
  }

  def getReport: PathConstraint = {
    report
  }

  def printReports(): Unit = {
    report.foreach( (constraint: ConcolicConstraint) => {
      println(constraint.toString)
    })
  }

  def lookupVariable(name: String): Option[String] = {
    def loopEnv(env: SymbolicEnvironment): Option[String] = env match {
      case scope :: rest => scope.get(name) match {
        case Some(symVar) =>
          Some(symVar)
        case None =>
          loopEnv(rest)
      }
      case Nil =>
        None
    }
    loopEnv(symbolicMemory)
  }

}
