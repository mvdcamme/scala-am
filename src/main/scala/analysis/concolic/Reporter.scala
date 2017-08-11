object Reporter {
  type PathConstraint = List[ConcolicConstraint]

  private var symbolicMemory: Map[String, String] = Map()
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
    symbolicMemory = Map()
  }

  def addConstraint(exp: ConcolicConstraint): Unit = {
    if (doConcolic) {
      exp match {
        case bc: BranchConstraint =>
          report :+= bc
        case sc: StatementConstraint =>
          symbolicMemory += sc.originalName -> sc.symbolicVariable
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
    symbolicMemory.get(name)
  }

  def addVariable(name: String): Unit = {

  }

}
