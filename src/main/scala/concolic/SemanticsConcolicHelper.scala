object SemanticsConcolicHelper {

  def handleIf(concolicExpression: ConcolicExpression, thenBranchTaken: Boolean): Unit = {
    if (Reporter.isConcolicEnabled) {
      concolicExpression match {
        case b: RelationalConcolicExpression =>
          val baseConstraint = BranchConstraint(b)
          ConcolicRunTimeFlags.setIfEncountered()
          Reporter.addBranchConstraint(baseConstraint, thenBranchTaken)
        case _ =>
          Logger.log(s"Using a non-BinaryConcolicExpression in a branch constraint: $concolicExpression", Logger.E)
      }
    }
  }

}
