import backend._

object SemanticsConcolicHelper {

  def handleIf(concolicExpression: ConcolicExpression, thenBranchTaken: Boolean): Unit = {
    if (ScalaAMReporter.isConcolicEnabled) {
      concolicExpression match {
        case b: RelationalConcolicExpression =>
          val baseConstraint = BranchConstraint(b)
          ScalaAMReporter.addBranchConstraint(baseConstraint, thenBranchTaken)
        case _ =>
          Logger.log(s"Using a non-RelationalConcolicExpression in a branch constraint: $concolicExpression", Logger.E)
      }
    }
  }

}
