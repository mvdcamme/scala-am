import backend.expression._
import backend.tree._

class SemanticsConcolicHelper[PCElementUsed, RTInitialState](private val rtAnalysisStarter: RTAnalysisStarter[RTInitialState], reporter: ScalaAMReporter[PCElementUsed, _]) {

  def handleIf(optConcolicExpression: Option[ConcolicExpression], thenBranchTaken: Boolean): Unit = {
    reporter.pathStorage.updateCurrentPath(thenBranchTaken)
    optConcolicExpression match {
      case Some(b: BooleanConcolicExpression) =>
        val baseConstraint = BranchConstraint(b)
        reporter.addBranchConstraint(baseConstraint, thenBranchTaken, rtAnalysisStarter)
      case Some(_) =>
        reporter.addUnusableConstraint(thenBranchTaken)
        Logger.log(s"Using a non-BooleanConcolicExpression in a branch constraint: $optConcolicExpression", Logger.E)
      case None => reporter.addUnusableConstraint(thenBranchTaken)

    }
  }

}
