import backend.expression._
import backend.tree._

class SemanticsConcolicHelper(private val rtAnalysisStarter: RTAnalysisStarter, reporter: ScalaAMReporter) {

  def handleIf(optConcolicExpression: Option[ConcolicExpression], thenBranchTaken: Boolean): Unit = {
    reporter.pathStorage.updateCurrentPath(thenBranchTaken)
    if (reporter.isConcolicEnabled) {
      optConcolicExpression match {
        case Some(b: BooleanConcolicExpression) =>
          val baseConstraint = BranchConstraint(b)
          reporter.addBranchConstraint(baseConstraint, thenBranchTaken, rtAnalysisStarter)
        case Some(_) =>
          reporter.addUnusableConstraint(thenBranchTaken, rtAnalysisStarter)
          Logger.log(s"Using a non-BooleanConcolicExpression in a branch constraint: $optConcolicExpression", Logger.E)
        case None => reporter.addUnusableConstraint(thenBranchTaken, rtAnalysisStarter)

      }
    }
  }

}
