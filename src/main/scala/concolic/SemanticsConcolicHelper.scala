import backend.expression._
import backend.tree._

object SemanticsConcolicHelper {

  def handleIf(optConcolicExpression: Option[ConcolicExpression], thenBranchTaken: Boolean, rTAnalysisStarter: RTAnalysisStarter): Unit = {
    ScalaAMReporter.pathStorage.addToCurrentPath(thenBranchTaken)
    if (ScalaAMReporter.isConcolicEnabled) {
      optConcolicExpression match {
        case Some(b: BooleanConcolicExpression) =>
          val baseConstraint = BranchConstraint(b)
          ScalaAMReporter.addBranchConstraint(baseConstraint, thenBranchTaken, rTAnalysisStarter)
        case Some(_) =>
          ScalaAMReporter.addUnusableConstraint(thenBranchTaken, rTAnalysisStarter)
          Logger.log(s"Using a non-BooleanConcolicExpression in a branch constraint: $optConcolicExpression", Logger.E)
        case None => ScalaAMReporter.addUnusableConstraint(thenBranchTaken, rTAnalysisStarter)

      }
    }
  }

}
