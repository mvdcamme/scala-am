import backend.expression._
import backend.tree._

object SemanticsConcolicHelper {

  def handleIf(optConcolicExpression: Option[ConcolicExpression], thenBranchTaken: Boolean): Unit = {
    ScalaAMReporter.addToCurrentPath(thenBranchTaken)
    if (ScalaAMReporter.isConcolicEnabled) {
      optConcolicExpression match {
        case Some(b: BooleanConcolicExpression) =>
          val baseConstraint = BranchConstraint(b)
          ScalaAMReporter.addBranchConstraint(baseConstraint, thenBranchTaken)
        case Some(_) => Logger.log(s"Using a non-BooleanConcolicExpression in a branch constraint: $optConcolicExpression", Logger.E)
        case None =>
      }
    }
  }

}
