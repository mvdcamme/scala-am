import backend.expression._
import backend.tree._

object SemanticsConcolicHelper {

  def handleIf(concolicExpression: ConcolicExpression, thenBranchTaken: Boolean): Unit = {
    if (ScalaAMReporter.isConcolicEnabled) {
      concolicExpression match {
        case b: BooleanConcolicExpression =>
          val baseConstraint = BranchConstraint(b)
          ScalaAMReporter.addBranchConstraint(baseConstraint, thenBranchTaken)
        case _ =>
          Logger.log(s"Using a non-BooleanConcolicExpression in a branch constraint: $concolicExpression", Logger.E)
      }
    }
  }

}
