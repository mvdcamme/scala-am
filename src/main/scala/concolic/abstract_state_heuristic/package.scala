import backend.PathConstraintWith
import backend.tree._

package object abstract_state_heuristic {

  type AbstractStatePCElement[State] = (Constraint, Boolean, Option[State])
  type PathConstraintWithState[State] = PathConstraintWith[AbstractStatePCElement[State]]

}
