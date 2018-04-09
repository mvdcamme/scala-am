import backend.BaseReporter
import backend.tree._

import abstract_state_heuristic._

class AbstractStateReporter[State] extends BaseReporter[State, PathConstraintWithState[State]] {

  protected var optRoot: Option[SymbolicNode[State]] = None

  def getRoot: Option[SymbolicNode[State]] = optRoot
  /**
    * Completely removes the symbolic tree that was already explored.
    */
  def deleteSymbolicTree(): Unit = {
    optRoot = None
  }

  /**
    * Finds the first BranchConstraint in the list of constraints, if there is one, and returns a triple of
    * all non-BranchConstraints before the BranchConstraints, the BranchConstraint itself, and all constraints
    * that follows this BranchConstraint.
    * @param constraints
    * @return
    */
  private def findFirstBranchConstraint(constraints: PathConstraintWithState[State]): Option[(PathConstraintWithState[State], (BranchConstraint, Boolean, State), PathConstraintWithState[State])] = {
    val (prefix, remainder) = constraints.span({
      case (BranchConstraint(_), _, _) => false
      case _ => true
    })
    remainder match {
      case Nil => None
      /* If remainder is not empty, its first element should be a tuple where the first field is a BranchConstraint */
      case ((headConstraint: BranchConstraint, headConstraintTrue, state: State)) :: rest => Some((prefix, (headConstraint, headConstraintTrue, state), rest))
    }
  }

  def addExploredPath(constraints: PathConstraintWithState[State]): Unit = {

    if (constraints.isEmpty) {
      return
    } else if (! constraints.exists(_._1 match {
      case _: BranchConstraint => true
      case _ => false
    })) {
      println("PATHCONSTRAINT ONLY CONTAINS UNUSABLE CONSTRAINTS")
      return
    } else if (optRoot.exists({ case SafeNode(_) => true; case _ => false })) {
      println("SYMBOLIC TREE ROOT IS A SAFE NODE")
      return
    }

    /*
     * Constraints contains at least one element, now find the first BranchConstraint in the list of constraints.
     * Will throw an error if there are no BranchConstraints in the list of constraints.
     */
    val (_, (headConstraint, headWasTrue, headState), remainder) = findFirstBranchConstraint(constraints).get

    // Make sure root exists.
    optRoot = Some(optRoot.getOrElse({ branchConstraintToNode(headConstraint, headState) }))

    @scala.annotation.tailrec
    def loop(currentConstraints: PathConstraintWithState[State], setChild: SetChild[State]): Unit = currentConstraints.headOption match {
      case Some((constraint: BranchConstraint, currentConstraintWasTrue, state: State)) =>
        lazy val newNode: SymbolicNode[State] = branchConstraintToNode(constraint, state)
        val childNode = if (setChild.constraintWasTrue) {
          setChild.parent.thenBranch
        } else {
          setChild.parent.elseBranch
        }
        val node = if (childNode == UnexploredNode[State]()) newNode else childNode
        setChild.setChild(node)

        node match {
          case node: BranchSymbolicNode[State] =>
            val setChild = nodeToSetChild(node, currentConstraintWasTrue)
            loop(currentConstraints.tail, setChild)
          case SafeNode(_) => /* Stop expansion of the symbolic tree */
          case RegularLeafNode() | UnexploredNode() | UnsatisfiableNode() => throw new Exception("Should not happen: node should not be an UnexploredNode")
        }
      case Some((UnusableConstraint, _, _)) => loop(currentConstraints.tail, setChild)
      case None => setChild.setNoChild()
    }

    val setChildOfRoot = nodeToSetChild(optRoot.get.asInstanceOf[BranchSymbolicNode[State]], headWasTrue)
    loop(remainder, setChildOfRoot)
  }
}
