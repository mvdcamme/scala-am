import backend.BaseReporter
import backend.expression.ConcolicIdGenerator
import backend.tree.{BranchConstraint, UnusableConstraint}

import abstract_state_heuristic._

class AbstractStateReporter[State] extends BaseReporter[AbstractStateSymbolicNode[State], PathConstraintWithState[State]] {

  protected var optRoot: Option[AbstractStateSymbolicNode[State]] = None

  def getRoot: Option[AbstractStateSymbolicNode[State]] = optRoot
  /**
    * Completely removes the symbolic tree that was already explored.
    */
  def deleteSymbolicTree(): Unit = {
    optRoot = None
  }

  def clear(): Unit = {
    ConcolicIdGenerator.resetId()
  }

  sealed trait SetChild {
    def parent: AbstractStateBranchSymbolicNode[State]
    def constraintWasTrue: Boolean
    def setChild(child: AbstractStateSymbolicNode[State]): Unit
    def setSafeChild(): Unit
    def setNoChild(): Unit = {
      setChild(AbstractStateRegularLeafNode[State]())
    }
  }
  case class SetChildElseBranch(parent: AbstractStateBranchSymbolicNode[State]) extends SetChild {
    def constraintWasTrue: Boolean = false
    def setChild(child: AbstractStateSymbolicNode[State]): Unit = {
      parent.setElseBranch(child)
    }
    def setSafeChild(): Unit = {
      assert(parent.elseBranch == AbstractStateUnexploredNode() || parent.elseBranch.isInstanceOf[AbstractStateSafeNode[State]], parent.elseBranch)
      setChild(AbstractStateSafeNode(parent.elseBranch))
    }
  }
  case class SetChildThenBranch(parent: AbstractStateBranchSymbolicNode[State]) extends SetChild {
    def constraintWasTrue: Boolean = true
    def setChild(child: AbstractStateSymbolicNode[State]): Unit = {
      parent.setThenBranch(child)
    }
    def setSafeChild(): Unit = {
      assert(parent.thenBranch == AbstractStateUnexploredNode() || parent.thenBranch.isInstanceOf[AbstractStateSafeNode[State]], parent.thenBranch)
      setChild(AbstractStateSafeNode(parent.thenBranch))
    }
  }
  def nodeToSetChild(node: AbstractStateBranchSymbolicNode[State], constraintWasTrue: Boolean): SetChild = {
    if (constraintWasTrue) {
      SetChildThenBranch(node)
    } else {
      SetChildElseBranch(node)
    }
  }

  protected def branchConstraintToAbstractStateNode(constraint: BranchConstraint, state: State): AbstractStateBranchSymbolicNode[State] = {
    AbstractStateBranchSymbolicNode[State](constraint, AbstractStateUnexploredNode[State](), AbstractStateUnexploredNode[State](), state)
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
      println(s"PATHCONSTRAINT ONLY CONTAINS UNUSABLE CONSTRAINTS")
      return
    } else if (optRoot.exists({ case AbstractStateSafeNode(_) => true; case _ => false })) {
      println(s"SYMBOLIC TREE ROOT IS A SAFE NODE")
      return
    }

    /*
     * Constraints contains at least one element, now find the first BranchConstraint in the list of constraints.
     * Will throw an error if there are no BranchConstraints in the list of constraints.
     */
    val (_, (headConstraint, headWasTrue, headState), remainder) = findFirstBranchConstraint(constraints).get

    // Make sure root exists.
    optRoot = Some(optRoot.getOrElse({ branchConstraintToAbstractStateNode(headConstraint, headState) }))

    @scala.annotation.tailrec
    def loop(currentConstraints: PathConstraintWithState[State], setChild: SetChild): Unit = currentConstraints.headOption match {
      case Some((constraint: BranchConstraint, currentConstraintWasTrue, state: State)) =>
        lazy val newNode = branchConstraintToAbstractStateNode(constraint, state)
        val childNode = if (setChild.constraintWasTrue) {
          setChild.parent.thenBranch
        } else {
          setChild.parent.elseBranch
        }
        val node = if (childNode == AbstractStateUnexploredNode[State]()) newNode else childNode
        setChild.setChild(node)

        node match {
          case node: AbstractStateBranchSymbolicNode[State] =>
            val setChild = nodeToSetChild(node, currentConstraintWasTrue)
            loop(currentConstraints.tail, setChild)
          case AbstractStateSafeNode(_) => /* Stop expansion of the symbolic tree */
          case AbstractStateRegularLeafNode() | AbstractStateUnexploredNode() | AbstractStateUnsatisfiableNode() => throw new Exception("Should not happen: node should not be an UnexploredNode")
        }
      case Some((UnusableConstraint, _, _)) => loop(currentConstraints.tail, setChild)
      case None => setChild.setNoChild()
    }

    val setChildOfRoot = nodeToSetChild(optRoot.get.asInstanceOf[AbstractStateBranchSymbolicNode[State]], headWasTrue)
    loop(remainder, setChildOfRoot)
  }
}
