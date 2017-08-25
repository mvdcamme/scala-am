import SymbolicTreeHelper.TreePath

object Reporter {
  type PathConstraint = List[ConcolicConstraint]

  type SymbolicMemoryScope = Map[String, String]
  type SymbolicEnvironment = List[SymbolicMemoryScope]

  private var doConcolic: Boolean = false

  private var symbolicMemory: SymbolicEnvironment = List(Map())
  private var currentReport: PathConstraint = Nil

  private var optRoot: Option[SymbolicNode] = None
  private var optCurrentNode: Option[SymbolicNode] = None


  type Setter = (SymbolicNode) => Unit

  private def setRoot(symbolicNode: SymbolicNode): Unit = {
    optRoot = Some(symbolicNode)
    optCurrentNode = optRoot
  }
  private var integrateNode: Setter = setRoot

  private def generateStatementNodeSetter: Setter = optCurrentNode match {
    case Some(currentNode) => currentNode match {
      case s: StatementSymbolicNode =>
        (symNode: SymbolicNode) => {
          // Sanity check: if s.followUp is already defined, ONLY set the optCurrentNode variable?
//          s.followUp match {
//            case Some(followUp) =>
//              if (followUp != symNode) {
//                println("error")
//                assert(false)
//              }
//            case None =>
//          }
          s.followUp match {
            case Some(followUp) =>
              s.followUp = Some(followUp.combine(symNode))
            case None =>
              s.followUp = Some(symNode)
          }
          // TODO if s.followUp.isDefined, should try to _combine_ two nodes?
          // if symNode is a BranchNode, with only else-branch taken, and s.followUp is the same BranchNode (from previous iteration)
          // with only the else-branch taken, s.followUp should now refer to a BranchNode with both branches taken?
          optCurrentNode =  s.followUp
        }
    }
  }
  private def generateBranchNodeSetter(thenBranchTaken: Boolean): Setter = optCurrentNode match {
    case Some(currentNode) => currentNode match {
      case b: BranchSymbolicNode =>
        (symNode: SymbolicNode) => {
          if (thenBranchTaken) {
            // Sanity check: if b.thenBranch is already defined, ONLY set the optCurrentNode variable?
//            assert(!b.thenBranchTaken && b.thenBranch.isEmpty)

            b.thenBranchTaken = true
            b.thenBranch match {
              case Some(thenBranch) =>
                val combinedNode = thenBranch.combine(symNode)
                b.thenBranch = Some(combinedNode)
              case None =>
                b.thenBranch = Some(symNode)
            }
            optCurrentNode = b.thenBranch
          } else {
            // Sanity check: if b.elseBranchTaken is already defined, ONLY set the optCurrentNode variable?
//            assert(!b.elseBranchTaken && b.elseBranch.isEmpty)

            b.elseBranchTaken = true
            b.elseBranch match {
              case Some(elseBranch) =>
                val combinedNode = elseBranch.combine(symNode)
                b.elseBranch = Some(combinedNode)
              case None =>
                b.elseBranch = Some(symNode)
            }
            optCurrentNode = b.elseBranch
          }
        }
    }
  }


  def enableConcolic(): Unit = {
    doConcolic = true
  }
  def disableConcolic(): Unit = {
    doConcolic = false
  }
  def isConcolicEnabled: Boolean = doConcolic
  def clear(isFirstClear: Boolean): Unit = {
    ConcolicIdGenerator.resetId()
    currentReport = Nil
    optCurrentNode = optRoot
    if (!isFirstClear) {
      // Make integrateNode a no-op: currentNode has already been set, and root doesn't change, so nothing needs to be done (correct???)
      integrateNode = (_) => {}
    }
    symbolicMemory = List(Map())
  }
  def pushEnvironment(): Unit = {
    if (doConcolic) {
      symbolicMemory ::= Map()
    }
  }
  def popEnvironment(): Unit = {
    if (doConcolic) {
      symbolicMemory = symbolicMemory.tail
    }
  }

  private def addVariable(originalName: String, symbolicVariable: String): Unit = {
    val updatedCurrentScope: SymbolicMemoryScope = symbolicMemory.head + (originalName -> symbolicVariable)
    symbolicMemory = updatedCurrentScope :: symbolicMemory.tail
  }

  private def statementConstraintToNode(constraint: StatementConstraint): SymbolicNode =
    StatementSymbolicNode(constraint, None)
  private def branchConstraintToNode(constraint: BranchConstraint, thenBranchTaken: Boolean) =
    BranchSymbolicNode(constraint, thenBranchTaken, !thenBranchTaken, None, None)

  private def addConstraint(constraint: ConcolicConstraint,
                            symbolicNode: SymbolicNode,
                            thenBranchTaken: Option[Boolean]): Unit = {
    // TODO To add a new constraint: first call the current Setter, then generate a new Setter depending on the type of the constraint argument

    currentReport :+= constraint
    integrateNode(symbolicNode)
    constraint match {
      case _: StatementConstraint =>
        integrateNode = generateStatementNodeSetter
      case _: BranchConstraint =>
        integrateNode = generateBranchNodeSetter(thenBranchTaken.get)
    }
  }

  def addStatementConstraint(constraint: StatementConstraint): Unit = {
    if (!doConcolic) {
      return
    }
    addVariable(constraint.originalName, constraint.symbolicVariable)
    addConstraint(constraint, statementConstraintToNode(constraint), None)
  }

  def addBranchConstraint(constraint: BranchConstraint, thenBranchTaken: Boolean): Unit = {
    if (!doConcolic) {
      return
    }
    val symbolicNode = branchConstraintToNode(constraint, thenBranchTaken)
    addConstraint(constraint, symbolicNode, Some(thenBranchTaken))
  }

  def getReport: PathConstraint = {
    currentReport
  }

  def lookupVariable(name: String): Option[String] = {
    def loopEnv(env: SymbolicEnvironment): Option[String] = env match {
      case scope :: rest => scope.get(name) match {
        case Some(symVar) =>
          Some(symVar)
        case None =>
          loopEnv(rest)
      }
      case Nil =>
        None
    }
    loopEnv(symbolicMemory)
  }

  def printReports(): Unit = {
    currentReport.foreach( (constraint: ConcolicConstraint) => {
      println(constraint.toString)
    })
  }
  def printTree(): Unit = {
    val optRoott = optRoot
    val optCurrentNodee = optCurrentNode
    val unexplored = SymbolicTreeHelper.findFirstUnexploredNode(optRoot.get)
    unexplored
  }

  def findUnexploredNode: Option[TreePath] = optRoot match {
    case Some(root) =>
      SymbolicTreeHelper.findFirstUnexploredNode(root)
    case None =>
      None
  }

}
