import SymbolicTreeHelper.TreePath

case object AbortConcolicRunException extends Exception

object Reporter {
  type ErrorPath = List[SemanticsFilterAnnotation]
  type PathConstraint = List[BranchConstraint]

  type SymbolicMemoryScope = Map[String, ConcolicExpression]
  type SymbolicStore = List[SymbolicMemoryScope]

  private var doConcolic: Boolean = false

  private var symbolicMemory: SymbolicStore = List(Map())
  private var currentReport: PathConstraint = Nil

  private var optRoot: Option[BranchSymbolicNode] = None
  private var optCurrentNode: Option[BranchSymbolicNode] = None

  private var optCurrentErrorPaths: Option[List[ErrorPath]] = None

  type Setter = (BranchSymbolicNode) => Unit

  def getRoot: Option[BranchSymbolicNode] = optRoot
  def getCurrentNode: Option[BranchSymbolicNode] = optCurrentNode

  private def setRoot(symbolicNode: BranchSymbolicNode): Unit = {
    optRoot = Some(symbolicNode)
    optCurrentNode = optRoot
  }
  private var integrateNode: Setter = setRoot

  private def generateBranchNodeSetter(thenBranchTaken: Boolean): Setter = optCurrentNode match {
    case Some(currentNode) => currentNode match {
      case b: BranchSymbolicNode =>
        (symNode: BranchSymbolicNode) => {
          if (thenBranchTaken) {
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
    InputVariableStore.reset()
    ConcolicIdGenerator.resetId()
    currentReport = Nil
    optCurrentNode = optRoot
    optCurrentErrorPaths = ConcolicSolver.getInitialErrorPaths
    if (!isFirstClear) {
      integrateNode = (node) => {
        optRoot = Some(optRoot.get.combine(node))
        optCurrentNode = optRoot
      }
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

  def addVariable(originalName: String, concolicExpression: ConcolicExpression): Unit = {
    val updatedCurrentScope: SymbolicMemoryScope = symbolicMemory.head + (originalName -> concolicExpression)
    symbolicMemory = updatedCurrentScope :: symbolicMemory.tail
  }

  def lookupVariable(name: String): Option[ConcolicExpression] = {
    def loopEnv(env: SymbolicStore): Option[ConcolicExpression] = env match {
      case scope :: rest => scope.get(name) match {
        case Some(concolicExp) =>
          Some(concolicExp)
        case None =>
          loopEnv(rest)
      }
      case Nil =>
        None
    }
    loopEnv(symbolicMemory)
  }

  def setVariable(name: String, newConcolicExp: ConcolicExpression): Unit = {
    def loopEnv(env: SymbolicStore): SymbolicStore = env match {
      case scope :: rest =>
        if (scope.contains(name)) {
          scope.updated(name, newConcolicExp) :: rest
        } else {
          scope :: loopEnv(rest)
        }
      case Nil =>
        Nil
    }
    val newSymbolicMemory = loopEnv(symbolicMemory)
    symbolicMemory = newSymbolicMemory
  }

  private def branchConstraintToNode(constraint: BranchConstraint, thenBranchTaken: Boolean) =
    BranchSymbolicNode(constraint, thenBranchTaken, !thenBranchTaken, None, None)

  private def addConstraint(constraint: BranchConstraint,
                            symbolicNode: BranchSymbolicNode,
                            thenBranchTaken: Boolean): Unit = {
    // To add a new constraint: first call the current Setter (i.e., integrateNode),
    // then generate a new Setter depending on the type of the constraint argument

    currentReport :+= (if (thenBranchTaken) constraint else constraint.negate)
    integrateNode(symbolicNode)
    integrateNode = generateBranchNodeSetter(thenBranchTaken)
  }

  def addBranchConstraint(constraint: BranchConstraint, thenBranchTaken: Boolean): Unit = {
    if (!doConcolic) {
      return
    }
    val symbolicNode = branchConstraintToNode(constraint, thenBranchTaken)

    addConstraint(constraint, symbolicNode, thenBranchTaken)
    if (ConcolicRunTimeFlags.checkAnalysis) {
      optCurrentErrorPaths match {
        case Some(currentErrorPaths) =>
          // If node does not follow a path along which an error is located, make the corresponding branch ineligable for testing
          val nonEmptyPaths = currentErrorPaths.filter(_.nonEmpty)
          val startsWithThen = nonEmptyPaths.filter(_.head == ThenBranchTaken)
          val startsWithElse = nonEmptyPaths.filter(_.head == ElseBranchTaken)
          if (startsWithElse.isEmpty) {
            symbolicNode.elseBranchTaken = true
          }
          if (startsWithThen.isEmpty) {
            symbolicNode.thenBranchTaken = true
          }

          if (thenBranchTaken) {
            if (startsWithThen.isEmpty) {
              Logger.log("Execution no longer follows an errorpath, aborting this concolic run", Logger.U)
              // The current path does not follow an existing errorpath, so abort this run
              throw AbortConcolicRunException
            }
            // Continue with paths that follow the then-branch.
            val tailStartsWithThen = startsWithThen.map(_.tail)
            optCurrentErrorPaths = Some(tailStartsWithThen)
          } else {
            if (startsWithElse.isEmpty) {
              Logger.log("Execution no longer follows an errorpath, aborting this concolic run", Logger.U)
              // The current path does not follow an existing errorpath, so abort this run
              throw AbortConcolicRunException
            }
            // Continue with paths that follow the else-branch.
            val tailStartsWithElse = startsWithElse.map(_.tail)
            optCurrentErrorPaths = Some(tailStartsWithElse)
          }
        case None =>
          Logger.log("Reporter not doing anything with error paths", Logger.U)
          // Assuming concolic testing is only really started if at least one errorpath is defined,
          // and assuming we check for every branch that was encountered whether execution still
          // follows _some_ errorpath (as is done in this function), this should never happen.
          throw AbortConcolicRunException
      }
    }
  }

  def getReport: PathConstraint = {
    currentReport
  }

  def printReports(): Unit = {
    Logger.log(s"Reporter recorded path: ${currentReport.mkString("; ")}", Logger.U)
  }

  def printTree(): Unit = {
    // TODO This code is mostly for debugging
    val optRoott = optRoot
    val optCurrentNodee = optCurrentNode
    optRoot.foreach(SymbolicTreeHelper.findFirstUnexploredNode)
  }

  def findUnexploredNode: Option[TreePath] = optRoot match {
    case Some(root) =>
      SymbolicTreeHelper.findFirstUnexploredNode(root)
    case None =>
      None
  }

}
