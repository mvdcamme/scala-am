import scala.annotation.tailrec

/**
  * Created by mvdcamme on 24/02/16.
  */
class TraceOptimizer[Exp : Expression, Addr : Address, Time : Timestamp]
  (val sem: SchemeSemanticsTraced[HybridLattice.L, HybridAddress.A, Time], val hybridMachine: HybridMachine[Exp, Time]) {

  type TraceInstructionInfo = HybridMachine[Exp, Time]#TraceInstructionInfo
  type TraceInstruction = HybridMachine[Exp, Time]#TraceInstruction
  type TraceWithoutStates = HybridMachine[Exp, Time]#TraceWithoutStates
  type Trace = HybridMachine[Exp, Time]#TraceWithInfos
  type TraceFull = HybridMachine[Exp, Time]#TraceFull

  type HybridValue = HybridLattice.L

  val sabs = implicitly[IsSchemeLattice[HybridValue]]

  val variableAnalyzer = new VariableAnalysis[SchemeExp, HybridAddress.A, Time](sem, hybridMachine)

  val basicOptimizations: List[(Boolean, (TraceFull => TraceFull))] =
    List((GlobalFlags.APPLY_OPTIMIZATION_CONTINUATIONS_LOADING, optimizeContinuationLoading(_)),
         (GlobalFlags.APPLY_OPTIMIZATION_ENVIRONMENTS_LOADING, optimizeEnvironmentLoading(_)),
         (GlobalFlags.APPLY_OPTIMIZATION_MERGE_ACTIONS, optimizeMergeActions(_)))

  def detailedOptimizations(boundVariables: List[String]): List[(Boolean, (TraceFull => TraceFull))] =
    List((GlobalFlags.APPLY_OPTIMIZATION_VARIABLE_FOLDING, optimizeVariableFolding(boundVariables)),
         (GlobalFlags.APPLY_OPTIMIZATION_CONSTANT_FOLDING, optimizeConstantFolding(_)),
         (GlobalFlags.APPLY_OPTIMIZATION_TYPE_SPECIALIZED_ARITHMETICS, optimizeTypeSpecialization(_)))

  def foldOptimisations(traceFull: TraceFull, optimisations: List[(Boolean, (TraceFull => TraceFull))]): TraceFull = {
    optimisations.foldLeft(traceFull)({ (traceFull, pair) =>
      val function: TraceFull => TraceFull = pair._2
      if (pair._1) { function(traceFull) } else { traceFull }})
  }

  def optimize(trace: TraceFull, boundVariables: List[String], someAnalysisOutput: Option[AnalysisOutput]): TraceFull = {
    Logger.log(s"Size of unoptimized trace = ${trace.trace.length}", Logger.V)
    val basicAssertedOptimizedTrace = foldOptimisations(trace, basicOptimizations)
    Logger.log(s"Size of basic optimized trace = ${basicAssertedOptimizedTrace.trace.length}", Logger.V)
    val tier2AssertedOptimizedTrace = foldOptimisations(basicAssertedOptimizedTrace, detailedOptimizations(boundVariables))
    Logger.log(s"Size of advanced optimized trace = ${tier2AssertedOptimizedTrace.trace.length}", Logger.V)
    val tier3AssertedOptimizedTrace = someAnalysisOutput match {
      case Some(analysisOutput) =>
        applyStaticAnalysisOptimization(tier2AssertedOptimizedTrace, analysisOutput)
      case None =>
        tier2AssertedOptimizedTrace
    }
    Logger.log(s"Size of statically optimized trace = ${tier3AssertedOptimizedTrace.trace.length}", Logger.V)
    val finalAssertedOptimizedTrace = removeFunCallBlockActions(tier3AssertedOptimizedTrace)
    Logger.log(s"Size of final optimized trace = ${finalAssertedOptimizedTrace.trace.length}", Logger.V)
    finalAssertedOptimizedTrace
  }

  /********************************************************************************************************************
   *                                                 COMMON FUNCTIONS                                                 *
   ********************************************************************************************************************/

  def isGuard(action: TraceInstruction): Boolean = action.isInstanceOf[ActionGuardT[Exp, HybridValue, Addr]]

  private case class ActionStateMap(actionState: TraceInstructionInfo, var isUsed: Boolean)

  private def removeMatchingActions(trace: Trace, isAPushingAction: TraceInstruction => Boolean,
                                    isAPoppingAction: TraceInstruction => Boolean, isAnInterferingAction: TraceInstruction => Boolean): Trace = {

    var stack = List[ActionStateMap]()
    var optimizedTrace: List[ActionStateMap] = List()

    def handleAction(actionState: TraceInstructionInfo): Unit = {

      val actionStateMap = ActionStateMap(actionState, true)
      actionState._1 match {
        case _ if isAnInterferingAction(actionState._1)  =>
          stack.foreach(actionStateMap => actionStateMap.isUsed = true)
        case _ if isAPushingAction(actionState._1) =>
          actionStateMap.isUsed = false
          stack = actionStateMap :: stack
        case _ if isAPoppingAction(actionState._1) =>
          stack.headOption match {
            case Some(action) =>
              actionStateMap.isUsed = action.isUsed
              stack = stack.tail
            case None =>
          }
        case _ =>
      }
      optimizedTrace = optimizedTrace :+ actionStateMap
    }
    trace.foreach(handleAction(_))
    optimizedTrace.filter(_.isUsed).map(_.actionState)
  }

  private def constructedFullTrace(traceFull: TraceFull, optimisedTrace: Trace): TraceFull = {
    hybridMachine.TraceFull(traceFull.startProgramState, traceFull.assertions, optimisedTrace)
  }

  /********************************************************************************************************************
   *                                         ENVIRONMENT LOADING OPTIMIZATION                                         *
   ********************************************************************************************************************/

  private def optimizeEnvironmentLoading(traceFull: TraceFull): TraceFull = {
    def isAnInterferingAction(action: TraceInstruction) = action match {
      case ActionAllocVarsT(_) |
           ActionEndTrace(_) |
           ActionExtendEnvT(_) =>
        true
      case _ if isGuard(action) =>
        true
      case _ =>
        false
    }
    val optimizedTrace = removeMatchingActions(traceFull.trace, _.isInstanceOf[ActionSaveEnvT[Exp, HybridValue, Addr]],
      _.isInstanceOf[ActionRestoreEnvT[Exp, HybridValue, Addr]], isAnInterferingAction)
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                        CONTINUATION LOADING OPTIMIZATION                                         *
   ********************************************************************************************************************/

  private def optimizeContinuationLoading(traceFull: TraceFull): TraceFull = {
    def isAnInterferingAction(action: TraceInstruction): Boolean = action match {
      case ActionEndTrace(_) =>
        true
      case _ if isGuard(action) =>
        true
      case _ =>
        false
    }
    val optimizedTrace = removeMatchingActions(traceFull.trace, _.isInstanceOf[ActionEvalPushT[Exp, HybridValue, Addr]],
                                               _.isInstanceOf[ActionPopKontT[Exp, HybridValue, Addr]], isAnInterferingAction)
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                          CONSTANT FOLDING OPTIMIZATION                                           *
   ********************************************************************************************************************/

  case class ActionStartOptimizedBlock[Exp : Expression, Abs : JoinLattice, Addr : Address]() extends Action[Exp, Abs, Addr]
  case class ActionEndOptimizedBlock[Exp : Expression, Abs : JoinLattice, Addr : Address]() extends Action[Exp, Abs, Addr]

  private def findNextPushVal(trace: Trace): Option[Trace] = {
    val updatedTrace = trace.dropWhile({case (ActionPushValT(), _) => false
                                        case _ => true})
    if (updatedTrace.isEmpty) {
      None
    } else {
      Some(updatedTrace.tail)
    }
  }

  private def findNextAction(trace: Trace, pred: TraceInstruction => Boolean): Option[(Trace, Trace)] = {
    val (traceBefore, traceAtEndPrimCall) =
      trace.span({ (traceInstructionState) => pred(traceInstructionState._1)})
    if (traceAtEndPrimCall.isEmpty) {
      None
    } else {
      val action = traceAtEndPrimCall.head._1
      Some((traceBefore, traceAtEndPrimCall))
    }
  }

  private def findNextEndPrimCall(trace: Trace): Option[(Trace, Trace)] = {
    findNextAction(trace, { case ActionEndPrimCallT() => false
                            case _ => true})
  }

  private def findNextStartFunCall(trace: Trace): Option[(Trace, Trace)] = {
    findNextAction(trace, { case ActionStartFunCallT() => false
                            case _ => true})
  }

  private def findNextEndOptimizedBlock(trace: Trace): Option[(Trace, Trace)] = {
    findNextAction(trace, { case ActionEndOptimizedBlock() => false
    case _ => true})
  }

  private def findNextStartOptimizedBlock(trace: Trace): Option[(Trace, Trace)] = {
    findNextAction(trace, { case ActionStartOptimizedBlock() => false
    case _ => true})
  }

  /*
   * Have to filter the optimized blocks out because otherwise they will get removed
   * when the outer block is constant folded -> lose the guards stored in these optimized blocks.
   */
  private def filterAllOptimizedBlocks(trace: Trace): List[Trace] = {
    var continue = true
    var currentTrace = trace
    var acc = List[Trace]()
    while (continue) {
      findNextEndOptimizedBlock(currentTrace) match {
        case Some((traceBefore, traceAtEndOptimizedBlock)) =>
          findNextStartOptimizedBlock(traceAtEndOptimizedBlock) match {
            case Some((traceBetweenMarks, traceAtStartOptimizedBlock)) =>
              val optimizedPart = (traceAtEndOptimizedBlock.head :: traceBetweenMarks) :+ traceAtStartOptimizedBlock.head
              acc = acc :+ optimizedPart
              currentTrace = traceAtStartOptimizedBlock.tail
            case None =>
              /* Should not happen */
              continue = false
          }
        case None =>
          continue = false
      }
    }
    acc
  }

  private def changesValueRegister(action: TraceInstruction): Boolean = action match {
    case ActionCreateClosureT(_) => true
    case ActionLookupVariableT(_, _, _) => true
    case ActionPrimCallT(_, _, _) => true
    case ActionReachedValueT(_, _, _) => true
      /* Also add ActionPushTraced to guard against the case where there is no action that changes the value register
       * in between two ActionPushTraced actions */
    case ActionPushValT() => true
    case _ => false
  }

  private def checkPrimitive(trace: Trace, n: Integer): Option[Trace] = {
    val onlyUsesConstants = 1.to(n - 1).foldLeft(Some(trace): Option[Trace])({ (previousResult: Option[Trace], x) =>
      previousResult.flatMap({ currentTrace =>
        val someTraceAfterPush = findNextPushVal(currentTrace)
        someTraceAfterPush.flatMap({ traceAfterPush: Trace =>
          val traceAtValueChangingAction = traceAfterPush.dropWhile({ (instructionState) => ! changesValueRegister(instructionState._1)})
          if (traceAtValueChangingAction.isEmpty) {
            None
          } else {
            if (traceAtValueChangingAction.head._1.isInstanceOf[ActionReachedValueT[Exp, HybridValue, Addr]]) {
              Some(traceAfterPush.tail)
            } else {
              /* The value that was pushed as an operand is not a constant */
              None
            }
          }
        })
      })
    })
    onlyUsesConstants.flatMap({ (traceAfterLastConstant) =>
      findNextPushVal(traceAfterLastConstant)
    })
  }

  private def doOneConstantFold(firstPart: Trace, trace: Trace): Option[Trace] = {
    findNextEndPrimCall(trace) match {
      case Some((traceBefore, traceAtPrimCall)) =>
       findNextStartFunCall(traceAtPrimCall.tail) match {
         case Some((traceBetweenMarks, traceAtStartCall)) =>
           val optimizedBlocks = filterAllOptimizedBlocks(traceBetweenMarks)
           val actionStatePrimCall = traceBetweenMarks.find(_._1.isInstanceOf[ActionPrimCallT[Exp, HybridValue, Addr]])
           actionStatePrimCall match {
             case Some((ActionPrimCallT(n, _, _), Some(PrimitiveAppliedInfo(result, _)))) =>
               val x = findNextEndPrimCall(traceBetweenMarks)
               x match {
                 /* Another primitive is applied in this block */
                 case Some((_, betweenInnerEndPrimCallAndOuterStartMark)) =>
                   val betweenOuterPrimCallAndInnerEndPrimCall = findNextEndPrimCall(traceBetweenMarks).get._1
                   val newFirstPart = firstPart ++ (traceBefore :+ traceAtPrimCall.head) ++ betweenOuterPrimCallAndInnerEndPrimCall
                   val newTrace = betweenInnerEndPrimCallAndOuterStartMark ++ traceAtStartCall
                   doOneConstantFold(newFirstPart, newTrace)
                 case _ =>
                   checkPrimitive(traceBetweenMarks, n).flatMap({ (traceAfterOperatorPush) =>
                     //val guard = (ActionGuardSamePrimitive(), None)
                     val replacingConstantAction: TraceInstructionInfo = (ActionReachedValueT[Exp, HybridValue, HybridAddress.A](result), None)
                     val actionEndOptimizedBlock = (ActionEndOptimizedBlock[Exp, HybridValue, HybridAddress.A](), None)
                     val actionStartOptimizedBlock = (ActionStartOptimizedBlock[Exp, HybridValue, HybridAddress.A](), None)
                     val replacingTrace = firstPart ++ (traceBefore :+ actionEndOptimizedBlock :+ replacingConstantAction) ++
                                          /* Add all parts of the inner optimized blocks, except for the constants themselves that were folded there; those are folded away in the new block */
                                          optimizedBlocks.foldLeft(List(): Trace)({ (acc, current) => acc ++ current.filter({ (actionState) => ! actionState._1.isInstanceOf[ActionReachedValueT[Exp, HybridValue, Addr]] })}) ++
                                          (traceAfterOperatorPush :+ actionStartOptimizedBlock) ++ traceAtStartCall.tail
                     Some(replacingTrace)
                   })
               }
             /* Should not happen: a primitive application block should always contains an ActionPrimCallTraced */
             case None => None
           }
           /* Start of the primitive application is not part of the trace (e.g. in the case of (+ 1 (traced-loop) 2) ) */
         case None => None
       }
      /* Absolutely no primitive is applied in the given trace  */
      case None => None
    }
  }

  private def optimizeConstantFolding(traceFull: TraceFull): TraceFull = {
    def loop(trace: Trace): Trace = {
      doOneConstantFold(List(), trace) match {
        case Some(updatedTrace) =>
          loop(updatedTrace)
        case None =>
          trace
      }
    }
    val optimizedTrace = loop(traceFull.trace.reverse).reverse
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                          TYPE SPECIALIZATION OPTIMIZATION                                        *
   ********************************************************************************************************************/

  val isTracableLattice = implicitly[TracableLattice[HybridValue]]

  private def typeSpecializePrimitive(prim: Primitive[HybridAddress.A, HybridValue],
                                      operandsTypes: SimpleTypes.Value): Primitive[HybridAddress.A, HybridValue] = prim match {
    case sem.primitives.Plus => operandsTypes match {
      case SimpleTypes.Float => sem.primitives.PlusFloat
      case SimpleTypes.Integer => sem.primitives.PlusInteger
      case _ => prim
    }
    case sem.primitives.Minus => operandsTypes match {
      case SimpleTypes.Float => sem.primitives.MinusFloat
      case SimpleTypes.Integer => sem.primitives.MinusInteger
      case _ => prim
    }
    case _ => prim
  }

  private def optimizeTypeSpecialization(traceFull: TraceFull): TraceFull = {
    def loop(trace: Trace, acc: Trace): Trace = trace match {
      case Nil => acc.reverse
      case (actionState1@(_, someInfo)) :: (actionState2@(ActionPrimCallT(n, fExp, argsExps), _)) :: rest => someInfo match {
        case Some(PrimitiveAppliedInfo(_, vStack)) =>
          val nrOfArgs = n - 1
          val operands = vStack.take(nrOfArgs).map(_.getVal)
          val supposedOperator = vStack(nrOfArgs).getVal
          val operandsTypes = HybridLattice.LatticeConverter.getValuesTypes(operands)
          val somePrimitive = sabs.getPrimitives[HybridAddress.A, HybridValue](supposedOperator).headOption
          somePrimitive match {
            case None => throw new Exception(s"Operation being type-specialized is not a primitive: $supposedOperator")
            case primitive => primitive match {
                case None =>
                  /* Primitive application could not be type-specialized, so we do not replace this part of the trace. */
                  loop(rest, actionState2 :: actionState1 :: acc)
                case Some(primitive) =>
                  val specializedPrim = typeSpecializePrimitive(primitive, operandsTypes)
                  val restartPoint = RestartSpecializedPrimitive(primitive, n, fExp, argsExps)
                  val specializedPrimGuard = ActionGuardSpecializedPrimitive[Exp, HybridValue, HybridAddress.A](operandsTypes, nrOfArgs, restartPoint, GuardIDCounter.incCounter())
                  val specializedPrimCallAction = ActionSpecializePrimitive[Exp, HybridValue, HybridAddress.A](operandsTypes, specializedPrim, n, fExp, argsExps)
                  loop(rest, (specializedPrimCallAction, actionState2._2) :: (specializedPrimGuard, None) :: actionState1 :: acc)
              }
          }
        /* Since the state before applying the function was not recorded, we cannot know what the types of the operands were */
        case _ =>
          loop(rest, actionState2 :: actionState1 :: acc)
      }
      case action :: rest =>
        loop(rest, action :: acc)
    }
    val optimizedTrace = loop(traceFull.trace, Nil)
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                            VARIABLE FOLDING OPTIMIZATION                                         *
   ********************************************************************************************************************/

  def optimizeVariableFolding(initialBoundVariables: List[String])(traceFull: TraceFull): TraceFull = {

    var registerIndex: Integer = 0

    val boundVariables = variableAnalyzer.analyzeBoundVariables(initialBoundVariables.toSet, traceFull)

    var variablesConverted: List[(String, Integer)] = Nil

    val initialState: ProgramState[Exp, Time] = traceFull.startProgramState match {
      case s: ProgramState[Exp, Time] => s
      case _ => throw new Exception(s"Variable folding optimization expected state of type ProgramState[Exp, Time], got state ${traceFull.startProgramState} instead")
    }

    def replaceVariableLookups(action: ActionLookupVariableT[Exp, HybridValue, HybridAddress.A],
                               boundVariables: Set[String]): TraceInstructionInfo = {

      def generateIndex(variable: String): Option[Integer] = {
        if (registerIndex < RegisterStore.MAX_REGISTERS) {
          val someIndex = Some(registerIndex)
          registerIndex += 1
          someIndex
        } else {
          None
        }
      }

      def generateAction(varName: String): Action[Exp, HybridValue, HybridAddress.A] = {
        val defaultAction = ActionLookupVariableT[Exp, HybridValue, HybridAddress.A](varName)
        variablesConverted.find( _._1 == action.varName) match {
          case Some((_, index)) =>
            /* Variable lookup was already replaced by a register lookup */
            ActionLookupRegister[Exp, HybridValue, HybridAddress.A](index)
          case None =>
          /* Variable can be replaced but isn't entered in the list yet.
             Generate an action to put it in some register if possible */
          generateIndex(action.varName) match {
            case Some(index) =>
              variablesConverted = variablesConverted :+ (action.varName, index)
              ActionLookupRegister[Exp, HybridValue, HybridAddress.A](index)
            case None =>
              /* Variable couldn't be placed into a register, e.g., because there are no more registers left */
              defaultAction
          }
        }
      }

      if (boundVariables.contains(action.varName)) {
        /* Variable is bound and can therefore not be replaced */
        (action, None)
      } else {
        val newAction = generateAction(action.varName)
        (newAction, None)
      }
    }

    val optimisedTrace: Trace = traceFull.trace.map({
      case (action @ ActionLookupVariableT(varName, _, _), _) =>
        replaceVariableLookups(action, boundVariables)
      case (action, someState) => (action, someState)
    })

    val putRegisterActions: List[Action[Exp, HybridValue, HybridAddress.A]] =
      variablesConverted.map(tuple => ActionPutRegister[Exp, HybridValue, HybridAddress.A](tuple._1, tuple._2))

    hybridMachine.TraceFull(traceFull.startProgramState, traceFull.assertions ++ putRegisterActions, optimisedTrace)
  }

  /********************************************************************************************************************
   *                                              MERGE ACTIONS OPTIMIZATION                                          *
   ********************************************************************************************************************/

  def optimizeMergeActions(traceFull: TraceFull): TraceFull = {

    @tailrec
    def loop(trace: Trace, acc: Trace): Trace = trace match {
      case Nil => acc.reverse
      case (ActionLookupVariableT(varName, read, write), s1) :: (ActionPushValT(), s2) :: rest =>
        loop(rest, (ActionLookupVariablePushT[Exp, HybridValue, HybridAddress.A](varName, read, write), s2) :: acc)
      case (ActionReachedValueT(lit, read, write), s1) :: (ActionPushValT(), s2) :: rest =>
        loop(rest, (ActionReachedValuePushT[Exp, HybridValue, HybridAddress.A](lit, read, write), s2) :: acc)
      case (ActionRestoreEnvT(), s1) :: (ActionSaveEnvT(), s2) :: rest =>
        loop(rest, (ActionRestoreSaveEnvT[Exp, HybridValue, HybridAddress.A](), s2) :: acc)
      case otherAction :: rest =>
        loop(rest, otherAction :: acc)
    }

    val optimizedTrace = loop(traceFull.trace, Nil)
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                       FUNCALL BLOCK FILTERING OPTIMIZATION                                       *
   ********************************************************************************************************************/

  def removeFunCallBlockActions(traceFull: TraceFull): TraceFull = {
    val optimizedTrace = traceFull.trace.filter({
      case (ActionEndClosureCallT(), _) => false
      case (ActionEndOptimizedBlock(), _) => false
      case (ActionEndPrimCallT(), _) => false
      case (ActionStartFunCallT(), _) => false
      case (ActionStartOptimizedBlock(), _) => false
      case (_, _) => true
    })
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /*********************************************************************************************************************
   *                                            STATIC ANALYSIS OPTIMIZATION                                           *
   *********************************************************************************************************************/

  type AnalysisOutput = HybridMachine[Exp, Time]#AAMOutput[HybridMachine[Exp, Time]#PS, HybridMachine[Exp, Time]#TraceWithoutStates]

  val APPLY_OPTIMIZATION_VARIABLE_FOLDING_ASSERTIONS = false
  val APPLY_OPTIMIZATION_DEAD_STORE_ELIMINATION = false

  val staticAnalysisOptimisations: List[(Boolean, (TraceFull, AnalysisOutput) => TraceFull)] =
    List((APPLY_OPTIMIZATION_VARIABLE_FOLDING_ASSERTIONS, optimizeVariableFoldingAssertions(_, _)),
         (APPLY_OPTIMIZATION_DEAD_STORE_ELIMINATION, optimizeDeadStoreElimination(_, _)))

  def foldStaticOptimisations(traceFull: TraceFull, output: AnalysisOutput, optimisations: List[(Boolean, (TraceFull, AnalysisOutput) => TraceFull)]): TraceFull = {
    optimisations.foldLeft(traceFull)({ (traceFull, pair) =>
      val function: (TraceFull, AnalysisOutput) => TraceFull = pair._2
      if (pair._1) { function(traceFull, output) } else { traceFull }})
  }

  def applyStaticAnalysisOptimization(trace: TraceFull, output: AnalysisOutput): TraceFull = {
    foldStaticOptimisations(trace, output, staticAnalysisOptimisations)
  }

  /*********************************************************************************************************************
   *                                      VARIABLE FOLDING ASSERTIONS OPTIMIZATION                                     *
   *********************************************************************************************************************/

  /*
   * Takes a list of variables and
   */
  private def findAssignedFreeVariables(freeVariables: List[String], output: AnalysisOutput): List[String] = {
    var assignedFreeVariables = List[String]()
    for ((_, transitions) <- output.graph.get.edges) {
      for ((trace, _) <- transitions) {
        trace.foreach({
          case ActionSetVarT(variableName) =>
            if (freeVariables.contains(variableName) && ! assignedFreeVariables.contains(variableName)) {
              assignedFreeVariables = variableName :: assignedFreeVariables
            }
          case _ =>
        })
      }
    }
    assignedFreeVariables
  }

  private def filterUnassignedFreeVariables(assertions: TraceWithoutStates, assignedFreeVariables: List[String]): TraceWithoutStates = {
    assertions.filter({
      case ActionGuardAssertFreeVariable(variableName, _, _, _) =>
        assignedFreeVariables.contains(variableName)
      case _ => true})
  }

  private def optimizeVariableFoldingAssertions(trace: TraceFull, output: AnalysisOutput): TraceFull = {
    val assertions = trace.assertions
    val freeVariables = assertions.flatMap({
      case ActionGuardAssertFreeVariable(variableName, _, _, _) => List(variableName)
      case _ => List() })
    val assignedFreeVariables = findAssignedFreeVariables(freeVariables, output)
    val optimizedAssertions = filterUnassignedFreeVariables(assertions, assignedFreeVariables)
    Logger.log(s"Unoptimized assertions: ${assertions.length}", Logger.V)
    Logger.log(s"Optimized assertions: ${optimizedAssertions.length}", Logger.V)
    hybridMachine.TraceFull(trace.startProgramState, optimizedAssertions, trace.trace)
  }

  /*********************************************************************************************************************
   *                                         DEAD STORE ELIMINATION OPTIMIZATION                                       *
   *********************************************************************************************************************/

  private def optimizeDeadStoreElimination(traceFull: TraceFull, output: AnalysisOutput): TraceFull = {
    var deadVariables = variableAnalyzer.analyzeDeadVariables(traceFull.trace)
    Logger.log(s"Dead variables in the trace $deadVariables", Logger.V)
    for ((_, transitions) <- output.graph.get.edges) {
      for ((trace, _) <- transitions) {
        trace.foreach({
          case ActionLookupVariableT(variableName, _, _) =>
            if (deadVariables.contains(variableName)) {
              deadVariables = deadVariables - variableName
            }
          case _ =>
        })
      }
    }
    Logger.log(s"All truly dead variables: $deadVariables", Logger.V)
    traceFull
  }

}
