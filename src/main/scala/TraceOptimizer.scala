import scala.annotation.tailrec

/**
  * Created by mvdcamme on 24/02/16.
  */
class TraceOptimizer[Exp : Expression, Abs, Addr, Time : Timestamp](val sem: SemanticsTraced[Exp, Abs, Addr, Time], val hybridMachine : HybridMachine[Exp, Time]) {

  type TraceInstructionInfo = HybridMachine[Exp, Time]#TraceInstructionInfo
  type TraceInstruction = HybridMachine[Exp, Time]#TraceInstruction
  type TraceWithoutStates = HybridMachine[Exp, Time]#TraceWithoutStates
  type Trace = HybridMachine[Exp, Time]#TraceWithInfos
  type TraceFull = HybridMachine[Exp, Time]#TraceFull

  type HybridValue = HybridLattice.Hybrid

  val variableAnalyzer = new VariableAnalysis(sem, hybridMachine)

  val APPLY_OPTIMIZATION_ENVIRONMENTS_LOADING = true
  val APPLY_OPTIMIZATION_CONTINUATIONS_LOADING = true
  val APPLY_OPTIMIZATION_CONSTANT_FOLDING = true
  val APPLY_OPTIMIZATION_TYPE_SPECIALIZED_ARITHMETICS = true
  val APPLY_OPTIMIZATION_VARIABLE_FOLDING = true
  val APPLY_OPTIMIZATION_MERGE_ACTIONS = true

  val basicOptimizations : List[(Boolean, (TraceFull => TraceFull))] =
    List((APPLY_OPTIMIZATION_CONTINUATIONS_LOADING, optimizeContinuationLoading(_)),
         (APPLY_OPTIMIZATION_ENVIRONMENTS_LOADING, optimizeEnvironmentLoading(_)))

  def detailedOptimizations(boundVariables : List[String]) : List[(Boolean, (TraceFull => TraceFull))] =
    List((APPLY_OPTIMIZATION_VARIABLE_FOLDING, optimizeVariableFolding(boundVariables)),
         (APPLY_OPTIMIZATION_CONSTANT_FOLDING, optimizeConstantFolding(_)),
         (APPLY_OPTIMIZATION_TYPE_SPECIALIZED_ARITHMETICS, optimizeTypeSpecialization(_)),
         (APPLY_OPTIMIZATION_MERGE_ACTIONS, optimizeMergeActions(_)))

  def foldOptimisations(traceFull: TraceFull, optimisations : List[(Boolean, (TraceFull => TraceFull))]) : TraceFull = {
    optimisations.foldLeft(traceFull)({ (traceFull, pair) =>
      val function : TraceFull => TraceFull = pair._2
      if (pair._1) { function(traceFull) } else { traceFull }})
  }

  def optimize(trace : TraceFull, boundVariables : List[String], someAnalysisOutput : Option[AnalysisOutput]) : TraceFull = {
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

  def isGuard(action : TraceInstruction): Boolean = action.isInstanceOf[ActionGuardTraced[Exp, Abs, Addr]]

  private case class ActionStateMap(actionState: TraceInstructionInfo, var isUsed: Boolean)

  private def removeMatchingActions(trace: Trace, isAPushingAction: TraceInstruction => Boolean,
                                    isAPoppingAction: TraceInstruction => Boolean, isAnInterferingAction: TraceInstruction => Boolean) : Trace = {

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

  private def constructedFullTrace(traceFull: TraceFull, optimisedTrace : Trace) : TraceFull = {
    hybridMachine.TraceFull(traceFull.startProgramState, traceFull.assertions, optimisedTrace)
  }

  /********************************************************************************************************************
   *                                         ENVIRONMENT LOADING OPTIMIZATION                                         *
   ********************************************************************************************************************/

  private def optimizeEnvironmentLoading(traceFull : TraceFull) : TraceFull = {
    def isAnInterferingAction(action : TraceInstruction) = action match {
      case ActionAllocVarsTraced(_) |
           ActionDefineVarsTraced(_) |
           ActionEndTrace(_) |
           ActionExtendEnvTraced(_) =>
        true
      case _ if isGuard(action) =>
        true
      case _ =>
        false
    }
    val optimizedTrace = removeMatchingActions(traceFull.trace, _.isInstanceOf[ActionSaveEnvTraced[Exp, Abs, Addr]],
      _.isInstanceOf[ActionRestoreEnvTraced[Exp, Abs, Addr]], isAnInterferingAction)
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                        CONTINUATION LOADING OPTIMIZATION                                         *
   ********************************************************************************************************************/

  private def optimizeContinuationLoading(traceFull : TraceFull) : TraceFull = {
    def isAnInterferingAction(action : TraceInstruction) : Boolean = action match {
      case ActionEndTrace(_) =>
        true
      case _ if isGuard(action) =>
        true
      case _ =>
        false
    }
    val optimizedTrace = removeMatchingActions(traceFull.trace, _.isInstanceOf[ActionPushTraced[Exp, Abs, Addr]],
                                               _.isInstanceOf[ActionPopKontTraced[Exp, Abs, Addr]], isAnInterferingAction)
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                          CONSTANT FOLDING OPTIMIZATION                                           *
   ********************************************************************************************************************/

  case class ActionStartOptimizedBlock[Exp : Expression, Abs : AbstractValue, Addr : Address]() extends Action[Exp, Abs, Addr]
  case class ActionEndOptimizedBlock[Exp : Expression, Abs : AbstractValue, Addr : Address]() extends Action[Exp, Abs, Addr]

  private def findNextPushVal(trace : Trace) : Option[Trace] = {
    val updatedTrace = trace.dropWhile({case (ActionPushValTraced(), _) => false
                                        case _ => true})
    if (updatedTrace.isEmpty) {
      None
    } else {
      Some(updatedTrace.tail)
    }
  }

  private def findNextAction(trace : Trace, pred : TraceInstruction => Boolean) : Option[(Trace, Trace)] = {
    val (traceBefore, traceAtEndPrimCall) =
      trace.span({ (traceInstructionState) => pred(traceInstructionState._1)})
    if (traceAtEndPrimCall.isEmpty) {
      None
    } else {
      val action = traceAtEndPrimCall.head._1
      Some((traceBefore, traceAtEndPrimCall))
    }
  }

  private def findNextEndPrimCall(trace: Trace) : Option[(Trace, Trace)] = {
    findNextAction(trace, { case ActionEndPrimCallTraced() => false
                            case _ => true})
  }

  private def findNextStartFunCall(trace: Trace) : Option[(Trace, Trace)] = {
    findNextAction(trace, { case ActionStartFunCallTraced() => false
                            case _ => true})
  }

  private def findNextEndOptimizedBlock(trace: Trace) : Option[(Trace, Trace)] = {
    findNextAction(trace, { case ActionEndOptimizedBlock() => false
    case _ => true})
  }

  private def findNextStartOptimizedBlock(trace: Trace) : Option[(Trace, Trace)] = {
    findNextAction(trace, { case ActionStartOptimizedBlock() => false
    case _ => true})
  }

  /*
   * Have to filter the optimized blocks out because otherwise they will get removed
   * when the outer block is constant folded -> lose the guards stored in these optimized blocks.
   */
  private def filterAllOptimizedBlocks(trace : Trace) : List[Trace] = {
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

  private def changesValueRegister(action : TraceInstruction) : Boolean = action match {
    case ActionCreateClosureTraced(_) => true
    case ActionLookupVariableTraced(_, _, _) => true
    case ActionPrimCallTraced(_, _, _) => true
    case ActionReachedValueTraced(_, _, _) => true
      /* Also add ActionPushTraced to guard against the case where there is no action that changes the value register
       * in between two ActionPushTraced actions */
    case ActionPushValTraced() => true
    case _ => false
  }

  private def checkPrimitive(trace : Trace, n : Integer) : Option[Trace] = {
    val onlyUsesConstants = 1.to(n - 1).foldLeft(Some(trace) : Option[Trace])({ (previousResult : Option[Trace], x) =>
      previousResult.flatMap({ currentTrace =>
        val someTraceAfterPush = findNextPushVal(currentTrace)
        someTraceAfterPush.flatMap({ traceAfterPush : Trace =>
          val traceAtValueChangingAction = traceAfterPush.dropWhile({ (instructionState) => ! changesValueRegister(instructionState._1)})
          if (traceAtValueChangingAction.isEmpty) {
            None
          } else {
            if (traceAtValueChangingAction.head._1.isInstanceOf[ActionReachedValueTraced[Exp, Abs, Addr]]) {
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

  private def doOneConstantFold(firstPart : Trace, trace : Trace) : Option[Trace] = {
    findNextEndPrimCall(trace) match {
      case Some((traceBefore, traceAtPrimCall)) =>
       findNextStartFunCall(traceAtPrimCall.tail) match {
         case Some((traceBetweenMarks, traceAtStartCall)) =>
           val optimizedBlocks = filterAllOptimizedBlocks(traceBetweenMarks)
           val actionStatePrimCall = traceBetweenMarks.find(_._1.isInstanceOf[ActionPrimCallTraced[Exp, Abs, Addr]])
           actionStatePrimCall match {
             case Some((ActionPrimCallTraced(n, _, _), Some(PrimitiveAppliedInfo(result, _)))) =>
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
                     val replacingConstantAction : TraceInstructionInfo = (ActionReachedValueTraced[Exp, HybridValue, HybridAddress](result), None)
                     val actionEndOptimizedBlock = (ActionEndOptimizedBlock[Exp, HybridValue, HybridAddress](), None)
                     val actionStartOptimizedBlock = (ActionStartOptimizedBlock[Exp, HybridValue, HybridAddress](), None)
                     val replacingTrace = firstPart ++ (traceBefore :+ actionEndOptimizedBlock :+ replacingConstantAction) ++
                                          /* Add all parts of the inner optimized blocks, except for the constants themselves that were folded there; those are folded away in the new block */
                                          optimizedBlocks.foldLeft(List() : Trace)({ (acc, current) => acc ++ current.filter({ (actionState) => ! actionState._1.isInstanceOf[ActionReachedValueTraced[Exp, Abs, Addr]] })}) ++
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

  private def optimizeConstantFolding(traceFull: TraceFull) : TraceFull = {
    def loop(trace : Trace) : Trace = {
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

  private def typeSpecializePrimitive(prim: Primitive[HybridAddress, HybridValue],
                                      operandsTypes: AbstractType): Primitive[HybridAddress, HybridValue] = prim match {
    case hybridMachine.primitives.Plus => operandsTypes match {
      case AbstractType.AbstractFloat => hybridMachine.primitives.PlusFloat
      case AbstractType.AbstractInt => hybridMachine.primitives.PlusInteger
      case _ => prim
    }
    case hybridMachine.primitives.Minus => operandsTypes match {
      case AbstractType.AbstractFloat => hybridMachine.primitives.MinusFloat
      case AbstractType.AbstractInt => hybridMachine.primitives.MinusInteger
      case _ => prim
    }
    case _ => prim
  }

  private def optimizeTypeSpecialization(traceFull: TraceFull) : TraceFull = {
    def loop(trace: Trace) : Trace = trace match {
      case Nil => Nil
      case (actionState1@(_, someInfo)) :: (actionState2@(ActionPrimCallTraced(n, fExp, argsExps), _)) :: rest => someInfo match {
        case Some(PrimitiveAppliedInfo(_, vStack)) =>
          val operands = vStack.take(n - 1).map(_.getVal)
          val operator = vStack(n - 1).getVal
          val operandsTypes = HybridLattice.checkValuesTypes(operands)
          val specializedOperator = operator match {
            case prim: HybridLattice.Prim[HybridAddress, HybridValue] => prim match {
              case HybridLattice.Prim(primitive) => primitive match {
                case primitive: Primitive[HybridAddress, HybridValue] =>
                  val specializedPrim = typeSpecializePrimitive(primitive, operandsTypes)
                  HybridLattice.Prim(specializedPrim)
              }
            }
          }
          val specializedPrimCallAction = ActionSpecializePrimitive[Exp, HybridValue, HybridAddress](operandsTypes, specializedOperator, operator, n, fExp, argsExps)
          actionState1 :: (specializedPrimCallAction, actionState2._2) :: loop(rest)
        /* Since the state before applying the function was not recorded, we cannot know what the types of the operands were */
        case _ =>
          actionState1 :: actionState2 :: loop(rest)
      }
      case action :: rest =>
        action :: loop(rest)
    }
    val optimizedTrace = loop(traceFull.trace)
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                            VARIABLE FOLDING OPTIMIZATION                                         *
   ********************************************************************************************************************/

  def optimizeVariableFolding(initialBoundVariables: List[String])(traceFull: TraceFull) : TraceFull = {
    val boundVariables = variableAnalyzer.analyzeBoundVariables(initialBoundVariables.toSet, traceFull)

    var variablesToCheck : List[(String, HybridValue)] = List()

    val initialState: ProgramState[Exp, Time] = traceFull.startProgramState match {
      case s: ProgramState[Exp, Time] => s
      case _ => throw new Exception(s"Variable folding optimization expected state of type ProgramState[Exp, Time], got state ${traceFull.startProgramState} instead")
    }

    def replaceVariableLookups(action: ActionLookupVariableTraced[Exp, HybridValue, HybridAddress],
                               boundVariables: Set[String]) : TraceInstructionInfo = {
      if (boundVariables.contains(action.varName)) {
        /* Variable is bound and can therefore not be replaced */
        (action, None)
      } else {
        val ρ = initialState.ρ
        val σ = initialState.σ
        ρ.lookup(action.varName) match {
          case Some(address) =>
            val variableValue : HybridValue = σ.lookup(address)
            if (! variablesToCheck.exists(_._1 == action.varName)) {
              /* Add a guard for this free variable, if no guard for this variable exists already */
              variablesToCheck = variablesToCheck :+ (action.varName, variableValue)
            }
            val newAction = ActionReachedValueTraced[Exp, HybridValue, HybridAddress](variableValue)
            (newAction, None)
          case None => (action, None) /* Variable could not be found in the store for some reason */
        }
      }
    }

    val optimisedTrace : Trace = traceFull.trace.map({
      case (action @ ActionLookupVariableTraced(varName, _, _), _) =>
        replaceVariableLookups(action, boundVariables)
      case (action, someState) => (action, someState)
    })

    val assertions : TraceWithoutStates = variablesToCheck.map({ (freeVariable) =>
      ActionGuardAssertFreeVariable[Exp, HybridValue, HybridAddress](freeVariable._1, freeVariable._2, RestartAssertion[Exp, HybridValue, HybridAddress]())
    })

    hybridMachine.TraceFull(traceFull.startProgramState, traceFull.assertions ++ assertions, optimisedTrace)
  }

  /********************************************************************************************************************
   *                                              MERGE ACTIONS OPTIMIZATION                                          *
   ********************************************************************************************************************/

  def optimizeMergeActions(traceFull : TraceFull) : TraceFull = {
    def loop(trace: Trace) : Trace = trace match {
      case Nil => Nil
      case (ActionLookupVariableTraced(varName, read, write), s1) :: (ActionPushValTraced(), s2) :: rest =>
        (ActionLookupVariablePushTraced[Exp, HybridValue, HybridAddress](varName, read, write), s2) :: loop(rest)
      case (ActionReachedValueTraced(lit, read, write), s1) :: (ActionPushValTraced(), s2) :: rest =>
        (ActionReachedValuePushTraced[Exp, HybridValue, HybridAddress](lit, read, write), s2) :: loop(rest)
      case (ActionRestoreEnvTraced(), s1) :: (ActionSaveEnvTraced(), s2) :: rest =>
        (ActionRestoreSaveEnvTraced[Exp, HybridValue, HybridAddress](), s2) :: loop(rest)
      case otherAction :: rest =>
        otherAction :: loop(rest)
    }

    val optimizedTrace = loop(traceFull.trace)
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                       FUNCALL BLOCK FILTERING OPTIMIZATION                                       *
   ********************************************************************************************************************/

  def removeFunCallBlockActions(traceFull: TraceFull): TraceFull = {
    val optimizedTrace = traceFull.trace.filter({
      case (ActionEndClosureCallTraced(), _) => false
      case (ActionEndOptimizedBlock(), _) => false
      case (ActionEndPrimCallTraced(), _) => false
      case (ActionStartFunCallTraced(), _) => false
      case (ActionStartOptimizedBlock(), _) => false
      case (_, _) => true
    })
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /*********************************************************************************************************************
   *                                            STATIC ANALYSIS OPTIMIZATION                                           *
   *********************************************************************************************************************/

  type AnalysisOutput = HybridMachine[Exp, Time]#AAMOutput[HybridMachine[Exp, Time]#APS, HybridMachine[Exp, Time]#TraceWithoutStates]

  val APPLY_OPTIMIZATION_VARIABLE_FOLDING_ASSERTIONS = false
  val APPLY_OPTIMIZATION_DEAD_STORE_ELIMINATION = false

  val staticAnalysisOptimisations : List[(Boolean, (TraceFull, AnalysisOutput) => TraceFull)] =
    List((APPLY_OPTIMIZATION_VARIABLE_FOLDING_ASSERTIONS, optimizeVariableFoldingAssertions(_, _)),
         (APPLY_OPTIMIZATION_DEAD_STORE_ELIMINATION, optimizeDeadStoreElimination(_, _)))

  def foldStaticOptimisations(traceFull: TraceFull, output : AnalysisOutput, optimisations : List[(Boolean, (TraceFull, AnalysisOutput) => TraceFull)]) : TraceFull = {
    optimisations.foldLeft(traceFull)({ (traceFull, pair) =>
      val function : (TraceFull, AnalysisOutput) => TraceFull = pair._2
      if (pair._1) { function(traceFull, output) } else { traceFull }})
  }

  def applyStaticAnalysisOptimization(trace : TraceFull, output : AnalysisOutput) : TraceFull = {
    foldStaticOptimisations(trace, output, staticAnalysisOptimisations)
  }

  /*********************************************************************************************************************
   *                                      VARIABLE FOLDING ASSERTIONS OPTIMIZATION                                     *
   *********************************************************************************************************************/

  /*
   * Takes a list of variables and
   */
  private def findAssignedFreeVariables(freeVariables : List[String], output : AnalysisOutput) : List[String] = {
    var assignedFreeVariables = List[String]()
    for ((_, transitions) <- output.graph.get.edges) {
      for ((trace, _) <- transitions) {
        trace.foreach({
          case ActionSetVarTraced(variableName) =>
            if (freeVariables.contains(variableName) && ! assignedFreeVariables.contains(variableName)) {
              assignedFreeVariables = variableName :: assignedFreeVariables
            }
          case _ =>
        })
      }
    }
    assignedFreeVariables
  }

  private def filterUnassignedFreeVariables(assertions : TraceWithoutStates, assignedFreeVariables : List[String]) : TraceWithoutStates = {
    assertions.filter({
      case ActionGuardAssertFreeVariable(variableName, _, _) =>
        assignedFreeVariables.contains(variableName)
      case _ => true})
  }

  private def optimizeVariableFoldingAssertions(trace : TraceFull, output : AnalysisOutput) : TraceFull = {
    val assertions = trace.assertions
    val freeVariables = assertions.flatMap({
      case ActionGuardAssertFreeVariable(variableName, _, _) => List(variableName)
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

  private def optimizeDeadStoreElimination(traceFull: TraceFull, output: AnalysisOutput) : TraceFull = {
    var deadVariables = variableAnalyzer.analyzeDeadVariables(traceFull.trace)
    Logger.log(s"Dead variables in the trace $deadVariables", Logger.V)
    for ((_, transitions) <- output.graph.get.edges) {
      for ((trace, _) <- transitions) {
        trace.foreach({
          case ActionLookupVariableTraced(variableName, _, _) =>
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
