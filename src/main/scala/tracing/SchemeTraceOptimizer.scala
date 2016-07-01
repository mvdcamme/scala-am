import scala.annotation.tailrec

/**
  * Created by mvdcamme on 24/02/16.
  */
class SchemeTraceOptimizer[Addr : Address, Time : Timestamp]
  (val sem: SchemeSemanticsTraced[HybridLattice.L, HybridAddress.A, Time]) {

  type TraceInstructionInfo = Tracer[SchemeExp, Time]#TraceInstructionInfo
  type TraceInstruction = Tracer[SchemeExp, Time]#TraceInstruction
  type TraceWithoutStates = Tracer[SchemeExp, Time]#TraceWithoutStates
  type Trace = Tracer[SchemeExp, Time]#TraceWithInfos

  type HybridValue = HybridLattice.L

  val sabs = implicitly[IsSchemeLattice[HybridValue]]

  val variableAnalyzer = new VariableAnalysis[SchemeExp, HybridAddress.A, Time](sem)

  val basicOptimizations: List[(Boolean, (TraceFull[SchemeExp, Time] => TraceFull[SchemeExp, Time]))] =
    List((GlobalFlags.APPLY_OPTIMIZATION_CONTINUATIONS_LOADING, optimizeContinuationLoading(_)),
         (GlobalFlags.APPLY_OPTIMIZATION_ENVIRONMENTS_LOADING, optimizeEnvironmentLoading(_)),
         (GlobalFlags.APPLY_OPTIMIZATION_MERGE_ACTIONS, optimizeMergeActions(_)))

  val detailedOptimizations: List[(Boolean, (TraceFull[SchemeExp, Time] => TraceFull[SchemeExp, Time]))] =
    List((GlobalFlags.APPLY_OPTIMIZATION_CONSTANT_FOLDING, optimizeConstantFolding(_)),
         (GlobalFlags.APPLY_OPTIMIZATION_TYPE_SPECIALIZED_ARITHMETICS, optimizeTypeSpecialization(_)))

  def foldOptimisations(traceFull: TraceFull[SchemeExp, Time], optimisations: List[(Boolean, (TraceFull[SchemeExp, Time] => TraceFull[SchemeExp, Time]))]): TraceFull[SchemeExp, Time] = {
    optimisations.foldLeft(traceFull)({ (traceFull, pair) =>
      val function: TraceFull[SchemeExp, Time] => TraceFull[SchemeExp, Time] = pair._2
      if (pair._1) { function(traceFull) } else { traceFull }})
  }

  def optimize(trace: TraceFull[SchemeExp, Time], someAnalysisOutput: StaticAnalysisResult): TraceFull[SchemeExp, Time] = {
    Logger.log(s"Size of unoptimized trace = ${trace.trace.length}", Logger.V)
    val basicAssertedOptimizedTrace = foldOptimisations(trace, basicOptimizations)
    Logger.log(s"Size of basic optimized trace = ${basicAssertedOptimizedTrace.trace.length}", Logger.V)
    val tier2AssertedOptimizedTrace = foldOptimisations(basicAssertedOptimizedTrace, detailedOptimizations)
    Logger.log(s"Size of advanced optimized trace = ${tier2AssertedOptimizedTrace.trace.length}", Logger.V)
    val tier3AssertedOptimizedTrace = someAnalysisOutput match {
      case NonConstantAddresses(addresses) =>
        applyConstantVariablesOptimizations(tier2AssertedOptimizedTrace, addresses)
      case NoStaticisAnalysisResult =>
        possiblyOptimizeVariableFolding(tier2AssertedOptimizedTrace)
    }
    Logger.log(s"Size of statically optimized trace = ${tier3AssertedOptimizedTrace.trace.length}", Logger.V)
    val finalAssertedOptimizedTrace = removeFunCallBlockActions(tier3AssertedOptimizedTrace)
    Logger.log(s"Size of final optimized trace = ${finalAssertedOptimizedTrace.trace.length}", Logger.V)
    finalAssertedOptimizedTrace
  }

  /********************************************************************************************************************
   *                                                 COMMON FUNCTIONS                                                 *
   ********************************************************************************************************************/

  def isGuard(action: TraceInstruction): Boolean = action.isInstanceOf[ActionGuardT[SchemeExp, HybridValue, Addr]]

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

  private def constructedFullTrace(traceFull: TraceFull[SchemeExp, Time], optimisedTrace: Trace): TraceFull[SchemeExp, Time] =
    traceFull.copy(trace = optimisedTrace)

  /********************************************************************************************************************
   *                                         ENVIRONMENT LOADING OPTIMIZATION                                         *
   ********************************************************************************************************************/

  private def optimizeEnvironmentLoading(traceFull: TraceFull[SchemeExp, Time]): TraceFull[SchemeExp, Time] = {
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
    val optimizedTrace = removeMatchingActions(traceFull.trace, _.isInstanceOf[ActionSaveEnvT[SchemeExp, HybridValue, Addr]],
      _.isInstanceOf[ActionRestoreEnvT[SchemeExp, HybridValue, Addr]], isAnInterferingAction)
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                        CONTINUATION LOADING OPTIMIZATION                                         *
   ********************************************************************************************************************/

  private def optimizeContinuationLoading(traceFull: TraceFull[SchemeExp, Time]): TraceFull[SchemeExp, Time] = {
    def isAnInterferingAction(action: TraceInstruction): Boolean = action match {
      case ActionEndTrace(_) =>
        true
      case _ if isGuard(action) =>
        true
      case _ =>
        false
    }
    val optimizedTrace = removeMatchingActions(traceFull.trace, _.isInstanceOf[ActionEvalPushT[SchemeExp, HybridValue, Addr]],
                                               _.isInstanceOf[ActionPopKontT[SchemeExp, HybridValue, Addr]], isAnInterferingAction)
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                          CONSTANT FOLDING OPTIMIZATION                                           *
   ********************************************************************************************************************/

  case class ActionStartOptimizedBlock[Exp : Expression, Abs : JoinLattice, Addr : Address]()
    extends ActionT[Exp, Abs, Addr]
  case class ActionEndOptimizedBlock[Exp : Expression, Abs : JoinLattice, Addr : Address]()
    extends ActionT[Exp, Abs, Addr]

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
            if (traceAtValueChangingAction.head._1.isInstanceOf[ActionReachedValueT[SchemeExp, HybridValue, Addr]]) {
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
            val actionStatePrimCall = traceBetweenMarks.find(_._1.isInstanceOf[ActionPrimCallT[SchemeExp, HybridValue, Addr]])
            actionStatePrimCall match {
              case Some((ActionPrimCallT(n, _, _), infos)) =>
                infos.flatMap[Trace](
                  { case PrimitiveAppliedInfo(_, _) => true; case _ => false },
                  { case PrimitiveAppliedInfo(v, _) =>
                    val result = v
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
                          val replacingConstantAction: TraceInstructionInfo = (ActionReachedValueT[SchemeExp, HybridValue, HybridAddress.A](result), TraceInfos.nil[HybridValue, HybridAddress.A])
                          val actionEndOptimizedBlock = (ActionEndOptimizedBlock[SchemeExp, HybridValue, HybridAddress.A](), TraceInfos.nil[HybridValue, HybridAddress.A])
                          val actionStartOptimizedBlock = (ActionStartOptimizedBlock[SchemeExp, HybridValue, HybridAddress.A](), TraceInfos.nil[HybridValue, HybridAddress.A])
                          val replacingTrace = firstPart ++ (traceBefore :+ actionEndOptimizedBlock :+ replacingConstantAction) ++
                            /* Add all parts of the inner optimized blocks, except for the constants themselves that were folded there; those are folded away in the new block */
                            optimizedBlocks.foldLeft(List(): Trace)({ (acc, current) => acc ++ current.filter({ (actionState) => !actionState._1.isInstanceOf[ActionReachedValueT[SchemeExp, HybridValue, Addr]] }) }) ++
                            (traceAfterOperatorPush :+ actionStartOptimizedBlock) ++ traceAtStartCall.tail
                          Some(replacingTrace)
                        })
                    }
                  })
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

  private def optimizeConstantFolding(traceFull: TraceFull[SchemeExp, Time]): TraceFull[SchemeExp, Time] = {
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

  private def optimizeTypeSpecialization(traceFull: TraceFull[SchemeExp, Time]): TraceFull[SchemeExp, Time] = {
    def loop(trace: Trace, acc: Trace): Trace = trace match {
      case Nil => acc.reverse
      case (actionState1@(_, infos)) :: (actionState2@(ActionPrimCallT(n, fExp, argsExps), _)) :: rest =>
        infos.find[Trace](
          { case PrimitiveAppliedInfo(_, _) => true; case _ => false},
          { case PrimitiveAppliedInfo(_, vStack) =>
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
                val specializedPrimGuard = ActionGuardSpecializedPrimitive[SchemeExp, HybridValue, HybridAddress.A](operandsTypes, nrOfArgs, restartPoint, GuardIDCounter.incCounter())
                val specializedPrimCallAction = ActionSpecializePrimitive[SchemeExp, HybridValue, HybridAddress.A](operandsTypes, specializedPrim, n, fExp, argsExps)
                loop(rest, (specializedPrimCallAction, actionState2._2) ::(specializedPrimGuard, infos) :: actionState1 :: acc)
            }
          }
        }) match {
          case Some(result) => result
          /* Since the state before applying the function was not recorded, we cannot know what the types of the operands were */
          case None => loop(rest, actionState2 :: actionState1 :: acc)
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

  def optimizeVariableFolding(traceFull: TraceFull[SchemeExp, Time]): TraceFull[SchemeExp, Time] = {

    val initialBoundVariables = traceFull.info.boundVariables.map(_._1)
    var registerIndex: Integer = 0
    val boundVariables = variableAnalyzer.analyzeBoundVariables(initialBoundVariables.toSet, traceFull)
    var variablesConverted: List[(String, Integer)] = Nil

    val initialState: ProgramState[SchemeExp, Time] = traceFull.info.startState match {
      case s: ProgramState[SchemeExp, Time] => s
      case _ => throw new Exception(s"Variable folding optimization expected state of type ProgramState[Exp, Time], got state ${traceFull.info.startState} instead")
    }

    def replaceVariableLookups(action: ActionLookupVariableT[SchemeExp, HybridValue, HybridAddress.A],
                               infos: CombinedInfos[HybridValue, HybridAddress.A],
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

      def generateAction(varName: String): ActionT[SchemeExp, HybridValue, HybridAddress.A] = {
        val defaultAction = ActionLookupVariableT[SchemeExp, HybridValue, HybridAddress.A](varName)
        variablesConverted.find( _._1 == action.varName) match {
          case Some((_, index)) =>
            /* Variable lookup was already replaced by a register lookup */
            ActionLookupRegister[SchemeExp, HybridValue, HybridAddress.A](index)
          case None =>
          /* Variable can be replaced but isn't entered in the list yet.
             Generate an action to put it in some register if possible */
          generateIndex(action.varName) match {
            case Some(index) =>
              variablesConverted = variablesConverted :+ (action.varName, index)
              ActionLookupRegister[SchemeExp, HybridValue, HybridAddress.A](index)
            case None =>
              /* Variable couldn't be placed into a register, e.g., because there are no more registers left */
              defaultAction
          }
        }
      }

      if (boundVariables.contains(action.varName)) {
        /* Variable is bound and can therefore not be replaced */
        (action, infos)
      } else {
        val newAction = generateAction(action.varName)
        (newAction, infos.filter[VariableLookedUp[HybridValue, HybridAddress.A]])
      }
    }

    val optimisedTrace: Trace = traceFull.trace.map({
      case (action @ ActionLookupVariableT(varName, _, _), infos) =>
        replaceVariableLookups(action, infos, boundVariables)
      case (action, someState) => (action, someState)
    })

    val putRegisterActions: List[ActionT[SchemeExp, HybridValue, HybridAddress.A]] =
      variablesConverted.map(tuple => ActionPutRegister[SchemeExp, HybridValue, HybridAddress.A](tuple._1, tuple._2))

    TraceFull(traceFull.info, traceFull.assertions ++ putRegisterActions, optimisedTrace)
  }

  /**
    * Applies the variable folding optimization iff the APPLY_OPTIMIZATION_VARIABLE_FOLDING flag is enabled.
    * @param traceFull The trace to be optimized via variable folding
    * @return Either the optimized trace if the APPLY_OPTIMIZATION_VARIABLE_FOLDING flag was enabled,
    *         or the input trace if not.
    */
  def possiblyOptimizeVariableFolding(traceFull: TraceFull[SchemeExp, Time]): TraceFull[SchemeExp, Time] = {
    if (GlobalFlags.APPLY_OPTIMIZATION_VARIABLE_FOLDING) {
      optimizeVariableFolding(traceFull)
    } else {
      traceFull
    }
  }

  /********************************************************************************************************************
   *                                              MERGE ACTIONS OPTIMIZATION                                          *
   ********************************************************************************************************************/

  def optimizeMergeActions(traceFull: TraceFull[SchemeExp, Time]): TraceFull[SchemeExp, Time] = {

    @tailrec
    def loop(trace: Trace, acc: Trace): Trace = trace match {
      case Nil => acc.reverse
      case (ActionLookupVariableT(varName, read, write), i1) :: (ActionPushValT(), i2) :: rest =>
        loop(rest, (ActionLookupVariablePushT[SchemeExp, HybridValue, HybridAddress.A](varName, read, write), i1.join(i2)) :: acc)
      case (ActionReachedValueT(lit, read, write), i1) :: (ActionPushValT(), i2) :: rest =>
        loop(rest, (ActionReachedValuePushT[SchemeExp, HybridValue, HybridAddress.A](lit, read, write), i1.join(i2)) :: acc)
      case (ActionRestoreEnvT(), i1) :: (ActionSaveEnvT(), i2) :: rest =>
        loop(rest, (ActionRestoreSaveEnvT[SchemeExp, HybridValue, HybridAddress.A](), i1.join(i2)) :: acc)
      case otherAction :: rest =>
        loop(rest, otherAction :: acc)
    }

    val optimizedTrace = loop(traceFull.trace, Nil)
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                       FUNCALL BLOCK FILTERING OPTIMIZATION                                       *
   ********************************************************************************************************************/

  def removeFunCallBlockActions(traceFull: TraceFull[SchemeExp, Time]): TraceFull[SchemeExp, Time] = {
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

//  val APPLY_OPTIMIZATION_VARIABLE_FOLDING_ASSERTIONS = false
//  val APPLY_OPTIMIZATION_DEAD_STORE_ELIMINATION = false
//
//  val staticAnalysisOptimisations: List[(Boolean, (TraceFull[SchemeExp, Time], AnalysisOutput) => TraceFull[SchemeExp, Time])] =
//    List((APPLY_OPTIMIZATION_VARIABLE_FOLDING_ASSERTIONS, replaceVariablesWithConstants(_, _)))
//
//  def foldStaticOptimisations(traceFull: TraceFull[SchemeExp, Time], addresses: Set[HybridAddress.A],
//                              optimisations: List[(Boolean, (TraceFull[SchemeExp, Time], AnalysisOutput) => TraceFull[SchemeExp, Time])]): TraceFull[SchemeExp, Time] = {
//    optimisations.foldLeft(traceFull)({ (traceFull, pair) =>
//      val function: (TraceFull[SchemeExp, Time], AnalysisOutput) => TraceFull[SchemeExp, Time] = pair._2
//      if (pair._1) { function(traceFull, output) } else { traceFull }})
//  }

  /**
    *
    * @param trace The (full) trace to be optimized.
    * @param addresses The addresses whose value may change over the execution of the program after the trace.
    * @return The optimized (full) trace
    */
  def applyConstantVariablesOptimizations(trace: TraceFull[SchemeExp, Time], addresses: Set[HybridAddress.A]): TraceFull[SchemeExp, Time] = {
    replaceVariablesWithConstants(trace, addresses)
  }

  /*********************************************************************************************************************
   *                                    STATIC VARIABLE LOOKUP REMOVAL OPTIMIZATION                                    *
   *********************************************************************************************************************/

  /*
   * Takes a list of variables and
   */
//  private def findAssignedFreeVariables(freeVariables: List[String], output: StaticAnalysisResult): List[String] = {
//    var assignedFreeVariables = List[String]()
//    for ((_, transitions) <- output.graph.get.edges) {
//      for ((trace, _) <- transitions) {
//        trace.foreach({
//          case ActionSetVarT(variableName) =>
//            if (freeVariables.contains(variableName) && ! assignedFreeVariables.contains(variableName)) {
//              assignedFreeVariables = variableName :: assignedFreeVariables
//            }
//          case _ =>
//        })
//      }
//    }
//    assignedFreeVariables
//  }
//
//  private def filterUnassignedFreeVariables(assertions: TraceWithoutStates, assignedFreeVariables: List[String]): TraceWithoutStates = {
//    assertions.filter({
//      case ActionGuardAssertFreeVariable(variableName, _, _, _) =>
//        assignedFreeVariables.contains(variableName)
//      case _ => true})
//  }
//
//  private def replaceVariablesWithConstants(trace: TraceFull[SchemeExp, Time], addresses: Set[HybridAddress.A]): TraceFull[SchemeExp, Time] = {
//    val startState = trace.info.startState
//    val boundVariables = variableAnalyzer.analyzeBoundVariables(trace.info.boundVariables.toSet, trace)
//    val freeVariables = assertions.flatMap({
//      case ActionGuardAssertFreeVariable(variableName, _, _, _) => List(variableName)
//      case _ => List() })
//    val assignedFreeVariables = findAssignedFreeVariables(freeVariables, output)
//    val optimizedAssertions = filterUnassignedFreeVariables(assertions, assignedFreeVariables)
//    Logger.log(s"Unoptimized assertions: ${assertions.length}", Logger.V)
//    Logger.log(s"Optimized assertions: ${optimizedAssertions.length}", Logger.V)
//    TraceFull(trace.startProgramState, optimizedAssertions, trace.trace)
//  }

  private def collectTraceBoundAddresses(trace: Trace): Set[HybridAddress.A] =
    trace.foldRight(Set[HybridAddress.A]())((stateInfo, boundAddresses) => stateInfo match {
      case (_, infos) =>
        infos.find[Set[HybridAddress.A]](
          { case VariablesAllocated(_) => true; case _ => false},
          { case VariablesAllocated(addresses) => boundAddresses ++ addresses.toSet }).getOrElse(boundAddresses)
      case (_, infos) =>
        infos.find[Set[HybridAddress.A]](
          { case VariablesReassigned(_) => true; case _ => false},
          { case VariablesReassigned(addresses) => boundAddresses ++ addresses.toSet }).getOrElse(boundAddresses)
      case _ => boundAddresses
    })

  /**
    * Finds all addresses of variables that are bound, both inside and outside of the trace.
    * @param traceFull The trace which will be scanned to collect all addresses that are bound inside the trace.
    * @param traceExteriorBoundAddresses The set of addresses that are bound outside of the trace.
    *                                    One can rely on the ConstantVariableAnalysis to find these addresses via
    *                                    abstract interpretation.
    *
    * @return The union of both variables that are bound inside the trace and the variables that are bound outside
    *         of the trace.
    */
  private def findAllBoundAddresses(traceFull: TraceFull[SchemeExp, Time],
                                    traceExteriorBoundAddresses: Set[HybridAddress.A]): Set[HybridAddress.A] = {
    /* The addresses that are bound at the start of the trace. */
    val initialBoundAddresses = traceFull.info.boundVariables.map( {case (_, address) => address })
    /* The addresses that become bound by actions in the trace itself, e.g., addresses that are reassigned or allocated
     * within the trace. */
    val traceBoundAddresses = collectTraceBoundAddresses(traceFull.trace)
    /* Combination of the above two sets of addresses, plus the set of addresses that become bound in
     * the state graph after the trace. */
    traceBoundAddresses ++ initialBoundAddresses ++ traceExteriorBoundAddresses
  }

  private def replaceVariableLookups(trace: Trace, boundAddresses: Set[HybridAddress.A]): Trace = {

    def addressBound(address: HybridAddress.A): Boolean = {
      val abstractAddress = HybridAddress.convertAddress(address)
      /* Check both the abstracted address and the concrete address */
      boundAddresses.contains(abstractAddress) || boundAddresses.contains(address)
    }

    def replaceVariableLookup(address: HybridAddress.A, value: HybridValue,
                              originalActionInfo: TraceInstructionInfo,
                              newActionInfo: TraceInstructionInfo): TraceInstructionInfo = {
      if (addressBound(address)) {
        originalActionInfo
      } else {
        Logger.log(s"Replaced variable lookup for address $address with new actionInfo $newActionInfo", Logger.E)
        newActionInfo
      }
    }

    def replaceActionInfo(actionInfo: TraceInstructionInfo): TraceInstructionInfo = actionInfo match {
      case (ActionLookupVariableT(_, _, _), infos) =>
        infos.find[TraceInstructionInfo](
          { case VariableLookedUp(_, _, _) => true; case _ => false},
          { case VariableLookedUp(_, address, value) =>
          val newActionInfo = (ActionReachedValueT[SchemeExp, HybridValue, HybridAddress.A](value),
                               infos.filter[VariableLookedUp[HybridValue, HybridAddress.A]])
          replaceVariableLookup(address, value, actionInfo, newActionInfo) }).getOrElse(actionInfo)
      case (ActionLookupVariablePushT(_, _, _), infos) =>
        infos.find[TraceInstructionInfo](
          { case VariableLookedUp(_, _, _) => true; case _ => false},
          { case VariableLookedUp(_, address, value) =>
          val newActionInfo = (ActionReachedValuePushT[SchemeExp, HybridValue, HybridAddress.A](value),
            infos.filter[VariableLookedUp[HybridValue, HybridAddress.A]])
          replaceVariableLookup(address, value, actionInfo, newActionInfo) }).getOrElse(actionInfo)
      case _ =>
        actionInfo
    }

    trace.map(replaceActionInfo)
  }

  private def replaceVariablesWithConstants(traceFull: TraceFull[SchemeExp, Time], addresses: Set[HybridAddress.A]): TraceFull[SchemeExp, Time] = {
    val allBoundAddresses = findAllBoundAddresses(traceFull, addresses)
    Logger.log(s"Initiating new variable folding optimization; allBoundAddresses = $allBoundAddresses", Logger.D)
    val optimizedTrace = replaceVariableLookups(traceFull.trace, allBoundAddresses)
    constructedFullTrace(traceFull, optimizedTrace)
  }

  /*********************************************************************************************************************
   *                                         REMOVE CLOSURE GUARDS OPTIMIZATION                                        *
   *********************************************************************************************************************/

  /*********************************************************************************************************************
   *                                         DEAD STORE ELIMINATION OPTIMIZATION                                       *
   *********************************************************************************************************************/

//  private def optimizeDeadStoreElimination(traceFull: TraceFull[SchemeExp, Time], output: AnalysisOutput): TraceFull[SchemeExp, Time] = {
//    var deadVariables = variableAnalyzer.analyzeDeadVariables(traceFull.trace)
//    Logger.log(s"Dead variables in the trace $deadVariables", Logger.V)
//    for ((_, transitions) <- output.graph.get.edges) {
//      for ((trace, _) <- transitions) {
//        trace.foreach({
//          case ActionLookupVariableT(variableName, _, _) =>
//            if (deadVariables.contains(variableName)) {
//              deadVariables = deadVariables - variableName
//            }
//          case _ =>
//        })
//      }
//    }
//    Logger.log(s"All truly dead variables: $deadVariables", Logger.V)
//    traceFull
//  }

}
