import scala.annotation.tailrec

/**
  * Created by mvdcamme on 24/02/16.
  */
class SchemeTraceOptimizer//[Abs : IsSchemeLattice] TODO
  (val sem: SchemeSemanticsTraced[ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T],
//   constantsAnalysisLauncher: ConstantsAnalysisLauncher[Abs, SchemeExp],
   tracingFlags: TracingFlags)
  (implicit sabs: IsSchemeLattice[ConcreteConcreteLattice.L], latInfoProv: LatticeInfoProvider[ConcreteConcreteLattice.L])
  extends TraceOptimizer[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T] {

  type TraceInstructionInfo = Tracer[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]#TraceInstructionInfo
  type TraceInstruction = Tracer[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]#TraceInstruction
  type TraceWithoutStates = Tracer[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]#TraceWithoutStates
  type Trace = Tracer[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]#TraceWithInfos

  type SpecTraceFull = TraceFull[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]

  type ConcreteValue = ConcreteConcreteLattice.L

  val basicOptimizations: List[(Boolean, (SpecTraceFull => SpecTraceFull))] =
    List((GlobalFlags.APPLY_OPTIMIZATION_CONTINUATIONS_LOADING, optimizeContinuationLoading(_)),
         (GlobalFlags.APPLY_OPTIMIZATION_ENVIRONMENTS_LOADING, optimizeEnvironmentLoading(_)),
         (GlobalFlags.APPLY_OPTIMIZATION_MERGE_ACTIONS, optimizeMergeActions(_)))

  val detailedOptimizations: List[(Boolean, (SpecTraceFull => SpecTraceFull))] =
    List((GlobalFlags.APPLY_OPTIMIZATION_CONSTANT_FOLDING, optimizeConstantFolding(_)),
         (GlobalFlags.APPLY_OPTIMIZATION_TYPE_SPECIALIZED_ARITHMETICS, optimizeTypeSpecialization(_)))

  def foldOptimisations(traceFull: SpecTraceFull, optimisations: List[(Boolean, (SpecTraceFull => SpecTraceFull))]): SpecTraceFull= {
    optimisations.foldLeft(traceFull)({ (traceFull, pair) =>
      val function: SpecTraceFull => SpecTraceFull = pair._2
      if (pair._1) { function(traceFull) } else { traceFull }})
  }

  def optimize(trace: SpecTraceFull,
               state: ConcreteTracingProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T])
              :SpecTraceFull = {
    Logger.log(s"Size of unoptimized trace = ${trace.trace.length}", Logger.V)
    val basicAssertedOptimizedTrace = foldOptimisations(trace, basicOptimizations)
    Logger.log(s"Size of basic optimized trace = ${basicAssertedOptimizedTrace.trace.length}", Logger.V)
    val tier2AssertedOptimizedTrace = foldOptimisations(basicAssertedOptimizedTrace, detailedOptimizations)
    Logger.log(s"Size of advanced optimized trace = ${tier2AssertedOptimizedTrace.trace.length}", Logger.V)
    val tier3AssertedOptimizedTrace = tier2AssertedOptimizedTrace
//      if (tracingFlags.SWITCH_ABSTRACT) {
//        /* Runtime analyses (+ an initial analysis) performed. */
//        val addressesLookedUp = TraceAnalyzer.collectAddressesLookedUp[SchemeExp, HybridTimestamp.T](trace.trace)
//        val analysisOutput = constantsAnalysisLauncher.runStaticAnalysis(state, addressesLookedUp)
//        analysisOutput match {
//          case ConstantAddresses(_, nonConstants) =>
//            val staticallyOptimizedTrace = applyConstantVariablesOptimizations(tier2AssertedOptimizedTrace, nonConstants.asInstanceOf[Set[HybridAddress.A]])
//            Logger.log(s"Size of statically optimized trace = ${staticallyOptimizedTrace.trace.length}", Logger.V)
//            staticallyOptimizedTrace
//          case NoStaticisAnalysisResult =>
//            possiblyOptimizeVariableFolding(tier2AssertedOptimizedTrace)
//        }
//      } else if (tracingFlags.DO_INITIAL_ANALYSIS) {
//        /* Only an initial analysis performed. */
//        constantsAnalysisLauncher.constantsAnalysis.initialAnalysisResults.get match {
//          case ConstantAddresses(_, nonConstants) =>
//            val staticallyOptimizedTrace = applyConstantVariablesOptimizations(tier2AssertedOptimizedTrace, nonConstants.asInstanceOf[Set[HybridAddress.A]])
//            Logger.log(s"Size of statically optimized trace = ${staticallyOptimizedTrace.trace.length}", Logger.V)
//            staticallyOptimizedTrace
//        }
//      } else {
//        /* No analyses performed at all. */
//        possiblyOptimizeVariableFolding(tier2AssertedOptimizedTrace)
//      }
    val finalAssertedOptimizedTrace = removeFunCallBlockActions(tier3AssertedOptimizedTrace)
    Logger.log(s"Size of final optimized trace = ${finalAssertedOptimizedTrace.trace.length}", Logger.V)
    finalAssertedOptimizedTrace
  }

  /********************************************************************************************************************
   *                                                 COMMON FUNCTIONS                                                 *
   ********************************************************************************************************************/

  /*
   * Maps an instruction to a flag which indicates whether this instruction should be included (true) or not (false).
   */
  private case class ActionStateMap(actionState: TraceInstructionInfo, var isUsed: Boolean)

  private def removeMatchingActions(trace: Trace, isAPushingAction: TraceInstruction => Boolean,
                                    isAPoppingAction: TraceInstruction => Boolean, isAnInterferingAction: TraceInstruction => Boolean): Trace = {

    /* Stores all "pushing" actions for which no matching "popping" action has been encountered yet. */
    var stack = List[ActionStateMap]()

    /* Equivalent to the input trace to be optimized, but instead of just storing trace instructions, this variable
     * stores ActionStateMaps. The ActionStateMap at each point in this optimizedTrace equals the corresponding
      * trace instruction in the input trace, plus the isUsed flag, which may be true or false. */
    var optimizedTrace: List[ActionStateMap] = List()

    /*
     * Checks to see what should be done with the given trace instruction info.
     */
    def handleAction(actionState: TraceInstructionInfo): Unit = {

      /* The optimization assumes by default that all trace instructions are used. */
      val actionStateMap = ActionStateMap(actionState, true)
      actionState._1 match {
        /* An "interfering" action, e.g. a guard failure, is found. If this interfering action would be triggered,
         * it would require all the "pushing" actions which haven't been matched yet (i.e., the actions currently
         * residing on the stack) to be placed in the trace, to ensure soundness.
         *
         * We therefore set the "isUsed" flag of all actions in the stack to true, so that they are
         * definitely placed in the trace. */
        case _ if isAnInterferingAction(actionState._1)  =>
          stack.foreach(actionStateMap => actionStateMap.isUsed = true)
        /* If the action is a "pushing" action, it might be removed, so the isUsed flag is set to false.
         * The action is also pushed onto the stack, until (if) its matching "popping" action is found. */
        case _ if isAPushingAction(actionState._1) =>
          actionStateMap.isUsed = false
          stack = actionStateMap :: stack
        /* A "popping" action was found. This instruction matches the instruction at the top of the stack.
         *
         * If this matching "pushing" action can be removed, then so can this "popping" action, so we set the flag
         * of this popping action to whatever the value of the "pushing" action is.*/
        case _ if isAPoppingAction(actionState._1) =>
          /* It could be that there is no matching "pushing" action, i.e., the stack is empty. This could be because
           * the tracer started recording between the execution of the matching "pushing" action and this "popping"
           * action. In that case, this "popping" action cannot be removed. */
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

    /* If there are still "pushing" actions left on the stack (e.g., because the trace ended before reaching their
     * matching "popping" action), those actions cannot be removed. */
    stack.foreach(_.isUsed = true)
    /* Actually filter away all actions which might be removed by the optimization. */
    optimizedTrace.filter(_.isUsed).map(_.actionState)
  }

  private def createFullTrace(traceFull: SpecTraceFull, optimisedTrace: Trace): SpecTraceFull =
    traceFull.copy(trace = optimisedTrace)

  /********************************************************************************************************************
   *                                         ENVIRONMENT LOADING OPTIMIZATION                                         *
   ********************************************************************************************************************/

  private def optimizeEnvironmentLoading(traceFull: SpecTraceFull): SpecTraceFull = {
    def isAnInterferingAction(action: TraceInstruction) = action match {
      case ActionAllocVarsT(_) |
           ActionEndTrace(_) |
           ActionExtendEnvT(_) =>
        true
      case _ if action.isGuard =>
        true
      case _ =>
        false
    }
    val optimizedTrace = removeMatchingActions(traceFull.trace,
                                               { case ActionSaveEnvT() => true; case _ => false },
                                               { case ActionRestoreEnvT() => true; case _ => false },
                                               isAnInterferingAction)
    createFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                        CONTINUATION LOADING OPTIMIZATION                                         *
   ********************************************************************************************************************/

  private def optimizeContinuationLoading(traceFull: SpecTraceFull): SpecTraceFull = {
    def isAnInterferingAction(action: TraceInstruction): Boolean = action match {
      case ActionEndTrace(_) =>
        true
      case _ if action.isGuard =>
        true
      case _ =>
        false
    }
    val optimizedTrace = removeMatchingActions(traceFull.trace,
                                               { case ActionEvalPushT(_, _, _, _) => true; case _ => false },
                                               { case ActionPopKontT() => true; case _ => false },
                                               isAnInterferingAction)
    createFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                          CONSTANT FOLDING OPTIMIZATION                                           *
   ********************************************************************************************************************/

  case class ActionStartOptimizedBlock[Exp : Expression, Abs : JoinLattice]()
    extends ActionT[Exp, Abs, HybridAddress.A]
  case class ActionEndOptimizedBlock[Exp : Expression, Abs : JoinLattice]()
    extends ActionT[Exp, Abs, HybridAddress.A]

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

  /* TODO */
  private def changesValueRegister(action: TraceInstruction): Boolean = action match {
    case _ if action.changesValueReg => true
      /* Also add ActionPushValT to guard against the case where there is no action that changes the value register
       * in between two ActionPushValT actions */
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
            if (traceAtValueChangingAction.head._1.isInstanceOf[ActionReachedValueT[SchemeExp, ConcreteValue, HybridAddress.A]]) {
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
            val actionStatePrimCall = traceBetweenMarks.find(_._1.isInstanceOf[ActionPrimCallT[SchemeExp, ConcreteValue, HybridAddress.A]])
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
                          val replacingConstantAction: TraceInstructionInfo = (ActionReachedValueT[SchemeExp, ConcreteValue, HybridAddress.A](result), TraceInfos.nil[ConcreteValue, HybridAddress.A])
                          val actionEndOptimizedBlock = (ActionEndOptimizedBlock[SchemeExp, ConcreteValue](), TraceInfos.nil[ConcreteValue, HybridAddress.A])
                          val actionStartOptimizedBlock = (ActionStartOptimizedBlock[SchemeExp, ConcreteValue](), TraceInfos.nil[ConcreteValue, HybridAddress.A])
                          val replacingTrace = firstPart ++ (traceBefore :+ actionEndOptimizedBlock :+ replacingConstantAction) ++
                            /* Add all parts of the inner optimized blocks, except for the constants themselves that were folded there; those are folded away in the new block */
                            optimizedBlocks.foldLeft(List(): Trace)({ (acc, current) => acc ++ current.filter({ (actionState) => !actionState._1.isInstanceOf[ActionReachedValueT[SchemeExp, ConcreteValue, HybridAddress.A]] }) }) ++
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

  private def optimizeConstantFolding(traceFull: SpecTraceFull): SpecTraceFull = {
    def loop(trace: Trace): Trace = {
      doOneConstantFold(List(), trace) match {
        case Some(updatedTrace) =>
          loop(updatedTrace)
        case None =>
          trace
      }
    }
    val optimizedTrace = loop(traceFull.trace.reverse).reverse
    createFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                          TYPE SPECIALIZATION OPTIMIZATION                                        *
   ********************************************************************************************************************/

  private def typeSpecializePrimitive(prim: Primitive[HybridAddress.A, ConcreteValue],
                                      operandsTypes: SimpleTypes.Value): Primitive[HybridAddress.A, ConcreteValue] = prim match {
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

  private def optimizeTypeSpecialization(traceFull: SpecTraceFull): SpecTraceFull = {
    def loop(trace: Trace, acc: Trace): Trace = trace match {
      case Nil => acc.reverse
      case (actionState1@(_, infos)) :: (actionState2@(ActionPrimCallT(n, fExp, argsExps), _)) :: rest =>
        infos.find[Trace](
          { case PrimitiveAppliedInfo(_, _) => true; case _ => false},
          { case PrimitiveAppliedInfo(_, vStack) =>
          val nrOfArgs = n - 1
          val operands = vStack.take(nrOfArgs).map(_.getVal)
          val supposedOperator = vStack(nrOfArgs).getVal
//          val latticeInfoProvider = implicitly[LatticeInfoProvider[ConcreteConcreteLattice.L]] TODO
//          val operandsTypes = latticeInfoProvider.simpleTypes(operands)
          val somePrimitive = sabs.getPrimitives[HybridAddress.A, ConcreteValue](supposedOperator).headOption
          somePrimitive match {
            case None => throw new Exception(s"Operation being type-specialized is not a primitive: $supposedOperator")
            case primitive => primitive match {
//              case None => TODO
              case _ =>
                /* Primitive application could not be type-specialized, so we do not replace this part of the trace. */
                loop(rest, actionState2 :: actionState1 :: acc)
//              case Some(primitive) => TODO
//                val specializedPrim = typeSpecializePrimitive(primitive, operandsTypes)
//                val restartPoint = RestartSpecializedPrimitive(primitive, n, fExp, argsExps)
//                val specializedPrimGuard = ActionGuardSpecializedPrimitive[SchemeExp, HybridValue, HybridAddress.A](operandsTypes, nrOfArgs, restartPoint, GuardIDCounter.incCounter())
//                val specializedPrimCallAction = ActionSpecializePrimitive[SchemeExp, HybridValue, HybridAddress.A](operandsTypes, specializedPrim, n, fExp, argsExps)
//                loop(rest, (specializedPrimCallAction, actionState2._2) ::(specializedPrimGuard, infos) :: actionState1 :: acc)
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
    createFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                            VARIABLE FOLDING OPTIMIZATION                                         *
   ********************************************************************************************************************/

  def optimizeVariableFolding(traceFull: SpecTraceFull): SpecTraceFull = {

    var registerIndex: Integer = 0
    val initialBoundAddresses = traceFull.info.boundVariables.map(_._2)
    val boundAddresses = TraceAnalyzer.collectTraceBoundAddresses[SchemeExp, HybridTimestamp.T](traceFull.trace) ++ initialBoundAddresses
    var variablesConverted: List[(String, Integer)] = Nil

    def replaceVariableLookups(action: ActionT[SchemeExp, ConcreteValue, HybridAddress.A],
                               varName: String,
                               generateRegisterLookupAction: (Int) => ActionT[SchemeExp, ConcreteValue, HybridAddress.A],
                               infos: CombinedInfos[ConcreteValue, HybridAddress.A],
                               boundAddresses: Set[HybridAddress.A]): TraceInstructionInfo = {

      def generateIndex(address: HybridAddress.A): Option[Integer] = {
        if (registerIndex < RegisterStore.MAX_REGISTERS) {
          val someIndex = Some(registerIndex)
          registerIndex += 1
          someIndex
        } else {
          None
        }
      }

      def generateAction(address: HybridAddress.A): ActionT[SchemeExp, ConcreteValue, HybridAddress.A] = {
        val defaultAction = action
        variablesConverted.find( _._1 == varName) match {
          case Some((_, index)) =>
            /* Variable lookup was already replaced by a register lookup */
            generateRegisterLookupAction(index)
          case None =>
          /* Variable can be replaced but isn't entered in the list yet.
             Generate an action to put it in some register if possible */
          generateIndex(address) match {
            case Some(index) =>
              variablesConverted = variablesConverted :+ (varName, index)
              generateRegisterLookupAction(index)
            case None =>
              /* Variable couldn't be placed into a register, e.g., because there are no more registers left */
              defaultAction
          }
        }
      }

      val someNewTraceInfo = infos.find[(ActionT[SchemeExp, ConcreteValue, HybridAddress.A], CombinedInfos[ConcreteValue, HybridAddress.A])](
        { case VariableLookedUp(_, address, _) => true
        case _ => false
        }, { case VariableLookedUp(_, address, _) =>
          if (boundAddresses.contains(address)) {
            /* Variable is bound and can therefore not be replaced */
            (action, infos)
          } else {
            val newAction = generateAction(address)
            (newAction, infos.filter[VariableLookedUp[ConcreteValue, HybridAddress.A]])
          }
        })
      someNewTraceInfo match {
        case None => throw new Exception(s"Error: ActionLookUpT without an associated VariableLookedUp info: $action")
        case Some(newTraceInfo) => newTraceInfo
      }
    }

    val optimisedTrace: Trace = traceFull.trace.map({
      case (action @ ActionLookupVariableT(varName, _, _), infos) =>
        replaceVariableLookups(action, varName, ActionLookupRegister[SchemeExp, ConcreteValue, HybridAddress.A](_), infos, boundAddresses)
      case (action @ ActionLookupVariablePushT(varName, _, _), infos) =>
        replaceVariableLookups(action, varName, ActionLookupRegisterPush[SchemeExp, ConcreteValue, HybridAddress.A](_), infos, boundAddresses)
      case (action, someState) => (action, someState)
    })

    val putRegisterActions: List[ActionT[SchemeExp, ConcreteValue, HybridAddress.A]] =
      variablesConverted.map(tuple => ActionPutRegister[SchemeExp, ConcreteValue, HybridAddress.A](tuple._1, tuple._2))

    TraceFull(traceFull.info, traceFull.assertions ++ putRegisterActions, optimisedTrace)
  }

  /**
    * Applies the variable folding optimization iff the APPLY_OPTIMIZATION_VARIABLE_FOLDING flag is enabled.
    * @param traceFull The trace to be optimized via variable folding
    * @return Either the optimized trace if the APPLY_OPTIMIZATION_VARIABLE_FOLDING flag was enabled,
    *         or the input trace if not.
    */
  def possiblyOptimizeVariableFolding(traceFull: SpecTraceFull): SpecTraceFull = {
    if (GlobalFlags.APPLY_OPTIMIZATION_VARIABLE_FOLDING) {
      optimizeVariableFolding(traceFull)
    } else {
      traceFull
    }
  }

  /********************************************************************************************************************
   *                                              MERGE ACTIONS OPTIMIZATION                                          *
   ********************************************************************************************************************/

  def optimizeMergeActions(traceFull: SpecTraceFull): SpecTraceFull = {

    @tailrec
    def loop(trace: Trace, acc: Trace): Trace = trace match {
      case Nil => acc.reverse
      case (ActionLookupVariableT(varName, read, write), i1) :: (ActionPushValT(), i2) :: rest =>
        loop(rest, (ActionLookupVariablePushT[SchemeExp, ConcreteValue, HybridAddress.A](varName, read, write), i1.join(i2)) :: acc)
      case (ActionReachedValueT(lit, read, write), i1) :: (ActionPushValT(), i2) :: rest =>
        loop(rest, (ActionReachedValuePushT[SchemeExp, ConcreteValue, HybridAddress.A](lit, read, write), i1.join(i2)) :: acc)
      case (ActionRestoreEnvT(), i1) :: (ActionSaveEnvT(), i2) :: rest =>
        loop(rest, (ActionRestoreSaveEnvT[SchemeExp, ConcreteValue, HybridAddress.A](), i1.join(i2)) :: acc)
      case otherAction :: rest =>
        loop(rest, otherAction :: acc)
    }

    val optimizedTrace = loop(traceFull.trace, Nil)
    createFullTrace(traceFull, optimizedTrace)
  }

  /********************************************************************************************************************
   *                                       FUNCALL BLOCK FILTERING OPTIMIZATION                                       *
   ********************************************************************************************************************/

  def removeFunCallBlockActions(traceFull: SpecTraceFull): SpecTraceFull = {
    val optimizedTrace = traceFull.trace.filter({
      case (ActionEndClosureCallT(), _) => false
      case (ActionEndOptimizedBlock(), _) => false
      case (ActionEndPrimCallT(), _) => false
      case (ActionStartFunCallT(), _) => false
      case (ActionStartOptimizedBlock(), _) => false
      case (_, _) => true
    })
    createFullTrace(traceFull, optimizedTrace)
  }

  /*********************************************************************************************************************
   *                                            STATIC ANALYSIS OPTIMIZATION                                           *
   *********************************************************************************************************************/

//  val APPLY_OPTIMIZATION_VARIABLE_FOLDING_ASSERTIONS = false
//  val APPLY_OPTIMIZATION_DEAD_STORE_ELIMINATION = false
//
//  val staticAnalysisOptimisations: List[(Boolean, (SpecTraceFull, AnalysisOutput) => SpecTraceFull)] =
//    List((APPLY_OPTIMIZATION_VARIABLE_FOLDING_ASSERTIONS, replaceVariablesWithConstants(_, _)))
//
//  def foldStaticOptimisations(traceFull: SpecTraceFull, addresses: Set[HybridAddress.A],
//                              optimisations: List[(Boolean, (SpecTraceFull, AnalysisOutput) => SpecTraceFull)]): SpecTraceFull = {
//    optimisations.foldLeft(traceFull)({ (traceFull, pair) =>
//      val function: (SpecTraceFull, AnalysisOutput) => SpecTraceFull = pair._2
//      if (pair._1) { function(traceFull, output) } else { traceFull }})
//  }

  /**
    *
    * @param traceFull The (full) trace to be optimized.
    * @param nonConstants The addresses whose value may change over the execution of the program after the trace.
    * @return The optimized (full) trace
    */
  def applyConstantVariablesOptimizations(traceFull: SpecTraceFull,
                                          nonConstants: Set[HybridAddress.A]): SpecTraceFull = {
    val allBoundAddresses = findAllBoundAddresses(traceFull, nonConstants)
    val varLookupsRemoved = replaceVariablesWithConstants(traceFull, allBoundAddresses)
    removeRedundantClosureGuards(varLookupsRemoved, allBoundAddresses)
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
//  private def replaceVariablesWithConstants(trace: SpecTraceFull, addresses: Set[HybridAddress.A]): SpecTraceFull = {
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
  private def findAllBoundAddresses(traceFull: SpecTraceFull,
                                    traceExteriorBoundAddresses: Set[HybridAddress.A]): Set[HybridAddress.A] = {
    /* The addresses that are bound at the start of the trace. */
    val initialBoundAddresses = traceFull.info.boundVariables.map( {case (_, address) => address })
    /* The addresses that become bound by actions in the trace itself, e.g., addresses that are reassigned or allocated
     * within the trace. */
    val traceBoundAddresses = TraceAnalyzer.collectTraceBoundAddresses[SchemeExp, HybridTimestamp.T](traceFull.trace)
    /* Combination of the above two sets of addresses, plus the set of addresses that become bound in
     * the state graph after the trace. */
    traceBoundAddresses ++ initialBoundAddresses ++ traceExteriorBoundAddresses
  }

  private def addressBound(address: HybridAddress.A, boundAddresses: Set[HybridAddress.A]): Boolean = {
    val abstractAddress = new DefaultHybridAddressConverter[SchemeExp]().convertAddress(address)
    /* Check both the abstracted address and the concrete address */
    boundAddresses.contains(abstractAddress) || boundAddresses.contains(address)
  }

  private def replaceVariableLookups(trace: Trace, boundAddresses: Set[HybridAddress.A]): Trace = {

    def replaceVariableLookup(address: HybridAddress.A, value: ConcreteValue,
                              originalActionInfo: TraceInstructionInfo,
                              newActionInfo: TraceInstructionInfo): TraceInstructionInfo = {
      if (addressBound(address, boundAddresses)) {
        originalActionInfo
      } else {
        Logger.log(s"Replaced variable lookup for address $address with new actionInfo $newActionInfo", Logger.V)
        newActionInfo
      }
    }

    def replaceActionInfo(actionInfo: TraceInstructionInfo): TraceInstructionInfo = actionInfo match {
      case (ActionLookupVariableT(_, _, _), infos) =>
        infos.find[TraceInstructionInfo](
          { case VariableLookedUp(_, _, _) => true; case _ => false},
          { case VariableLookedUp(_, address, value) =>
          val newActionInfo = (ActionReachedValueT[SchemeExp, ConcreteValue, HybridAddress.A](value),
                               infos.filter[VariableLookedUp[ConcreteValue, HybridAddress.A]])
          replaceVariableLookup(address, value, actionInfo, newActionInfo) }).getOrElse(actionInfo)
      case (ActionLookupVariablePushT(_, _, _), infos) =>
        infos.find[TraceInstructionInfo](
          { case VariableLookedUp(_, _, _) => true; case _ => false},
          { case VariableLookedUp(_, address, value) =>
          val newActionInfo = (ActionReachedValuePushT[SchemeExp, ConcreteValue, HybridAddress.A](value),
            infos.filter[VariableLookedUp[ConcreteValue, HybridAddress.A]])
          replaceVariableLookup(address, value, actionInfo, newActionInfo) }).getOrElse(actionInfo)
      case _ =>
        actionInfo
    }

    trace.map(replaceActionInfo)
  }

  private def replaceVariablesWithConstants(traceFull: SpecTraceFull,
                                            allBoundAddresses: Set[HybridAddress.A]): SpecTraceFull = {
    Logger.log(s"Initiating new variable folding optimization; allBoundAddresses = $allBoundAddresses", Logger.D)
    val optimizedTrace = replaceVariableLookups(traceFull.trace, allBoundAddresses)
    createFullTrace(traceFull, optimizedTrace)
  }

  /*********************************************************************************************************************
   *                                         REMOVE CLOSURE GUARDS OPTIMIZATION                                        *
   *********************************************************************************************************************/

  private def removeRedundantClosureGuards(traceFull: SpecTraceFull,
                                           allBoundAddresses: Set[HybridAddress.A]): SpecTraceFull = {
    Logger.log(s"Initiating closure guards removal optimization; allBoundAddresses = $allBoundAddresses", Logger.D)

    /**
      * Checks whether a TraceInstructionInfo puts a constant value in the value register.
      * @param traceInfo The TraceInstructionInfo to be checked
      * @return True iff the TraceInstructionInfo puts a constant value in the value register, false otherwise.
      */
    def isConstant(traceInfo: TraceInstructionInfo): Boolean = {
      val (action, infos) = traceInfo
      action match {
          /* The action puts a literal in the value register. */
        case ActionReachedValueT(_, _, _) | ActionReachedValuePushT(_, _, _) => true
          /* If the action looks up a variable which is actually constant, this action is constant as well. */
        case ActionLookupVariableT(variable, _, _) =>
          infos.find[Boolean]({case VariableLookedUp(_, _, _) => true case _ => false},
                              {case VariableLookedUp(_, address, _) => addressBound(address, allBoundAddresses)}).getOrElse(false)
        case ActionLookupVariablePushT(variable, _, _) =>
          /* If the action looks up a variable which is actually constant, this action is constant as well. */
          infos.find[Boolean]({case VariableLookedUp(_, _, _) => true case _ => false},
                              {case VariableLookedUp(_, address, _) => addressBound(address, allBoundAddresses)}).getOrElse(false)
        /* Creating a closure comes down to creating a constant value.
         * TODO closure environment might cause problems: check what happens if the closure environment changes  */
        case ActionCreateClosureT(_) => true
        case _ => false
      }
    }

    def removeMatchingClosureGuard(traceSoFar: Trace, remainingTrace: Trace): (Trace, Trace) = remainingTrace match {
        case Nil =>
          (traceSoFar, Nil)
        case (tuple@(action, _)) :: rest => action match {
          case ActionGuardSameClosure(_, _, _) =>
            Logger.log(s"Removed closure guard $action", Logger.D)
            (traceSoFar, rest)
          case _ if action.startsFunCallBlock =>
            val (newTraceSoFar, newRest) = inStartsFunCallBlock(traceSoFar :+ tuple, rest, None)
            removeMatchingClosureGuard(newTraceSoFar, newRest)
          case _ =>
            removeMatchingClosureGuard(traceSoFar :+ tuple, rest)
        }
    }

    def findNextFunctionEndBlock(traceSoFar: Trace, remainingTrace: Trace): (Trace, Trace) = remainingTrace match {
      case Nil =>
        (traceSoFar, Nil)
      case (tuple@(action, _)) :: rest =>
        if (action.endsFunCallBlock) {
          (traceSoFar :+ tuple, rest)
        } else if (action.startsFunCallBlock) {
          val (newTraceSoFar, newRest) = inStartsFunCallBlock(traceSoFar :+ tuple, rest, None)
          findNextFunctionEndBlock(newTraceSoFar, newRest)
        } else {
          findNextFunctionEndBlock(traceSoFar :+ tuple, rest)
        }
    }

    /**
      * Takes care of the part of the trace that is within a function call block.
      * @param traceSoFar The ENTIRE trace encountered so far.
      * @param remainingTrace
      * @param changesValueReg The last action, if any has been encountered yet, which changed the value register
      * @return The part of the trace BEFORE the position of the head, and the part ATFER this head.
      */
    def inStartsFunCallBlock(traceSoFar: Trace, remainingTrace: Trace, changesValueReg: Option[TraceInstructionInfo]): (Trace, Trace) = remainingTrace match {
      case Nil =>
        (traceSoFar, Nil)
      case (tuple@(action, infos)) :: rest =>
        if (action.endsFunCallBlock) {
          (traceSoFar :+ tuple, rest)
        //} else if (action.startsFunCallBlock) {
        //  inStartsFunCallBlock(traceSoFar :+ tuple, rest, None)
        } else if (action.changesValueReg) {
          /* We're not looking for any action that changes the value register, but we're looking for the first action
           * that changes the value register and where this value is pushed onto the value stack: this will be the
           * action that corresponds with the evaluation of the operator. */
          if (action.pushesValue && isConstant(tuple)) {
            /* Same action can change value register and immediately push this value. */
            val (newTraceSoFar, newRest) = removeMatchingClosureGuard(traceSoFar :+ tuple, rest)
            findNextFunctionEndBlock(newTraceSoFar, newRest)
          } else {
            /* Keep going forward through the trace until you find the location where this value is pushed onto the stack */
            inStartsFunCallBlock(traceSoFar :+ tuple, rest, Some(tuple))
          }
        } else if (action.pushesValue) {
          changesValueReg match {
            case None =>
              inStartsFunCallBlock(traceSoFar :+ tuple, rest, None)
            case Some(changedValueRegTuple) =>
              if (isConstant(changedValueRegTuple)) {
                /* Closure guard can be removed, since the action is constant */
                removeMatchingClosureGuard(traceSoFar :+ tuple, rest)
              } else {
                /* Action is NOT a constant, so the closure guard cannot be removed. Loop forward through the trace
                 * until you find the corresponding end of the function call block. */
                findNextFunctionEndBlock(remainingTrace :+ tuple, rest)
              }
          }
        } else {
          /* Action does not have special meaning, keep looping through the trace. */
          inStartsFunCallBlock(traceSoFar :+ tuple, rest, None)
        }
    }

    def loop(traceSoFar: Trace, remainingTrace: Trace): Trace = remainingTrace match {
      case Nil =>
        traceSoFar
      case (tuple@(action, infos)) :: rest =>
        if (action.startsFunCallBlock) {
          val (newTraceSoFar, newRest): (Trace, Trace) = inStartsFunCallBlock(traceSoFar :+ tuple, rest, None)
          loop(newTraceSoFar, newRest)
        } else {
          loop(traceSoFar :+ tuple, rest)
        }
    }

    val trace = traceFull.trace
    val optimizedTrace = loop(Nil, trace)
    createFullTrace(traceFull, optimizedTrace)
  }

  /*********************************************************************************************************************
   *                                         DEAD STORE ELIMINATION OPTIMIZATION                                       *
   *********************************************************************************************************************/

//  private def optimizeDeadStoreElimination(traceFull: SpecTraceFull, output: AnalysisOutput): SpecTraceFull = {
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
