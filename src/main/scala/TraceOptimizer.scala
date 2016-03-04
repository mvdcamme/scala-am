import scala.annotation.tailrec

/**
  * Created by mvdcamme on 24/02/16.
  */
class TraceOptimizer[Exp : Expression, Abs, Addr, Time : Timestamp](val sem: SemanticsTraced[Exp, Abs, Addr, Time], val hybridMachine : HybridMachine[Exp, Time]) {

  type ProgramState = HybridMachine[Exp, Time]#ProgramState
  type TraceInstructionStates = HybridMachine[Exp, Time]#TraceInstructionStates
  type TraceInstruction = HybridMachine[Exp, Time]#TraceInstruction
  type Trace = HybridMachine[Exp, Time]#TraceWithStates

  val APPLY_OPTIMIZATION_ENVIRONMENTS_LOADING = true
  val APPLY_OPTIMIZATION_CONTINUATIONS_LOADING = true
  val APPLY_OPTIMIZATION_CONSTANT_FOLDING = true
  val APPLY_OPTIMIZATION_TYPE_SPECIALIZED_ARITHMETICS = true

  type HybridValue = HybridLattice.Hybrid

  val basicOptimisations : List[(Boolean, (Trace => Trace))] =
    List((APPLY_OPTIMIZATION_ENVIRONMENTS_LOADING, optimizeEnvironmentLoading(_)),
      (APPLY_OPTIMIZATION_CONTINUATIONS_LOADING, optimizeContinuationLoading(_)))

  val detailedOptimisations : List[(Boolean, (Trace => Trace))] =
    List((APPLY_OPTIMIZATION_CONSTANT_FOLDING, optimizeConstantFolding(_)),
         (APPLY_OPTIMIZATION_TYPE_SPECIALIZED_ARITHMETICS, { (tr : Trace) => println(tr); optimizeTypeSpecialization(tr)} ))

  def foldOptimisations(trace : Trace, optimisations : List[(Boolean, (Trace => Trace))]) : Trace = {
    optimisations.foldLeft(trace)({ (trace, pair) => if (pair._1) { pair._2(trace) } else { trace }})
  }

  def optimize(trace : Trace) : Trace = {
    println(s"Size of unoptimized trace = ${trace.length}")
    if (TracerFlags.APPLY_OPTIMIZATIONS) {
      val basicOptimizedTrace = foldOptimisations(trace, basicOptimisations)
      println(s"Size of basic optimized trace = ${basicOptimizedTrace.length}")
      val tier2OptimizedTrace = if (TracerFlags.APPLY_DETAILED_OPTIMIZATIONS) {
        val detailedOptimizedTrace = foldOptimisations(basicOptimizedTrace, detailedOptimisations)
        println(s"Size of detailed optimized trace = ${detailedOptimizedTrace.length}")
        detailedOptimizedTrace
      } else {
        basicOptimizedTrace
      }
      val finalOptimizedTrace = removeFunCallBlockActions(tier2OptimizedTrace)
      println(s"Size of final optimized trace = ${finalOptimizedTrace.length}")
      finalOptimizedTrace
    } else {
      trace
    }
  }

  /********************************************************************************************************************
   *                                                 COMMON FUNCTIONS                                                 *
   ********************************************************************************************************************/

  def isGuard(action : TraceInstruction) : Boolean = action match {
    case ActionGuardFalseTraced(_) |
         ActionGuardTrueTraced(_) |
         ActionGuardSameClosure(_, _) |
         ActionGuardSamePrimitive(_, _) |
         ActionGuardFalseTraced(_) =>
      true
    case _ => false
  }

  private case class ActionStateMap(actionState : TraceInstructionStates, var isUsed : Boolean)

  private def removeMatchingActions(trace : Trace, isAPushingAction : TraceInstruction => Boolean,
                                    isAPoppingAction : TraceInstruction => Boolean, isAnInterferingAction : TraceInstruction => Boolean) : Trace = {

    var stack = List[ActionStateMap]()
    var optimizedTrace : List[ActionStateMap] = List()

    def handleAction(actionState : TraceInstructionStates) : Unit = {

      val actionStateMap = ActionStateMap(actionState, true)
      actionState._1 match {
        case _ if isAnInterferingAction(actionState._1)  =>
          stack.headOption match {
            case Some(action) => action.isUsed = true
            case None =>
          }
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

  /********************************************************************************************************************
   *                                         ENVIRONMENT LOADING OPTIMIZATION                                         *
   ********************************************************************************************************************/

  private def optimizeEnvironmentLoading(trace : Trace) : Trace = {
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
    removeMatchingActions(trace, _.isInstanceOf[ActionSaveEnvTraced[Exp, Abs, Addr]],
                          _.isInstanceOf[ActionRestoreEnvTraced[Exp, Abs, Addr]], isAnInterferingAction)
  }

  /********************************************************************************************************************
   *                                        CONTINUATION LOADING OPTIMIZATION                                         *
   ********************************************************************************************************************/

  private def optimizeContinuationLoading(trace : Trace) : Trace = {
    def isAnInterferingAction(action : TraceInstruction) : Boolean = action match {
      case ActionEndTrace(_) =>
        true
      case _ if isGuard(action) =>
        true
      case _ =>
        false
    }
    removeMatchingActions(trace, _.isInstanceOf[ActionPushTraced[Exp, Abs, Addr]],
                          _.isInstanceOf[ActionPopKontTraced[Exp, Abs, Addr]], isAnInterferingAction)
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

  private def doDifficultStuff(firstPart : Trace, trace : Trace) : Option[Trace] = {
    findNextEndPrimCall(trace) match {
      case Some((traceBefore, traceAtPrimCall)) =>
       findNextStartFunCall(traceAtPrimCall.tail) match {
         case Some((traceBetweenMarks, traceAtStartCall)) =>
           val optimizedBlocks = filterAllOptimizedBlocks(traceBetweenMarks)
           val actionStatePrimCall = traceBetweenMarks.find(_._1.isInstanceOf[ActionPrimCallTraced[Exp, Abs, Addr]])
           actionStatePrimCall match {
             case Some((ActionPrimCallTraced(n, _, _), state)) =>
               val result = state.get.v
               val x = findNextEndPrimCall(traceBetweenMarks)
               x match {
                 /* Another primitive is applied in this block */
                 case Some((_, betweenInnerEndPrimCallAndOuterStartMark)) =>
                   val betweenOuterPrimCallAndInnerEndPrimCall = findNextEndPrimCall(traceBetweenMarks).get._1
                   val newFirstPart = firstPart ++ (traceBefore :+ traceAtPrimCall.head) ++ betweenOuterPrimCallAndInnerEndPrimCall
                   val newTrace = betweenInnerEndPrimCallAndOuterStartMark ++ traceAtStartCall
                   doDifficultStuff(newFirstPart, newTrace)
                 case None =>
                   checkPrimitive(traceBetweenMarks, n).flatMap({ (traceAfterOperatorPush) =>
                     //val guard = (ActionGuardSamePrimitive(), None)
                     val replacingConstantAction : TraceInstructionStates = (ActionReachedValueTraced[Exp, HybridValue, HybridAddress](result), None)
                     val actionEndOptimizedBlock = (ActionEndOptimizedBlock[Exp, HybridValue, HybridAddress], None)
                     val actionStartOptimizedBlock = (ActionStartOptimizedBlock[Exp, HybridValue, HybridAddress], None)
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

  private def optimizeConstantFolding(trace: Trace) : Trace = {
    def loop(trace : Trace) : Trace = {
      doDifficultStuff(List(), trace) match {
        case Some(updatedTrace) =>
          loop(updatedTrace)
        case None =>
          trace
      }
    }
    loop(trace.reverse).reverse
  }

  /********************************************************************************************************************
   *                                          TYPE SPECIALIZATION OPTIMIZATION                                        *
   ********************************************************************************************************************/

  object OperandsTypes extends Enumeration {
    type OperandsTypes = Value
    val Bottom = Value("Bottom")
    val AllFloats = Value("Floats")
    val AllIntegers = Value("Integers")
    val Top = Value("Top")
  }

  private def getOperandType(operand : HybridValue) : OperandsTypes.Value = operand match {
    case HybridLattice.Left(AbstractConcrete.AbstractFloat(_)) => OperandsTypes.AllFloats
    case HybridLattice.Left(AbstractConcrete.AbstractInt(_)) => OperandsTypes.AllIntegers
    case _ => OperandsTypes.Top
  }

  private def checkOperandsTypes(operands : List[HybridValue]) : OperandsTypes.Value = {
    operands.foldLeft(OperandsTypes.Bottom)({ (operandsTypes, operand) =>
      if (operandsTypes == OperandsTypes.Bottom) {
        getOperandType(operand)
      } else if (operandsTypes == getOperandType(operand)) {
        operandsTypes
      } else {
        OperandsTypes.Top
      }
    })
  }

  val primitives = hybridMachine.primitives

  private def typeSpecializePrimitive(prim: Primitive[HybridAddress, HybridValue], operandsTypes: OperandsTypes.Value) : Primitive[HybridAddress, HybridValue] = prim match {
    case primitives.Plus => operandsTypes match {
      case OperandsTypes.AllFloats => println(s"Replacing Plus by PlusFloat"); primitives.PlusFloat
      case OperandsTypes.AllIntegers => println(s"Replacing Plus by PlusInteger"); primitives.PlusInteger
      case _ => println(s"Couldn't replace Plus"); prim
    }
    case primitives.Minus => operandsTypes match {
      case OperandsTypes.AllFloats => println(s"Replacing Minus by MinusFloat"); primitives.MinusFloat
      case OperandsTypes.AllIntegers => println(s"Replacing Minus by MinusInteger"); primitives.MinusInteger
      case _ => println(s"Couldn't replace Minus"); prim
    }
    case _ => println(s"Couldn't replace $prim"); prim
  }

  private def optimizeTypeSpecialization(trace : Trace) : Trace = {
    trace match {
      case Nil =>
        Nil
      case (actionState1@(_, someState)) :: (actionState2@(ActionPrimCallTraced(n, fExp, argsExps), _)) :: rest => someState match {
        case Some(state) =>
          val operands = state.vStack.take(n - 1)
          val operator = state.vStack(n - 1)
          val operandsTypes = checkOperandsTypes(operands.map(_.left.get))
          val specializedOperator = operator.left.get match {
            case prim: HybridLattice.Prim[HybridAddress, HybridValue] => prim match {
              case HybridLattice.Prim(primitive) => primitive match {
                case primitive: Primitive[HybridAddress, HybridValue] =>
                  println("here")
                  val specializedPrim = typeSpecializePrimitive(primitive, operandsTypes)
                  HybridLattice.Prim(specializedPrim)
              }
            }
          }
          val specializedPrimCallAction = ActionSpecializePrimitive[Exp, HybridValue, HybridAddress](specializedOperator, n, fExp, argsExps)
          actionState1 :: (specializedPrimCallAction, actionState2._2) :: optimizeTypeSpecialization(rest)
        /* Since the state before applying the function was not recorded, we cannot know what the types of the operands were */
        case None =>
          actionState1 :: actionState2 :: optimizeTypeSpecialization(rest)
      }
      case action :: rest =>
        action :: optimizeTypeSpecialization(rest)
    }
  }

  /********************************************************************************************************************
   *                                       FUNCALL BLOCK FILTERING OPTIMIZATION                                       *
   ********************************************************************************************************************/

  def removeFunCallBlockActions(trace : Trace) : Trace =
    trace.filter({
      case (ActionEndClosureCallTraced(), _) => false
      case (ActionEndOptimizedBlock(), _) => false
      case (ActionEndPrimCallTraced(), _) => false
      case (ActionStartFunCallTraced(), _) => false
      case (ActionStartOptimizedBlock(), _) => false
      case (_, _) => true})

}
