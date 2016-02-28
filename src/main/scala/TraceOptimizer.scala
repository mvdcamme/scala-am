/**
  * Created by mvdcamme on 24/02/16.
  */
class TraceOptimizer[Exp : Expression, Abs, Addr, Time : Timestamp](val sem: SemanticsTraced[Exp, Abs, Addr, Time]) {

  type ProgramState = HybridMachine[Exp, Time]#ProgramState
  type TraceInstructionStates = HybridMachine[Exp, Time]#TraceInstructionStates
  type TraceInstruction = HybridMachine[Exp, Time]#TraceInstruction
  type Trace = HybridMachine[Exp, Time]#TraceWithStates

  val APPLY_OPTIMIZATION_ENVIRONMENTS_LOADING = true
  val APPLY_OPTIMIZATION_CONTINUATIONS_LOADING = true
  val APPLY_OPTIMIZATION_CONSTANT_FOLDING = true

  type HybridValue = HybridLattice.Hybrid

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

  private def findNextPushVal(trace : Trace) : Option[Trace] = {
    val updatedTrace = trace.dropWhile({case (ActionPushValTraced(), _) => false
                                        case _ => true})
    if (updatedTrace.isEmpty) {
      None
    } else {
      Some(updatedTrace.tail)
    }
  }

  private def findNextPrimCall(trace : Trace) : Option[(Trace, Trace, HybridValue, Integer)] = {
    val (traceBefore, traceAfterPrimCall) =
      trace.span({case (ActionPrimCallTraced(_, _, _), Some(_)) => false
                  case _ => true})
    if (traceAfterPrimCall.isEmpty) {
      None
    } else {
      val action = traceAfterPrimCall.head._1
      val state = traceAfterPrimCall.head._2
      println(s"Found state $state")
      action match {
        case ActionPrimCallTraced(n, _, _) => Some((traceBefore, traceAfterPrimCall.tail, state.get.v, n))
      }
    }
  }

  private def checkPrimitive(trace : Trace, result : HybridValue, n : Integer) : Option[Trace] = {
    println(s"Checking whether primitive application only uses ${n - 1} constants")
    1.to(n - 1).foldLeft(Some(trace) : Option[Trace])({ (previousResult : Option[Trace], x) =>
      previousResult.flatMap({ currentTrace =>
        val pushFound = findNextPushVal(currentTrace)
        pushFound.flatMap({ updatedTrace =>
          if (updatedTrace.head._1.isInstanceOf[ActionReachedValueTraced[Exp, HybridValue, HybridAddress]]) {
            Some(updatedTrace.tail)
          } else {
            println(s"Does not work because uses action ${updatedTrace.head._1}")
            None
          }
        })
      })
    })
  }

  /*
  /* One of the operands in the primitive application is not a constant */

   */

  private def doDifficultStuff(trace : Trace) : Trace = {
    val originalTrace = trace
    findNextPrimCall(trace) match {
      case Some((traceBefore, traceAfterPrimCall, result, n)) =>
        val onlyUsesConstants = checkPrimitive(traceAfterPrimCall, result, n)
        onlyUsesConstants match {
          case Some(traceAfterLastConstant) =>
            println(s"Suitable primitive application found: $result")
            val dropOperatorPushAction : TraceInstructionStates = (ActionDropValsTraced(1), None)
            val replacingConstantAction : TraceInstructionStates = (ActionReachedValueTraced[Exp, HybridValue, HybridAddress](result), None)
            val newTrace = (traceBefore:+ replacingConstantAction :+ dropOperatorPushAction) ++ traceAfterLastConstant
            newTrace
          case None =>
            if (traceAfterPrimCall.isEmpty) {
              println("Can't go any further back in the trace")
              originalTrace
            } else {
              println("Going further back in the trace")
              doDifficultStuff(traceAfterPrimCall.tail)
            }
        }
      /* Absolutely no primitive is applied in the given trace  */
      case None => originalTrace
    }
  }

  private def optimizeConstantFolding(trace: Trace) : Trace = {
    doDifficultStuff(trace.reverse).reverse
  }

  val basicOptimisations : List[(Boolean, (Trace => Trace))] =
    List((APPLY_OPTIMIZATION_ENVIRONMENTS_LOADING, optimizeEnvironmentLoading(_)),
         (APPLY_OPTIMIZATION_CONTINUATIONS_LOADING, optimizeContinuationLoading(_)))

  val detailedOptimisations : List[(Boolean, (Trace => Trace))] =
    List((APPLY_OPTIMIZATION_CONSTANT_FOLDING, optimizeConstantFolding(_)))

  def foldOptimisations(trace : Trace, optimisations : List[(Boolean, (Trace => Trace))]) : Trace = {
    optimisations.foldLeft(trace)({ (trace, pair) => if (pair._1) { pair._2(trace) } else { trace }})
  }

  def optimize(trace : Trace) : Trace = {
    println(s"Size of unoptimized trace = ${trace.length}")
    if (TracerFlags.APPLY_OPTIMIZATIONS) {
      val basicOptimizedTrace = foldOptimisations(trace, basicOptimisations)
      println(s"Size of basic optimized trace = ${basicOptimizedTrace.length}")
      val detailedOptimizedTrace = foldOptimisations(basicOptimizedTrace, detailedOptimisations)
      println(s"Size of detailed optimized trace = ${detailedOptimizedTrace.length}")
      detailedOptimizedTrace
    } else {
      trace
    }
  }

}
