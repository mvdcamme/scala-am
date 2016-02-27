/**
  * Created by mvdcamme on 24/02/16.
  */
class TraceOptimizer[Exp, Abs, Addr, Time](val sem: SemanticsTraced[Exp, Abs, Addr, Time]) {

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

  private def findNextPushVal(it : Iterator[TraceInstructionStates]) : Boolean = {
    it.find({case (ActionPushValTraced(), _) => true
             case _ => false})
    it.hasNext
  }

  private def findNextPrimCall(it : Iterator[TraceInstructionStates]) : Option[(HybridValue, Integer)] = {
    it.find({case (ActionPrimCallTraced(_, _, _), Some(_)) => true
             case _ => false}) match {
      case Some((action, state)) => println(s"Found state $state"); action match {
        case ActionPrimCallTraced(n, _, _) => Some((state.get.v, n))
      }
      case None => None
    }
  }

  private def checkPrimitive(it : Iterator[TraceInstructionStates], result : HybridValue, n : Integer) : Boolean = {
    1.to(n - 1).foldLeft(true)({ (previousResult, _) =>
      val pushFound = findNextPushVal(it)
      previousResult && pushFound && it.next._1.isInstanceOf[ActionReachedValueTraced[Exp, HybridValue, HybridAddress]]
    })
  }

  private def doDifficultStuff(trace : Trace, it : Iterator[TraceInstructionStates]) : Trace = {
    findNextPrimCall(it) match {
      case Some((result, n)) =>
        val onlyUsesConstants = checkPrimitive(it, result, n)
        if (onlyUsesConstants) {
          println(s"Suitable primitive application found: $result")
          trace
        } else {
          println("No suitable primitive application found")
          if (it.hasNext) {
            println("Going further back in the trace")
            doDifficultStuff(trace, it)
          } else {
            println("Can't go any further back in the trace")
            trace
          }
        }
      /* Absolutely no primitive is applied in the given trace  */
      case None => trace
    }
  }

  private def optimizeConstantFolding(trace: Trace) : Trace = {
    val it = trace.reverse.toIterator
    doDifficultStuff(trace, it)
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
