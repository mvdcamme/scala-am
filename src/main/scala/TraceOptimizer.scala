/**
  * Created by mvdcamme on 24/02/16.
  */
class TraceOptimizer[Exp, Abs, Addr, Time](val sem: SemanticsTraced[Exp, Abs, Addr, Time]) {

  type ProgramState = HybridMachine[Exp, Time]#ProgramState
  type TraceInstruction = HybridMachine[Exp, Time]#TraceInstruction
  type Trace = HybridMachine[Exp, Time]#TraceWithStates

  val APPLY_OPTIMIZATIONS_ENVIRONMENTS_LOADING = true
  val APPLY_OPTIMIZATIONS_CONTINUATIONS_LOADING = true

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

  private case class ActionStateMap(actionState : (TraceInstruction, Option[ProgramState]), var isUsed : Boolean)

  private def removeMatchingActions(trace : Trace, isAPushingAction : TraceInstruction => Boolean,
                                    isAPoppingAction : TraceInstruction => Boolean, isAnInterferingAction : TraceInstruction => Boolean) : Trace = {

    var stack = List[ActionStateMap]()
    var optimizedTrace : List[ActionStateMap] = List()

    def handleAction(actionState : (TraceInstruction, Option[ProgramState])) : Unit = {

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

  val optimisations : List[(Boolean, (Trace => Trace))] =
    List((APPLY_OPTIMIZATIONS_ENVIRONMENTS_LOADING, optimizeEnvironmentLoading(_)),
         (APPLY_OPTIMIZATIONS_CONTINUATIONS_LOADING, optimizeContinuationLoading(_)))

  def optimize(trace : Trace) : Trace = {
    println(s"Size of unoptimized trace = ${trace.length}")
      val optimizedTrace = optimisations.foldLeft(trace)({ (trace, pair) => if (pair._1) { pair._2(trace) } else { trace }})
      println(s"Size of optimized trace = ${optimizedTrace.length}")
      optimizedTrace
    if (TracerFlags.APPLY_OPTIMIZATIONS) {
    } else {
      trace
    }
  }

}
