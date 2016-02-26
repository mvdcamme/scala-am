/**
  * Created by mvdcamme on 24/02/16.
  */
class TraceOptimizer[Exp, Abs, Addr, Time](val sem: SemanticsTraced[Exp, Abs, Addr, Time]) {

  val APPLY_OPTIMIZATIONS = true
  val APPLY_OPTIMIZATIONS_ENVIRONMENTS_LOADING = true
  val APPLY_OPTIMIZATIONS_CONTINUATIONS_LOADING = true

  type HybridValue = HybridLattice.Hybrid

  private case class ActionMap(action : Action[Exp, Abs, Addr], var isUsed : Boolean)

  def isGuard(action : Action[Exp, Abs, Addr]) : Boolean = action match {
    case ActionGuardFalseTraced(_) |
         ActionGuardTrueTraced(_) |
         ActionGuardSameClosure(_, _) |
         ActionGuardSamePrimitive(_, _) |
         ActionGuardFalseTraced(_) =>
      true
    case _ => false
  }

  private def removeMatchingActions(trace : sem.Trace, isAPushingAction : Action[Exp, Abs, Addr] => Boolean,
                                    isAPoppingAction : Action[Exp, Abs, Addr] => Boolean, isAnInterferingAction : Action[Exp, Abs, Addr] => Boolean) : sem.Trace = {
    var stack = List[ActionMap]()
    var optimizedTrace : List[ActionMap] = List()

    def handleAction(action : Action[Exp, Abs, Addr]) : Unit = {

      val actionMap = ActionMap(action, true)
      action match {
        case _ if isAnInterferingAction(action)  =>
          stack.headOption match {
            case Some(action) => action.isUsed = true
            case None =>
          }
        case _ if isAPushingAction(action) =>
          actionMap.isUsed = false
          stack = actionMap :: stack
        case _ if isAPoppingAction(action) =>
          stack.headOption match {
            case Some(action) =>
              actionMap.isUsed = action.isUsed
              stack = stack.tail
            case None =>
          }
        case _ =>
      }
      optimizedTrace = optimizedTrace :+ actionMap
    }
    trace.foreach(handleAction(_))
    optimizedTrace.filter(_.isUsed).map(_.action)
  }

  private def optimizeEnvironmentLoading(trace : sem.Trace) : sem.Trace = {
    def isAnInterferingAction(action : Action[Exp, Abs, Addr]) = action match {
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

  private def optimizeContinuationLoading(trace : sem.Trace) : sem.Trace = {
    def isAnInterferingAction(action : Action[Exp, Abs, Addr]) = action match {
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

  val optimisations : List[(Boolean, (sem.Trace => sem.Trace))] =
    List((APPLY_OPTIMIZATIONS_ENVIRONMENTS_LOADING, optimizeEnvironmentLoading(_)),
         (APPLY_OPTIMIZATIONS_CONTINUATIONS_LOADING, optimizeContinuationLoading(_)))

  def optimize(trace : sem.Trace) : sem.Trace = {
    println(s"Size of unoptimized trace = ${trace.length}")
    if (APPLY_OPTIMIZATIONS) {
      val optimizedTrace = optimisations.foldLeft(trace)({ (trace, pair) => if (pair._1) { pair._2(trace) } else { trace }})
      println(s"Size of optimized trace = ${optimizedTrace.length}")
      optimizedTrace
    } else {
      trace
    }
  }

}
