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
         ActionGuardSameClosure(_, _, _) |
         ActionGuardSamePrimitive(_, _, _) |
         ActionGuardFalseTraced(_) =>
      true
    case _ => false
  }

  private def optimizeEnvironmentLoading(trace : sem.Trace) : sem.Trace = {
    var stack = List[ActionMap]()
    var optimizedTrace : List[ActionMap] = List()

    def handleAction(action : Action[Exp, Abs, Addr]) : Unit = {

      def handleInterferingAction = stack.headOption match {
        case Some(action) => action.isUsed = true
        case None =>
      }

      val actionMap = ActionMap(action, true)
      action match {
        case ActionAllocVarsTraced(_) |
             ActionDefineVarsTraced(_) |
             ActionEndTrace(_) |
             ActionExtendEnvTraced(_)  =>
          handleInterferingAction
        case _ if isGuard(action) =>
          handleInterferingAction
        case ActionSaveEnvTraced() =>
          actionMap.isUsed = false
          stack = actionMap :: stack
        case ActionRestoreEnvTraced() =>
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

  val optimisations : List[(Boolean, (sem.Trace => sem.Trace))] =
    List((APPLY_OPTIMIZATIONS_ENVIRONMENTS_LOADING, optimizeEnvironmentLoading(_)))

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
