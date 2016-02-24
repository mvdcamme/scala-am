/**
  * Created by mvdcamme on 24/02/16.
  */
class TraceOptimizer[Exp, Abs, Addr, Time](val sem: SemanticsTraced[Exp, Abs, Addr, Time]) {

  val APPLY_OPTIMIZATIONS = true
  val APPLY_OPTIMIZATIONS_ENVIRONMENTS_LOADING = true

  type HybridValue = HybridLattice.Hybrid

  private case class ActionMap(action : Action[Exp, HybridValue, HybridAddress], isUsed : Boolean)

  private def optimizeEnvironmentLoading(trace : sem.Trace) : sem.Trace = trace

  def optimize(trace : sem.Trace) : sem.Trace = {
    if (APPLY_OPTIMIZATIONS) {
      val environmentsLoadingOptimizedTrace =
        if (APPLY_OPTIMIZATIONS_ENVIRONMENTS_LOADING) {
          optimizeEnvironmentLoading(trace)
        } else {
          trace
        }
      environmentsLoadingOptimizedTrace
    } else {
      trace
    }
  }

}
