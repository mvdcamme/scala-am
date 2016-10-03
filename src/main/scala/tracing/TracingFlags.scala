case class TracingFlags(DO_TRACING: Boolean = true,
                        TRACING_THRESHOLD: Integer = 5,
                        OPTIMIZATION: ShouldApplyOptimization =
                          ApplyAllOptimizations,
                        DO_INITIAL_ANALYSIS: Boolean = false,
                        SWITCH_ABSTRACT: Boolean = false,
                        DO_GUARD_TRACING: Boolean = true)

sealed trait ShouldApplyOptimization

case class ApplySpecificOptimizations(nr: List[Int])
    extends ShouldApplyOptimization
case object ApplyNoOptimizations extends ShouldApplyOptimization
case object ApplyAllOptimizations extends ShouldApplyOptimization
