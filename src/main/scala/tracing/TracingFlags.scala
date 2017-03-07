case class TracingFlags(
    DO_TRACING: Boolean = true,
    TRACING_THRESHOLD: Integer = 5,
    OPTIMIZATION: ShouldApplyOptimization = ApplyAllOptimizations,
    RUNTIME_ANALYSIS_INTERVAL: RunTimeAnalysisInterval = NoRunTimeAnalysis,
    DO_INITIAL_ANALYSIS: Boolean = false,
    SWITCH_ABSTRACT: Boolean = false,
    DO_GUARD_TRACING: Boolean = true)

sealed trait ShouldApplyOptimization

case class ApplySpecificOptimizations(nr: List[Int])
    extends ShouldApplyOptimization
case object ApplyNoOptimizations extends ShouldApplyOptimization
case object ApplyAllOptimizations extends ShouldApplyOptimization

sealed trait RunTimeAnalysisInterval

case object RegularRunTimeAnalysisEveryStep extends RunTimeAnalysisInterval
case object NoRunTimeAnalysis extends RunTimeAnalysisInterval
case class RunTimeAnalysisEvery(interval: Int) extends RunTimeAnalysisInterval
