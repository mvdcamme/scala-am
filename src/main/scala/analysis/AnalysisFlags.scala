case class AnalysisFlags(
    incrementalAnalysisInterval: IncrementalAnalysisInterval = NoIncrementalAnalysis,
    runTimeAnalysisInterval: RunTimeAnalysisInterval = NoRunTimeAnalysis,
    incrementalOptimisation: Boolean = true,
    doPropagationPhase: Boolean = true)

sealed trait IncrementalAnalysisInterval
case object NoIncrementalAnalysis extends IncrementalAnalysisInterval
case class IncrementalAnalysisEvery(interval: Int) extends IncrementalAnalysisInterval

sealed trait RunTimeAnalysisInterval
case object NoRunTimeAnalysis extends RunTimeAnalysisInterval
case class RunTimeAnalysisEvery(interval: Int) extends RunTimeAnalysisInterval
