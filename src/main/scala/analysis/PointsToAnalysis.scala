import java.io.{BufferedWriter, File, FileWriter}

import backend.PathConstraint

class PointsToAnalysisLauncher[Abs: IsConvertableLattice: LatticeInfoProvider](
    abstSem: ConvertableBaseSchemeSemantics[Abs, HybridAddress.A, HybridTimestamp.T])
    (implicit analysisFlags: AnalysisFlags)
    extends AnalysisLauncher[Abs](abstSem) {

  val usesGraph = new UsesGraph[SchemeExp, Abs, HybridAddress.A, SpecState]
  import usesGraph._
  implicit def g: GraphNode[SpecState, Set[SpecState]] = KickstartAAMGlobalStoreState.graphNode[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T]

  val abs = implicitly[IsConvertableLattice[Abs]]
  val lip = implicitly[LatticeInfoProvider[Abs]]

  val countFunCallsMetricsComputer = new CountFunCalls[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T, SpecState]
//  val countNonConstantsMetricsComputer = new CountNonConstants[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T, aam.State](lip.pointsTo)

  val incrementalMetricsOutputPath = s"${GlobalFlags.ANALYSIS_PATH}Closures_Points_To/incremental.txt"
  val initialMetricsOutputPath = s"${GlobalFlags.ANALYSIS_PATH}Closures_Points_To/initial_analysis.txt"
  val prunedMetricsOutputPath = s"${GlobalFlags.ANALYSIS_PATH}Closures_Points_To/pruned.txt"
  val runTimeAnalysisMetricsOutputPath = s"${GlobalFlags.ANALYSIS_PATH}Closures_Points_To/run_time.txt"

  val incrementalPointsToOutputPath =s"${GlobalFlags.ANALYSIS_PATH}/Points_To/Incremental/"
  val initialPointsToOutputPath =s"${GlobalFlags.ANALYSIS_PATH}/Points_To/Initial_analysis/"
  val runTimePointsToOutputPath =s"${GlobalFlags.ANALYSIS_PATH}/Points_To/Run_time/"
  val prunedPointsToOutputPath =s"${GlobalFlags.ANALYSIS_PATH}/Points_To/Pruned/"

  private def runMetrics(graph: AbstractGraph, stepCount: Integer, funCallsMetricPath: String, pointsToMetricPath: String,
                         programName:String, addressesUsed: Set[HybridAddress.A] = Set()): Unit = {
    Stopwatch.doPaused({
      countFunCallsMetricsComputer.computeAndWriteMetrics(graph, stepCount, funCallsMetricPath, programName)
//      countNonConstantsMetricsComputer.computeAndWriteMetrics(graph, stepCount, pointsToMetricPath, programName, addressesUsed)
    })
  }

  def analyze(toDot: Option[String], sem: ConvertableSemantics[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T],
              startState: aam.InitialState, isInitial: Boolean, stepSwitched: Option[Int]): aam.AAMOutput = {
    Logger.log("Starting static points-to analysis", Logger.I)
    val result = aam.kickstartEval(startState, sem, None, Timeout.none, stepSwitched)
    toDot.foreach(result.toFile)
    result
  }

  def runStaticAnalysisGeneric(startState: stateConverter.aam.InitialState, stepSwitched: Option[Int], toDotFile: Option[String],
                               pathConstraint: PathConstraint, isInitialAnalysis: Boolean): AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, SpecState] = {
    wrapRunAnalysis(
      () => {
        val result = analyze(toDotFile, abstSem, startState.asInstanceOf[aam.InitialState], isInitialAnalysis, stepSwitched)
        Logger.log(s"Static points-to analysis result is $result", Logger.U)
        /* The .asInstanceOf isn't actually required (the code also compiles without the cast), but Intellij seems unable to correctly infer the type */
        result.asInstanceOf[AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, SpecState]]
      })
  }

  def runStaticAnalysis(initialAbstractState: stateConverter.aam.InitialState, stepSwitched: Option[Int], addressesUsed: Set[HybridAddress.A],
                        pathConstraint: PathConstraint): AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, SpecState] = {
    val result = runStaticAnalysisGeneric(initialAbstractState, stepSwitched, None, pathConstraint, false)
    result
  }

  private def initializeAnalyses(graph: AbstractGraph, programName: String): Unit = {
    new BufferedWriter(new FileWriter(new File(initialMetricsOutputPath), false))
    runMetrics(graph, -1, initialMetricsOutputPath, initialPointsToOutputPath, programName)
    new BufferedWriter(new FileWriter(new File(incrementalMetricsOutputPath), false))
    new BufferedWriter(new FileWriter(new File(prunedMetricsOutputPath), false))
    new BufferedWriter(new FileWriter(new File(runTimeAnalysisMetricsOutputPath), false))
  }

  def runInitialStaticAnalysis(initialAbstractState: stateConverter.aam.InitialState, programName: String): AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, SpecState] =
    runStaticAnalysisGeneric(initialAbstractState, None, Some("initial_graph.dot"), Nil, true) match {
      case result: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, aam.MachineState] =>
        GraphDOTOutput.toFile(result.graph, result.halted)("initial_graph.dot")
        initializeAnalyses(result.graph, programName)
        result
      case other => throw new Exception(s"Expected initial analysis to produce a graph, got $other instead")
    }

}
