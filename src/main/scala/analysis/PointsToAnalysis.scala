import java.io.{BufferedWriter, File, FileWriter}

import ConcreteConcreteLattice.{L => ConcreteValue}
import EdgeAnnotation.graphAnnotation

class PointsToAnalysis[Exp: Expression, L: JoinLattice, Addr: Address, Time: Timestamp] {

  private def joinStores[Machine <: KickstartEvalEvalKontMachine[Exp, L, Addr, Time]](machine: Machine)(stores: Set[Store[Addr, L]]): Set[(Addr, L)] = {
    val joinedStore = stores.foldLeft(DeltaStore(Map(), Map()): Store[Addr, L]) {
      case (joinedStore, store) => joinedStore.join(store)
    }
    joinedStore.toSet
  }

  case class MetricsToWrite(max: Int, median: Double, average: Double, sum: Int, nrOfTops: Int)

  private def calculateMetrics(result: List[(Addr, Option[Int])]): MetricsToWrite = {
    val integerValues = result.flatMap(_._2)
    val numberValues = integerValues.map(_.toDouble).sortWith(_ < _)
    val length = numberValues.length
    if (length == 0) {
      MetricsToWrite(0, 0, 0, 0, 0)
    } else {
      val max = integerValues.max
      val sum = integerValues.sum
      val median = if (length % 2 == 0) {
        (numberValues((length / 2) - 1) + numberValues(length / 2)) / 2
      } else {
        numberValues(length / 2)
      }
      val average = numberValues.sum / length
      val nrOfTops = result.map(_._2).count(_.isEmpty)
      MetricsToWrite(max, median, average, sum, nrOfTops)
    }
  }

  private def possiblyWriteMetrics(stepSwitched: Int,
                                   metrics: MetricsToWrite): Unit = {
    val output =
      s"$stepSwitched;${metrics.max};${metrics.median};${metrics.average};${metrics.sum};${metrics.nrOfTops}"
    if (GlobalFlags.ANALYSIS_RESULTS_OUTPUT.isDefined) {
      val file = new File(GlobalFlags.ANALYSIS_RESULTS_OUTPUT.get)
      val bw = new BufferedWriter(new FileWriter(file, true))
      bw.write(output ++ "\n")
      bw.close()
    }
  }

  def analyze[Machine <: ProducesStateGraph[Exp, L, Addr, Time]](
      toDot: Option[String],
      machine: Machine,
      sem: ConvertableSemantics[Exp, L, Addr, Time],
      relevantAddress: Addr => Boolean)(
      startState: machine.InitialState,
      isInitial: Boolean,
      stepSwitched: Option[Int]): StaticAnalysisResult = {
    Logger.log("Starting static points-to analysis", Logger.I)
    val result = machine.kickstartEval(startState, sem, None, Timeout.none, stepSwitched)
    toDot.foreach(result.toFile)
    AnalysisOutputGraph[Exp, L, Addr, machine.MachineState](result)
  }
}

class PointsToAnalysisLauncher[Abs: IsConvertableLattice: LatticeInfoProvider](
    concSem: ConvertableSemantics[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T])
    (implicit analysisFlags: AnalysisFlags)
    extends AnalysisLauncher[Abs] {

  val usesGraph = new UsesGraph[SchemeExp, Abs, HybridAddress.A, aam.State]
  import usesGraph._
  implicit def g: GraphNode[aam.MachineState, Set[aam.MachineState]] = aam.State.graphNode

  val abs = implicitly[IsConvertableLattice[Abs]]
  val lip = implicitly[LatticeInfoProvider[Abs]]

  val pointsToAnalysis = new PointsToAnalysis[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T]
  val countFunCallsMetricsComputer = new CountFunCalls[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T, aam.State]
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

  def runStaticAnalysisGeneric(currentProgramState: PS, stepSwitched: Option[Int],
                               toDotFile: Option[String]): StaticAnalysisResult = {
    wrapRunAnalysis(
      () => {
        val pathConstraint = ScalaAMReporter.pathStorage.getCurrentReport.map(triple => (triple._1, triple._2))
        val startState = convertStateAAM(aam, concSem, abstSem, currentProgramState, pathConstraint)
        val result = pointsToAnalysis.analyze(toDotFile, aam, abstSem, (addr) => ! HybridAddress.isAddress.isPrimitive(addr))(startState, false, stepSwitched)
        Logger.log(s"Static points-to analysis result is $result", Logger.U)
        result
      })
  }

  def runStaticAnalysis(currentProgramState: PS,
                        stepSwitched: Option[Int],
                        addressesUsed: Set[HybridAddress.A]): StaticAnalysisResult = {
    val result = runStaticAnalysisGeneric(currentProgramState, stepSwitched, None)
    result
  }

  private def initializeAnalyses(graph: AbstractGraph, programName: String): Unit = {
    new BufferedWriter(new FileWriter(new File(initialMetricsOutputPath), false))
    runMetrics(graph, -1, initialMetricsOutputPath, initialPointsToOutputPath, programName)
    new BufferedWriter(new FileWriter(new File(incrementalMetricsOutputPath), false))
    new BufferedWriter(new FileWriter(new File(prunedMetricsOutputPath), false))
    new BufferedWriter(new FileWriter(new File(runTimeAnalysisMetricsOutputPath), false))
  }

  def runInitialStaticAnalysis(currentProgramState: PS, programName: String): StaticAnalysisResult =
    runStaticAnalysisGeneric(currentProgramState, None, Some("initial_graph.dot")) match {
      case result: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, aam.MachineState] =>
        GraphDOTOutput.toFile(result.hasGraph.graph, result.hasGraph.halted)("initial_graph.dot")
        initializeAnalyses(result.hasGraph.graph, programName)
        result
      case other => throw new Exception(s"Expected initial analysis to produce a graph, got $other instead")
    }

}
