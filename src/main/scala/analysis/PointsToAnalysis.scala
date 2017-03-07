import java.io.{FileWriter, BufferedWriter, File}

import ConcreteConcreteLattice.ConcreteValue

class PointsToAnalysis[Exp: Expression, L: JoinLattice, Addr: Address, Time: Timestamp] {

  private def joinStores[
      Machine <: KickstartEvalEvalKontMachine[Exp, L, Addr, Time]](
      machine: Machine)(stores: Set[Store[Addr, L]]): Set[(Addr, L)] = {
    val joinedStore = stores.foldLeft(Store.initial(Set()): Store[Addr, L]) {
      case (joinedStore, store) => joinedStore.join(store)
    }
    joinedStore.toSet
  }

  case class MetricsToWrite(max: Int,
                            median: Double,
                            average: Double,
                            sum: Int,
                            nrOfTops: Int)

  private def calculateMetrics(
      result: List[(Addr, Option[Int])]): MetricsToWrite = {
    val integerValues = result.map(_._2).filter(_.isDefined).map(_.get)
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

  private def analyzeOutput[
      Machine <: KickstartEvalEvalKontMachine[Exp, L, Addr, Time]](
      machine: Machine,
      pointsTo: L => Option[Int],
      relevantAddress: Addr => Boolean)(
      output: Machine#MachineOutput): List[(Addr, Option[Int])] = {
    val storeValues = joinStores(machine)(output.finalStores)
    val initial: List[(Addr, Option[Int])] = Nil
    val result: List[(Addr, Option[Int])] = storeValues.foldLeft(initial)({
      case (result, (address, value)) =>
        val numberOfObjectsPointedTo = pointsTo(value)
        if (relevantAddress(address) && numberOfObjectsPointedTo
              .getOrElse(1) > 0) {
          /* List all addresses pointing to more than one value */
          (address, numberOfObjectsPointedTo) :: result
        } else {
          result
        }
    })
    val metrics = calculateMetrics(result)
    Logger.log(
      s"Static points-to analysis completed:\n" +
      s"resulting value is ${output.finalValues}\n" +
      s"resulting set equals $result\n" +
      s"metrics equals $metrics\n",
      Logger.E)
    possiblyWriteMetrics(output.stepSwitched.getOrElse(-1), metrics)
    result
  }

  def analyze[Machine <: ProducesStateGraph[Exp, L, Addr, Time]](
      toDot: Option[String],
      machine: Machine,
      sem: Semantics[Exp, L, Addr, Time],
      pointsTo: L => Option[Int],
      relevantAddress: Addr => Boolean)(
      startState: machine.MachineState,
      isInitial: Boolean,
      stepSwitched: Option[Int]): StaticAnalysisResult = {
    Logger.log(s"Starting static points-to analysis", Logger.E)
    val result = machine.kickstartEval(startState, sem, None, None, stepSwitched)
    toDot.foreach(result.toDotFile)
    analyzeOutput(machine, pointsTo, relevantAddress)(result)
    AnalysisOutputGraph[Exp, L, Addr, machine.GraphNode](result)
  }
}

class PointsToAnalysisLauncher[
    Abs: IsConvertableLattice: PointsToableLatticeInfoProvider](
    concSem: ConvertableSemantics[SchemeExp,
                                  ConcreteConcreteLattice.L,
                                  HybridAddress.A,
                                  HybridTimestamp.T])
    extends AnalysisLauncher[Abs] {

  val aam: SpecAAM = new SpecAAM()
  implicit val stateDescriptor = new aam.StateDescriptor()

  val usesGraph = new UsesGraph[SchemeExp, Abs, HybridAddress.A, aam.State]
  import usesGraph._

  implicit val stateChangeEdgeApplier = aam.ActionReplayApplier
  val incrementalAnalysis = new IncrementalPointsToAnalysis[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T, aam.GraphNode](aam.AAMGraphPrinter)

  val abs = implicitly[IsConvertableLattice[Abs]]
  val lip = implicitly[PointsToableLatticeInfoProvider[Abs]]

  val pointsToAnalysis = new PointsToAnalysis[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T]
  val metricsComputer = new CountClosureCalls[SchemeExp, Abs, HybridAddress.A, aam.State]

  val incrementalMetricsOutputPath = s"${GlobalFlags.ANALYSIS_PATH}Closures_Points_To/incremental.txt"
  val initialMetricsOutputPath = s"${GlobalFlags.ANALYSIS_PATH}Closures_Points_To/initial_analysis.txt"
  val prunedMetricsOutputPath = s"${GlobalFlags.ANALYSIS_PATH}Closures_Points_To/pruned.txt"
  val runTimeAnalysisMetricsOutputPath = s"${GlobalFlags.ANALYSIS_PATH}Closures_Points_To/run_time.txt"

  def runStaticAnalysisGeneric(
      currentProgramState: PS,
      stepSwitched: Option[Int],
      toDotFile: Option[String]): StaticAnalysisResult = {
    wrapRunAnalysis(
      () => {
        val startState = convertStateAAM(aam, concSem, abstSem, currentProgramState)
        val result =
          pointsToAnalysis.analyze(toDotFile,
                                   aam,
                                   abstSem,
                                   lip.pointsTo,
                                   (addr) =>
                                     !HybridAddress.isAddress.isPrimitive(
                                       addr))(startState, false, stepSwitched)
        Logger.log(s"Static points-to analysis result is $result", Logger.D)
        result
      })
  }

  def runStaticAnalysis(currentProgramState: PS,
                        stepSwitched: Option[Int]): StaticAnalysisResult = {
    assert(incrementalAnalysis.hasInitialGraph)
    Stopwatch.doTimed(runStaticAnalysisGeneric(currentProgramState, stepSwitched, None))
  }

  private def initializeAnalyses(graph: AbstractGraph): Unit = {
    new BufferedWriter(new FileWriter(new File(initialMetricsOutputPath), false))
    metricsComputer.computeAndWriteMetrics(graph, -1, initialMetricsOutputPath)
    new BufferedWriter(new FileWriter(new File(incrementalMetricsOutputPath), false))
    new BufferedWriter(new FileWriter(new File(prunedMetricsOutputPath), false))
    new BufferedWriter(new FileWriter(new File(runTimeAnalysisMetricsOutputPath), false))
  }

  def runInitialStaticAnalysis(currentProgramState: PS): Unit =
    runStaticAnalysisGeneric(currentProgramState,
                             None,
                             Some("initial_graph.dot")) match {
      case result: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, aam.GraphNode] =>
        result.output.toDotFile("initial_graph.dot")
        incrementalAnalysis.initializeGraph(result.output.graph)
        initializeAnalyses(result.output.graph)
      case other =>
        throw new Exception(s"Expected initial analysis to produce a graph, got $other instead")
    }

  def doConcreteStep(convertValue: SchemePrimitives[HybridAddress.A, Abs] => ConcreteConcreteLattice.L => Abs,
                     convertFrame: (ConvertableSemantics[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T],
                                    BaseSchemeSemantics[Abs, HybridAddress.A, HybridTimestamp.T],
                                    ConcreteConcreteLattice.L => Abs) => SchemeFrame[ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]
                       => SchemeFrame[Abs, HybridAddress.A, HybridTimestamp.T],
                     filters: FilterAnnotations[SchemeExp, ConcreteValue, HybridAddress.A],
                     stepNumber: Int) = {
    val convertValueFun = convertValue(abstSem.primitives)
    incrementalAnalysis.computeSuccNodes(convertFrame(concSem, abstSem, convertValueFun), filters, stepNumber)
  }

  def end(): Unit = incrementalAnalysis.end()

  def filterReachable(stepCount: Int): Unit = {
    val optionPrunedGraph = incrementalAnalysis.filterReachable(stepCount)
    optionPrunedGraph.foreach( (prunedGraph) => {
      metricsComputer.computeAndWriteMetrics(prunedGraph, stepCount, prunedMetricsOutputPath)
      aam.AAMGraphPrinter.printGraph(prunedGraph, s"${GlobalFlags.ANALYSIS_PATH}Incremental/pruned_graph_$stepCount.dot")
    })
  }

  def applyEdgeActions(concreteState: PS, stepCount: Int): Unit = {
    val applyEdgeActions = () => {
      val convertedState = convertStateAAM(aam, concSem, abstSem, concreteState)
      val optionIncrementalGraph: Option[AbstractGraph] = incrementalAnalysis.applyEdgeActions(convertedState, stepCount)
//      optionIncrementalGraph.foreach( (incrementalGraph) => {
//        metricsComputer.computeAndWriteMetrics(incrementalGraph, stepCount, incrementalMetricsOutputPath)
//        aam.AAMGraphPrinter.printGraph(incrementalGraph, s"${GlobalFlags.ANALYSIS_PATH}Incremental/incremental_graph_$stepCount.dot")
////        assert(incrementalGraph.nodes.size <= incrementalAnalysis.prunedGraph.get.nodes.size)
////        assert(incrementalGraph.edges.size <= incrementalAnalysis.prunedGraph.get.edges.size)
//        val completelyNewGraph: AbstractGraph = runStaticAnalysis(concreteState, Some(stepCount)) match {
//          case AnalysisOutputGraph(output) =>
//            output.toDotFile(s"${GlobalFlags.ANALYSIS_PATH}Run_time/run_time_$stepCount.dot")
//            output.graph.asInstanceOf[AbstractGraph]
//        }
//        metricsComputer.computeAndWriteMetrics(completelyNewGraph, stepCount, runTimeAnalysisMetricsOutputPath)
//        val areEqual = incrementalAnalysis.subsumedGraphsEqual(completelyNewGraph, incrementalGraph)
//        Logger.log(s"Graphs equal? $areEqual\n", Logger.U)
//        assert(areEqual)
//      })
    }
    wrapAbstractEvaluation(applyEdgeActions)
  }

}
