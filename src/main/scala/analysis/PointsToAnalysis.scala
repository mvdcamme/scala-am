import java.io.{FileWriter, BufferedWriter, File}

class PointsToAnalysis[
    Exp: Expression, L: JoinLattice, Addr: Address, Time: Timestamp] {

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
      s"Static points-to analysis completed:\n +" +
      s"resulting value is ${output.finalValues}\n" +
      s"resulting set equals $result" +
      s"metrics equals $metrics",
      Logger.U)
    possiblyWriteMetrics(output.stepSwitched.getOrElse(-1), metrics)
    result
  }

  def analyze[Machine <: KickstartEvalEvalKontMachine[Exp, L, Addr, Time]](
      toDot: Option[String],
      machine: Machine,
      sem: Semantics[Exp, L, Addr, Time],
      pointsTo: L => Option[Int],
      relevantAddress: Addr => Boolean)(
      startState: machine.MachineState,
      isInitial: Boolean,
      stepSwitched: Option[Int]): StaticAnalysisResult = {
    Logger.log(s"Starting static points-to analysis", Logger.I)
    val result =
      machine.kickstartEval(startState, sem, None, None, stepSwitched)
    toDot.map(result.toDotFile)
    analyzeOutput(machine, pointsTo, relevantAddress)(result)
    AnalysisGraph[machine.GraphNode](result.graph.get)
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

  val incrementalAnalysis = new IncrementalPointsToAnalysis[SchemeExp, Abs, aam.GraphNode](aam)

  val abs = implicitly[IsConvertableLattice[Abs]]
  val lip = implicitly[PointsToableLatticeInfoProvider[Abs]]

  val pointsToAnalysis =
    new PointsToAnalysis[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T]

  def runStaticAnalysisGeneric(
      currentProgramState: PS,
      stepSwitched: Option[Int],
      toDotFile: Option[String]): StaticAnalysisResult = {
    wrapRunAnalysis(
      () => {
        val startStates =
          convertStateAAM(aam, concSem, abstSem, currentProgramState)
        val result =
          pointsToAnalysis.analyze(toDotFile,
                                   aam,
                                   abstSem,
                                   lip.pointsTo,
                                   (addr) =>
                                     !HybridAddress.isAddress.isPrimitive(
                                       addr))(startStates, false, stepSwitched)
        Logger.log(s"Static points-to analysis result is $result", Logger.D)
        result
      })
  }

  def runStaticAnalysis(currentProgramState: PS,
                        stepSwitched: Option[Int]): StaticAnalysisResult = {
    assert(incrementalAnalysis.hasInitialGraph)
    runStaticAnalysisGeneric(currentProgramState, stepSwitched, None)
  }

  def runInitialStaticAnalysis(currentProgramState: PS): Unit =
    runStaticAnalysisGeneric(currentProgramState,
                             None,
                             Some("initial_graph.dot")) match {
      case result: AnalysisGraph[aam.GraphNode] =>
        incrementalAnalysis.initializeGraph(result.graph)
      case other =>
        throw new Exception(
          s"Expected initial analysis to produce a graph, got $other instead")
    }

  def doConcreteStep(convertValue: SchemePrimitives[HybridAddress.A, Abs] => ConcreteConcreteLattice.L => Abs,
                     convertFrame: (ConvertableSemantics[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T],
                                    BaseSchemeSemantics[Abs, HybridAddress.A, HybridTimestamp.T],
                                    ConcreteConcreteLattice.L => Abs) => SchemeFrame[ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]
                       => SchemeFrame[Abs, HybridAddress.A, HybridTimestamp.T],
                     edgeInfos: List[EdgeAnnotation],
                     stepNumber: Int) = {
    val convertValueFun = convertValue(abstSem.primitives)
    incrementalAnalysis.computeSuccNodes(convertValueFun, convertFrame(concSem, abstSem, convertValueFun), edgeInfos,
      stepNumber)
  }

  def end(): Unit = incrementalAnalysis.end()

  def filterReachable(stepCount: Int): Unit =
    incrementalAnalysis.filterReachable(stepCount)

}
