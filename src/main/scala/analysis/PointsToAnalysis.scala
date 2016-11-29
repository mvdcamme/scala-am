import java.io.{FileWriter, BufferedWriter, File}

class PointsToAnalysis[
    Exp: Expression, L: JoinLattice, Addr: Address, Time: Timestamp] {

  private def joinStores(free: Free[Exp, L, Addr, Time])(
      stores: Set[Store[Addr, L]]): Set[(Addr, L)] = {
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

  private def analyzeOutput(free: Free[Exp, L, Addr, Time],
                            pointsTo: L => Option[Int],
                            relevantAddress: Addr => Boolean)(
      output: free.FreeOutput): List[(Addr, Option[Int])] = {
    val storeValues = joinStores(free)(output.finalStores)
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
    Logger.log(
      s"Static points-to analysis completed, resulting set equals $result",
      Logger.U)
    val metrics = calculateMetrics(result)
    Logger.log(s"Static points-to analysis completed, metrics equals $metrics",
               Logger.D)
    possiblyWriteMetrics(output.stepSwitched.get, metrics)
    result
  }

  def analyze[Machine <: KickstartEvalEvalKontMachine[Exp, L, Addr, Time]](machine: Machine,
                                                       sem: Semantics[Exp, L, Addr, Time],
                                                       pointsTo: L => Option[Int],
                                                       relevantAddress: Addr => Boolean)(startState: machine
  .MachineState,
                                                                                         isInitial: Boolean,
                                                                                         stepSwitched: Int): StaticAnalysisResult = {
    Logger.log(s"Starting static points-to analysis", Logger.I)
    val result = machine.kickstartEval(startState, sem, None, None, Some(stepSwitched))
    AnalysisGraph[machine.GraphNode](result.graph.get)
    //analyzeOutput(free, pointsTo, relevantAddress)(output)
  }
}

class PointsToAnalysisLauncher[
    Abs: IsConvertableLattice: PointsToableLatticeInfoProvider](
    concSem: SemanticsTraced[SchemeExp,
                             ConcreteConcreteLattice.L,
                             HybridAddress.A,
                             HybridTimestamp.T])
    extends AnalysisLauncher[Abs] {

  val abs = implicitly[IsConvertableLattice[Abs]]
  val lip = implicitly[PointsToableLatticeInfoProvider[Abs]]

  val pointsToAnalysis =
    new PointsToAnalysis[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T]

  def runStaticAnalysis(currentProgramState: PS,
                        stepSwitched: Int): StaticAnalysisResult =
    wrapRunAnalysis(() => {
      val aam: SpecAAM = new SpecAAM()
      val startStates =
        convertStateAAM(aam, concSem, abstSem, currentProgramState)
      val result = pointsToAnalysis
        .analyze(aam, abstSem, lip.pointsTo, (addr) => ! HybridAddress.isAddress.isPrimitive(addr))(startStates,
          false,
          stepSwitched)
//      Logger.log(s"Static points-to analysis result is $result", Logger.U)
      result
    })

}
