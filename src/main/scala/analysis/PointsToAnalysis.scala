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
                            sum: Int)

  private def calculateMetrics(result: List[(Addr, Int)]): MetricsToWrite = {
    val integerValues = result.map(_._2)
    val numberValues = integerValues.map(_.toDouble).sortWith(_ < _)
    val length = numberValues.length
    if (length == 0) {
      MetricsToWrite(0, 0, 0, 0)
    } else {
      val max = integerValues.max
      val sum = integerValues.sum
      val median = if (length % 2 == 0) {
        (numberValues((length / 2) - 1) + numberValues(length / 2)) / 2
      } else {
        numberValues(length / 2)
      }
      val average = numberValues.sum / length
      MetricsToWrite(max, median, average, sum)
    }
  }

  private def possiblyWriteMetrics(stepSwitched: Int,
                                   metrics: MetricsToWrite): Unit = {
    if (GlobalFlags.ANALYSIS_RESULTS_OUTPUT.isDefined) {
      val file = new File(GlobalFlags.ANALYSIS_RESULTS_OUTPUT.get)
      val bw = new BufferedWriter(new FileWriter(file, true))
      bw.write(
        s"$stepSwitched;${metrics.max};${metrics.median};${metrics.average};${metrics.sum}\n")
      bw.close()
    }
  }

  private def analyzeOutput(free: Free[Exp, L, Addr, Time],
                            pointsTo: L => Int,
                            relevantAddress: Addr => Boolean)(
      output: free.FreeOutput): List[(Addr, Int)] = {
    val storeValues = joinStores(free)(output.finalStores)
    val initial: List[(Addr, Int)] = Nil
    val result: List[(Addr, Int)] = storeValues.foldLeft(initial)({
      case (result, (address, value)) =>
        val numberOfObjectsPointedTo: Int = pointsTo(value)
        if (relevantAddress(address) && numberOfObjectsPointedTo > 0) {
          /* List all addresses pointing to more than one value */
          ((address, numberOfObjectsPointedTo)) :: result
        } else {
          result
        }
    })
    Logger.log(
      s"Static points-to analysis completed, resulting set equals $result",
      Logger.D)
    val metrics = calculateMetrics(result)
    Logger.log(s"Static points-to analysis completed, metrics equals $metrics",
               Logger.D)
    possiblyWriteMetrics(output.stepSwitched.get, metrics)
    result
  }

  def analyze(free: Free[Exp, L, Addr, Time],
              sem: Semantics[Exp, L, Addr, Time],
              pointsTo: L => Int,
              relevantAddress: Addr => Boolean)(startState: free.States,
                                  isInitial: Boolean,
                                  stepSwitched: Int): List[(Addr, Int)] = {
    Logger.log(s"Starting static points-to analysis", Logger.I)
    val output =
      free.kickstartEval(startState, sem, None, None, Some(stepSwitched))
    analyzeOutput(free, pointsTo, relevantAddress)(output)
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
      val free: SpecFree = new SpecFree()
      val startStates =
        convertState(free, concSem, abstSem, currentProgramState)
      val result = pointsToAnalysis
        .analyze(free, abstSem, lip.pointsTo, (addr) => ! HybridAddress.isPrimitiveAddress(addr))(startStates, false,
          stepSwitched)
      Logger.log(s"Static points-to analysis result is $result", Logger.U)
      PointsToSet(result)
    })

}
