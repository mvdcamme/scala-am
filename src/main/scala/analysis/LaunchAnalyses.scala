import backend.PathConstraint
import backend.path_filtering.PartialRegexMatcher

case class AnalysisResult[State](partialMatcher: PartialRegexMatcher,
  graph: Graph[State, EdgeAnnotation[_, _, HybridAddress.A], _],
  containsErrorStates: Boolean,
  containsUserErrorStates: Boolean) {
  def shouldContinueTesting: Boolean = containsUserErrorStates
}

class LaunchAnalyses[Abs: IsConvertableLattice: LatticeInfoProvider, PCElementUsed](
  val analysisLauncher: AnalysisLauncher[Abs],
  val reporter: ScalaAMReporter[PCElementUsed, _]) {

  private val errorPathDetector = new ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T, analysisLauncher.SpecState]

  private val analysisResultCache = new AnalysisResultCache[analysisLauncher.SpecInitState, analysisLauncher.SpecState]

  private def handleAnalysisResult(outputGraph: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, analysisLauncher.SpecState], currentConcolicRun: Int): AnalysisResult[analysisLauncher.SpecState] = {
    GraphDOTOutput.toFile(outputGraph.graph, outputGraph.halted)("rt_graph.dot")
    val maybePartialMatcher = errorPathDetector.detectErrors(outputGraph.graph, outputGraph.stepSwitched, currentConcolicRun)
    AnalysisResult[analysisLauncher.SpecState](maybePartialMatcher.get, outputGraph.graph.asInstanceOf[Graph[analysisLauncher.SpecState, EdgeAnnotation[_, _, HybridAddress.A], _]], outputGraph.errorStates.nonEmpty, outputGraph.errorStates.exists(_.isUserErrorState))
  }

  private def handleInitialAnalysisResult(result: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, analysisLauncher.SpecState]): AnalysisResult[analysisLauncher.SpecState] = {
    val maybeAnalysisResult = handleAnalysisResult(result, -1)
    PartialMatcherStore.setInitial(maybeAnalysisResult.partialMatcher)
    maybeAnalysisResult
  }

  private def handleRunTimeAnalysisResult(result: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, analysisLauncher.SpecState],
                                          convertedState: analysisLauncher.SpecInitState,
                                          currentConcolicRun: Int): AnalysisResult[analysisLauncher.SpecState] = {
    val maybeAnalysisResult = handleAnalysisResult(result, currentConcolicRun)
    analysisResultCache.addAnalysisResult(convertedState, maybeAnalysisResult)
    maybeAnalysisResult
  }

  /**
    * If an if-expression has just been encountered (and a corresponding branch node has been made), launch a
    * rum-time static analysis and use the results to further prune the symbolic tree.
    * @param state
    */
  def startRunTimeAnalysis(state: ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T],
                           thenBranchTaken: Boolean, stepCount: Int, pathConstraint: PathConstraint, currentConcolicRun: Int): AnalysisResult[analysisLauncher.SpecState] = {
    Logger.E("Starting run-time analysis")
    reporter.inputVariableStore.disableConcolic()
    val currentAddresses: Set[HybridAddress.A] = state.addressesReachable
    val addressConverter = DefaultHybridAddressConverter
    val convertedCurrentAddresses = currentAddresses.map(addressConverter.convertAddress)
    val convertedState = analysisLauncher.stateConverter.convertStateAAM(state, pathConstraint)
    val analysisResult = analysisResultCache.getAnalysisResult(convertedState) match {
      case Some(cachedAnalysisResult) =>
        Logger.E("Retrieving analysis result from cache")
        cachedAnalysisResult
      case None =>
        val result = analysisLauncher.runStaticAnalysis(state, Some(stepCount), convertedCurrentAddresses, pathConstraint)
        val analysisResult = handleRunTimeAnalysisResult(result, convertedState, currentConcolicRun)
        analysisResult
    }
    reporter.inputVariableStore.enableConcolic()
    PartialMatcherStore.setCurrentMatcher(analysisResult.partialMatcher)
    /*
     * A new partial matcher was generated using a run-time static analysis.
     * This matcher starts matching from a certain point _in the execution of the  program_ and hence skips the part
     * of the path that comes before this point.
     * The path should therefore be reset, otherwise the path (which includes every constraint encountered since
     * the start of the execution of the program) is matched with a matcher that only takes into account the constraints
     * encountered _from the current point in the program on_.
     */
    reporter.pathStorage.resetCurrentPath()
    reporter.pathStorage.updateCurrentPath(thenBranchTaken)
    analysisResult
  }

  def startInitialAnalysis(initialState: ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T],
                           programName: String): AnalysisResult[analysisLauncher.SpecState] = {
    reporter.inputVariableStore.disableConcolic()
    val analysisResult = analysisLauncher.runInitialStaticAnalysis(initialState, programName)
    val result = handleInitialAnalysisResult(analysisResult)
    reporter.inputVariableStore.enableConcolic()
    result
  }

}
