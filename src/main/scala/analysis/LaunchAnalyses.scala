import backend.PathConstraint
import backend.path_filtering.PartialRegexMatcher
import backend.tree.path.{ElseBranchTaken, ThenBranchTaken}

case class AnalysisResult(partialMatcher: PartialRegexMatcher, containsErrorStates: Boolean, containsUserErrorStates: Boolean) {
  def shouldContinueTesting: Boolean = containsUserErrorStates
}

class LaunchAnalyses[Abs: IsConvertableLattice: LatticeInfoProvider](analysisLauncher: AnalysisLauncher[Abs],
                                                                     reporter: ScalaAMReporter) {

  private val errorPathDetector = new ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T, analysisLauncher.aam.State](analysisLauncher.aam)

  private def handleAnalysisResult(outputGraph: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, analysisLauncher.aam.State], currentConcolicRun: Int): Option[AnalysisResult] = {
    GraphDOTOutput.toFile(outputGraph.graph, outputGraph.halted)(s"rt_graph_${currentConcolicRun}_${outputGraph.stepSwitched}.dot")
    val maybePartialMatcher = errorPathDetector.detectErrors(outputGraph.graph, outputGraph.stepSwitched, currentConcolicRun)
    val result = AnalysisResult(maybePartialMatcher.get, outputGraph.errorStates.nonEmpty, outputGraph.errorStates.exists(_.isUserErrorState))
    Some(result)
  }

  private def handleInitialAnalysisResult(result: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, analysisLauncher.aam.State]): AnalysisResult = {
    val maybeAnalysisResult = handleAnalysisResult(result, -1)
    PartialMatcherStore.setInitial(maybeAnalysisResult.get.partialMatcher)
    maybeAnalysisResult.get
  }

  private def handleRunTimeAnalysisResult(result: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, analysisLauncher.aam.State], thenBranchTaken: Boolean, currentConcolicRun: Int): AnalysisResult = {
//    def removeIfBranchAnnotation: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, analysisLauncher.aam.State] = {
//      val graph = result.graph
//      val root = graph.getNode(0).get
//      val originalFirstEdges = graph.nodeEdges(root)
//      val updatedFirstEdges = originalFirstEdges.map({ case (_, state) => (EdgeAnnotation.dummyEdgeAnnotation[SchemeExp, Abs, HybridAddress.A], state) })
//      val updatedGraph = new Graph[analysisLauncher.aam.State, EdgeAnnotation[SchemeExp, Abs, HybridAddress.A], Set[analysisLauncher.aam.State]](graph.ids, graph.next, graph.nodes, graph.edges + (root -> updatedFirstEdges))
//      result.replaceGraph(updatedGraph)
//    }
//    val ifBranchAnnotationRemoved = removeIfBranchAnnotation
    val maybeAnalysisResult = handleAnalysisResult(result, currentConcolicRun)
    //    val initialErrorPathsNotStartingWithPrefix = InitialErrorPaths.get.get.filterNot(_.startsWith(prefixErrorPath))
    //    val newInitialErrorPaths = initialErrorPathsNotStartingWithPrefix ++ automaton.map(prefixErrorPath ++ _)
    PartialMatcherStore.setCurrentMatcher(maybeAnalysisResult.get.partialMatcher)
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
    maybeAnalysisResult.get
  }

  /**
    * If an if-expression has just been encountered (and a corresponding branch node has been made), launch a
    * rum-time static analysis and use the results to further prune the symbolic tree.
    * @param state
    */
  def startRunTimeAnalysis(state: ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T],
                           thenBranchTaken: Boolean, stepCount: Int, pathConstraint: PathConstraint, currentConcolicRun: Int): AnalysisResult = {
    reporter.disableConcolic()
    Logger.log("Starting run-time analysis", Logger.E)
    val currentAddresses: Set[HybridAddress.A] = state.addressesReachable
    val addressConverter = new DefaultHybridAddressConverter[SchemeExp]
    val convertedCurrentAddresses = currentAddresses.map(addressConverter.convertAddress)
    val analysisResult = analysisLauncher.runStaticAnalysis(state, Some(stepCount), convertedCurrentAddresses, pathConstraint)
    val result = handleRunTimeAnalysisResult(analysisResult, thenBranchTaken, currentConcolicRun)
    reporter.enableConcolic()
    result
  }

  def startInitialAnalysis(initialState: ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T],
                           programName: String): AnalysisResult = {
    reporter.disableConcolic()
    val analysisResult = analysisLauncher.runInitialStaticAnalysis(initialState, programName)
    val result = handleInitialAnalysisResult(analysisResult)
    reporter.enableConcolic()
    result
  }

}
