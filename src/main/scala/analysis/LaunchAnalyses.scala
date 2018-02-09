import backend.path_filtering.PartialRegexMatcher
import backend.tree.Constraint

case class AnalysisResult(partialMatcher: PartialRegexMatcher, containsErrorStates: Boolean)

class LaunchAnalyses[PAbs: IsConvertableLattice: LatticeInfoProvider](analysisLauncher: AnalysisLauncher[PAbs]) {

  val errorPathDetector = new ErrorPathDetector[SchemeExp, PAbs, HybridAddress.A, HybridTimestamp.T](analysisLauncher.aam)

  private def handleAnalysisResult[Abs: IsSchemeLattice](errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T],
                                                         result: StaticAnalysisResult): Option[AnalysisResult] = result match {
    case outputGraph: AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, errorPathDetector.aam.State] =>
      // TODO can't just pass the current partial matcher, because that one starts from some cached final state instead of the initial state
      // TODO and the ENTIRE path is passed to the matcher (so including the part it has actually already matched).
      // TODO Just passing the initial matcher would mean the current matcher wouldn't be used at all though.
      // TODO Solution: when an AbortConcolicExecution-error is thrown, pass the path that was already created
      // TODO to the backend and ask to invalidate that one?
      val maybePartialMatcher = errorPathDetector.detectErrors(outputGraph.hasGraph.graph)
      val result = AnalysisResult(maybePartialMatcher.get, outputGraph.hasGraph.errorStates.nonEmpty)
      Some(result)
    case _ =>
      Logger.log(s"### Concolic did not get expected graph, got $result instead", Logger.U)
      None
  }

  private def handleInitialAnalysisResult[Abs: IsSchemeLattice](errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T],
                                                        result: StaticAnalysisResult): AnalysisResult = {
    val maybeAnalysisResult = handleAnalysisResult[Abs](errorPathDetector, result)
    PartialMatcherStore.setInitial(maybeAnalysisResult.get.partialMatcher)
    maybeAnalysisResult.get
  }

  private def handleRunTimeAnalysisResult[Abs: IsSchemeLattice](errorPathDetector: ErrorPathDetector[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T],
    result: StaticAnalysisResult, thenBranchTaken: Boolean): AnalysisResult = {
    val maybeAnalysisResult = handleAnalysisResult[Abs](errorPathDetector, result)
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
    ScalaAMReporter.pathStorage.resetCurrentPath()
    maybeAnalysisResult.get
  }

  /**
    * If an if-expression has just been encountered (and a corresponding branch node has been made), launch a
    * rum-time static analysis and use the results to further prune the symbolic tree.
    * @param state
    */
  def startRunTimeAnalysis(state: ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T],
                           thenBranchTaken: Boolean, stepCount: Int,
                           pathConstraint: List[(Constraint, Boolean)]): AnalysisResult = {
    ScalaAMReporter.disableConcolic()
    Logger.log("Starting run-time analysis", Logger.E)
    val currentAddresses: Set[HybridAddress.A] = state.addressesReachable
    val addressConverter = new DefaultHybridAddressConverter[SchemeExp]
    val convertedCurrentAddresses = currentAddresses.map(addressConverter.convertAddress)
    val analysisResult = analysisLauncher.runStaticAnalysis(state, Some(stepCount), convertedCurrentAddresses, pathConstraint)
    val result = handleRunTimeAnalysisResult[PAbs](errorPathDetector, analysisResult, thenBranchTaken)
    ScalaAMReporter.enableConcolic()
    result
  }

  def startInitialAnalysis(initialState: ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T],
                           programName: String): AnalysisResult = {
    ScalaAMReporter.disableConcolic()
    val analysisResult = analysisLauncher.runInitialStaticAnalysis(initialState, programName)
    val result = handleInitialAnalysisResult[PAbs](errorPathDetector, analysisResult)
    ScalaAMReporter.enableConcolic()
    result
  }

}