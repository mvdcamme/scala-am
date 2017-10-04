/* An analysis for finding variables whose value never changes throughout the lifetime of a program. */

class ConstantVariableAnalysis[
    Exp: Expression, L: JoinLattice, Addr: Address, Time: Timestamp] {

  var initialAnalysisResults: Option[ConstantAddresses[Addr]] = None

  private def joinStores(free: Free[Exp, L, Addr, Time])(
      stores: Set[Store[Addr, L]]): Set[(Addr, L)] = {
    val joinedStore = stores.foldLeft(Store.initial(Set()): Store[Addr, L]) {
      case (joinedStore, store) => joinedStore.join(store)
    }
    joinedStore.toSet
  }

  private def analyzeOutput(free: Free[Exp, L, Addr, Time],
                            isConstantValue: L => Boolean)(
      output: free.FreeOutput): ConstantAddresses[Addr] = {
    val storeValues = joinStores(free)(output.finalStores)
    val initial = ConstantAddresses[Addr](Set(), Set())
    val result = storeValues.foldLeft(initial)({
      case (result, (address, value)) =>
        if (isConstantValue(value)) {
          val extendedConstants = result.constants + address
          result.copy(constants = extendedConstants)
        } else {
          val extendedNonConstants = result.nonConstants + address
          result.copy(nonConstants = extendedNonConstants)
        }
    })
    Logger.log(
      s"Static constants analysis completed, constant addresses are ${result.constants}",
      Logger.D)
    Logger.log(
      s"Static constants analysis completed, nonConstant addresses are ${result.nonConstants}",
      Logger.D)
    result
  }

  def analyze(free: Free[Exp, L, Addr, Time],
              sem: Semantics[Exp, L, Addr, Time],
              isConstantValue: L => Boolean)(
      startStates: free.States,
      addressedLookedUp: Set[Addr],
      isInitial: Boolean): ConstantAddresses[Addr] = {
    /* The addresses determined to refer to constants by the initial global analysis.
     * These addresses don't have to be checked again, because if the initial analysis could determine them to
     * remain constant, and if this analysis was sound, this new analysis will just determine again that they're
     * constant. */
    val initialConstants: Set[Addr] =
      initialAnalysisResults.fold(Set[Addr]())(_.constants)
    val relevantAddresses = addressedLookedUp -- initialConstants
    Logger.log(
      s"Starting static constants analysis, relevantAddresses = $relevantAddresses",
      Logger.I)
    /* Stop exploring the state once all relevant addresses were found to be non-constant. */
    val pred = (states: free.States) =>
      relevantAddresses.forall(addr =>
        states.store.lookup(addr) match {
          case None =>
            /* If the address is not in the store, which shouldn't happen?, definitely keep exploring the state. */
            Logger.log(s"Found an abstract address $addr not in the store during static analysis", Logger.E)
            false
          case Some(value) =>
            !isConstantValue(value)
      })
    val output = free.kickstartEval(startStates,
                                    sem,
                                    if (isInitial) None else Some(pred),
                                    None,
                                    None)
    analyzeOutput(free, isConstantValue)(output)
  }

  /**
    * Performs an initial global analysis of the entire program and saves the results for later use.
    */
  def initialAnalyze(free: Free[Exp, L, Addr, Time],
                     sem: Semantics[Exp, L, Addr, Time],
                     isConstantValue: L => Boolean)(
      startStates: free.States): ConstantAddresses[Addr] = {
    val output = free.kickstartEval(startStates, sem, None, None, None)
    val result = analyzeOutput(free, isConstantValue)(output)
    initialAnalysisResults = Some(result)
    result
  }
}

class ConstantsAnalysisLauncher[Abs: IsConvertableLattice: ConstantableLatticeInfoProvider](
    concSem: ConvertableSemantics[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T])
    extends AnalysisLauncher[Abs] {

  val abs = implicitly[IsConvertableLattice[Abs]]
  val lip = implicitly[ConstantableLatticeInfoProvider[Abs]]

  val constantsAnalysis = new ConstantVariableAnalysis[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T]

  protected def launchAnalysis(free: SpecFree)(
      startStates: free.States,
      addressedLookedUp: Set[HybridAddress.A])
    : ConstantAddresses[HybridAddress.A] = {
    val abstractAddressesLookedup = addressedLookedUp.map(
      new DefaultHybridAddressConverter[SchemeExp]().convertAddress)
    constantsAnalysis.analyze(free, abstSem, lip.isConstantValue)(
      startStates,
      abstractAddressesLookedup,
      false)
  }

  protected def launchInitialAnalysis(free: SpecFree)(
      startStates: free.States): ConstantAddresses[HybridAddress.A] = {
    Logger.log(s"Running initial static constants analysis", Logger.I)
    val result = constantsAnalysis.initialAnalyze(free, abstSem, lip.isConstantValue)(startStates)
    Logger.log(s"Finished running initial static constants analysis", Logger.I)
    result
  }

  /*
   * The type of the second argument of the launchAnalysis higher-order parameter should be the State of the
   * first parameter of launchAnalyis, but path dependent typing seems to cause problems when combined with
   * higher-order functions.
   */
  private def startStaticAnalysis(
      currentProgramState: PS,
      launchAnalysis: (SpecFree, Any) => ConstantAddresses[HybridAddress.A])
    : ConstantAddresses[HybridAddress.A] = {
    val free = new Free[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T]
    val startState = convertStateFree(free, concSem, abstSem, currentProgramState)
    val result = launchAnalysis(free, startState)
    Logger.log(s"Static constants analysis result is $result", Logger.I)
    result
  }

  def runStaticAnalysis(
      currentProgramState: PS,
      addressesLookedUp: Set[HybridAddress.A]): StaticAnalysisResult = {
    val fun = () =>
      startStaticAnalysis(
        currentProgramState,
        /* The type of startState should be aam.State, but path dependent types seem
         * to cause problems when combined with higher-order functions. */
        (free: SpecFree, startState) =>
          launchAnalysis(free)(startState.asInstanceOf[free.States],
                               addressesLookedUp))
    wrapRunAnalysis(fun)
  }

  def runInitialStaticAnalysis(currentProgramState: PS): StaticAnalysisResult = {
    val fun = () =>
      startStaticAnalysis(
        currentProgramState,
        /* The type of startState should be aam.State, but path dependent types seem
         * to cause problems when combined with higher-order functions. */
        (free: SpecFree, startState) =>
          launchInitialAnalysis(free)(startState.asInstanceOf[free.States]))
    wrapRunAnalysis(fun)
  }
}
