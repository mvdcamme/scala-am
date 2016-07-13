/* An analysis for finding variables whose value never changes throughout the lifetime of a program. */

class ConstantVariableAnalysis[Exp: Expression, L : JoinLattice, Addr : Address, Time : Timestamp] {

  var initialAnalysisResults: Option[ConstantAddresses[Addr]] = None

  private def joinStores(aam: AAM[Exp, L, Addr, Time])
                        (stores: Set[Store[Addr, L]]): Set[(Addr, L)] = {
    val joinedStore = stores.foldLeft(Store.initial(Set()): Store[Addr, L])
      { case (joinedStore, store) => joinedStore.join(store) }
    joinedStore.toSet
  }

  private def analyzeOutput(aam: AAM[Exp, L, Addr, Time],isConstantValue: L => Boolean)
                           (output: aam.AAMOutput): ConstantAddresses[Addr] = {
    val storeValues = joinStores(aam)(output.finalStores)
    val initial = ConstantAddresses[Addr](Set(), Set())
    val result = storeValues.foldLeft(initial)({ case (result, (address, value)) =>
        if (isConstantValue(value)) {
          val extendedConstants = result.constants + address
          result.copy(constants = extendedConstants)
        } else {
          val extendedNonConstants = result.nonConstants + address
          result.copy(nonConstants = extendedNonConstants)
        }
    })
    Logger.log(s"Constant addresses are ${result.constants}", Logger.D)
    Logger.log(s"NonConstant addresses are ${result.nonConstants}", Logger.D)
    result
  }

  def analyze(aam: AAM[Exp, L, Addr, Time], sem: Semantics[Exp, L, Addr, Time], isConstantValue: L => Boolean)
             (startState: aam.State, addressedLookedUp: Set[Addr],
             isInitial: Boolean): ConstantAddresses[Addr] = {
    /* The addresses determined to refer to constants by the initial global analysis.
     * These addresses don't have to be checked again, because if the initial analysis could determine them to
     * remain constant, and if this analysis was sound, this new analysis will just determine again that they're
     * constant. */
    val initialConstants: Set[Addr] = initialAnalysisResults.fold(Set[Addr]())(_.constants)
    val relevantAddresses = addressedLookedUp -- initialConstants
    Logger.log(s"relevantAddresses = $relevantAddresses", Logger.E)
    /* Stop exploring the state once all relevant addresses were found to be non-constant. */
    val pred = (state: aam.State) => relevantAddresses.forall(addr => state.store.lookup(addr) match {
      case None =>
        /* If the address is not in the store, which shouldn't happen?, definitely keep exploring the state. */
        Logger.log(s"Found an abstract address not in the store: $addr", Logger.E)
        false
      case Some(value) =>
        ! isConstantValue(value)
    })
    val output = aam.kickstartEval(startState, sem, if (isInitial) None else Some(pred), None, false)
    analyzeOutput(aam, isConstantValue)(output)
  }

  /**
    * Performs an initial global analysis of the entire program and saves the results for later use.
    */
  def initialAnalyze(aam: AAM[Exp, L, Addr, Time], sem: Semantics[Exp, L, Addr, Time], isConstantValue: L => Boolean)
                    (startState: aam.State)
                    :ConstantAddresses[Addr] = {
    val output = aam.kickstartEval(startState, sem, None, None, false)
    val result = analyzeOutput(aam, isConstantValue)(output)
    initialAnalysisResults = Some(result)
    result
  }
}

class ConstantsAnalysisLauncher[Exp : Expression](
     sem: SemanticsTraced[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T],
     tracingFlags: TracingFlags) {

  /* The concrete program state the static analysis gets as input. This state is then converted to an
   * abstract state and fed to the AAM. */
  type PS = HybridMachine[Exp]#PS
  /* The specific type of AAM used for this analysis: an AAM using the HybridLattice, HybridAddress and ZeroCFA
   * components. */
  type SpecAAM = AAM[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T]
  /* The specific environment used in the concrete state: an environment using the HybridAddress components. */
  type SpecEnv = Environment[HybridAddress.A]

  final val constantsAnalysis = new ConstantVariableAnalysis[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T]

  private def switchToAbstract(): Unit = {
    Logger.log("HybridMachine switching to abstract", Logger.I)
    HybridTimestamp.switchToAbstract()
    HybridLattice.switchToAbstract()
    HybridAddress.switchToAbstract()
  }

  protected def launchAnalysis(aam: SpecAAM)
                              (startState: aam.State, addressedLookedUp: Set[HybridAddress.A])
                              :ConstantAddresses[HybridAddress.A] = {
    val abstractAddressesLookedup = addressedLookedUp.map(HybridAddress.convertAddress(_, HybridTimestamp.convertTime))
    constantsAnalysis.analyze(aam, sem.absSem, HybridLattice.isConstantValue)(startState, abstractAddressesLookedup, false)
  }

  protected def launchInitialAnalysis(aam: SpecAAM)
                                     (startState: aam.State)
                                     :ConstantAddresses[HybridAddress.A] = {
    Logger.log(s"Running static analysis before actually executing program", Logger.I)
    val result = constantsAnalysis.initialAnalyze(aam, sem.absSem, HybridLattice.isConstantValue)(startState)
    Logger.log(s"Finished running static analysis before actually executing program", Logger.I)
    result
  }

  /*
   * The type of the second argument of the launchAnalysis higher-order parameter should be the State of the
   * first parameter of launchAnalyis, but path dependent typing seems to cause problems when combined with
   * higher-order functions.
   */
  private def startStaticAnalysis(currentProgramState: PS,
                                  launchAnalysis: (SpecAAM, Any) => ConstantAddresses[HybridAddress.A])
                                 :ConstantAddresses[HybridAddress.A] = {
    val aam = new AAM[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T]
    val (control, _, store, kstore, a, t) = currentProgramState.convertState(aam)(sem)
    val convertedControl = control match {
      case ConvertedControlError(reason) => aam.ControlError(reason)
      case ConvertedControlEval(exp, env) => aam.ControlEval(exp, env)
      case ConvertedControlKont(v) => aam.ControlKont(v)
    }
    val startState = aam.State(convertedControl, store, kstore, a, t)
    // TODO timeout
    val result = launchAnalysis(aam, startState)
    Logger.log(s"analysis result is $result", Logger.I)
    result
  }

  private def switchToConcrete(): Unit = {
    Logger.log("HybridMachine switching to concrete", Logger.I)
    HybridTimestamp.switchToConcrete()
    HybridLattice.switchToConcrete()
    HybridAddress.switchToConcrete()
  }

  private def wrapRunAnalysis(runAnalysis: () => StaticAnalysisResult)
                             :StaticAnalysisResult = {
    if (tracingFlags.SWITCH_ABSTRACT) {
      switchToAbstract()
      val result = runAnalysis()
      switchToConcrete()
      result
    } else {
      NoStaticisAnalysisResult
    }
  }

  def runStaticAnalysis(currentProgramState: PS,
                        addressesLookedUp: Set[HybridAddress.A])
                       :StaticAnalysisResult = {
    val fun = () => startStaticAnalysis(currentProgramState,
                                       /* The type of startState should be aam.State, but path dependent types seem
                                        * to cause problems when combined with higher-order functions. */
                                       (aam: SpecAAM, startState) => launchAnalysis(aam)(startState.asInstanceOf[aam.State], addressesLookedUp))
    wrapRunAnalysis(fun)
  }

  def runInitialStaticAnalysis(currentProgramState: PS): StaticAnalysisResult = {
    val fun = () => startStaticAnalysis(currentProgramState,
                                        /* The type of startState should be aam.State, but path dependent types seem
                                         * to cause problems when combined with higher-order functions. */
                                       (aam: SpecAAM, startState) => launchInitialAnalysis(aam)(startState.asInstanceOf[aam.State]))
    wrapRunAnalysis(fun)
  }
}
