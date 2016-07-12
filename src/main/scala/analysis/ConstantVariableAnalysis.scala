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
    Logger.log(s"Constant addresses are ${result.constants}", Logger.E)
    Logger.log(s"NonConstant addresses are ${result.nonConstants}", Logger.E)
    result
  }

  def analyze(aam: AAM[Exp, L, Addr, Time], sem: Semantics[Exp, L, Addr, Time], isConstantValue: L => Boolean)
             (startState: aam.State, initialEnv: Environment[Addr], addressedLookedUp: Set[Addr]): ConstantAddresses[Addr] = {
    /* The addresses determined to refer to constants by the initial global analysis.
     * These addresses don't have to be checked again, because if the initial analysis could determine them to
     * remain constant, and if this analysis was sound, this new analysis will just determine again that they're
     * constant. */
    //TODO addressesLookedUp must be the abstract HybridAddresses
    val initialConstants: Set[Addr] = initialAnalysisResults.fold(Set[Addr]())(_.constants)
    val relevantAddresses = addressedLookedUp -- initialConstants
    /* Stop exploring the state once all relevant addresses were found to be non-constant. */
    val pred = (state: aam.State) => relevantAddresses.forall(addr => state.store.lookup(addr) match {
      case None =>
        /* If the address is not in the store, which shouldn't happen?, definitely keep exploring the state. */
        Logger.log(s"Found an abstract address not in the store: $addr", Logger.E)
        false
      case Some(value) =>
        ! isConstantValue(value)
    })
    val output = aam.kickstartEval(startState, sem, None, None, false)
    analyzeOutput(aam, isConstantValue)(output)
  }

  /**
    * Performs an initial global analysis of the entire program and saves the results for later use.
    */
  def initialAnalyze(aam: AAM[Exp, L, Addr, Time], sem: Semantics[Exp, L, Addr, Time], isConstantValue: L => Boolean)
  (startState: aam.State, initialEnv: Environment[Addr]): ConstantAddresses[Addr] = {
    val results = analyze(aam, sem, isConstantValue)(startState, initialEnv, Set())
    initialAnalysisResults = Some(results)
    results
  }
}

class ConstantsAnalysisLauncher[Exp : Expression, Time : Timestamp]
(sem: SemanticsTraced[Exp, HybridLattice.L, HybridAddress.A, Time]) {

  /* The concrete program state the static analysis gets as input. This state is then converted to an
   * abstract state and fed to the AAM. */
  type PS = HybridMachine[Exp, Time]#PS
  /* The specific type of AAM used for this analysis: an AAM using the HybridLattice, HybridAddress and ZeroCFA
   * components. */
  type SpecAAM = AAM[Exp, HybridLattice.L, HybridAddress.A, ZeroCFA.T]
  /* The specific environment used in the concrete state: an environment using the HybridAddress components. */
  type SpecEnv = Environment[HybridAddress.A]

  val constantsAnalysis = new ConstantVariableAnalysis[Exp, HybridLattice.L, HybridAddress.A, ZeroCFA.T]

  private def switchToAbstract(): Unit = {
    Logger.log("HybridMachine switching to abstract", Logger.I)
    HybridLattice.switchToAbstract()
    HybridAddress.switchToAbstract()
  }

  protected def launchAnalysis(aam: SpecAAM)
                              (startState: aam.State, env: SpecEnv, addressedLookedUp: Set[HybridAddress.A]): ConstantAddresses[HybridAddress.A] = {
    val abstractAddressesLookedup = addressedLookedUp.map(HybridAddress.convertAddress(_))
    constantsAnalysis.analyze(aam, sem.absSem, HybridLattice.isConstantValue)(startState, env, abstractAddressesLookedup)
  }

  private def startStaticAnalysis(currentProgramState: PS, addressedLookedUp: Set[HybridAddress.A]): ConstantAddresses[HybridAddress.A] = {
    val aam = new AAM[Exp, HybridLattice.L, HybridAddress.A, ZeroCFA.T]
    val (control, env, store, kstore, a, t) = currentProgramState.convertState(aam)(sem)
    val convertedControl = control match {
      case ConvertedControlError(reason) => aam.ControlError(reason)
      case ConvertedControlEval(exp, env) => aam.ControlEval(exp, env)
      case ConvertedControlKont(v) => aam.ControlKont(v)
    }
    val startState = aam.State(convertedControl, store, kstore, a, t)
    // TODO timeout
    val result = launchAnalysis(aam)(startState, env, addressedLookedUp)
    Logger.log(s"analysis result is $result", Logger.I)
    result
  }

  private def switchToConcrete(): Unit = {
    Logger.log("HybridMachine switching to concrete", Logger.I)
    HybridLattice.switchToConcrete()
    HybridAddress.switchToConcrete()
  }

  def runStaticAnalysis(currentProgramState: PS, addressedLookedUp: Set[HybridAddress.A]): StaticAnalysisResult = {
    switchToAbstract()
    val result = startStaticAnalysis(currentProgramState, addressedLookedUp)
    switchToConcrete()
    result
  }
}

class InitialConstantsAnalysisLauncher[Exp : Expression, Time : Timestamp]
(sem: SemanticsTraced[Exp, HybridLattice.L, HybridAddress.A, Time])
  extends ConstantsAnalysisLauncher[Exp, Time](sem) {

  protected def launchAnalysis(aam: SpecAAM)(startState: aam.State, env: SpecEnv): ConstantAddresses[HybridAddress.A] = {
    Logger.log(s"Running static analysis before actually executing program", Logger.I)
    val result = constantsAnalysis.initialAnalyze(aam, sem.absSem, HybridLattice.isConstantValue)(startState, env)
    Logger.log(s"Finished running static analysis before actually executing program", Logger.I)
    result
  }
}

class ConstantsAnalyisLauncher[Exp : Expression, Time : Timestamp](tracingFlags: TracingFlags) {

  private def runAnalysis(analysisLauncher: ConstantsAnalysisLauncher[Exp, Time],
                          state: HybridMachine[Exp, Time]#PS,
                          addressedLookedUp: Set[HybridAddress.A]): StaticAnalysisResult = {
    if (tracingFlags.SWITCH_ABSTRACT) {
      analysisLauncher.runStaticAnalysis(state, addressedLookedUp)
    } else {
      NoStaticisAnalysisResult
    }
  }

  def runInitialStaticAnalyis(sem: SemanticsTraced[Exp, HybridLattice.L, HybridAddress.A, Time],
                              state: HybridMachine[Exp, Time]#PS): StaticAnalysisResult = {
    val constantsAnalysisLauncher = new InitialConstantsAnalysisLauncher[Exp, Time](sem)
    runAnalysis(constantsAnalysisLauncher, state, Set())
  }

  def runStaticAnalyis(sem: SemanticsTraced[Exp, HybridLattice.L, HybridAddress.A, Time],
                       state: HybridMachine[Exp, Time]#PS,
                       addressedLookedUp: Set[HybridAddress.A]): StaticAnalysisResult = {
    val constantsAnalysisLauncher = new ConstantsAnalysisLauncher[Exp, Time](sem)
    runAnalysis(constantsAnalysisLauncher, state, addressedLookedUp)
  }

}
