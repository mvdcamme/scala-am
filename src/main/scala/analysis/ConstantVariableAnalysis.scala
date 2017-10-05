///* An analysis for finding variables whose value never changes throughout the lifetime of a program. */
//
//class ConstantVariableAnalysis[
//    Exp: Expression, L: JoinLattice, Addr: Address, Time: Timestamp] {
//
//  var initialAnalysisResults: Option[ConstantAddresses[Addr]] = None
//
//  private def joinStores(aam: AAM[Exp, L, Addr, Time])(
//      stores: Set[Store[Addr, L]]): Set[(Addr, L)] = {
//    val joinedStore = stores.foldLeft(Store.initial(Set()): Store[Addr, L]) {
//      case (joinedStore, store) => joinedStore.join(store)
//    }
//    joinedStore.toSet
//  }
//
//  private def analyzeOutput(aam: AAM[Exp, L, Addr, Time],
//                            isConstantValue: L => Boolean)(
//      output: aam.AAMOutput): ConstantAddresses[Addr] = {
//    val storeValues = joinStores(aam)(output.finalStores)
//    val initial = ConstantAddresses[Addr](Set(), Set())
//    val result = storeValues.foldLeft(initial)({
//      case (result, (address, value)) =>
//        if (isConstantValue(value)) {
//          val extendedConstants = result.constants + address
//          result.copy(constants = extendedConstants)
//        } else {
//          val extendedNonConstants = result.nonConstants + address
//          result.copy(nonConstants = extendedNonConstants)
//        }
//    })
//    Logger.log(
//      s"Static constants analysis completed, constant addresses are ${result.constants}",
//      Logger.D)
//    Logger.log(
//      s"Static constants analysis completed, nonConstant addresses are ${result.nonConstants}",
//      Logger.D)
//    result
//  }
//
//  def analyze(aam: AAM[Exp, L, Addr, Time],
//              sem: Semantics[Exp, L, Addr, Time],
//              isConstantValue: L => Boolean)(
//      startStates: aam.States,
//      addressedLookedUp: Set[Addr],
//      isInitial: Boolean): ConstantAddresses[Addr] = {
//    /* The addresses determined to refer to constants by the initial global analysis.
//     * These addresses don't have to be checked again, because if the initial analysis could determine them to
//     * remain constant, and if this analysis was sound, this new analysis will just determine again that they're
//     * constant. */
//    val initialConstants: Set[Addr] =
//      initialAnalysisResults.fold(Set[Addr]())(_.constants)
//    val relevantAddresses = addressedLookedUp -- initialConstants
//    Logger.log(
//      s"Starting static constants analysis, relevantAddresses = $relevantAddresses",
//      Logger.I)
//    /* Stop exploring the state once all relevant addresses were found to be non-constant. */
//    val pred = (states: aam.States) =>
//      relevantAddresses.forall(addr =>
//        states.store.lookup(addr) match {
//          case None =>
//            /* If the address is not in the store, which shouldn't happen?, definitely keep exploring the state. */
//            Logger.log(s"Found an abstract address $addr not in the store during static analysis", Logger.E)
//            false
//          case Some(value) =>
//            !isConstantValue(value)
//      })
//    val output = aam.kickstartEval(startStates,
//                                    sem,
//                                    if (isInitial) None else Some(pred),
//                                    None,
//                                    None)
//    analyzeOutput(aam, isConstantValue)(output)
//  }
//}
//
//class ConstantsAnalysisLauncher[Abs: IsConvertableLattice: ConstantableLatticeInfoProvider](
//    concSem: ConvertableSemantics[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T])
//    extends AnalysisLauncher[Abs] {
//
//  val abs = implicitly[IsConvertableLattice[Abs]]
//  val lip = implicitly[ConstantableLatticeInfoProvider[Abs]]
//
//  val constantsAnalysis = new ConstantVariableAnalysis[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T]
//
//  def end(): Unit = {}
//
//  def incrementalAnalysis(concreteState: PS, stepCount: Int, programName: String, addressesUsed: Set[HybridAddress.A]): Unit = {}
//
//  def runStaticAnalysisGeneric(
//    currentProgramState: PS,
//    stepSwitched: Option[Int],
//    toDotFile: Option[String]): StaticAnalysisResult = {
//    wrapRunAnalysis(
//      () => {
//        val startState = convertStateAAM(aam, concSem, abstSem, currentProgramState)
//        val result =
//          constantsAnalysis.analyze(aam,
//            abstSem,
//            (addr) => ! HybridAddress.isAddress.isPrimitive(addr))(
//            startState,
//            false,
//            stepSwitched)
//        result
//      })
//  }
//
//  def runStaticAnalysis(currentProgramState: ConvertableProgramState[SchemeExp,HybridAddress.A,HybridTimestamp.T],
//                        stepSwitched: Option[Int],
//                        programName: String,
//                        addressesUsed: Set[HybridAddress.A]): StaticAnalysisResult = {
//    runStaticAnalysisGeneric(currentProgramState, stepSwitched, None)
//  }
//
//  def runInitialStaticAnalysis(currentProgramState: PS, programName: String): Unit = {
//    val fun = () =>
//      startStaticAnalysis(
//        currentProgramState,
//        /* The type of startState should be aam.State, but path dependent types seem
//         * to cause problems when combined with higher-order functions. */
//        (free: SpecFree, startState) =>
//          launchInitialAnalysis(free)(startState.asInstanceOf[free.States]))
//    wrapRunAnalysis(fun)
//  }
//
//  def doConcreteStep(convertValue: SchemePrimitives[HybridAddress.A,Abs] => (ConcreteConcreteLattice.L => Abs),
//                     convertFrame: (ConvertableSemantics[SchemeExp,ConcreteConcreteLattice.L,HybridAddress.A,HybridTimestamp.T],
//                                    BaseSchemeSemantics[Abs,HybridAddress.A,HybridTimestamp.T],
//                                    ConcreteConcreteLattice.L => Abs)
//                                    => SchemeFrame[ConcreteConcreteLattice.L,HybridAddress.A,HybridTimestamp.T]
//                                    => SchemeFrame[Abs,HybridAddress.A,HybridTimestamp.T],
//                     filters: FilterAnnotations[SchemeExp,ConcreteConcreteLattice.ConcreteValue,HybridAddress.A],
//                     stepNumber: Int): Unit = {
//    // TODO Not yet implemented
//  }
//}
