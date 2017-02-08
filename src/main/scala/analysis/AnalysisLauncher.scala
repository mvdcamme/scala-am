import ConcreteConcreteLattice.ConcreteValue

abstract class AnalysisLauncher[Abs: IsConvertableLattice] {

  protected val abstSem =
    new BaseSchemeSemantics[Abs, HybridAddress.A, HybridTimestamp.T](
      new SchemePrimitives[HybridAddress.A, Abs])
//  Previously, this was SchemeSemantics, but switched back to BaseSchemeSemantics for now to avoid atomic optimisations

  /* The concrete program state the static analysis gets as input. This state is then converted to an
   * abstract state and fed to the AAM. */
  type PS =
    ConcreteTracingProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T]
  /* The specific type of AAM used for this analysis: an AAM using the HybridLattice, HybridAddress and ZeroCFA
   * components. */
  type SpecAAM = AAM[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T]
  /* The specific type of P4F used for this analysis: a P4F using the HybridLattice, HybridAddress and ZeroCFA
   * components. */
  type SpecFree = Free[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T]
  /* The specific environment used in the concrete state: an environment using the HybridAddress components. */
  type SpecEnv = Environment[HybridAddress.A]

  protected def switchToAbstract(): Unit = {
    Logger.log("HybridMachine switching to abstract", Logger.I)
    HybridTimestamp.switchToAbstract()
  }

  protected def switchToConcrete(): Unit = {
    Logger.log("HybridMachine switching to concrete", Logger.I)
    HybridTimestamp.switchToConcrete()
  }

  protected def wrapAbstractEvaluation[Result](doSomething: () => Result): Result = {
    switchToAbstract()
    val result = doSomething()
    switchToConcrete()
    result
  }

  protected def wrapRunAnalysis(
      runAnalysis: () => StaticAnalysisResult): StaticAnalysisResult = {
    Logger.log("analyzing", Logger.I)
    wrapAbstractEvaluation[StaticAnalysisResult](runAnalysis)
  }

  /**
    * Maps a KontAddr to a FreeKontAddr.
    * @param address The KontAddr to be converted.
    * @param someEnv Possible an environment to used to allocate the FreeKontAddr.
    * @return The converted FreeKontAddr.
    */
  private def mapKontAddressToFree(address: KontAddr,
                             someEnv: Option[Environment[HybridAddress.A]]): FreeKontAddr =
    address match {
      case address: NormalKontAddress[SchemeExp, HybridTimestamp.T] =>
        FreeNormalKontAddress(address.exp, someEnv.get)
      case HaltKontAddress => FreeHaltKontAddress
    }

  /**
    * Converts the given state to a new state corresponding to the state employed by the given P4F machine.
    * @param free The P4F machine for which a new, converted, state must be generated.
    * @param concSem The semantics currently being used.
    * @param abstSem The semantics to be used during the analysis.
    * @param programState The program state to be converted.
    */
  protected def convertStateFree(
      free: Free[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T],
      concSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
      abstSem: BaseSchemeSemantics[Abs, HybridAddress.A, HybridTimestamp.T],
      programState: PS): free.States = {
    val (control, store, kstore, a, t) =
      programState.convertState[Abs, FreeKontAddr](concSem, abstSem, FreeHaltKontAddress, mapKontAddressToFree)
    val convertedControl = control match {
      case ConvertedControlError(reason) => free.ControlError(reason)
      case ConvertedControlEval(exp, env) => free.ControlEval(exp, env)
      case ConvertedControlKont(v) => free.ControlKont(v)
    }
    free.States(Set(free.Configuration(convertedControl, a, t)), store, kstore)
  }
  /**
    * Converts the given state to a new state corresponding to the state employed by the given AAM.
    * @param aam The AAM for which a new, converted, state must be generated.
    * @param concSem The semantics currently being used.
    * @param abstSem The semantics to be used during the analysis.
    * @param programState The program state to be converted.
    */
  protected def convertStateAAM(
      aam: AAM[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T],
  concSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
  abstSem: BaseSchemeSemantics[Abs, HybridAddress.A, HybridTimestamp.T],
  programState: PS): aam.State = {
    val (control, store, kstore, a, t) =
      programState.convertState[Abs, KontAddr](concSem, abstSem, HaltKontAddress, (x, _) => x)
    val convertedControl = control match {
      case ConvertedControlError(reason) => aam.ControlError(reason)
      case ConvertedControlEval(exp, env) => aam.ControlEval(exp, env)
      case ConvertedControlKont(v) => aam.ControlKont(v)
    }
    aam.State(convertedControl, store, kstore, a, t)
  }

}
