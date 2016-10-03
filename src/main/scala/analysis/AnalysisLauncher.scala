abstract class AnalysisLauncher[Abs: IsConvertableLattice] {

  protected val abstSem =
    new SchemeSemantics[Abs, HybridAddress.A, HybridTimestamp.T](
      new SchemePrimitives[HybridAddress.A, Abs])

  /* The concrete program state the static analysis gets as input. This state is then converted to an
   * abstract state and fed to the AAM. */
  type PS =
    ConcreteTracingProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T]
  /* The specific type of AAM used for this analysis: an AAM using the HybridLattice, HybridAddress and ZeroCFA
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

  protected def wrapRunAnalysis(
      runAnalysis: () => StaticAnalysisResult): StaticAnalysisResult = {
    Logger.log("analyzing", Logger.I)
    switchToAbstract()
    val result = runAnalysis()
    switchToConcrete()
    result
  }

  /**
    * Converts the given state to a new state corresponding to the state employed by the given P4F machine.
    * @param free The P4F machine for which a new, converted, state must be generated.
    * @param concSem The semantics currently being used.
    * @param abstSem The semantics to be used during the analysis.
    * @param programState The program state to be converted.
    */
  protected def convertState(
      free: Free[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T],
      concSem: SemanticsTraced[SchemeExp,
                               ConcreteConcreteLattice.L,
                               HybridAddress.A,
                               HybridTimestamp.T],
      abstSem: BaseSchemeSemantics[Abs, HybridAddress.A, HybridTimestamp.T],
      programState: PS): free.States = {
    val (control, _, store, kstore, a, t) =
      programState.convertState[Abs](free, concSem, abstSem)
    val convertedControl = control match {
      case ConvertedControlError(reason) => free.ControlError(reason)
      case ConvertedControlEval(exp, env) => free.ControlEval(exp, env)
      case ConvertedControlKont(v) => free.ControlKont(v)
    }
    free.States(Set(free.Configuration(convertedControl, a, t)), store, kstore)
  }

}
