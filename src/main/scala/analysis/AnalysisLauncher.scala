abstract class AnalysisLauncher[Exp : Expression]
  (sem: SemanticsTraced[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T]) {

  /* The concrete program state the static analysis gets as input. This state is then converted to an
   * abstract state and fed to the AAM. */
  type PS = HybridMachine[Exp]#PS
  /* The specific type of AAM used for this analysis: an AAM using the HybridLattice, HybridAddress and ZeroCFA
   * components. */
  type SpecAAM = AAM[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T]
  /* The specific environment used in the concrete state: an environment using the HybridAddress components. */
  type SpecEnv = Environment[HybridAddress.A]

  protected def switchToAbstract(): Unit = {
    Logger.log("HybridMachine switching to abstract", Logger.I)
    HybridTimestamp.switchToAbstract()
    HybridLattice.switchToAbstract()
  }

  protected def switchToConcrete(): Unit = {
    Logger.log("HybridMachine switching to concrete", Logger.I)
    HybridTimestamp.switchToConcrete()
    HybridLattice.switchToConcrete()
  }

  protected def wrapRunAnalysis(runAnalysis: () => StaticAnalysisResult): StaticAnalysisResult = {
    Logger.log("analyzing", Logger.I)
    switchToAbstract()
    val result = runAnalysis()
    switchToConcrete()
    result
  }

  /**
   * Converts the given state to a new state corresponding to the state employed by the given AAM.
   * @param aam The AAM for which a new, converted, state must be generated.
   * @param programState The program state to be converted.
   */
  protected def convertState(aam: AAM[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T],
                             programState: PS): aam.State = {
    val (control, _, store, kstore, a, t) = programState.convertState(aam)(sem)
    val convertedControl = control match {
      case ConvertedControlError(reason) => aam.ControlError(reason)
      case ConvertedControlEval(exp, env) => aam.ControlEval(exp, env)
      case ConvertedControlKont(v) => aam.ControlKont(v)
    }
    aam.State(convertedControl, store, kstore, a, t)
  }

}
