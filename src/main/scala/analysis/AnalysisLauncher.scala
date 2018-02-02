import ConcreteConcreteLattice.{L => ConcreteValue}
import backend.PathConstraint

abstract class AnalysisLauncher[Abs: IsConvertableLattice] {

  protected val abstSem = new ConvertableSchemeSemantics[Abs, HybridAddress.A, HybridTimestamp.T](new SchemePrimitives[HybridAddress.A, Abs])

  /* The concrete program state the static analysis gets as input. This state is then converted to an
   * abstract state and fed to the KickstartAAM. */
  type PS = ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T]
  /* The specific type of KickstartAAM used for this analysis: a KickstartAAM using the HybridLattice, HybridAddress and ZeroCFA
   * components. */
  type SpecAAM = KickstartAAMGlobalStore[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T]
  /* The specific environment used in the concrete state: an environment using the HybridAddress components. */
  type SpecEnv = Environment[HybridAddress.A]

  val aam: SpecAAM = new SpecAAM()

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
    * Converts the given state to a new state corresponding to the state employed by the given KickstartAAM.
    * @param aam The KickstartAAM for which a new, converted, state must be generated.
    * @param concSem The semantics currently being used.
    * @param abstSem The semantics to be used during the analysis.
    * @param programState The program state to be converted.
    */
  protected def convertStateAAM(aam: KickstartAAMGlobalStore[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T],
                                concSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                abstSem: ConvertableBaseSchemeSemantics[Abs, HybridAddress.A, HybridTimestamp.T],
                                programState: PS, pathConstraint: PathConstraint): aam.InitialState = {
    val (control, store, kstore, a, t) = programState.convertState[Abs](concSem, abstSem, HaltKontAddress, (x, _) => x, pathConstraint)
    val deltaStore = aam.GlobalStore(DeltaStore[HybridAddress.A, Abs](store.toSet.toMap, Map()), Map())
    val convertedControl = control match {
      case ConvertedControlError(reason) => aam.ControlError(reason)
      case ConvertedControlEval(exp, env) => aam.ControlEval(exp, env)
      case ConvertedControlKont(v) => aam.ControlKont(v)
    }
    (aam.State(convertedControl, a, t), deltaStore, kstore)
  }

  def runInitialStaticAnalysis(currentProgramState: PS, programName: String): StaticAnalysisResult
  def runStaticAnalysis(currentProgramState: PS, stepSwitched: Option[Int], addressesUsed: Set[HybridAddress.A]): StaticAnalysisResult

}
