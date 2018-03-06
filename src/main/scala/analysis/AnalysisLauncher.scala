import ConcreteConcreteLattice.{L => ConcreteValue}
import backend.PathConstraint

trait AnalysisOutputGraph[Exp, Abs, Addr, State <: StateTrait[Exp, Abs, Addr, _]] {
  def halted: Set[State]
  def errorStates: Set[State]
  def graph: Graph[State, EdgeAnnotation[Exp, Abs, Addr], Set[State]]
  def finalStores: Set[Store[Addr, Abs]]
  def stepSwitched: Option[Int]
  def toFile(path: String)(output: GraphOutput): Unit
  def replaceGraph(graph: Graph[State, EdgeAnnotation[Exp, Abs, Addr], Set[State]]): AnalysisOutputGraph[Exp, Abs, Addr, State]
}

abstract class AnalysisLauncher[Abs: IsConvertableLattice](
  val concSem: ConvertableSemantics[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T],
  val abstSem: ConvertableBaseSchemeSemantics[Abs, HybridAddress.A, HybridTimestamp.T]) {

  /* The concrete program state the static analysis gets as input. This state is then converted to an
   * abstract state and fed to the KickstartAAM. */
  type PS = ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T]
  /* The specific type of KickstartAAM used for this analysis: a KickstartAAM using the HybridLattice, HybridAddress and ZeroCFA
   * components. */
  type SpecAAM = KickstartAAMGlobalStore[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T]
  /* The specific environment used in the concrete state: an environment using the HybridAddress components. */
  type SpecEnv = Environment[HybridAddress.A]

  val aam: SpecAAM = new SpecAAM()
  val stateConverter: StateConverter[Abs] = new StateConverter[Abs](aam, concSem, abstSem)

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

  protected def wrapRunAnalysis(runAnalysis: () => AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, aam.State]): AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, aam.State] = {
    Logger.log("analyzing", Logger.I)
    wrapAbstractEvaluation[AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, aam.State]](runAnalysis)
  }

  def runInitialStaticAnalysis(initialAbstractState: stateConverter.aam.InitialState, programName: String): AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, aam.State]
  def runInitialStaticAnalysis(currentProgramState: PS, programName: String): AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, aam.State] = {
    runInitialStaticAnalysis(stateConverter.convertStateAAM(currentProgramState, Nil), programName)
  }
  def runStaticAnalysis(initialAbstractState: stateConverter.aam.InitialState, stepSwitched: Option[Int], addressesUsed: Set[HybridAddress.A],
                        pathConstraint: PathConstraint): AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, aam.State]
  def runStaticAnalysis(state: PS, stepSwitched: Option[Int], addressesUsed: Set[HybridAddress.A],
                        pathConstraint: PathConstraint): AnalysisOutputGraph[SchemeExp, Abs, HybridAddress.A, aam.State] = {
    runStaticAnalysis(stateConverter.convertStateAAM(state, pathConstraint), stepSwitched, addressesUsed, pathConstraint)
  }

}
