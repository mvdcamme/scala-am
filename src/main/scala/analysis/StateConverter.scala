import ConcreteConcreteLattice.{L => ConcreteValue}
import backend.PathConstraint

/**
  *
  * @param aam The KickstartAAM for which a new, converted, state must be generated.
  * @param concSem The semantics currently being used.
  * @param abstSem The semantics to be used during the analysis.
  * @tparam Abs
  */
class StateConverter[Abs: IsConvertableLattice](val aam: KickstartAAMGlobalStore[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T],
  val concSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
  val abstSem: ConvertableBaseSchemeSemantics[Abs, HybridAddress.A, HybridTimestamp.T]) {

  /**
    * Converts the given state to a new state corresponding to the state employed by the given KickstartAAM.
    * @param programState The program state to be converted.
    * @param pathConstraint
    */
  def convertStateAAM(programState: ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T], pathConstraint: PathConstraint): aam.InitialState = {
    val (control, store, kstore, a, t) = programState.convertState[Abs](concSem, abstSem, pathConstraint)
    val deltaStore = aam.GlobalStore(DeltaStore[HybridAddress.A, Abs](store.toSet.toMap, Map()), Map())
    val convertedControl = control match {
      case ConvertedControlError(reason) => aam.ControlError(reason)
      case ConvertedControlEval(exp, env) => aam.ControlEval(exp, env)
      case ConvertedControlKont(v) => aam.ControlKont(v)
    }
    (aam.State(convertedControl, a, t), deltaStore, kstore)
  }

}
