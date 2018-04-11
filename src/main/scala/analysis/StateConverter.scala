import backend.PathConstraint

import aam_global_store.GlobalStore

/**
  *
  * @param aam The KickstartAAM for which a new, converted, state must be generated.
  * @param abstSem The semantics to be used during the analysis.
  * @tparam Abs
  */
class StateConverter[Abs: IsConvertableLattice](val aam: KickstartAAMGlobalStore[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T],
  val abstSem: ConvertableBaseSchemeSemantics[Abs, HybridAddress.A, HybridTimestamp.T]) {

  /**
    * Converts the given state to a new state corresponding to the state employed by the given KickstartAAM.
    * @param programState The program state to be converted.
    * @param pathConstraint
    */
  def convertStateAAM(programState: ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T], pathConstraint: PathConstraint): aam.InitialState = {
    val (control, store, kstore, a, t) = programState.convertState[Abs](abstSem, pathConstraint)
    val deltaStore = GlobalStore(DeltaStore[HybridAddress.A, Abs](store.toSet.toMap, Map()), Map())
    val convertedControl = control match {
      case ConvertedControlError(reason) => ControlError[SchemeExp, Abs, HybridAddress.A](reason)
      case ConvertedControlEval(exp, env) => ControlEval[SchemeExp, Abs, HybridAddress.A](exp, env)
      case ConvertedControlKont(v) => ControlKont[SchemeExp, Abs, HybridAddress.A](v)
    }
    (KickstartAAMState(convertedControl, a, t), deltaStore, kstore)
  }

}
