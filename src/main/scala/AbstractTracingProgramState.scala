trait AbstractTracingProgramState[Exp, Abs, Addr] {

  def applyActionAbstract(action: Action[Exp, Abs, Addr]): Set[AbstractTracingProgramState[Exp, Abs, Addr]]

}

class AbstractProgramState[Exp : Expression, Time : Timestamp](concreteState: ProgramState[Exp, Time])
  extends AbstractTracingProgramState[Exp, HybridLattice.Hybrid, HybridAddress] {

  type HybridValue = HybridLattice.Hybrid

  def applyActionAbstract(action : Action[Exp, HybridValue, HybridAddress]): Set[AbstractTracingProgramState[Exp, HybridValue, HybridAddress]] = {
    try {
      action match {
        case ActionPopKontTraced() =>
          val nextsSet = if (concreteState.a == HaltKontAddress) {
            Set(HaltKontAddress)
          } else {
            concreteState.kstore.lookup(concreteState.a).map(_.next)
          }
          nextsSet.map((ka: KontAddr) =>
            new AbstractProgramState(new ProgramState[Exp, Time](TracingControlKont(concreteState.a),
                                                                 concreteState.ρ,
                                                                 concreteState.σ,
                                                                 concreteState.kstore,
                                                                 ka,
                                                                 concreteState.t,
                                                                 concreteState.v,
                                                                 concreteState.vStack)))
        case _ =>
          val result = concreteState.applyAction(action)
          result match {
            case NormalInstructionStep(newState, _) => newState match {
              case newState: ProgramState[Exp, Time] =>
                Set(new AbstractProgramState[Exp, Time](newState))
              case _ =>
                throw new Exception(s"$this expected newState of type ${ProgramState[Exp, Time]}, received $newState instead")
            }
            case GuardFailed(_) => Set(this) /* Guard failures (though they might happen) are not relevant here, so we ignore them */
            case _ => throw new Exception(s"Encountered an unexpected result while performing abstract interpretation: $result")
          }
      }
    } catch {
      case _ : IncorrectStackSizeException |
           _ : IncorrectStorableException |
           _ : VariableNotFoundException |
           _ : NotAPrimitiveException =>
        Set()
    }
  }

}
