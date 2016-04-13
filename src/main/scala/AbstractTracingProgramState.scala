trait AbstractTracingProgramState[Exp, Abs, Addr, Time] extends TracingProgramState[Exp, Abs, Addr, Time] {

  val sem = implicitly[SemanticsTraced[Exp, Abs, Addr, Time]]

  def applyActionAbstract(action: Action[Exp, Abs, Addr]): Set[AbstractTracingProgramState[Exp, Abs, Addr, Time]]

  def stepAbstract(): Set[(AbstractTracingProgramState[Exp, Abs, Addr, Time], sem.Trace)]

}

class AbstractProgramState[Exp : Expression, Time : Timestamp](concreteState: ProgramState[Exp, Time])
  extends AbstractTracingProgramState[Exp, HybridLattice.Hybrid, HybridAddress, Time] {

  type HybridValue = HybridLattice.Hybrid

  val abs = implicitly[AbstractValue[HybridValue]]

  def applyActionAbstract(action : Action[Exp, HybridValue, HybridAddress]): Set[AbstractProgramState[Exp, Time]] = {
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

  private def applyTraceAbstract(trace : sem.Trace) : Set[(AbstractProgramState[Exp, Time], sem.Trace)] = {
    val newStates = trace.foldLeft(Set(this))({ (currentStates, action) =>
      currentStates.flatMap(_.applyActionAbstract(action))
    })
    newStates.map({ (newState) => (newState, trace) })
  }

  private def integrate(a: KontAddr, interpreterReturns: Set[sem.InterpreterReturn]): Set[(AbstractProgramState[Exp, Time], sem.Trace)] = {
    interpreterReturns.flatMap({itpRet => itpRet match {
      case sem.InterpreterReturn(trace, _) =>
        applyTraceAbstract(trace)
    }})
  }

  def stepAbstract(): Set[(AbstractProgramState[Exp, Time], sem.Trace)] = {
    concreteState.control match {
      /* In a eval state, call the semantic's evaluation method */
      case TracingControlEval(e) => integrate(concreteState.a, sem.stepEval(e, concreteState.ρ, concreteState.σ, concreteState.t))
      /* In a continuation state, if the value reached is not an error, call the
       * semantic's continuation method */
      case TracingControlKont(_) if abs.isError(concreteState.v) => Set()
      case TracingControlKont(ka) => concreteState.kstore.lookup(ka).flatMap({
        case Kont(frame, next) => integrate(next, sem.stepKont(concreteState.v, frame, concreteState.σ, concreteState.t))
      })
      /* In an error state, the state is not able to make a step */
      case TracingControlError(_) => Set()
    }
  }

  /**
    * Returns the set of final values that can be reached
    */
  def finalValues = control match {
    case TracingControlKont(_) => Set[HybridValue](v)
    case _ => Set[HybridValue]()
  }

}
