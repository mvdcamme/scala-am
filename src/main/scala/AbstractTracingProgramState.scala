trait AbstractTracingProgramState[Exp, Abs, Addr, Time] extends TracingProgramState[Exp, Abs, Addr, Time] {

  def applyActionAbstract(sem: SemanticsTraced[Exp, Abs, Addr, Time],
                          action: Action[Exp, Abs, Addr]): Set[AbstractTracingProgramState[Exp, Abs, Addr, Time]]

  def stepAbstract(sem: SemanticsTraced[Exp, Abs, Addr, Time]):
    Set[(AbstractTracingProgramState[Exp, Abs, Addr, Time], List[Action[Exp, Abs, Addr]])]

}

case class AbstractProgramState[Exp : Expression, Time : Timestamp](concreteState: ProgramState[Exp, Time])
  extends AbstractTracingProgramState[Exp, HybridLattice.Hybrid, HybridAddress, Time]
  with ConcretableTracingProgramState[Exp, Time] {

  type HybridValue = HybridLattice.Hybrid

  def abs = implicitly[AbstractValue[HybridValue]]
  def addr = implicitly[Address[HybridAddress]]
  def time = implicitly[Timestamp[Time]]

  def applyActionAbstract(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
                          action : Action[Exp, HybridValue, HybridAddress]): Set[AbstractTracingProgramState[Exp, HybridValue, HybridAddress, Time]] = {
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
          val result = concreteState.applyAction(sem, action)
          result match {
            case NormalInstructionStep(newState, _) => newState match {
              case newState: ProgramState[Exp, Time] =>
                Set(new AbstractProgramState[Exp, Time](newState))
              case _ =>
                throw new Exception(s"$this expected newState of type ProgramState[Exp, Time], received $newState instead")
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

  private def applyTraceAbstract(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
                                 trace: List[Action[Exp, HybridValue, HybridAddress]]):
    Set[(AbstractTracingProgramState[Exp, HybridValue, HybridAddress, Time], List[Action[Exp, HybridValue, HybridAddress]])] = {
    val newStates = trace.foldLeft(Set[AbstractTracingProgramState[Exp, HybridValue, HybridAddress, Time]](this))({ (currentStates, action) =>
      currentStates.flatMap(_.applyActionAbstract(sem, action))
    })
    newStates.map({ (newState) => (newState, trace) })
  }

  private def integrate(sem: SemanticsTraced[Exp, HybridLattice.Hybrid, HybridAddress, Time])
                       (a: KontAddr,
                        interpreterReturns: Set[Step[Exp, HybridValue, HybridAddress]]):
    Set[(AbstractTracingProgramState[Exp, HybridValue, HybridAddress, Time], List[Action[Exp, HybridValue, HybridAddress]])] = {
    interpreterReturns.flatMap({itpRet => itpRet match {
      case Step(trace, _) =>
        applyTraceAbstract(sem, trace)
    }})
  }

  def stepAbstract(sem: SemanticsTraced[Exp, HybridLattice.Hybrid, HybridAddress, Time]): Set[(AbstractTracingProgramState[Exp, HybridValue, HybridAddress, Time], List[Action[Exp, HybridValue, HybridAddress]])] = {
    concreteState.control match {
      /* In a eval state, call the semantic's evaluation method */
      case TracingControlEval(e) => integrate(sem)(concreteState.a, sem.stepEval(e, concreteState.ρ, concreteState.σ, concreteState.t))
      /* In a continuation state, if the value reached is not an error, call the
       * semantic's continuation method */
      case TracingControlKont(_) if abs.isError(concreteState.v) => Set()
      case TracingControlKont(ka) => concreteState.kstore.lookup(ka).flatMap({
        case Kont(frame, next) => integrate(sem)(next, sem.stepKont(concreteState.v, frame, concreteState.σ, concreteState.t))
      })
      /* In an error state, the state is not able to make a step */
      case TracingControlError(_) => Set()
    }
  }

  def concretableState = concreteState

  def subsumes(that: TracingProgramState[Exp, HybridValue, HybridAddress, Time]): Boolean = that match {
    case that: AbstractProgramState[Exp, Time] =>
      concreteSubsumes(that)
    case _ => false
  }

}
