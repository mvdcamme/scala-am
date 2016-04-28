case class AmbProgramState[Exp : Expression, Time : Timestamp]
(normalState: ProgramState[Exp, Time],
 failStack: List[FailureStackElement[Exp, HybridLattice.Hybrid, HybridAddress]])
  extends ConcretableTracingProgramState[Exp, Time]
  with ConcreteTracingProgramState[Exp, HybridLattice.Hybrid, HybridAddress, Time] {

  def abs = implicitly[AbstractValue[HybridValue]]
  def addr = implicitly[Address[HybridAddress]]
  def time = implicitly[Timestamp[Time]]

  def wrapStep(step: InstructionStep[Exp, HybridValue, HybridAddress, Time, ProgramState[Exp, Time]]): InstructionStep[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]] = step match {
    case NormalInstructionStep(state, action) =>
      NormalInstructionStep(AmbProgramState(state, failStack), action)
    case GuardFailed(rp) => GuardFailed[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]](rp)
    case TraceEnded(rp) => TraceEnded[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]](rp)
  }

  def addFailAction(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
                    action: Action[Exp, HybridValue, HybridAddress],
                    failAction: Action[Exp, HybridValue, HybridAddress]):
  InstructionStep[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]] = {
    addFailActions(sem, action, List(failAction))
  }

  def addFailActions(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
                     action: Action[Exp, HybridValue, HybridAddress],
                     failActions: List[Action[Exp, HybridValue, HybridAddress]]):
  InstructionStep[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]] = {
    val step: InstructionStep[Exp, HybridValue, HybridAddress, Time, ProgramState[Exp, Time]] = normalState.applyAction(sem, action)
    step match {
      case NormalInstructionStep(state, action) =>
        NormalInstructionStep(AmbProgramState(state, failActions.map(UndoAction(_)) ++ failStack), action)
      case GuardFailed(rp) => GuardFailed[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]](rp)
      case TraceEnded(rp) => TraceEnded[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]](rp)
    }
  }

  def convertState(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time]) = normalState.convertState(sem)

  def runAssertions(assertions: List[Action[Exp, HybridValue, HybridAddress]]): Boolean = normalState.runAssertions(assertions)

  def restart(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
              restartPoint: RestartPoint[Exp, HybridValue, HybridAddress]): AmbProgramState[Exp, Time] = this

  def step(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time]): Option[Step[Exp, HybridValue, HybridAddress]] =
    normalState.step(sem)

  def applyAction(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
                  action: Action[Exp, HybridValue, HybridAddress]): InstructionStep[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]] = action match {
    case ActionPopFailKontTraced() => NormalInstructionStep(AmbProgramState(normalState, failStack.tail), action)
    case ActionPopKontTraced() =>
      val exp = normalState.control match {
        case TracingControlEval(e) => e
      }
      val failAction = ActionPush[Exp, HybridValue, HybridAddress](exp,
                                                                   normalState.kstore.lookup(normalState.a).head.frame,
                                                                   normalState.ρ,
                                                                   normalState.σ)
      addFailAction(sem, action, failAction)
    case ActionPrimCallTraced(n, fExp, argsExps) =>
      val (vals, _) = normalState.vStack.splitAt(n)
      val actionsSaveVal = vals.map({ (storable: Storable) => ActionPushSpecificValTraced[Exp, HybridValue, HybridAddress](storable.getVal) })
      addFailActions(sem, action, actionsSaveVal)
    case ActionPushFailKontTraced(failureStackElement) => failureStackElement match {
      case FailureFrame(frame) =>
        NormalInstructionStep(AmbProgramState(normalState, failureStackElement :: failStack), action)
      case UndoAction(undoAction) =>
        /* Should actually never happen:
         * actions to be undone should be added by directly pushing them to the failure stack in AmbProgramState,
         * not by including them in an ActionPushFailKontTraced
         */
        NormalInstructionStep(AmbProgramState(normalState, UndoAction(undoAction) :: failStack), action)
    }
    case ActionEvalPushTraced(e, frame, _, _) =>
      addFailAction(sem, action, ActionPopKontTraced[Exp, HybridValue, HybridAddress]())
    case ActionPushSpecificValTraced(value) =>
      val newNormalState = normalState.copy(vStack = StoreVal(value) :: normalState.vStack)
      NormalInstructionStep(AmbProgramState(newNormalState, failStack), action)
    case ActionPushValTraced() =>
      addFailAction(sem, action, ActionRestoreValTraced[Exp, HybridValue, HybridAddress]())
    case ActionRestoreEnvTraced() =>
      addFailAction(sem, action, ActionSaveSpecificEnvTraced[Exp, HybridValue, HybridAddress](normalState.vStack.head.getEnv))
    case ActionRestoreValTraced() =>
      NormalInstructionStep(AmbProgramState(normalState.copy(v = normalState.vStack.head.getVal, vStack = normalState.vStack.tail), failStack), action)
    case ActionSaveEnvTraced() =>
      addFailAction(sem, action, ActionRestoreEnvTraced[Exp, HybridValue, HybridAddress]())
    case ActionStepInTraced(fexp, e, args, argsv, n, frame, _, _) =>
      val (vals, _) = normalState.vStack.splitAt(n)
      val actionsSaveVal = vals.map({ (storable: Storable) => ActionPushSpecificValTraced[Exp, HybridValue, HybridAddress](storable.getVal) })
      addFailActions(sem, action, actionsSaveVal)
    case _ => wrapStep(normalState.applyAction(sem, action))
  }

  def generateTraceInformation(action : Action[Exp, HybridValue, HybridAddress]):
  Option[TraceInformation[HybridValue]] =
    normalState.generateTraceInformation(action)

  def concretableState = normalState

  def subsumes(that: TracingProgramState[Exp, HybridValue, HybridAddress, Time]): Boolean = that match {
    case that: AmbProgramState[Exp, Time] =>
      concreteSubsumes(that)
    case _ => false
  }

}
