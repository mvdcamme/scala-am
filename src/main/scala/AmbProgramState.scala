case class HaltFailFrame() extends Frame {
  def subsumes(that: Frame): Boolean = that.equals(this)
}

object HaltFailKontAddress extends KontAddr {
  override def toString = "HaltFailKontAddress"
}

case class AmbProgramState[Exp : Expression, Time : Timestamp]
(normalState: ProgramState[Exp, Time],
 failStack: List[Frame])
  extends ConcretableTracingProgramState[Exp, Time]
  with ConcreteTracingProgramState[Exp, HybridLattice.Hybrid, HybridAddress, Time] {

  def abs = implicitly[AbstractValue[HybridValue]]
  def addr = implicitly[Address[HybridAddress]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[Time]]

  def this(normalState: ProgramState[Exp, Time]) = this(normalState, List(HaltFailFrame()))

  def wrapApplyAction(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
                      action: Action[Exp, HybridValue, HybridAddress]):
    InstructionStep[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]] = normalState.applyAction(sem, action) match {
    case NormalInstructionStep(state, action) =>
      NormalInstructionStep(AmbProgramState(state, failStack), action)
    case GuardFailed(rp) => GuardFailed[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]](rp)
    case TraceEnded(rp) => TraceEnded[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]](rp)
  }

  def addFailAction(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
                    action: Action[Exp, HybridValue, HybridAddress],
                    failAction: ActionSingleT[Exp, HybridValue, HybridAddress]):
  InstructionStep[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]] = {
    addFailActions(sem, action, List(failAction))
  }

  def addFailActions(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
                     action: Action[Exp, HybridValue, HybridAddress],
                     failActions: List[ActionSingleT[Exp, HybridValue, HybridAddress]]):
  InstructionStep[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]] = {
    val step: InstructionStep[Exp, HybridValue, HybridAddress, Time, ProgramState[Exp, Time]] = normalState.applyAction(sem, action)
    step match {
      case NormalInstructionStep(state, action) =>
        val failureFrames = failActions.map({ action => UndoActionFrame(action) })
        NormalInstructionStep(AmbProgramState(state, failureFrames ++ failStack), action)
      case GuardFailed(rp) => GuardFailed[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]](rp)
      case TraceEnded(rp) => TraceEnded[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]](rp)
    }
  }

  def convertState(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time]) = normalState.convertState(sem)

  def runAssertions(assertions: List[Action[Exp, HybridValue, HybridAddress]]): Boolean = normalState.runAssertions(assertions)

  def restart(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
              restartPoint: RestartPoint[Exp, HybridValue, HybridAddress]): AmbProgramState[Exp, Time] = restartPoint match {
      case RestartStoppedInBacktrack() =>
        val newNormalState = normalState.restart(sem, RestartTraceEnded())
        AmbProgramState(newNormalState, failStack)
      case _ =>
        val newNormalState = normalState.restart(sem, restartPoint)
        AmbProgramState(newNormalState, failStack)
    }

  def step(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time]): Option[Step[Exp, HybridValue, HybridAddress]] = {
    val someStep = normalState.step(sem)
    someStep match {
      case None => None /* Step did not succeed */
      case Some(step) =>
        type PopKontAction = ActionSinglePopKontT[Exp, HybridValue, HybridAddress]
        val trace = step.trace
        val someActionSinglePopKont = trace.find(_.isInstanceOf[PopKontAction])
        someActionSinglePopKont match {
          case None => someStep
          case Some(actionSinglePopKont) =>
            val someTopKont = normalState.kstore.lookup(normalState.a).headOption
            someTopKont match {
              case None => someStep
              case Some(topKont) =>
                val someBody = sem.getClosureBody(topKont.frame)
                someBody match {
                  case None => someStep
                  case Some(body) =>
                    val signal = TracingSignalEnd(body, RestartStoppedInBacktrack[Exp, HybridValue, HybridAddress]())
                    Some(Step(trace, signal))
                }
            }
        }
    }
  }

  def applyAction(sem: SemanticsTraced[Exp, HybridValue, HybridAddress, Time],
                  action: Action[Exp, HybridValue, HybridAddress]):
  InstructionStep[Exp, HybridValue, HybridAddress, Time, AmbProgramState[Exp, Time]] = action match {
    case ActionEvalPushT(e, frame, _, _) =>
      addFailAction(sem, action, ActionSinglePopKontT[Exp, HybridValue, HybridAddress]())
    case ActionExtendEnvT(_) =>
      addFailAction(sem, action, ActionSingleSaveValT[Exp, HybridValue, HybridAddress](normalState.vStack.head.getVal))
    case ActionPopFailKontT() => failStack match {
      case head :: tail =>
        /* Transfer frame that was allocated at the failstack to the normal continuation stack:
         * Allocate a new address that will point to this frame and then extend the kstore with the new address
         * and the frame. */
        val next = head match {
          case HaltFailFrame() => HaltFailKontAddress
          case _ => NormalKontAddress(exp.zeroExp, addr.variable("__kont__", normalState.t)) // Hack to get infinite number of addresses in concrete mode
        }
        val extendedKStore = normalState.kstore.extend(next, Kont(head, normalState.a))
        val newNormalState = normalState.copy(control = TracingControlKont(next), kstore = extendedKStore)
        NormalInstructionStep(AmbProgramState(newNormalState, failStack.tail), action)
      case Nil =>
        addFailActions(sem, ActionErrorT("Failstack empty!"), Nil)
    }
    case ActionPopKontT() =>
      if (normalState.a == HaltKontAddress) {
        addFailActions(sem, action, Nil)
      } else {
        val failAction = ActionSinglePushKontT[Exp, HybridValue, HybridAddress](normalState.kstore.lookup(normalState.a).head.frame)
        addFailAction(sem, action, failAction)
      }
    case ActionPrimCallT(n, fExp, argsExps) =>
      val (vals, _) = normalState.vStack.splitAt(n)
      val actionsSaveVal = vals.map({ (storable: Storable) => ActionSingleSaveValT[Exp, HybridValue, HybridAddress](storable.getVal) })
      addFailActions(sem, action, actionsSaveVal)
    case ActionPushFailKontT(failureFrame) =>
      NormalInstructionStep(AmbProgramState(normalState, failureFrame :: failStack), action)
    case ActionPushValT() =>
      addFailAction(sem, action, ActionSingleRestoreValT[Exp, HybridValue, HybridAddress]())
    case ActionRestoreEnvT() =>
      addFailAction(sem, action, ActionSingleSaveSpecificEnvT[Exp, HybridValue, HybridAddress](normalState.vStack.head.getEnv, normalState.ρ))
    case ActionSaveEnvT() =>
      addFailAction(sem, action, ActionSingleRestoreEnvT[Exp, HybridValue, HybridAddress]())
    case ActionStepInT(fexp, e, args, argsv, n, frame, _, _) =>
      val (vals, _) = normalState.vStack.splitAt(n)
      /*
       * Applying a closure => pop values (operator + operands) from stack;
       *                       save extended lexical environment
       *                       push FrameFunBody continuation
       */
      val actionsSaveVal = vals.map({ (storable: Storable) => ActionSingleSaveValT[Exp, HybridValue, HybridAddress](storable.getVal) })
      val failActions = ActionSinglePopKontT[Exp, HybridValue, HybridAddress]() ::
                        ActionSingleRestoreEnvT[Exp, HybridValue, HybridAddress]() ::
                        actionsSaveVal
      addFailActions(sem, action, failActions)
    case ActionSinglePopKontT() =>
      wrapApplyAction(sem, ActionPopKontT())
    case ActionSinglePushKontT(frame) =>
      val next = NormalKontAddress(exp.zeroExp, addr.variable("__kont__", normalState.t)) // Hack to get infinite number of addresses in concrete mode
      val extendedKStore = normalState.kstore.extend(next, Kont(frame, normalState.a))
      val newNormalState = normalState.copy(kstore = extendedKStore, a = next)
      NormalInstructionStep(AmbProgramState(newNormalState, failStack), action)
    case ActionSingleRestoreEnvT() =>
      wrapApplyAction(sem, ActionRestoreEnvT())
    case ActionSingleRestoreValT() => normalState.vStack match {
      case head :: rest =>
        val newNormalState = normalState.copy(v = head.getVal, vStack = rest)
        NormalInstructionStep(AmbProgramState(newNormalState, failStack), action)
      case Nil =>
        throw new Exception("Value stack is empty!")
    }
    case ActionSingleSaveSpecificEnvT(ρToSave, ρToReplace) =>
      val newNormalState = normalState.copy(ρ = ρToReplace, vStack = StoreEnv(ρToSave) :: normalState.vStack)
      NormalInstructionStep(AmbProgramState(newNormalState, failStack), action)
    case ActionSingleSaveValT(value) =>
      val newNormalState = normalState.copy(vStack = StoreVal(value) :: normalState.vStack)
      NormalInstructionStep(AmbProgramState(newNormalState, failStack), action)
    case _ => wrapApplyAction(sem, action)
  }

  override def halted = normalState.control match {
    case TracingControlKont(HaltFailKontAddress) => true
    case _ => super.halted
  }

  def generateTraceInformation(action: Action[Exp, HybridValue, HybridAddress]):
  Option[TraceInformation[HybridValue]] =
    normalState.generateTraceInformation(action)

  def concretableState = normalState

  def subsumes(that: TracingProgramState[Exp, HybridValue, HybridAddress, Time]): Boolean = that match {
    case that: AmbProgramState[Exp, Time] =>
      concreteSubsumes(that)
    case _ => false
  }

}
