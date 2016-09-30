case class HaltFailFrame() extends Frame {
  def subsumes(that: Frame): Boolean = that.equals(this)
}

object HaltFailKontAddress extends KontAddr {
  override def toString = "HaltFailKontAddress"
}

case object CannotBacktrackError extends SemanticError {
  override def toString = "Program cannot backtrack any further"
}

case class AmbProgramState[Exp : Expression]
(normalState: ProgramState[Exp],
 failStack: List[Frame])
  extends ConcreteTracingProgramState[Exp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]
  with ConcretableTracingProgramState[Exp] {

  def abs = implicitly[JoinLattice[ConcreteConcreteLattice.L]] //TODO change back to HybridValue
  def addr = implicitly[Address[HybridAddress.A]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[HybridTimestamp.T]]

  def this(normalState: ProgramState[Exp]) = this(normalState, List(HaltFailFrame()))

  def wrapApplyAction(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T],
                      action: ActionT[Exp, HybridValue, HybridAddress.A]):
    ActionReturn[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T, AmbProgramState[Exp]] = normalState.applyAction(sem, action) match {
    case ActionStep(state, action) =>
      ActionStep(AmbProgramState(state, failStack), action)
    case GuardFailed(rp, guardID) => GuardFailed[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T, AmbProgramState[Exp]](rp, guardID)
    case TraceEnded(rp) => TraceEnded[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T, AmbProgramState[Exp]](rp)
  }

  def addFailAction(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T],
                    action: ActionT[Exp, HybridValue, HybridAddress.A],
                    failAction: ActionSingleT[Exp, HybridValue, HybridAddress.A]):
  ActionReturn[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T, AmbProgramState[Exp]] = {
    addFailActions(sem, action, List(failAction))
  }

  def addFailActions(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T],
                     action: ActionT[Exp, HybridValue, HybridAddress.A],
                     failActions: List[ActionSingleT[Exp, HybridValue, HybridAddress.A]]):
  ActionReturn[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T, AmbProgramState[Exp]] = {
    val step: ActionReturn[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T, ProgramState[Exp]] = normalState.applyAction(sem, action)
    step match {
      case ActionStep(state, action) =>
        val failureFrames = failActions.map({ action => UndoActionFrame(action) })
        ActionStep(AmbProgramState(state, failureFrames ++ failStack), action)
      case GuardFailed(rp, guardID) => GuardFailed[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T, AmbProgramState[Exp]](rp, guardID)
      case TraceEnded(rp) => TraceEnded[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T, AmbProgramState[Exp]](rp)
    }
  }

  def convertState[Abs : IsSchemeLattice](free: Free[Exp, Abs, HybridAddress.A, HybridTimestamp.T],
                   oldSem: SemanticsTraced[Exp, Abs, HybridAddress.A, HybridTimestamp.T]) =
    normalState.convertState(free, oldSem)

  def runHeader(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T],
                assertions: List[ActionT[Exp, HybridValue, HybridAddress.A]]): Option[AmbProgramState[Exp]] =
    normalState.runHeader(sem, assertions).fold(None: Option[AmbProgramState[Exp]])(programState => Some(AmbProgramState(programState, failStack)))

  def restart(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T],
              restartPoint: RestartPoint[Exp, HybridValue, HybridAddress.A]): AmbProgramState[Exp] = restartPoint match {
      case RestartStoppedInBacktrack() =>
        val newNormalState = normalState.restart(sem, RestartTraceEnded())
        AmbProgramState(newNormalState, failStack)
      case _ =>
        val newNormalState = normalState.restart(sem, restartPoint)
        AmbProgramState(newNormalState, failStack)
    }

  def step(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T])
          :Option[InterpreterStep[Exp, HybridValue, HybridAddress.A]] = {
    val someStep = normalState.step(sem)
    someStep match {
      case None => None /* Step did not succeed */
      case Some(step) =>
        /* Check whether a function body frame was just popped from the stack.
         * If so, send a SignalEndLoop, in case the tracer had started tracing at this function call. */
        type PopKontAction = ActionSinglePopKontT[Exp, HybridValue, HybridAddress.A]
        val trace = step.trace
        val someActionSinglePopKont = trace.find(_.isInstanceOf[PopKontAction])
        someActionSinglePopKont match {
          case None => Some(step)
          case Some(actionSinglePopKont) =>
            val someTopKont = normalState.kstore.lookup(normalState.a).headOption
            someTopKont match {
              case None => Some(step)
              case Some(topKont) =>
                val someBody = sem.getClosureBody(topKont.frame)
                someBody match {
                  case None => Some(step)
                  case Some(body) =>
                    val signal = SignalEndLoop(body, RestartStoppedInBacktrack[Exp, HybridValue, HybridAddress.A]())
                    Some(InterpreterStep(trace, signal))
                }
            }
        }
    }
  }

  def applyAction(sem: SemanticsTraced[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T],
                  action: ActionT[Exp, HybridValue, HybridAddress.A]):
  ActionReturn[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T, AmbProgramState[Exp]] = action match {
    case ActionEvalPushT(e, frame, _, _) =>
      addFailAction(sem, action, ActionSinglePopKontT[Exp, HybridValue, HybridAddress.A]())
    case ActionExtendEnvT(_) =>
      addFailAction(sem, action, ActionSingleSaveValT[Exp, HybridValue, HybridAddress.A](normalState.vStack.head.getVal))
    case ActionPopFailKontT() => failStack match {
      case head :: tail =>
        /* Transfer frame that was allocated at the failstack to the normal continuation stack:
         * Allocate a new address that will point to this frame and then extend the kstore with the new address
         * and the frame. */
        val next = head match {
          case HaltFailFrame() => HaltFailKontAddress
          case _ => NoExpKontAddress(normalState.t)
        }
        val extendedKStore = normalState.kstore.extend(next, Kont(head, normalState.a))
        val newNormalState = normalState.copy(control = TracingControlKont(next),
                                              kstore = extendedKStore,
                                              t = time.tick(normalState.t))
        ActionStep(AmbProgramState(newNormalState, failStack.tail), action)
      case Nil =>
        addFailActions(sem, ActionErrorT(CannotBacktrackError), Nil)
    }
    case ActionPopKontT() =>
      if (normalState.a == HaltKontAddress) {
        addFailActions(sem, action, Nil)
      } else {
        val failAction = ActionSinglePushKontT[Exp, HybridValue, HybridAddress.A](normalState.kstore.lookup(normalState.a).head.frame)
        addFailAction(sem, action, failAction)
      }
    case ActionPrimCallT(n, fExp, argsExps) =>
      val (vals, _) = normalState.vStack.splitAt(n)
      val actionsSaveVal = vals.map({ (storable: Storable[HybridValue, HybridAddress.A]) => ActionSingleSaveValT[Exp, HybridValue, HybridAddress.A](storable.getVal) })
      addFailActions(sem, action, actionsSaveVal)
    case ActionPushFailKontT(failureFrame) =>
      ActionStep(AmbProgramState(normalState, failureFrame :: failStack), action)
    case ActionPushValT() =>
      addFailAction(sem, action, ActionSingleRestoreValT[Exp, HybridValue, HybridAddress.A]())
    case ActionRestoreEnvT() =>
      addFailAction(sem, action, ActionSingleSaveSpecificEnvT[Exp, HybridValue, HybridAddress.A](normalState.vStack.head.getEnv, normalState.ρ))
    case ActionSaveEnvT() =>
      addFailAction(sem, action, ActionSingleRestoreEnvT[Exp, HybridValue, HybridAddress.A]())
    case ActionStepInT(fexp, e, args, argsv, n, frame, _, _) =>
      val (vals, _) = normalState.vStack.splitAt(n)
      /*
       * Applying a closure => pop values (operator + operands) from stack;
       *                       save extended lexical environment
       *                       push FrameFunBody continuation
       */
      val actionsSaveVal = vals.map({ (storable: Storable[HybridValue, HybridAddress.A]) => ActionSingleSaveValT[Exp, HybridValue, HybridAddress.A](storable.getVal) })
      val failActions = ActionSinglePopKontT[Exp, HybridValue, HybridAddress.A]() ::
                        ActionSingleRestoreEnvT[Exp, HybridValue, HybridAddress.A]() ::
                        actionsSaveVal
      addFailActions(sem, action, failActions)
    case ActionSinglePopKontT() =>
      wrapApplyAction(sem, ActionPopKontT())
    case ActionSinglePushKontT(frame) =>
      val next = NoExpKontAddress(normalState.t) // Hack to get infinite number of addresses in concrete mode
      val extendedKStore = normalState.kstore.extend(next, Kont(frame, normalState.a))
      val newNormalState = normalState.copy(kstore = extendedKStore, a = next, t = time.tick(normalState.t))
      ActionStep(AmbProgramState(newNormalState, failStack), action)
    case ActionSingleRestoreEnvT() =>
      wrapApplyAction(sem, ActionRestoreEnvT())
    case ActionSingleRestoreValT() => normalState.vStack match {
      case head :: rest =>
        val newNormalState = normalState.copy(v = head.getVal, vStack = rest)
        ActionStep(AmbProgramState(newNormalState, failStack), action)
      case Nil =>
        throw new Exception("Value stack is empty!")
    }
    case ActionSingleSaveSpecificEnvT(ρToSave, ρToReplace) =>
      val newNormalState = normalState.copy(ρ = ρToReplace, vStack = StoreEnv[HybridValue, HybridAddress.A](ρToSave) :: normalState.vStack)
      ActionStep(AmbProgramState(newNormalState, failStack), action)
    case ActionSingleSaveValT(value) =>
      val newNormalState = normalState.copy(vStack = StoreVal[HybridValue, HybridAddress.A](value) :: normalState.vStack)
      ActionStep(AmbProgramState(newNormalState, failStack), action)
    case _ => wrapApplyAction(sem, action)
  }

  override def halted = normalState.control match {
    case TracingControlKont(HaltFailKontAddress) => true
    case _ => super.halted
  }

  def generateTraceInformation(action: ActionT[Exp, HybridValue, HybridAddress.A]):
  CombinedInfos[HybridValue, HybridAddress.A] =
    normalState.generateTraceInformation(action)

  def concretableState = normalState

  def subsumes(that: TracingProgramState[Exp, HybridValue, HybridAddress.A, HybridTimestamp.T]): Boolean = that match {
    case that: AmbProgramState[Exp] =>
      concreteSubsumes(that)
    case _ => false
  }

}
