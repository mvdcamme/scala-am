/*
 * Tracing signals
 */
trait TracingSignal[Exp, Abs, Addr]

case class SignalFalse[Exp : Expression, Abs : JoinLattice, Addr : Address]()
  extends TracingSignal[Exp, Abs, Addr]
case class SignalStartLoop[Exp : Expression, Abs : JoinLattice, Addr : Address]
  (label: List[Exp])
  extends TracingSignal[Exp, Abs, Addr]
case class SignalEndLoop[Exp : Expression, Abs : JoinLattice, Addr : Address]
  (label: List[Exp], restartPoint: RestartPoint[Exp, Abs, Addr])
  extends TracingSignal[Exp, Abs, Addr]

/**
  * Signals the tracer to start a run-time static analysis.
  * Has no effect on tracing.
  */
case class SignalStartAnalysis[Exp : Expression, Abs : JoinLattice, Addr : Address]()
  extends TracingSignal[Exp, Abs, Addr]

/*
 * Interpreter return
 */
case class InterpreterStep[Exp : Expression, Abs : JoinLattice, Addr : Address]
  (trace: List[ActionT[Exp, Abs, Addr]], tracingSignal: TracingSignal[Exp, Abs, Addr])