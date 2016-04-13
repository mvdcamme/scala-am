/*
 * Tracing signals
 */
trait TracingSignal[Exp, Abs, Addr]

case class TracingSignalFalse[Exp : Expression, Abs : AbstractValue, Addr : Address]()
  extends TracingSignal[Exp, Abs, Addr]
case class TracingSignalStart[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (label: List[Exp]) extends TracingSignal[Exp, Abs, Addr]
case class TracingSignalEnd[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (label: List[Exp], restartPoint: RestartPoint[Exp, Abs, Addr]) extends TracingSignal[Exp, Abs, Addr]

/*
 * Interpreter return
 */
case class InterpreterReturn[Exp : Expression, Abs : AbstractValue, Addr : Address]
  (trace: List[Action[Exp, Abs, Addr]], tracingSignal: TracingSignal[Exp, Abs, Addr])