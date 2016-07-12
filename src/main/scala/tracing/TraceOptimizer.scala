trait TraceOptimizer[Exp, Abs, Addr, Time] {

  def optimize(trace: TraceFull[Exp, Abs, Addr, Time],
               state: ConcreteTracingProgramState[Exp, Abs, Addr, Time])
              :TraceFull[Exp, Abs, Addr, Time]

}
