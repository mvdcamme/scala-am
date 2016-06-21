case class TraceFull[Exp : Expression, Time : Timestamp]
  (startProgramState: ConcreteTracingProgramState[Exp, HybridLattice.L, HybridAddress.A, Time],
   assertions: List[Action[Exp, HybridLattice.L, HybridAddress.A]],
   trace: List[(Action[Exp, HybridLattice.L, HybridAddress.A], Option[TraceInformation[HybridLattice.L]])])