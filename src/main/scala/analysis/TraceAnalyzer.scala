object TraceAnalyzer {

  type Trace[Exp, Time] = Tracer[Exp, Time]#TraceWithInfos

  def collectTraceBoundAddresses[Exp : Expression, Time : Timestamp](trace: Trace[Exp, Time]): Set[HybridAddress.A] =
    trace.foldRight(Set[HybridAddress.A]())((stateInfo, boundAddresses) => stateInfo match {
      case (_, infos) =>
        infos.find[Set[HybridAddress.A]](
          { case VariablesAllocated(_) => true; case _ => false},
          { case VariablesAllocated(addresses) => boundAddresses ++ addresses.toSet }).getOrElse(boundAddresses)
      case (_, infos) =>
        infos.find[Set[HybridAddress.A]](
          { case VariablesReassigned(_) => true; case _ => false},
          { case VariablesReassigned(addresses) => boundAddresses ++ addresses.toSet }).getOrElse(boundAddresses)
      case _ => boundAddresses
    })

}
