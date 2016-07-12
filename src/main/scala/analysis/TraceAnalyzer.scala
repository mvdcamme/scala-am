object TraceAnalyzer {

  type Trace[Exp, Time] = Tracer[Exp, HybridLattice.L, HybridAddress.A, Time]#TraceWithInfos

  def collectTraceBoundAddresses[Exp : Expression, Time : Timestamp](trace: Trace[Exp, Time]): Set[HybridAddress.A] =
    trace.foldRight(Set[HybridAddress.A]())((stateInfo, boundAddresses) => stateInfo match {
      case (_, infos) =>
        infos.find[Set[HybridAddress.A]](
          { case AddressesAllocated(_) => true; case _ => false},
          { case AddressesAllocated(addresses) => boundAddresses ++ addresses.toSet }).getOrElse(boundAddresses)
      case (_, infos) =>
        infos.find[Set[HybridAddress.A]](
          { case AddressesReassigned(_) => true; case _ => false},
          { case AddressesReassigned(addresses) => boundAddresses ++ addresses.toSet }).getOrElse(boundAddresses)
      case _ => boundAddresses
    })

}
