object TraceAnalyzer {

  type Trace[Exp, Time] = Tracer[Exp,
                                 ConcreteConcreteLattice.L,
                                 HybridAddress.A,
                                 Time]#TraceWithInfos

  def collectTraceBoundAddresses[Exp: Expression, Time: Timestamp](
      trace: Trace[Exp, Time]): Set[HybridAddress.A] =
    trace.foldRight(Set[HybridAddress.A]())((stateInfo, boundAddresses) =>
      stateInfo match {
        case (_, infos) =>
          infos
            .find[Set[HybridAddress.A]]({
              case AddressesAllocated(_) => true;
              case AddressesReassigned(_) => true; case _ => false
            }, {
              case AddressesAllocated(addresses) =>
                boundAddresses ++ addresses.toSet;
              case AddressesReassigned(addresses) =>
                boundAddresses ++ addresses.toSet
            })
            .getOrElse(boundAddresses)
        case _ => boundAddresses
    })

  /**
    * Collects all the addresses that are looked up in the given trace.
    * @param trace The trace to be scanned for addresses that are looked up.
    */
  def collectAddressesLookedUp[Exp: Expression, Time: Timestamp](
      trace: Trace[Exp, Time]): Set[HybridAddress.A] =
    trace.foldLeft(Set[HybridAddress.A]())(
      (addressesLookedUp, instructionInfo) =>
        instructionInfo._2.find[Set[HybridAddress.A]]({
          case VariableLookedUp(_, _, _) => true
          case _ => false
        }, {
          case VariableLookedUp(_, address, _) => addressesLookedUp + address
        }) match {
          case Some(newAddressesLookedUp) => newAddressesLookedUp
          case None => addressesLookedUp
      })

}
