class UsesGraph[Exp : Expression,
                Abs : IsSchemeLattice,
                Addr : Address,
                State <: StateTrait[Exp, Abs, Addr, _]] {

  type EdgeAnnotation2 = EdgeAnnotation[Exp, Abs, Addr]
  type Edge = (EdgeAnnotation2, State)
  type AbstractGraph = Graph[State, EdgeAnnotation2]

  type AbstractFrame = SchemeFrame[Abs, HybridAddress.A, HybridTimestamp.T]
  type ConcreteFrame = SchemeFrame[ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]

}

/*
 * TODO add type paremeters to FilterAnnotations
 */
case class FilterAnnotations[Exp : Expression, Abs: IsSchemeLattice, Addr : Address](
    machineFilters: Set[MachineFilterAnnotation],
    semanticsFilters: Set[SemanticsFilterAnnotation]) {

  def +(machineFilter: MachineFilterAnnotation): FilterAnnotations[Exp, Abs, Addr] =
    this.copy(machineFilters = machineFilters + machineFilter)

  def +(semanticsFilter: SemanticsFilterAnnotation): FilterAnnotations[Exp, Abs, Addr] =
    this.copy(semanticsFilters = semanticsFilters + semanticsFilter)

  def exists(pred: (MachineFilterAnnotation => Boolean)): Boolean =
    machineFilters.exists(pred)

  def exists(pred: (SemanticsFilterAnnotation => Boolean)): Boolean =
    semanticsFilters.exists(pred)

  def exists(pred: (FilterAnnotation => Boolean)): Boolean =
    machineFilters.exists(pred) || semanticsFilters.exists(pred)

  def isSubsumptionAnnotation: Boolean = {
    if (exists( (filter: MachineFilterAnnotation) => filter match {
      case StateSubsumed(_ , _) =>
        true
      case _ => false
    })) {
      /*
       * Make sure that an edge is ONLY annotated with StateSubsumed. It should not be possible
       * to have a StateSubsumed edge with any other annotation.
       */
      assert(machineFilters.size == 1 && semanticsFilters.isEmpty,
             s"StateSubsumed edge contains more than 1 filter:$this")
      true
    } else {
      false
    }
  }
}

case class EdgeAnnotation[Exp : Expression, Abs: IsSchemeLattice, Addr : Address](
    filters: FilterAnnotations[Exp, Abs, Addr],
    actions: List[ActionReplay[Exp, Abs, Addr]])

object EdgeAnnotation {

  def subsumptionEdge[Exp : Expression, Abs : IsSchemeLattice, Addr : Address]
                     (subsumptionFilter: StateSubsumed[Abs, Addr]): EdgeAnnotation[Exp, Abs, Addr] =
    EdgeAnnotation(FilterAnnotations(Set(subsumptionFilter), Set()), Nil)

}