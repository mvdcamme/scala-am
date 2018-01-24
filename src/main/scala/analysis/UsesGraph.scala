import ConcreteConcreteLattice.{ L => ConcreteValue }

class UsesGraph[Exp : Expression, Abs : IsSchemeLattice, Addr : Address, State <: StateTrait[Exp, Abs, Addr, _]] {

  type EdgeAnnotation2 = EdgeAnnotation[Exp, Abs, Addr]
  type Edge = (EdgeAnnotation2, State)
  type AbstractGraph = Graph[State, EdgeAnnotation2, Set[State]]

  type AbstractFrame = ConvertableSchemeFrame[Abs, HybridAddress.A, HybridTimestamp.T]
  type ConcreteFrame = ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]

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

  def contains(filter: MachineFilterAnnotation): Boolean =
    machineFilters.contains(filter)

  def contains(filter: SemanticsFilterAnnotation): Boolean =
    semanticsFilters.contains(filter)

  def foldLeft[B](initial: B)(f: (B, FilterAnnotation) => B): B = {
    val allFilters: Set[FilterAnnotation] = machineFilters ++ semanticsFilters
    allFilters.foldLeft(initial)(f)
  }

  def isSubsumptionAnnotation: Boolean = {
    assert(GlobalFlags.AAM_CHECK_SUBSUMES, "Should not be called if there are no subsumpion edges")
    if (machineExists({
      case StateSubsumed =>
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

  /*
   * Parameter type of exists and map functions are erased during compilation, causing the compiler to complain
   * about double definitions of the exists and map functions.
   * Solution: http://stackoverflow.com/questions/3307427/scala-double-definition-2-methods-have-the-same-type-erasure
   */

//  case class MBool(b: Boolean)
//  case class SBool(b: Boolean)
//  case class FBool(b: Boolean)
//  implicit private def mb(b: Boolean): MBool =
//    MBool(b)
//  implicit private def sb(b: Boolean): SBool =
//    SBool(b)
//  implicit private def fb(b: Boolean): FBool =
//    FBool(b)

//  case class MachineFilterExists(pred: MachineFilterAnnotation => Boolean)
//  case class SemanticsFilterExists(pred: SemanticsFilterAnnotation => Boolean)
//  case class FilterExists(pred: FilterAnnotation => Boolean)
//  implicit private def mfe(pred: MachineFilterAnnotation => Boolean): MachineFilterExists =
//    MachineFilterExists(pred)
//  implicit private def sfe(pred: SemanticsFilterAnnotation => Boolean): SemanticsFilterExists =
//    SemanticsFilterExists(pred)
//  implicit private def fe(pred: FilterAnnotation => Boolean): FilterExists =
//    FilterExists(pred)

  def machineExists(pred: MachineFilterAnnotation => Boolean)
            (implicit d: DummyImplicit): Boolean =
    machineFilters.exists(pred)

  def exists(pred: FilterAnnotation => Boolean)
            (implicit d: DummyImplicit): Boolean = {
    val allFilters = machineFilters ++ semanticsFilters
    allFilters.exists(pred)
  }

//  def exists(pred: FilterExists)
//            (implicit d: DummyImplicit): Boolean =
//    machineFilters.exists(pred.pred) || semanticsFilters.exists(pred.pred)

  case class MachineFilterFunction(function: MachineFilterAnnotation => MachineFilterAnnotation)
  case class SemanticsFilterFunction(function: SemanticsFilterAnnotation => SemanticsFilterAnnotation)
  implicit private def mff(function: MachineFilterAnnotation => MachineFilterAnnotation): MachineFilterFunction =
    MachineFilterFunction(function)
  implicit private def sff(function: SemanticsFilterAnnotation => SemanticsFilterAnnotation): SemanticsFilterFunction =
    SemanticsFilterFunction(function)

  def map(f: MachineFilterFunction)
         (implicit d: DummyImplicit): FilterAnnotations[Exp, Abs, Addr] =
    this.copy(machineFilters = machineFilters.map(f.function))

  def map(f: SemanticsFilterFunction)
         (implicit d: DummyImplicit): FilterAnnotations[Exp, Abs, Addr] =
    this.copy(semanticsFilters = semanticsFilters.map(f.function))
}

case class EdgeAnnotation[Exp : Expression, Abs: IsSchemeLattice, Addr : Address](
    filters: FilterAnnotations[Exp, Abs, Addr],
    actions: List[ActionReplay[Exp, Abs, Addr]])

object EdgeAnnotation {
  def dummyEdgeAnnotation[Exp : Expression, Abs: IsSchemeLattice, Addr : Address] =
    EdgeAnnotation[Exp, Abs, Addr](FilterAnnotations[Exp, Abs, Addr](Set(), Set()), Nil)
  def subsumptionEdge[Exp : Expression, Abs : IsSchemeLattice, Addr : Address]: EdgeAnnotation[Exp, Abs, Addr] = {
    assert(GlobalFlags.AAM_CHECK_SUBSUMES, "Should not be called if flag is turned off")
    EdgeAnnotation(FilterAnnotations(Set(StateSubsumed), Set()), Nil)
  }
  implicit def graphAnnotation[Exp : Expression, Abs: IsSchemeLattice, Addr : Address]: GraphAnnotation[EdgeAnnotation[Exp, Abs, Addr], Unit] =
    new GraphAnnotation[EdgeAnnotation[Exp, Abs, Addr], Unit] {
      override def label(annot: EdgeAnnotation[Exp, Abs, Addr]) = {
        val filterEdgeString = annot.filters.machineFilters.mkString(", ") + annot.filters.semanticsFilters.mkString(", ")
        val fullString = s"[$filterEdgeString], [${annot.actions.mkString(", ")}]"
        if (GlobalFlags.PRINT_EDGE_ANNOTATIONS_FULL) {
          fullString
        } else {
          fullString.take(40)
        }
      }
    }
}