import ConcreteConcreteLattice._

class FilterEdgeFilterAnnotations[Exp : Expression,
                                  Abs : IsSchemeLattice,
                                  Addr : Address,
                                  Time : Timestamp,
                                  State <: StateTrait[Exp, Abs, Addr, Time] : Descriptor]
                                 (implicit val actionRApplier: ActionReplayApplier[Exp, Abs, Addr, Time, State]) {

  /*
   * We definitely have to convert the Timestamps here, because the Timestamps encoded in the Concrete
   * EdgeFilterAnnotations are concrete, while those from the existing graph are abstract.
   */
  val kontAddrConverter: KontAddrConverter[KontAddr] = new ConvertTimestampKontAddrConverter[Exp](ConvertTimeStampConverter)

  val usesGraph = new UsesGraph[Exp, Abs, Addr, State]
  import usesGraph._

  class SubsumesOrdering[T](subsumes: (T, T) => Boolean)
    extends PartialOrdering[T] {

    def lteq(x: T, y: T): Boolean = {
      (subsumes(x, y), subsumes(y, x)) match {
        case (false, true) => true
        case _ => false
      }
    }

    def tryCompare(x: T, y: T): Option[Int] = {
      (subsumes(x, y), subsumes(y, x)) match {
        case _ if x == y => Some(0)
        case (true, true) => Some(0)
        case (true, false) => Some(1)
        case (false, true) => Some(-1)
        case (false, false) => None
      }
    }
  }

  def findMinimallySubsuming[T](edges: Set[(Edge, T)],
                                ordering: SubsumesOrdering[T]): Set[Edge] = {
    edges.filter( (edge1) => {
        /*
         * Only keep a value n if it holds that n does not subsume any other value m.
         * Only keep a value n if it holds that n is either smaller than (subsumed by), 'equal' to or incomparable with
         * every other node m.
         */
        val excluded = edges.filter(_ != edge1)
        excluded.forall( (edge2) =>
          ordering.tryCompare(edge1._2, edge2._2) match {
            case Some(1) => false
            case _ => true
          })
      })
      .map(_._1)
  }

  private def frameUsedSubsumes(getFrameFromInfo: MachineFilterAnnotation => Option[AbstractFrame])
                               (info: MachineFilterAnnotation, convertedFrame: AbstractFrame)
  : Option[AbstractFrame] =
    getFrameFromInfo(info) match {
      case Some(abstractFrame) =>
        val subsumes = abstractFrame.subsumes(convertedFrame)
        Logger.log(s"frameUsedSubsumes: $abstractFrame subsumes $convertedFrame ? $subsumes", Logger.D)
        if (subsumes) {
          Some(abstractFrame)
        } else {
          None
        }
      case _ =>
        None
    }

  def findMinimallySubsumingEdges(edges: Set[Edge]): Set[Edge] = {
    assert(edges.forall( (edge) => edge._1.filters.isSubsumptionAnnotation))
    val ordering = new SubsumesOrdering[State]( (state1, state2) => {
      actionRApplier.subsumes(state1, state2)
    })
    val minFrameFollowedEdges: Set[Edge] = findMinimallySubsuming(edges.map( (edge) => (edge, edge._2)), ordering)
    Logger.log(s"findMinimallySubsumingEdges = $minFrameFollowedEdges\n" +
               s"All edges = $edges", Logger.D)
    minFrameFollowedEdges
  }

  def filterFrameEdges(convertedFrame: AbstractFrame,
                       subsumesFrame: (MachineFilterAnnotation, AbstractFrame) => Option[AbstractFrame],
                       abstractEdges: Set[Edge]): Set[Edge] = {
    if (!convertedFrame.meaningfullySubsumes) {
      // TODO hack: implement a proper subsumes method for every frame
      abstractEdges
    } else {
      /*
       * All edges containing a FrameFollowed annotation whose abstract value actually subsumes the abstracted
       * concrete value, zipped together with the abstract value that was reached.
       */
      val edgesContainingFrames: Set[(Edge, AbstractFrame)] =
        abstractEdges
          .flatMap[(Edge, AbstractFrame), Set[(Edge, AbstractFrame)]](
          (edge: Edge) => {
            val someFound: Option[AbstractFrame] = edge._1.filters.machineFilters.map(subsumesFrame(_, convertedFrame))
              .foldLeft[Option[AbstractFrame]](None)({
              case (Some(x), _) =>
                Some(x)
              case (None, y) =>
                y
            })
            someFound.foldLeft[Set[(Edge, AbstractFrame)]](Set())({
              case (_, abstractFrame) => Set((edge, abstractFrame))
            })
          })
      val ordering = new SubsumesOrdering[AbstractFrame]( (frame1, frame2) =>
        frame1.subsumes(frame2))
      val minFrameFollowedEdges: Set[Edge] = findMinimallySubsuming(edgesContainingFrames, ordering)
      Logger.log(s"minFrameFollowedEdges = $minFrameFollowedEdges", Logger.D)
      minFrameFollowedEdges
    }
  }

  def findMinimallySubsumesFrameFollowedEdges(edges: Set[Edge], frame: AbstractFrame): Set[Edge] = {
    filterFrameEdges(frame, frameUsedSubsumes({
      case info: FrameFollowed[Abs] =>
        Some(info.frame)
      case _ =>
        None}),
      edges)
  }

  private def convertKontAddrFilter(filter: MachineFilterAnnotation): MachineFilterAnnotation = filter
  match {
    case KontAddrPopped(oldKa, newKa) =>
      val convertedOldKa = kontAddrConverter.convertKontAddr(oldKa)
      val convertedNewKa = kontAddrConverter.convertKontAddr(newKa)
      KontAddrPopped(convertedOldKa, convertedNewKa)
    case KontAddrPushed(ka) =>
      val convertedKa = kontAddrConverter.convertKontAddr(ka)
      KontAddrPushed(convertedKa)
    case _ =>
      filter
  }

  private def convertKontAddrFilters(filters: FilterAnnotations[Exp, Abs, Addr]): FilterAnnotations[Exp, Abs, Addr] = {
    filters.copy(machineFilters = filters.machineFilters.map(convertKontAddrFilter))
  }

  def filterSingleEdgeInfo(abstractEdges: Set[Edge],
                           concreteEdgeInfo: FilterAnnotation): Set[Edge] =
    concreteEdgeInfo match {

      case info: FrameFollowed[Abs] =>
        findMinimallySubsumesFrameFollowedEdges(abstractEdges, info.frame)

      case _ =>
        abstractEdges.filter( (abstractEdge: Edge) => concreteEdgeInfo match {
          case ElseBranchTaken =>
            abstractEdge._1.filters.contains(ElseBranchTaken)
          case filter: EvaluatingExpression[Exp] =>
            abstractEdge._1.filters.contains(filter)
          case filter: FunCallMark[Exp, Abs, Time] =>
            true
          case filter: KontAddrPopped =>
            abstractEdge._1.filters.contains(filter)
          case filter: KontAddrPushed =>
            abstractEdge._1.filters.contains(filter)
          case ThenBranchTaken =>
            abstractEdge._1.filters.contains(ThenBranchTaken)
        })
    }

  private def convertConcreteFilters(concreteFilters: FilterAnnotations[Exp, ConcreteValue, Addr],
                                     convertFrameFun: ConcreteFrame => AbstractFrame):  FilterAnnotations[Exp, Abs, Addr] = {
    /* First convert the values in the Frames of the FrameFollowed annotation to abstract values. */
    val convertedFrameMachineFilters = concreteFilters.machineFilters.map({
      case filter: FrameFollowed[ConcreteValue] =>
        FrameFollowed[Abs](convertFrameFun(filter.frame))
      case other =>
        other
    })
    val convertedFilters = FilterAnnotations(convertedFrameMachineFilters, concreteFilters.semanticsFilters)
    /* Then convert the (timestamps in the) KontAddresses to abstract (timestamps of) KontAddresses. */
    convertKontAddrFilters(convertedFilters)
  }

  /**
    * Filter the abstract edges relative to the given filters.
    * @param abstractEdges
    * @param filters
    * @return
    */
  def filterToFilterEdge(abstractEdges: Set[Edge],
                         filters: FilterAnnotations[Exp, Abs, Addr]): Set[Edge] = {
    val convertedAbstractEdges: Set[Edge] = abstractEdges.map( (edge) =>
      edge.copy(_1 = edge._1.copy(filters = convertKontAddrFilters(edge._1.filters))))
    val convertedFilterEdge = convertKontAddrFilters(filters)
    convertedFilterEdge.foldLeft[Set[Edge]](convertedAbstractEdges)(
      (filteredAbstractEdges, edgeFilterAnnotation) => filterSingleEdgeInfo(filteredAbstractEdges, edgeFilterAnnotation))
  }

  /**
    * Filter the abstract edges relative to the concrete filters.
    * @param abstractEdges
    * @param concreteFilters
    * @param convertFrameFun
    * @return
    */
  def filterConcreteFilterEdge(abstractEdges: Set[Edge],
                               concreteFilters: FilterAnnotations[Exp, ConcreteValue, Addr],
                               convertFrameFun: ConcreteFrame => AbstractFrame): Set[Edge] = {
    val convertedConcreteEdgeFilters = convertConcreteFilters(concreteFilters, convertFrameFun)
    filterToFilterEdge(abstractEdges, convertedConcreteEdgeFilters)
  }

}
