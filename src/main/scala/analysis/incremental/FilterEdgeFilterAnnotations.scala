import ConcreteConcreteLattice.{L => ConcreteValue}

class FilterEdgeFilterAnnotations[Exp: Expression, Abs: IsSchemeLattice, Addr: Address, Time: Timestamp, State] {

  val usesGraph = new UsesGraph[Exp, Abs, Addr, State]
  import usesGraph._

  /*
   * We definitely have to convert the Timestamps here, because the Timestamps encoded in the Concrete
   * EdgeFilterAnnotations are concrete, while those from the existing graph are abstract.
   */
  val kontAddrConverter: KontAddrConverter[KontAddr] = new ConvertTimestampKontAddrConverter[Exp](ConvertTimeStampConverter)

  class SubsumesOrdering[T](subsumes: (T, T) => Boolean)
    extends PartialOrdering[T] {

    def lteq(x: T, y: T): Boolean = {
      (subsumes(x, y), subsumes(y, x)) match {
        case (false, true) => true
        case _ => false
      }
    }

    def tryCompare(x: T, y: T): Option[scala.Int] = {
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
  //
  //  def findMinimallySubsumesFrameFollowedEdges(edges: Set[Edge], frame: AbstractFrame): Set[Edge] = {
  //    filterFrameEdges(frame, frameUsedSubsumes({
  //      case info: FrameFollowed => Some(info.frame)
  //      case _ => None}),
  //      edges)
  //  }

  private def convertKontAddrFilter(filter: MachineFilterAnnotation): MachineFilterAnnotation = filter
  match {
    case KontAddrPopped(oldKa, newKa) =>
      val convertedOldKa = kontAddrConverter.convertKontAddr(oldKa)
      val convertedNewKa = kontAddrConverter.convertKontAddr(newKa)
      KontAddrPopped(convertedOldKa, convertedNewKa)
    case _ =>
      filter
  }

  private def convertKontAddrFilters(filters: FilterAnnotations): FilterAnnotations = {
    filters.copy(machineFilters = filters.machineFilters.map(convertKontAddrFilter))
  }

  def filterSingleEdgeInfo(abstractEdges: Set[Edge], concreteEdgeInfo: FilterAnnotation): Set[Edge] = {
    abstractEdges.filter( (abstractEdge: Edge) => concreteEdgeInfo match {
      case filter: FrameFollowed[Abs, Addr, Time] =>
        val result = abstractEdge._1.filters.exists({
          case abstractEdgeFilter: FrameFollowed[Abs, Addr, Time] =>
            /*
             * Check whether the frame on the abstract edge subsumes the "concrete" frame, unless the "subsumes"
             * relation is not meaningful (read: is not implemented) for this frame.
             */
            (! abstractEdgeFilter.frame.meaningfullySubsumes) || abstractEdgeFilter.frame.subsumes(filter.frame)
          case _ => false
        })
        result
      case ElseBranchFilter => abstractEdge._1.filters.contains(ElseBranchFilter)
      case filter: EvaluatingExpression[Exp] => abstractEdge._1.filters.contains(filter)
      case _: FunCallMark[Exp, Abs, Time] => true
      case filter: KontAddrPopped => abstractEdge._1.filters.contains(filter)
      case ThenBranchFilter => abstractEdge._1.filters.contains(ThenBranchFilter)
    })
  }

  private def convertConcreteFilters(concreteFilters: FilterAnnotations,
                                     convertFrameFun: ConvertableSchemeFrame[ConcreteValue, Addr, Time] => AbstractFrame):  FilterAnnotations = {
    val convertedFrameMachineFilters = concreteFilters.machineFilters.map({
      /* Convert the values in the Frames of the FrameFollowed annotation to abstract values. */
      case filter: FrameFollowed[ConcreteValue, Addr, Time] => FrameFollowed(convertFrameFun(filter.frame))
      /* Convert the (timestamps in the) KontAddresses to abstract (timestamps of) KontAddresses. */
      case KontAddrPopped(oldKa, newKa) =>
        val convertedOldKa = kontAddrConverter.convertKontAddr(oldKa)
        val convertedNewKa = kontAddrConverter.convertKontAddr(newKa)
        KontAddrPopped(convertedOldKa, convertedNewKa)
      case other => other
    })
    FilterAnnotations(convertedFrameMachineFilters, concreteFilters.semanticsFilters)
  }

  /**
    * Filter the abstract edges relative to the given filters.
    * @param abstractEdges
    * @param filters
    * @return
    */
  def filterToFilterEdge(abstractEdges: Set[Edge], filters: FilterAnnotations): Set[Edge] = {
    val convertedAbstractEdges: Set[Edge] = abstractEdges.map(edge => edge.copy(_1 = edge._1.copy(filters = convertKontAddrFilters(edge._1.filters))))
    filters.foldLeft[Set[Edge]](convertedAbstractEdges)((filteredAbstractEdges, edgeFilterAnnotation) => {
      filterSingleEdgeInfo(filteredAbstractEdges, edgeFilterAnnotation)
    })
  }

  /**
    * Filter the abstract edges relative to the concrete filters.
    * @param abstractEdges
    * @param concreteFilters
    * @return
    */
  def filterConcreteFilterEdge(abstractEdges: Set[Edge], concreteFilters: FilterAnnotations,
                               convertFrameFun: ConvertableSchemeFrame[ConcreteValue, Addr, Time] => AbstractFrame): Set[Edge] = {
    val convertedConcreteEdgeFilters = convertConcreteFilters(concreteFilters, convertFrameFun)
    filterToFilterEdge(abstractEdges, convertedConcreteEdgeFilters)
  }

}