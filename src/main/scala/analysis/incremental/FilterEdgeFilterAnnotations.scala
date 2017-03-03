import ConcreteConcreteLattice._

class FilterEdgeFilterAnnotations[Exp : Expression,
                                  AbstL : IsSchemeLattice,
                                  Addr : Address,
                                  State <: StateTrait[Exp, AbstL, Addr, _]] {

  /*
   * We definitely have to convert the Timestamps here, because the Timestamps encoded in the Concrete
   * EdgeFilterAnnotations are concrete, while those from the existing graph are abstract.
   */
  val kontAddrConverter: KontAddrConverter[KontAddr] = new ConvertTimestampKontAddrConverter[Exp](ConvertTimeStampConverter)

  val usesGraph = new UsesGraph[Exp, AbstL, Addr, State]
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
    edges
      .filter(tuple1 => {
        /*
         * Only keep a value n if it holds that n does not subsume any other value m.
         * Only keep a value n if it holds that n is either smaller than (subsumed by), 'equal' to or incomparable with
         * every other node m.
         */
        val excluded = edges.filter(_ != tuple1)
        excluded.forall(tuple2 =>
          ordering.tryCompare(tuple1._2, tuple2._2) match {
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
      val ordering = new SubsumesOrdering[AbstractFrame]((frame1, frame2) =>
        frame1.subsumes(frame2))
      val minFrameFollowedEdges: Set[Edge] =
        findMinimallySubsuming(edgesContainingFrames, ordering)
      Logger.log(s"minFrameFollowedEdges = $minFrameFollowedEdges", Logger.D)
      minFrameFollowedEdges
    }
  }

  def findMinimallySubsumesFrameFollowedEdges(edges: Set[Edge], frame: AbstractFrame): Set[Edge] = {
    filterFrameEdges(frame, frameUsedSubsumes({
      case info: FrameFollowed[AbstL] =>
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

  private def convertKontAddrFilters(filters: FilterAnnotations[Exp, AbstL, Addr]): FilterAnnotations[Exp, AbstL, Addr] = {
    filters.copy(machineFilters = filters.machineFilters.map(convertKontAddrFilter))
  }

  def filterSingleEdgeInfo(abstractEdges: Set[Edge],
                           concreteEdgeInfo: FilterAnnotation): Set[Edge] =
    concreteEdgeInfo match {

      case info: FrameFollowed[AbstL] =>
        findMinimallySubsumesFrameFollowedEdges(abstractEdges, info.frame)

      case _ =>
        abstractEdges.filter( (abstractEdge: Edge) => concreteEdgeInfo match {
          case filter: EvaluatingExpression[Exp] =>
            abstractEdge._1.filters.contains(filter)
          case filter: KontAddrPopped =>
            abstractEdge._1.filters.contains(filter)
          case filter: KontAddrPushed =>
            abstractEdge._1.filters.contains(filter)
        })
    }

  private def convertConcreteFilters(concreteFilters: FilterAnnotations[Exp, ConcreteValue, Addr],
                                     convertFrameFun: ConcreteFrame => AbstractFrame):  FilterAnnotations[Exp, AbstL, Addr] = {
    /* First convert the values in the Frames of the FrameFollowed annotation to abstract values. */
    val convertedFrameMachineFilters = concreteFilters.machineFilters.map({
      case filter: FrameFollowed[ConcreteValue] =>
        FrameFollowed[AbstL](convertFrameFun(filter.frame))
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
                         filters: FilterAnnotations[Exp, AbstL, Addr]): Set[Edge] = {
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
