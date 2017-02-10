class FilterEdgeFilterAnnotations[Exp : Expression,
                                  AbstL : IsSchemeLattice,
                                  Addr : Address,
                                  State <: StateTrait[Exp, AbstL, Addr, _]] {

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

  private def frameUsedSubsumes(getFrameFromInfo: EdgeFilterAnnotation => Option[AbstractFrame])
                               (info: EdgeFilterAnnotation,
                                convertedFrame: AbstractFrame)
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
                       subsumesFrame: (EdgeFilterAnnotation,
                         AbstractFrame) => Option[AbstractFrame],
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
            val someFound: Option[AbstractFrame] = edge._1._1.map(subsumesFrame(_, convertedFrame)).foldLeft[Option[AbstractFrame]](None)({
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

  def filterSingleEdgeInfo(convertValueFun: ConcreteConcreteLattice.L => AbstL,
                           convertFrameFun: ConcreteFrame => AbstractFrame,
                           abstractEdges: Set[Edge],
                           concreteEdgeInfo: EdgeFilterAnnotation): Set[Edge] =
    concreteEdgeInfo match {

      case info: FrameFollowed[ConcreteConcreteLattice.L] =>
        filterFrameEdges(convertFrameFun(info.frame), frameUsedSubsumes({
          case info: FrameFollowed[AbstL] =>
            Some(info.frame)
          case _ =>
            None}),
          abstractEdges)

      case _ =>
        abstractEdges.filter({
          case ((abstractEdgeInfos, _), _) =>
            concreteEdgeInfo match {
              case EvaluatingExpression(e) =>
                abstractEdgeInfos.contains(concreteEdgeInfo)
              case KontAddrPopped(oldA, newA) =>
                val KAConverter = new ConvertTimestampKontAddrConverter(ConvertTimeStampConverter)
                val convertedOlda = KAConverter.convertKontAddr(oldA)
                val convertedNewa = KAConverter.convertKontAddr(newA)
                abstractEdgeInfos.contains(KontAddrPopped(convertedOlda, convertedNewa))
              case KontAddrPushed(ka) =>
                val convertedKa = new ConvertTimestampKontAddrConverter(ConvertTimeStampConverter).convertKontAddr(ka)
                abstractEdgeInfos.contains(KontAddrPushed(convertedKa))
            }
        })
    }

  def filterAllEdgeInfos(convertValueFun: ConcreteConcreteLattice.L => AbstL,
                         convertFrameFun: ConcreteFrame => AbstractFrame,
                         abstractEdges: Set[Edge],
                         concreteEdgeInfos: List[EdgeFilterAnnotation]): Set[Edge] = {
    concreteEdgeInfos.foldLeft[Set[Edge]](abstractEdges)(
      (filteredAbstractEdges, concreteEdgeInfo) => filterSingleEdgeInfo(convertValueFun,
        convertFrameFun,
        filteredAbstractEdges,
        concreteEdgeInfo))
  }

}
