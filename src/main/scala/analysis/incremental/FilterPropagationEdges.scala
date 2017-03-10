class FilterPropagationEdges[Exp: Expression,
                             Abs: IsSchemeLattice,
                             Addr: Address,
                             Time: Timestamp,
                             State <: StateTrait[Exp, Abs, Addr, Time] : Descriptor]
                            (implicit actionRApplier: ActionReplayApplier[Exp, Abs, Addr, Time, State]) {

  val usesGraph = new UsesGraph[Exp, Abs, Addr, State]
  import usesGraph._

  def filterWithStore(newState: State, edges: Set[Edge]): Set[Edge] = {

    def hasSemanticsFilter(edge: (EdgeAnnotation2, State), filter: SemanticsFilterAnnotation): Boolean =
      edge._1.filters.contains(filter)
    /*
     * If there is a ThenBranchTaken-annotation and current state did NOT evaluate to true, take all edges not
     * containing a ThenBranchTaken-annotation.
     */
    val filteredTrue: Set[(EdgeAnnotation2, State)] = if (edges.exists(hasSemanticsFilter(_, ThenBranchTaken)) &&
      (! actionRApplier.evaluatedTrue(newState))) {
      edges.filter(!hasSemanticsFilter(_, ThenBranchTaken))
    } else {
      edges
    }
    /*
     * If there is an ElseBranchTaken-annotation and current state did NOT evaluate to false, take all edges not
     * containing an ElseBranchTaken-annotation.
     */
    val filteredFalse: Set[(EdgeAnnotation2, State)] = if (edges.exists(hasSemanticsFilter(_, ElseBranchTaken)) &&
      (!actionRApplier.evaluatedFalse(newState))) {
      edges.filter(! hasSemanticsFilter(_, ElseBranchTaken))
    } else {
      edges
    }

    val filteredEdges = filteredTrue.intersect(filteredFalse)
    if (filteredEdges.size != edges.size) {
      Logger.log(s"## Difference between edges and filteredEdges! ##", Logger.U)
    }
    filteredEdges
  }

  def filterWithKStore(newState: State, edges: Set[Edge]): Set[Edge] = {

    type RelevantFrame = FrameFuncallOperands[Abs, HybridAddress.A, HybridTimestamp.T]
    val sabs = implicitly[IsSchemeLattice[Abs]]
    /*
     * Checks if the given frame directly leads to a closure call. If yes, returns the frame, casted as a RelevantFrame.
     * If not, returns None.
     */
    def frameLeadsToClosureCall(frame: Frame): Option[RelevantFrame] = frame match {
      case frame: RelevantFrame =>
        if (sabs.getClosures(frame.f).nonEmpty && frame.toeval.isEmpty) {
          Some(frame)
        } else {
          None
        }
      case _ =>
        None
    }

    val actualKonts = actionRApplier.getKonts(newState)
    val relevantActualFrames: Set[RelevantFrame] = actualKonts.map(_.frame).flatMap((frame: Frame) => {
      val optionRelevantFrame = frameLeadsToClosureCall(frame)
      optionRelevantFrame.fold[Set[RelevantFrame]](Set())((relevantFrame: RelevantFrame) => Set(relevantFrame))
    })

    /*
     * edgesWith: all edges that contain a FrameFollowed EdgeAnnotation with a frame that leads to a closure call.
     * edgesWithout: all edges that don't satisfy the above condition.
     */
    val (edgesWith, edgesWithout) = edges.partition( (edge) => {
      val filters = edge._1.filters
      filters.machineExists({
        case annot: FrameFollowed[Abs] =>
          frameLeadsToClosureCall(annot.frame).isDefined
        case _ =>
          false
      })
    })

    val filteredEdgesWith: Set[Edge] = relevantActualFrames.flatMap( (relevantFrame) => {
      val actualClosures = sabs.getClosures(relevantFrame.f)
      val actualLambdas: Set[Exp] = actualClosures.map(_._1)
      val result: Set[Edge] = edgesWith.filter( (edge: Edge) => {
        edge._1.filters.machineExists({
          case actionClosureCall: ClosureCallMark[Exp, Abs] =>
            actualLambdas.contains(actionClosureCall.lambda)
          case _ =>
            false
        })
      })
      result
    })
    filteredEdgesWith ++ edgesWithout
  }

}
