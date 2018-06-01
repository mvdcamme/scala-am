//class FilterPropagationEdges[Exp: Expression,
//                             Abs: IsSchemeLattice,
//                             Addr: Address,
//                             Time: Timestamp,
//                             State <: StateTrait[Exp, Abs, Addr, Time]]
//                            (implicit actionRApplier: ActionReplayApplier[Exp, Abs, Addr, Time, State],
//                                      stateInfoProvider: StateInfoProvider[Exp, Abs, Addr, Time, State]) {
//
//  val usesGraph = new UsesGraph[Exp, Abs, Addr, State]
//  import usesGraph._
//
//  def filterWithStore(newState: State, edges: Set[Edge]): Set[Edge] = {
//
//    def hasSemanticsFilter(edge: (EdgeAnnotation2, State), filter: SemanticsFilterAnnotation): Boolean =
//      edge._1.filters.contains(filter)
//    /*
//     * If there is a ThenBranchTaken-annotation and current state did NOT evaluate to true, take all edges not
//     * containing a ThenBranchTaken-annotation.
//     */
//    val filteredTrue: Set[(EdgeAnnotation2, State)] = if (edges.exists(hasSemanticsFilter(_, ThenBranchFilter)) &&
//      (!actionRApplier.evaluatedTrue(newState))) {
//      edges.filter(!hasSemanticsFilter(_, ThenBranchFilter))
//    } else {
//      edges
//    }
//    /*
//     * If there is an ElseBranchTaken-annotation and current state did NOT evaluate to false, take all edges not
//     * containing an ElseBranchTaken-annotation.
//     */
//    val filteredFalse: Set[(EdgeAnnotation2, State)] = if (edges.exists(hasSemanticsFilter(_, ElseBranchFilter)) &&
//      (!actionRApplier.evaluatedFalse(newState))) {
//      edges.filter(!hasSemanticsFilter(_, ElseBranchFilter))
//    } else {
//      edges
//    }
//
//    filteredTrue.intersect(filteredFalse)
//  }
//
//  def filterWithKStore(newState: State, edges: Set[Edge]): Set[Edge] = {
//
//    type RelevantOperandsFrame = FrameFuncallOperands[Abs, HybridAddress.A, HybridTimestamp.T]
//    type RelevantOperatorFrame = FrameFuncallOperator[Abs, HybridAddress.A, HybridTimestamp.T]
//    val sabs = implicitly[IsSchemeLattice[Abs]]
//    /*
//     * Checks if the given frame directly leads to a closure call. If yes, returns the closure that will be called
//     * If not, returns None.
//     */
//    def frameLeadsToClosureCall(frame: Frame): Option[Abs] = frame match {
//      case frame: RelevantOperandsFrame =>
//        if (sabs.getClosures(frame.f).nonEmpty && frame.toeval.isEmpty) {
//          Some(frame.f)
//        } else {
//          None
//        }
//      case frame: RelevantOperatorFrame =>
//        val functionToBeCalled = stateInfoProvider.valueReached(newState).get
//        /*
//         * A FrameFuncallOperator can directly lead to a function call if it doesn't have any arguments to evaluate.
//         * In this case the closure that will be called is currently stored in the control-component of the state.
//         */
//        if (frame.args.isEmpty && sabs.getClosures(functionToBeCalled).nonEmpty) {
//          Some(functionToBeCalled)
//        } else {
//          None
//        }
//      case _ =>
//        None
//    }
//
//    /*
//     * Trying to filter potential edges using the continuation store is only relevant when a continuation state
//     * has actually been reached.
//     * If not, the continuation store is irrelevant so this function should just return all edges.
//     */
//    if (stateInfoProvider.valueReached(newState).isEmpty) {
//      edges
//    } else {
//      val actualKonts = actionRApplier.getTopKonts(newState)
//      val closuresToBeCalled: Set[Abs] = actualKonts.map(_.frame).flatMap((frame: Frame) => {
//        val optionRelevantClosure = frameLeadsToClosureCall(frame)
//        optionRelevantClosure.fold[Set[Abs]](Set())((relevantFrame: Abs) => Set(relevantFrame))
//      })
//
//      /*
//       * edgesWith: all edges that contain a FrameFollowed EdgeAnnotation with a frame that leads to a closure call.
//       * edgesWithout: all edges that don't satisfy the above condition.
//       */
//      val (edgesWith, edgesWithout) = edges.partition((edge) => {
//        val filters = edge._1.filters
//        filters.machineExists({
//          case annot: FrameFollowed[Abs] =>
//            frameLeadsToClosureCall(annot.frame).isDefined
//          case _ =>
//            false
//        })
//      })
//
//      val filteredEdgesWith: Set[Edge] = closuresToBeCalled.flatMap((closureToBeCalled) => {
//        val actualClosures = sabs.getClosures(closureToBeCalled)
//        val actualLambdas: Set[Exp] = actualClosures.map(_._1)
//        val result: Set[Edge] = edgesWith.filter((edge: Edge) => {
//          edge._1.actions.exists({
//            case actionClosureCall: ActionClosureCallR[Exp, Abs, Time] =>
//              actualLambdas.contains(actionClosureCall.lambda)
//            case _ =>
//              false
//          })
//        })
//        result
//      })
//      Logger.D(s"FilterWithKStore: closuresToBeCalled = $closuresToBeCalled")
//      Logger.D(s"FilterWithKStore: edges = $edges")
//      Logger.D(s"FilterWithKStore: ${filteredEdgesWith ++ edgesWithout}")
//      filteredEdgesWith ++ edgesWithout
//    }
//  }
//
//}
