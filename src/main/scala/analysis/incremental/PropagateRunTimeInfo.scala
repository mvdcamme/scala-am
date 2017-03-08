import java.io.{File, BufferedWriter, FileWriter}

class PropagateRunTimeInfo[Exp: Expression,
                           Abs: IsSchemeLattice,
                           Addr: Address,
                           Time: Timestamp,
                           State <: StateTrait[Exp, Abs, Addr, Time] : Descriptor]
                          (graphPrinter: GraphPrinter[Graph[State, EdgeAnnotation[Exp, Abs, Addr]]])
                          (implicit actionRApplier: ActionReplayApplier[Exp, Abs, Addr, Time, State]) {

  val usesGraph = new UsesGraph[Exp, Abs, Addr, State]
  import usesGraph._

  val filterEdgeFilterAnnotations = new FilterEdgeFilterAnnotations[Exp, Abs, Addr, Time, State]

  val LogPropagation = Logger.D

  type ActionEdge = List[ActionReplay[Exp, Abs, Addr]]
  type FilterEdge = List[FilterAnnotation]

  private def filterWithStore(newState: State, edges: Set[Edge]): Set[Edge] = {

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

  private def filterWithKStore(newState: State, edges: Set[Edge]): Set[Edge] = {

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
      filters.machineExists((machineFilter: MachineFilterAnnotation) => machineFilter match {
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
        edge._1.actions.exists({
          case actionClosureCall: ActionClosureCallMarkR[Exp, Abs, Addr] =>
            actualLambdas.contains(actionClosureCall.lambda)
          case _ =>
            false
        })
      })
      result
    })
    filteredEdgesWith ++ edgesWithout
  }

  /*
   * originalState: The original state that was present in the initial abstract graph.
   * newState: the new state that was computed by following the ActionTs
   */
  case class StateCombo(originalState: State, newState: State)

  case class StepEval(nonSubsumptionStateCombos: Set[(EdgeAnnotation2, StateCombo)],
                      subsumptionStateCombos: Set[StateCombo],
                      newVisited: Set[State])

  private def isSubsumptionEdge(edge: Edge): Boolean = {
    edge._1.filters.isSubsumptionAnnotation
  }

  /*
   * Recursively follow all StateSubsumed edges.
   *
   * When encountering an edge annotation with StateSubsumed, this edge should automatically
   * be followed, as this implies that the current concrete state can match both states, and as
   * the first state (the state from which the edge originates) will be a dead end, matching of
   * the concrete state should proceed with the state that subsumes the 'dead' state.
   */
  def followStateSubsumedEdges(node: State, prunedGraph: AbstractGraph): Set[State] =
    if (prunedGraph.nodeEdges(node).isEmpty) {
      Set(node)
    } else {
      val edges = prunedGraph.nodeEdges(node)
      edges.headOption match {
        case None =>
          Set()
        case Some(edge) if edge._1.filters.isSubsumptionAnnotation =>
          /*
           * Make sure subsumption edge does not point to itself.
           */
          assert(edge._2 != node)
          val filterSubsumptionEdges = filterEdgeFilterAnnotations.findMinimallySubsumingEdges(edges)
          filterSubsumptionEdges.flatMap( (edge) => {
            followStateSubsumedEdges(edge._2, prunedGraph)
          })
        case Some(edge) =>
          Set(node)
      }
    }

  protected def stepEval(newState: State,
                         visited: Set[State],
                         mapping: Map[State, Set[State]],
                         graph: AbstractGraph,
                         prunedGraph: AbstractGraph): StepEval = {
    val originalStates = mapping(newState)
    val afterSubsumptionStates: Set[State] = originalStates.flatMap(followStateSubsumedEdges(_, prunedGraph))

    Logger.log(s"stepEval of ${originalStates.map(PrintState.stateToString[State](_, prunedGraph))}", Logger.D)

    /* TODO Only used for debugging */
    val originalStatePrunedIds = originalStates.map(prunedGraph.nodeId)
    /*
     * All outgoing edges in abstract graph
     * If originalState does not have any outgoing edges, return empty set
     */
    val edges: Set[Edge] = originalStates.flatMap(prunedGraph.edges.getOrElse(_, Set()))
    val afterSubsumptionEdges: Set[Edge] = afterSubsumptionStates.flatMap(prunedGraph.edges.getOrElse(_, Set()))
    val storeFilteredEdges = filterWithStore(newState, afterSubsumptionEdges)
    val kstoreFilteredEdges = filterWithKStore(newState, storeFilteredEdges)

    val (subsumptionEdges, nonSubsumptionEdges) = kstoreFilteredEdges.partition(isSubsumptionEdge)

    /* The combination of action-edges from all edges. */
    val mergedActionEdges: Set[ActionEdge] = nonSubsumptionEdges.map(_._1.actions)

    Logger.log(s"Using edges $edges", LogPropagation)
    Logger.log(s"Using afterSubsumptionEdges $afterSubsumptionEdges", LogPropagation)
    Logger.log(s"Using store filteredEdges $storeFilteredEdges", LogPropagation)
    Logger.log(s"Using kontstore filteredEdges $kstoreFilteredEdges", LogPropagation)
    Logger.log(s"Using mergedActionEdges $mergedActionEdges", LogPropagation)

    /*
     * Used for following subsumption edges: these edges don't contain any actions, so the newState won't change
     * by following the subsumption edge, but the newState must still be saved, as it must be used as input or the
     * actions stored in the edges following this subsumption edge.
     *
     * TODO can be removed now, after adding the rule for following subsumption edges.
     */
//    val usedSubsumptionEdges = subsumptionEdges.nonEmpty

    /*
     * Compute the set of new states via the set of merged action edges, and, while applying the action edges,
     * collect the filter edge for each of the applied action edges.
     */
    val results: Set[(State, Set[MachineFilterAnnotation])] = mergedActionEdges.flatMap((actionEdge) =>
      /*
       * For each action edge, take all actionRs a1, a2 ... ak, and apply them consecutively on newState.
       * Each actionR may produce a set of new newStates.
       */
      actionEdge.foldLeft[Set[(State, Set[MachineFilterAnnotation])]](Set((newState, Set[MachineFilterAnnotation]())))(
        (intermediaryStates: Set[(State, Set[MachineFilterAnnotation])], actionR: ActionReplay[Exp, Abs, Addr]) =>
          intermediaryStates.flatMap((intermediaryState: (State, Set[MachineFilterAnnotation])) => {
            val intermediaryFilters = intermediaryState._2
            val nextIntermediaryStepSet = actionRApplier.applyActionReplay(intermediaryState._1, actionR)
            nextIntermediaryStepSet.map({ case (nextIntermediaryState, nextIntermediaryFilters) =>
              (nextIntermediaryState, intermediaryFilters ++ nextIntermediaryFilters)
            })
          })
      )
    )

    def replaceFilters(edgeAnnotation: EdgeAnnotation2,
                       newMachineFilters: Set[MachineFilterAnnotation]): EdgeAnnotation2 = {
      edgeAnnotation.copy(filters = FilterAnnotations(newMachineFilters, edgeAnnotation.filters.semanticsFilters))
    }

    Logger.log(s"Results: $results", LogPropagation)

    /*
     * Using the filter edges gathered while applying the action edges, find the appropriate edge in the original
     * graph. This edge then provides an EdgeAnnotation (used to print text over the edge in the outputted graph)
     * and a StateCombo, consisting of the state at the end of the edge in the initial graph and the newly computed
     * state.
     */
    val nonSubsumptionStateCombos: Set[(EdgeAnnotation2, StateCombo)] = results.flatMap({
      case (newNewState, machineFilters) =>
        val currentId = graph.nodeId(newState)
        val initialGraphFilteredEdge: Set[Edge] =
          filterEdgeFilterAnnotations.filterToFilterEdge(nonSubsumptionEdges, FilterAnnotations(machineFilters, Set()))
        Logger.log(s"FilterEdge for state $newNewState (current ID $currentId) and concrete-ish " +
                   s"$machineFilters is $initialGraphFilteredEdge", LogPropagation)
        initialGraphFilteredEdge.map( (edge) => {
          /* Use the new filters, generated by applying the actionEdge. */
          val edgeAnnotation = replaceFilters(edge._1, machineFilters)
          val newOriginalState = edge._2
          (edgeAnnotation, StateCombo(newOriginalState, newNewState))
        })
    })
    Logger.log(s"nonSubsumptionStateCombos: $nonSubsumptionStateCombos", LogPropagation)

    /*
     * Calculate new StateCombos for subsumption edges.
     */
    val subsumptionStateCombos: Set[StateCombo] = subsumptionEdges.map( (edge: Edge) => StateCombo(edge._2, newState) )

    Logger.log(s"newStateCombos = ${nonSubsumptionStateCombos.map( (sc: (EdgeAnnotation2, StateCombo)) =>
                 prunedGraph.nodeId(sc._2.originalState))}", Logger.D)
    val newVisited = visited + newState // TODO remove if (usedSubsumptionEdges) visited else visited + newState
    StepEval(nonSubsumptionStateCombos, subsumptionStateCombos, newVisited)
  }

  case class TodoPair(todo: Set[State], mapping: Map[State, Set[State]]) {

    def dropHead: TodoPair =
      TodoPair(todo.tail, mapping)

    def dropHeadAndAddStates(stateCombos: Set[StateCombo]): TodoPair =
      TodoPair(todo.tail ++ stateCombos.map(_.newState), TodoPair.extendMapping(mapping, stateCombos))
  }

  object TodoPair {

    private def extendMapping(initial:  Map[State, Set[State]], stateCombos: Set[StateCombo]): Map[State, Set[State]] = {
      stateCombos.foldLeft(initial)( (newMapping, sc) => {
        val oldValues = newMapping.getOrElse(sc.newState, Set())
        newMapping + (sc.newState -> (oldValues + sc.originalState))
      })
    }

    def init(stateCombos: Set[StateCombo]): TodoPair =
      TodoPair(stateCombos.map(_.newState), extendMapping(Map.empty, stateCombos))
  }

  protected def addAllEdges(initialState: State,
                            graph: AbstractGraph): AbstractGraph = {
    def loop(todo: Set[State], visited: Set[State], graph: AbstractGraph): AbstractGraph = todo.headOption match {
      case None =>
        graph
      case Some(state) if ! visited.contains(state) =>
        val edges = graph.edges(state)
        val targetStates = edges.map(_._2)
        val newGraph = graph.addEdges(edges.map( (edge) => (state, edge._1, edge._2)) )
        loop(todo.tail ++ targetStates, visited + state, newGraph)
      case _ =>
        loop(todo.tail, visited, graph)
    }
    loop(Set(initialState), Set(), graph)
  }

  /*
   * Keep track of the set of visited originalStates, but not of the set of newStates.
   */
  private def evalLoop(todoPair: TodoPair,
                       visited: Set[State],
                       graph: Graph[State, EdgeAnnotation2],
                       stepCount: Int,
                       initialGraph: AbstractGraph,
                       prunedGraph: AbstractGraph): AbstractGraph = {
    Logger.log(s"Size of visited set ${visited.size}", Logger.D)
    Logger.log(s"Size of todo set ${todoPair.todo.size}", Logger.D)
    val checkSubsumes = true
    todoPair.todo.headOption
    match {
      case None =>
        graph
      case Some(newState) =>
//        val originalStateId = prunedGraph.nodeId(originalState)
//        Logger.log(s"Incrementally evaluating original state ${initialGraph.nodeId(originalState)} " +
//                   s"(currentID: $originalStateId) $originalState with new state $newState", LogPropagation)
        if (actionRApplier.halted(newState)) {
          Logger.log(s"State halted", LogPropagation)
          evalLoop(todoPair.dropHead, visited + newState, graph, stepCount, initialGraph, prunedGraph)
        } else if (visited.contains(newState)) {
          Logger.log(s"State already visited", LogPropagation)
          evalLoop(todoPair.dropHead, visited, graph, stepCount, initialGraph, prunedGraph)
        } else if (checkSubsumes && visited.exists((s2) => actionRApplier.subsumes(s2, newState))) {
          Logger.log(s"State subsumed", LogPropagation)
          val updatedGraph = visited.foldLeft[AbstractGraph](graph)({
            case (graph, s2) =>
              if (actionRApplier.subsumes(s2, newState)) {
                graph.addEdge(newState, EdgeAnnotation.subsumptionEdge, s2)
              } else {
                graph
              }
          })
          evalLoop(todoPair.dropHead, visited, updatedGraph, stepCount, initialGraph, prunedGraph)
        } else {
          val StepEval(nonSubsumptionStateCombos, subsumptionStateCombos, newVisited) =
            stepEval(newState, visited, todoPair.mapping, graph, prunedGraph)
          evalLoop(todoPair.dropHeadAndAddStates(subsumptionStateCombos ++ nonSubsumptionStateCombos.map(_._2)),
                   newVisited,
                   graph.addEdges(nonSubsumptionStateCombos.map({
                     case (edgeAnnotation, StateCombo(_, newNewState)) =>
                       (newState, edgeAnnotation, newNewState)
                   })),
                   stepCount,
                   initialGraph,
                   prunedGraph)
        }
    }
  }

  def convertGraph[Node: Descriptor, EdgeAnnotation, NewEdgeAnnotation](g: Graph[Node, EdgeAnnotation],
                                                                        f: EdgeAnnotation => NewEdgeAnnotation): Graph[Node, NewEdgeAnnotation] = {
    val newValues: Map[Node, Set[(NewEdgeAnnotation, Node)]] = g.edges.mapValues((value: Set[(EdgeAnnotation, Node)]) =>
      value.map((value: (EdgeAnnotation, Node)) => (f(value._1), value._2)))
    new HyperlinkedGraph[Node, NewEdgeAnnotation](g.ids, g.next, g.nodes, newValues)
  }

  def applyEdgeActions(convertedState: State,
                       stepCount: Int,
                       rootNodes: Set[State],
                       initialGraph: AbstractGraph,
                       prunedGraph: AbstractGraph): AbstractGraph = {
    rootNodes.foreach((node) => Logger.log(s"node id: ${initialGraph.nodeId(node)}", Logger.U))
    if (GlobalFlags.INCREMENTAL_OPTIMISATION && rootNodes.size == 1 && rootNodes.head == convertedState) {
      Logger.log(s"Skipping propagation phase because convertedState equals single root state", Logger.U)
      prunedGraph
    } else {
      /*
       * Associate the (one) abstracted concrete state with all states in the CurrentNodes set, as the states in
       * this set ought to correspond with this concrete state.
       */
      val rootStateCombos = rootNodes.map( (state) => StateCombo(state, convertedState) )
      evalLoop(TodoPair.init(rootStateCombos),
               Set(),
               new HyperlinkedGraph[State, EdgeAnnotation2],
               stepCount,
               initialGraph,
               prunedGraph)
    }
  }

}