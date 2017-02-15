class PropagateRunTimeInfo[Exp : Expression,
                           AbstL : IsSchemeLattice,
                           Addr : Address,
                           State <: StateTrait[Exp, AbstL, Addr, _] : Descriptor]
                          (implicit actionTApplier: ActionReplayApplier[Exp, AbstL, Addr, State]) {

  val usesGraph = new UsesGraph[Exp, AbstL, Addr, State]
  import usesGraph._

  val filterEdgeFilterAnnotations = new FilterEdgeFilterAnnotations[Exp, AbstL, Addr, State]

  type ActionEdge = List[ActionReplay[Exp, AbstL, Addr]]
  type FilterEdge = List[EdgeFilterAnnotation]

  private def filterWithStore(newState: State, edges: Set[Edge]): Set[Edge] = {

    def hasEdgeAnnot(edge: (EdgeAnnotation, State), edgeAnnotation: EdgeFilterAnnotation): Boolean =
      edge._1._1.exists({
        case annot if annot == edgeAnnotation => true
        case _ => false
      })
    /*
     * If there is a ThenBranchTaken-annotation and current state did NOT evaluate to true, take all edges not
     * containing a ThenBranchTaken-annotation.
     */
    val filteredTrue: Set[(EdgeAnnotation, State)] = if (edges.exists(hasEdgeAnnot(_, ThenBranchTaken)) &&
      (!actionTApplier.evaluatedTrue(newState))) {
      edges.filter(!hasEdgeAnnot(_, ThenBranchTaken))
    } else {
      edges
    }
    /*
     * If there is an ElseBranchTaken-annotation and current state did NOT evaluate to false, take all edges not
     * containing an ElseBranchTaken-annotation.
     */
    val filteredFalse: Set[(EdgeAnnotation, State)] = if (edges.exists(hasEdgeAnnot(_, ElseBranchTaken)) &&
      (!actionTApplier.evaluatedFalse(newState))) {
      edges.filter(!hasEdgeAnnot(_, ElseBranchTaken))
    } else {
      edges
    }

    val filteredEdges = filteredTrue.intersect(filteredFalse)
    if (filteredEdges.size != edges.size) {
      Logger.log(s"## Difference between edges and filteredEdges! ##", Logger.U)
    }
    filteredEdges
  }

  /*
   * originalState: The original state that was present in the initial abstract graph.
   * newState: the new state that was computed by following the ActionTs
   */
  case class StateCombo(originalState: State, newState: State)

  case class StepEval(newStateCombos: Set[(EdgeAnnotation, StateCombo)], newVisited: Set[State])

  protected def stepEval(sc: StateCombo,
                         visited: Set[State],
                         graph: AbstractGraph,
                         prunedGraph: AbstractGraph): StepEval = {

    val StateCombo(originalState, newState) = sc
    /*
     * If originalState does not have any outgoing edges, return empty set
     */
    val edges: Set[Edge] = prunedGraph.edges.getOrElse(originalState, Set()) // All outgoing edges in abstract graph
    val filteredEdges = filterWithStore(newState, edges)

    Logger.log(s"Using edges $edges", Logger.D)
    Logger.log(s"Using filteredEdges $filteredEdges", Logger.D)

    /* The combination of action-edges from all edges. */
    val mergedActionEdges: Set[ActionEdge] = filteredEdges.map(_._1._2)

    var somethingChanged = false

    /*
     * Compute the set of new states via the set of merged action edges, and, while applying the action edges,
     * collect the filter edge for each of the applied action edges.
     */
    val results: Set[(State, FilterEdge)] = mergedActionEdges.flatMap((actionEdge) =>
      /*
       * For each action edge, take all actionRs a1, a2 ... ak, and apply them consecutively on newState.
       * Each actionR may produce a set of new newStates.
       */
      actionEdge.foldLeft[Set[(State, FilterEdge)]](Set((newState, Nil)))(
        (intermediaryStates: Set[(State, FilterEdge)], actionR: ActionReplay[Exp, AbstL, Addr]) =>
          intermediaryStates.flatMap((intermediaryState: (State, FilterEdge)) => {
            somethingChanged = true
            val intermediaryFilters = intermediaryState._2
            val nextIntermediaryStepSet = actionTApplier.applyActionReplay(intermediaryState._1, actionR)
            nextIntermediaryStepSet.map({ case (nextIntermediaryState, nextIntermediaryFilters) =>
              (nextIntermediaryState, intermediaryFilters ++ nextIntermediaryFilters)
            })
          })
      )
    )
    /*
     * Using the filter edges gathered while applying the action edges, find the appropriate edge in the original
     * graph. This edge then provides an EdgeAnnotation (used to print text over the edge in the outputted graph)
     * and a StateCombo, consisting of the state at the end of the edge in the initial graph and the newly computed
     * state.
     */
    val newStateCombos: Set[(EdgeAnnotation, StateCombo)] = results.flatMap({
      case (newNewState, filterEdge) =>
        val currentId = graph.nodeId(newState)
        val initialGraphFilteredEdge: Set[Edge] = filterEdgeFilterAnnotations.filterAllEdgeInfos(filteredEdges, filterEdge)
        Logger.log(s"FilterEdge for state $newNewState (current ID $currentId) and concrete-ish " +
                   s"$filterEdge is $initialGraphFilteredEdge", Logger.D)
        initialGraphFilteredEdge.map( (edge) => {
          /* Use the new filterEdge, generated by applying the actionEdge. */
          val edgeAnnotation = edge._1.copy(_1 = filterEdge)
          val newOriginalState = edge._2
          (edgeAnnotation, StateCombo(newOriginalState, newNewState))
        })
    })

    Logger.log(s"newStateCombos = ${newStateCombos.map( (sc: (EdgeAnnotation, StateCombo)) =>
      prunedGraph.nodeId(sc._2.originalState))
    }", Logger.D)
    val newVisited = if (somethingChanged) visited + newState else visited
    StepEval(newStateCombos, newVisited)
  }

  /*
   * Keep track of the set of visited originalStates, but not of the set of newStates.
   */
  private def evalLoop(todo: Set[StateCombo],
                       visited: Set[State],
                       graph: Option[Graph[State, EdgeAnnotation]],
                       stepCount: Int,
                       initialGraph: AbstractGraph,
                       prunedGraph: AbstractGraph):
  Option[Graph[State, EdgeAnnotation]] = {
    val checkSubsumes = false
    todo.headOption
    match {
      case None =>
        graph.get.toDotFile(s"Analysis/Incremental/incremental_graph_$stepCount.dot",
                            node => List(scala.xml.Text(node.toString.take(40))),
                            (s) => Colors.Green,
                            node => {
                              val fullString = s"[${node._1.mkString(", ")}], [${node._2.mkString(", ")}]"
                              List(scala.xml.Text(fullString.take(300)))
                            },
                            None)
        graph
      case Some(sc@(StateCombo(originalState, newState))) =>
        val originalStateId = prunedGraph.nodeId(originalState)
        Logger.log(s"Incrementally evaluating original state ${initialGraph.nodeId(originalState)} " +
          s"(currentID: $originalStateId) $originalState with new state $newState", Logger.D)
        if (actionTApplier.halted(newState)) {
          Logger.log(s"State halted", Logger.D)
          evalLoop(todo.tail, visited + newState, graph, stepCount, initialGraph, prunedGraph)
        } else if (checkSubsumes && visited.exists((s2) => actionTApplier.subsumes(s2, newState))) {
          Logger.log(s"State subsumed", Logger.D)
          val updatedGraph = graph.map(visited.foldLeft[AbstractGraph](_)({
            case (graph, s2) =>
              if ( actionTApplier.subsumes(s2, newState))
                graph.addEdge(newState, (List(StateSubsumed), Nil), s2)
              else
                graph}))
          evalLoop(todo.tail, visited, updatedGraph, stepCount, initialGraph, prunedGraph)
        } else if (visited.contains(newState)) {
          Logger.log(s"State already visited", Logger.D)
          evalLoop(todo.tail, visited, graph, stepCount, initialGraph, prunedGraph)
        } else {
          val stepEvalResult = stepEval(sc, visited, graph.get, prunedGraph)
          evalLoop(todo.tail ++ stepEvalResult.newStateCombos.map(_._2), stepEvalResult.newVisited,
            graph.map(_.addEdges(stepEvalResult.newStateCombos.map({
              case (edgeAnnotation, StateCombo(_, newNewState)) =>
                (newState, edgeAnnotation, newNewState)
            }))), stepCount, initialGraph, prunedGraph)
        }
    }
  }

  def convertGraph[Node : Descriptor, EdgeAnnotation, NewEdgeAnnotation](g: Graph[Node, EdgeAnnotation],
                                                                         f: EdgeAnnotation => NewEdgeAnnotation): Graph[Node, NewEdgeAnnotation] = {
    val newValues: Map[Node, Set[(NewEdgeAnnotation, Node)]] = g.edges.mapValues( (value: Set[(EdgeAnnotation, Node)]) =>
      value.map( (value: (EdgeAnnotation, Node)) => (f(value._1), value._2)))
    new HyperlinkedGraph[Node, NewEdgeAnnotation](g.ids, g.next, g.nodes, newValues)
  }

  def applyEdgeActions(convertedState: State,
                       stepCount: Int,
                       currentNodes: Set[State],
                       initialGraph: AbstractGraph,
                       prunedGraph: AbstractGraph): Option[AbstractGraph] = {
    prunedGraph.toDotFile(s"Analysis/Incremental/pruned_graph_$stepCount.dot",
                           node => List(scala.xml.Text(node.toString.take(40))),
                           (s) => Colors.Green,
                           node => {
                             val fullString = s"[${node._1.mkString(", ")}], [${node._2.mkString(", ")}]"
                             List(scala.xml.Text(fullString.take(300)))
                           },
                           None)
    currentNodes.foreach((node) => Logger.log(s"node id: ${initialGraph.nodeId(node)}", Logger.U))
    /*
     * Associate the (one) abstracted concrete state with all states in the CurrentNodes set, as the states in
     * this set ought to correspond with this concrete state.
     */
    val rootNodes = currentNodes.map((state) => StateCombo(state, convertedState))
    evalLoop(rootNodes, Set(), Some(new HyperlinkedGraph[State, EdgeAnnotation]), stepCount, initialGraph, prunedGraph)
  }


}