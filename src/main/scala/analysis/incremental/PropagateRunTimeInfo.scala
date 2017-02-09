class PropagateRunTimeInfo[Exp : Expression,
                           AbstL : IsSchemeLattice,
                           Addr : Address,
                           State <: StateTrait[Exp, AbstL, Addr, _] : Descriptor]
                          (implicit actionTApplier: ActionReplayApplier[Exp, AbstL, Addr, State]) {

  val usesGraph = new UsesGraph[Exp, AbstL, Addr, State]
  import usesGraph._

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

  /*
   * Keep track of the set of visited originalStates, but not of the set of newStates.
   */
  private def evalLoop(todo: Set[StateCombo],
                       visited: Set[State],
                       graph: Option[Graph[State, EdgeAnnotation]],
                       stepCount: Int,
                       initialGraph: AbstractGraph,
                       currentGraph: AbstractGraph):
  Option[Graph[State, EdgeAnnotation]] =
    todo.headOption
    match {
      case None =>
        graph.get.toDotFile(s"Analysis/Incremental/incremental_graph_$stepCount.dot",
          node => List(scala.xml.Text(node.toString.take(40))),
          (s) => Colors.Green,
          node => List(scala.xml.Text(("[" + node._1.mkString(", ") + "], [" + node._2.mkString(", ") + "]").take(300))),
          None)
        graph
      case Some(sc@(StateCombo(originalState, newState))) =>
        val originalStateId = currentGraph.nodeId(originalState)
        Logger.log(s"Incrementally evaluating original state ${initialGraph.nodeId(originalState)} " +
          s"(currentID: $originalStateId) " +
          s"$originalState with new state $newState", Logger.D)
        if (actionTApplier.halted(newState)) {
          Logger.log(s"State halted", Logger.D)
          evalLoop(todo.tail, visited + newState, graph, stepCount, initialGraph, currentGraph)
        } else if (visited.exists( (s2) => actionTApplier.subsumes(s2, newState) )) {
          Logger.log(s"State subsumed", Logger.D)
          evalLoop(todo.tail, visited + newState, graph, stepCount, initialGraph, currentGraph)
        } else if (visited.contains(newState)) {
          Logger.log(s"State already visited", Logger.D)
          evalLoop(todo.tail, visited, graph, stepCount, initialGraph, currentGraph)
        } else {
          /*
           * If originalState does not have any outgoing edges, return empty set
           */

          val edges: Set[Edge] = currentGraph.edges.getOrElse(originalState, Set()) // All outgoing edges in abstract graph
          val filteredEdges = filterWithStore(newState, edges)

          Logger.log(s"Using edges $edges", Logger.D)
          Logger.log(s"Using filteredEdges $filteredEdges", Logger.D)

          var somethingChanged = false

          /*
           * For all filteredEdges e, take all actionTs a1, a2 ... ak, and apply them consecutively on newState.
           * Each actionT may produce a set of new newStates.
           */
          val newStateCombos: Set[(EdgeAnnotation, StateCombo)] = filteredEdges.flatMap( (edge) => {
            val edgeAnnotation = edge._1
            val actionTs = edgeAnnotation._2
            val newOriginalState = edge._2
            /*
             * Compute, for all actions in one particular edge, the state resulting from the consecutive application of
             * all actions.
             */
            val newStates = actionTs.foldLeft[Set[State]](Set(newState))( (states, actionT) => {
              somethingChanged = true
              states.flatMap( (state) => actionTApplier.applyActionReplay(state, actionT))
            })
            newStates.map( (newState) => (edgeAnnotation, StateCombo(newOriginalState, newState)))
          })
          Logger.log(s"newStateCombos = ${newStateCombos.map( (sc: (EdgeAnnotation, StateCombo)) => currentGraph.nodeId(sc._2.originalState))}", Logger.D)
          val newVisited = if (somethingChanged) visited + newState else visited
          evalLoop(todo.tail ++ newStateCombos.map(_._2), newVisited, graph.map(_.addEdges(newStateCombos.map({
            case (edgeAnnotation, StateCombo(_, newNewState)) =>
              (newState, edgeAnnotation, newNewState) } ))), stepCount, initialGraph, currentGraph)
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
                       currentGraph: AbstractGraph): Unit = {
    val g = convertGraph[State, EdgeAnnotation, List[ActionReplay[Exp, AbstL, Addr]]](currentGraph, (edge: EdgeAnnotation) => edge._2)
    g.toDotFile(s"Analysis/Incremental/current_graph_$stepCount.dot", node => List(scala.xml.Text(node.toString.take(40))),
      (s) => Colors.Green,
      node => List(scala.xml.Text(node.mkString(", ").take(300))),
      None)
    currentNodes.foreach((node) => Logger.log(s"node id: ${currentGraph.nodeId(node)}", Logger.U))
    /*
     * Associate the (one) abstracted concrete state with all states in the CurrentNodes set, as the states in
     * this set ought to correspond with this concrete state.
     */
    val rootNodes = currentNodes.map((state) => StateCombo(state, convertedState))
    val updatedGraph = evalLoop(rootNodes, Set(), Some(new HyperlinkedGraph[State, EdgeAnnotation]), stepCount, initialGraph, currentGraph)
  }


}