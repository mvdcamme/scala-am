import ConcreteConcreteLattice.{ L => ConcreteValue }

class TrackAbstractNodes[Exp : Expression, Abs : IsSchemeLattice, Addr : Address, Time : Timestamp, State]
                        (val graph: Graph[State, EdgeAnnotation[Exp, Abs, Addr], Set[State]]) {

  val usesGraph = new UsesGraph[Exp, Abs, Addr, State]
  import usesGraph._

  val filterEdgeFilterAnnotations = new FilterEdgeFilterAnnotations[Exp, Abs, Addr, Time, State]

  private def initialCurrentNodes: Set[State] = {
    Set(graph.getNode(0).get)
  }
  private var currentNodes: Set[State] = initialCurrentNodes
  def getCurrentNodes: Set[State] = currentNodes
  def resetCurrentNodes(): Unit = {
    currentNodes = initialCurrentNodes
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
        case None => Set()
        case Some(edge) if edge._1.filters.isSubsumptionAnnotation =>
//          val filterSubsumptionEdges = filterEdgeFilterAnnotations.findMinimallySubsumingEdges(edges)
          edges.flatMap( (edge) => {
            followStateSubsumedEdges(edge._2, prunedGraph)
          })
        case Some(_) => Set(node)
      }
    }

  def computeSuccNode(convertFrameFun: ConvertableSchemeFrame[ConcreteValue, Addr, Time] => AbstractFrame,
                      node: State,
                      concreteFilters: FilterAnnotations,
                      prunedGraph: AbstractGraph): Set[State] = {
    val abstractEdges: Set[Edge] = prunedGraph.nodeEdges(node)
    Logger.D(s"abstractEdgeInfos = ${abstractEdges.map(_._1)}")
    val filteredAbstractEdges = filterEdgeFilterAnnotations.filterConcreteFilterEdge(abstractEdges, concreteFilters, convertFrameFun)
    filteredAbstractEdges.map(_._2)
  }

  def computeSuccNodes(convertFrameFun: ConvertableSchemeFrame[ConcreteValue, Addr, Time] => AbstractFrame,
                       filters: FilterAnnotations,
                       stepNumber: Int): Set[State] = {
    Logger.D(s"In step $stepNumber before: currentNodes = ${currentNodes.zip(currentNodes.map(graph.nodeId))}$filters")
    /* First follow all StateSubsumed edges before trying to use the concrete edge information */
    val nodesSubsumedEdgesFollowed: Set[State] = if (GlobalFlags.AAM_CHECK_SUBSUMES) {
      currentNodes.flatMap(followStateSubsumedEdges(_, graph))
    } else {
      currentNodes
    }
    Logger.D(s"In step $stepNumber, followed subsumption edges: ${nodesSubsumedEdgesFollowed.map(graph.nodeId)}")
    val succNodes = nodesSubsumedEdgesFollowed.flatMap(computeSuccNode(convertFrameFun, _, filters, graph))
    Logger.D(s"succNodes = ${succNodes.zip(succNodes.map(graph.nodeId))}")
    Logger.D(s"In step $stepNumber, succNodes before subsumption edges: ${succNodes.map(graph.nodeId)}")
    val newCurrentNodes = if (GlobalFlags.AAM_CHECK_SUBSUMES) {
      succNodes.flatMap(followStateSubsumedEdges(_, graph))
    } else {
      succNodes
    }
    Logger.D(s"In step $stepNumber after: newCurrentNodes = ${newCurrentNodes.zip(currentNodes.map(graph.nodeId))}")
    if (newCurrentNodes.isEmpty) {
      "debug"
    }
    assert(newCurrentNodes.nonEmpty)
    currentNodes = newCurrentNodes
    newCurrentNodes
  }

}
