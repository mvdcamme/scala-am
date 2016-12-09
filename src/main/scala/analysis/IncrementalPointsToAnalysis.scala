class IncrementalPointsToAnalysis[AbstL : IsSchemeLattice, GraphNode](val aam: AAM[_, _, _, _]) {

  type AbstractGraph = Graph[GraphNode, List[EdgeInformation]]

  var initialGraph: Option[Graph[GraphNode, List[EdgeInformation]]] = None
  var currentGraph: Option[Graph[GraphNode, List[EdgeInformation]]] = None
  var currentNodes: Set[GraphNode] = Set()

  def hasInitialGraph: Boolean = currentGraph.isDefined
  def initializeGraph(graph: AbstractGraph) = {
    initialGraph = Some(graph)
    currentGraph = initialGraph
    val someStartNode = graph.getNode(0)
    assert(someStartNode.isDefined)
    currentNodes = Set(someStartNode.get)
  }

  def containsNode(node: GraphNode): Boolean = {
    currentGraph.get.nodeId(node) != -1
  }

  /*
   * Recursively follow all StateSubsumed edges.
   *
   * When encountering an edge annotation with StateSubsumed, this edge should automatically
   * be followed, as this implies that the current concrete state can match both states, and as
   * the first state (the state from which the edge originates) will be a dead end, matching of
   * the concrete state should proceed with the state that subsumes the 'dead' state.
   */
  def followStateSubsumedEdges(node: GraphNode): Set[GraphNode] =
    currentGraph.get
      .nodeEdges(node)
      .flatMap((edge) =>
        if (edge._1.contains(StateSubsumed)) {
          /* Make sure that an edge is ONLY annotated with StateSubsumed. It should not be possible
           * to have a StateSubsumed edge with any other annotation. */
          assert(edge._1.size == 1, s"StateSubsumed edge contains more than 1 edge: ${edge._1}")
          followStateSubsumedEdges(edge._2)
        } else {
          Set(node)
        })

  def filterSingleEdgeInfo( convertValueFun: ConcreteConcreteLattice.L => AbstL,
                            abstractEdges: Set[(List[EdgeInformation], GraphNode)],
                            concreteEdgeInfo: EdgeInformation): Set[(List[EdgeInformation], GraphNode)] = {

    abstractEdges.filter({
      case (abstractEdgeInfos, node) =>
        concreteEdgeInfo match {
          case ThenBranchTaken | ElseBranchTaken =>
            abstractEdgeInfos.contains(concreteEdgeInfo)
          case EvaluatingExpression(e) =>
            abstractEdgeInfos.contains(concreteEdgeInfo)
          case OperatorTaken(_) | FrameFollowed(_) =>
            true
          case ReachedConcreteValue(concreteValue) =>
            abstractEdgeInfos.exists({
              /* Is there any ReachedValue edge containing an abstract value that subsumes the abstracted concrete
              value? There should be at least one ReachedValue annotation. If not, this edge should definitely be
              filtered. */
              case info: ReachedValue[AbstL] =>
                val isSchemeLattice = implicitly[IsSchemeLattice[AbstL]]
                val convertedValue = convertValueFun(concreteValue)
                isSchemeLattice.subsumes(info.v, convertedValue)
              case wrong: ReachedValue[_] => //TODO Should not happen
                assert(false)
                false
              case _ => false})
        }
    })
  }

  def computeSuccNode( convertValueFun: ConcreteConcreteLattice.L => AbstL,
                       node: GraphNode,
                       concreteEdgeInfos: List[EdgeInformation]): Set[GraphNode] = {
    assert(currentNodes.nonEmpty && currentGraph.isDefined)
    val abstractEdges = currentGraph.get.nodeEdges(node)
    val filteredAbstractEdges =
      concreteEdgeInfos.foldLeft[Set[(List[EdgeInformation], GraphNode)]](
        abstractEdges)((filteredAbstractEdges, concreteEdgeInfo) =>
        filterSingleEdgeInfo(convertValueFun, filteredAbstractEdges, concreteEdgeInfo))
    filteredAbstractEdges.map(_._2)
  }

  var i = 0
  var sizes: List[(Int, Int, List[Int])] = Nil

  def computeSuccNodes(convertValueFun: ConcreteConcreteLattice.L => AbstL,
                       edgeInfos: List[EdgeInformation]) = {
    /* First follow all StateSubsumed edges before trying to use the concrete edge information */
    val nodesSubsumedEdgesFollowed =
      currentNodes.flatMap(followStateSubsumedEdges)
    val succNodes =
      nodesSubsumedEdgesFollowed.flatMap(computeSuccNode(convertValueFun, _, edgeInfos))
    currentNodes = succNodes.flatMap(followStateSubsumedEdges)
    i += 1
    sizes = sizes :+ (i, currentNodes.size, currentNodes.toList.map(initialGraph.get.nodeId))
  }

  def end(): Unit = {
    val f = new java.io.File("Analysis/concrete_nodes_size.txt")
    val bw = new java.io.BufferedWriter(new java.io.FileWriter(f))
    sizes.foreach((tuple) =>
      bw.write(s"${tuple._1};${tuple._2}\n"))
    bw.close()
  }

  case class ReachablesIntermediateResult(graph: AbstractGraph,
                                          nodesDone: Set[GraphNode],
                                          todoQueue: List[GraphNode])

  /*
   * Remove variable todo, let addReachableEdges return tuple of graph and todo-list
   */
  def addReachableEdges(reachables: ReachablesIntermediateResult,
                        node: GraphNode): ReachablesIntermediateResult = {
    val graph = reachables.graph
    val todoQueue = reachables.todoQueue
    val nodesDone = reachables.nodesDone
    if (!nodesDone.contains(node)) {
      val edges = currentGraph.get.nodeEdges(node)
      val newGraphTodo = edges.foldLeft((graph, todoQueue))( (graphTodo, edge) => {
        (graphTodo._1.addEdge(node, edge._1, edge._2), graphTodo._2 :+ edge._2)
      })
      ReachablesIntermediateResult(newGraphTodo._1, nodesDone + node, newGraphTodo._2)
    } else {
      /* If the current node was already placed in the graph, all of its edges should also
       * already have been placed in the graph, so we don't need to recursively call ourselves here. */
      reachables
    }
  }

  def breadthFirst(reachables: ReachablesIntermediateResult): ReachablesIntermediateResult = reachables.todoQueue match {
    case Nil =>
      reachables
    case node :: rest =>
      val newReachables = addReachableEdges(ReachablesIntermediateResult(reachables.graph, reachables.nodesDone,
        rest), node)
      breadthFirst(newReachables)
  }

  def filterReachable(stepCount: Int): Unit = {
    assert(currentGraph.isDefined)
    val f = new java.io.FileWriter("Analysis/graph_size.txt", true)
    val bw = new java.io.BufferedWriter(f)
    val reachables = ReachablesIntermediateResult(new Graph(), Set(), currentNodes.toList)
    val edgesSize = currentGraph.get.edges.size
    val filteredGraph = breadthFirst(reachables).graph
    currentGraph = Some(filteredGraph)
    val newEdgesSize = currentGraph.get.edges.size
    bw.write(s"$stepCount;$edgesSize;$newEdgesSize\n")
    bw.close()
  }
}