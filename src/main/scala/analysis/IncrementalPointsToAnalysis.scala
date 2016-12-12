class IncrementalPointsToAnalysis[AbstL : IsSchemeLattice, GraphNode](val aam: AAM[_, _, _, _]) {

  type AbstractGraph = Graph[GraphNode, List[EdgeInformation]]

  var initialGraph: Option[Graph[GraphNode, List[EdgeInformation]]] = None
  var currentGraph: Option[Graph[GraphNode, List[EdgeInformation]]] = initialGraph
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

  object AbstLOrdering extends PartialOrdering[AbstL] {

    def lteq(x: AbstL, y: AbstL): Boolean = {
      val isSchemeLattice = implicitly[IsSchemeLattice[AbstL]]
      (isSchemeLattice.subsumes(x, y), isSchemeLattice.subsumes(y, x)) match {
        case (false, true) => true
        case _ => false
      }
    }

    def tryCompare(x: AbstL, y: AbstL): Option[Int] = {
      val isSchemeLattice = implicitly[IsSchemeLattice[AbstL]]
      (isSchemeLattice.subsumes(x, y), isSchemeLattice.subsumes(y, x)) match {
        case _ if x == y => Some(0)
        case (true, true) => Some(0)
        case (true, false) => Some(1)
        case (false, true) => Some(-1)
        case (false, false) => None
      }
    }
  }

  def iter(edgesContainingReachedValue: Set[((List[EdgeInformation], GraphNode), AbstL)]): Set[(
    (List[EdgeInformation], GraphNode), AbstL)] = {
    edgesContainingReachedValue.filter(tuple1 => {
      /*
       * Only keep a value n if it holds that n does not subsume any other value m.
       * Only keep a value n if it holds that n is either smaller than (subsumed by), 'equal' to or incomparable with
        * every other node m.
       */
      val excluded = edgesContainingReachedValue.filter(_ != tuple1)
      excluded.forall(tuple2 => AbstLOrdering.tryCompare(tuple1._2, tuple2._2) match {
        case Some(1) => false
        case _ => true
      })
    })
  }

  def filterSingleEdgeInfo( convertValueFun: ConcreteConcreteLattice.L => AbstL,
                            abstractEdges: Set[(List[EdgeInformation], GraphNode)],
                            concreteEdgeInfo: EdgeInformation): Set[(List[EdgeInformation], GraphNode)] = concreteEdgeInfo match {
    case ReachedConcreteValue(concreteValue) =>
      /* All edges containing a ReachedValue annotation whose abstract value actually subsumes the abstracted
      concrete value, zipped together with the abstract value that was reached. */
      val edgesContainingReachedValue: Set[((List[EdgeInformation], GraphNode), AbstL)] = abstractEdges.flatMap[(
        (List[EdgeInformation], GraphNode), AbstL), Set[((List[EdgeInformation], GraphNode), AbstL)]](
        (tuple: (List[EdgeInformation], GraphNode)) => {
          val reachedValueFound: Option[EdgeInformation] = tuple._1.find({
          case info: ReachedValue[AbstL] =>
            val isSchemeLattice = implicitly[IsSchemeLattice[AbstL]]
            val convertedValue = convertValueFun(concreteValue)
            isSchemeLattice.subsumes(info.v, convertedValue)
          case info: ReachedValue[_] => //TODO Should not happen
            assert(false)
            false
          case _ =>
            false
        })
          reachedValueFound match {
            case info: Some[ReachedValue[AbstL]] =>
              Set((tuple, info.get.v))
            case _ =>
              Set()
          }
        })
      implicit val ordering: PartialOrdering[AbstL] = AbstLOrdering
      val minReachedValueEdges: Set[(List[EdgeInformation], GraphNode)] = iter(edgesContainingReachedValue).map(_._1)
      minReachedValueEdges
    case _ => abstractEdges.filter({
      case (abstractEdgeInfos, node) =>
        concreteEdgeInfo match {
          case ThenBranchTaken | ElseBranchTaken =>
            abstractEdgeInfos.contains(concreteEdgeInfo)
          case EvaluatingExpression(e) =>
            abstractEdgeInfos.contains(concreteEdgeInfo)
          case OperatorTaken(_) | FrameFollowed(_) =>
            true
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

  var graphSize: List[(Int, Int)] = Nil
  var concreteNodes: List[(Int, Int, List[Int])] = Nil

  def computeSuccNodes(convertValueFun: ConcreteConcreteLattice.L => AbstL,
                       edgeInfos: List[EdgeInformation],
                       stepNumber: Int) = {
    /* First follow all StateSubsumed edges before trying to use the concrete edge information */
    val nodesSubsumedEdgesFollowed =
      currentNodes.flatMap(followStateSubsumedEdges)
    val succNodes =
      nodesSubsumedEdgesFollowed.flatMap(computeSuccNode(convertValueFun, _, edgeInfos))
    currentNodes = succNodes.flatMap(followStateSubsumedEdges)
    concreteNodes = concreteNodes :+ (stepNumber, currentNodes.size, currentNodes.toList.map(initialGraph.get.nodeId))
  }

  def end(): Unit = {
    /*
     * Write the evolution of the size + ids of the concrete nodes.
     */
    val fileWithoudIds = new java.io.File("Analysis/Concrete nodes/concrete_nodes_size.txt")
    val fileWithIds = new java.io.File("Analysis/Concrete nodes/concrete_nodes_size_with_ids.txt")
    val bwWithoutIds = new java.io.BufferedWriter(new java.io.FileWriter(fileWithoudIds))
    val bwWithIds = new java.io.BufferedWriter(new java.io.FileWriter(fileWithIds))
    concreteNodes.foreach((tuple) => {
      bwWithoutIds.write(s"${tuple._1};${tuple._2}\n")
      bwWithIds.write(s"${tuple._1};${tuple._2};${tuple._3.mkString(";")}\n")
    })
    bwWithoutIds.close()
    bwWithIds.close()

    /*
     * Write the evolution in the number of edges of the graph.
     */
    val f = new java.io.FileWriter("Analysis/Graph size/graph_size.txt")
    val bw = new java.io.BufferedWriter(f)
    graphSize.foreach( (tuple) => bw.write(s"${tuple._1};${tuple._2}\n") )
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
    val reachables = ReachablesIntermediateResult(new Graph(), Set(), currentNodes.toList)
    val filteredGraph = breadthFirst(reachables).graph
    currentGraph = Some(filteredGraph)
    val newEdgesSize = currentGraph.get.edges.size
    graphSize = graphSize :+ (stepCount, newEdgesSize)
  }
}