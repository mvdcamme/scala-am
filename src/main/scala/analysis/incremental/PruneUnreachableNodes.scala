import ConcreteConcreteLattice.ConcreteValue

class PruneUnreachableNodes[Exp : Expression,
                            AbstL : IsSchemeLattice,
                            Addr : Address,
                            State <: StateTrait[Exp, AbstL, Addr, _] : Descriptor] {

  val usesGraph = new UsesGraph[Exp, AbstL, Addr, State]
  import usesGraph._

  val filterEdgeFilterAnnotations = new FilterEdgeFilterAnnotations[Exp, AbstL, Addr, State]

  var nodesVisited: Set[State] = Set()
  var edgesVisited: Set[(State, EdgeAnnotation, State)] = Set()

  /*
   * Recursively follow all StateSubsumed edges.
   *
   * When encountering an edge annotation with StateSubsumed, this edge should automatically
   * be followed, as this implies that the current concrete state can match both states, and as
   * the first state (the state from which the edge originates) will be a dead end, matching of
   * the concrete state should proceed with the state that subsumes the 'dead' state.
   */
  def followStateSubsumedEdges(node: State, currentGraph: AbstractGraph): Set[State] =
    if (currentGraph.nodeEdges(node).isEmpty) {
      Set(node)
    } else {
      currentGraph.nodeEdges(node).flatMap((edge) =>
        if (edge._1._1.contains(StateSubsumed)) {
          /* Make sure that an edge is ONLY annotated with StateSubsumed. It should not be possible
           * to have a StateSubsumed edge with any other annotation. */
          assert(edge._1._1.size == 1,
            s"StateSubsumed edge contains more than 1 edge: ${edge._1}")
          addEdgesVisited(node, Set(edge))
          followStateSubsumedEdges(edge._2, currentGraph)
        } else {
          Set(node)
        })
    }

  def computeSuccNode(convertFrameFun: ConcreteFrame => AbstractFrame,
                      node: State,
                      concreteEdgeInfos: List[EdgeFilterAnnotation],
                      currentGraph: AbstractGraph): Set[State] = {
    val abstractEdges = currentGraph.nodeEdges(node)
    Logger.log(s"abstractEdgeInfos = ${abstractEdges.map(_._1)}", Logger.D)
    val frameConvertedConcreteEdgeFilters = concreteEdgeInfos.map({
      case filter: FrameFollowed[ConcreteValue] =>
        FrameFollowed[AbstL](convertFrameFun(filter.frame))
      case other =>
        other
    })
    val filteredAbstractEdges = filterEdgeFilterAnnotations.filterAllEdgeInfos(abstractEdges, frameConvertedConcreteEdgeFilters)
    addEdgesVisited(node, filteredAbstractEdges)
    filteredAbstractEdges.map(_._2)
  }

  var graphSize: List[(Int, Int)] = Nil
  var concreteNodes: List[(Int, Int, List[Int])] = Nil

  private def addNodesVisited(nodes: Set[State]): Unit =
    nodes.foreach(nodesVisited += _)

  private def addEdgesVisited(node: State, edges: Set[Edge]): Unit =
    edges.foreach((tuple) => edgesVisited += ((node, tuple._1, tuple._2)))

  def computeSuccNodes(convertFrameFun: ConcreteFrame => AbstractFrame,
                       edgeInfos: List[EdgeFilterAnnotation],
                       stepNumber: Int,
                       currentNodes: Set[State],
                       initialGraph: AbstractGraph,
                       currentGraph: AbstractGraph): Set[State] = {
    Logger.log(s"In step $stepNumber before: currentNodes = " +
               s"${currentNodes.zip(currentNodes.map(initialGraph.nodeId))}\n" +
               s"concreteEdgeInfos = $edgeInfos, $edgeInfos", Logger.D)
    addNodesVisited(currentNodes)
    /* First follow all StateSubsumed edges before trying to use the concrete edge information */
    val nodesSubsumedEdgesFollowed = currentNodes.flatMap(followStateSubsumedEdges(_, currentGraph))
    Logger.log(s"In step $stepNumber, followed subsumption edges: ${nodesSubsumedEdgesFollowed.map(initialGraph.nodeId)}", Logger.D)
    addNodesVisited(nodesSubsumedEdgesFollowed)
    val succNodes = nodesSubsumedEdgesFollowed.flatMap(computeSuccNode(convertFrameFun, _, edgeInfos, currentGraph))
    addNodesVisited(succNodes)
    Logger.log(s"succNodes = ${succNodes.zip(succNodes.map(initialGraph.nodeId))}", Logger.D)
    Logger.log(s"In step $stepNumber, succNodes before subsumption edges: ${succNodes.map(initialGraph.nodeId)}", Logger.D)
    val newCurrentNodes = succNodes.flatMap(followStateSubsumedEdges(_, currentGraph))
    concreteNodes = concreteNodes :+ (stepNumber, currentNodes.size, currentNodes.toList.map(initialGraph.nodeId))
    Logger.log(s"In step $stepNumber after: currentNodes = ${currentNodes.zip(currentNodes.map(initialGraph.nodeId))}", Logger.D)
    newCurrentNodes
  }

  private def saveTraversedGraph(initialGraph: AbstractGraph,
                                 nodesVisited: Set[State],
                                 edgesVisited: Set[(State, EdgeAnnotation, State)]): Unit = {
    initialGraph.toDotFile(
      "Analysis/Traversed graph/traversed_graph.dot",
      node => List(scala.xml.Text(node.toString.take(40))),
      (s) => if (nodesVisited.contains(s)) Colors.Red else Colors.White,
      node => List(scala.xml.Text((node._1.mkString(", ") ++ node._2.mkString(", ")).take(300))),
      Some((edge) =>
        if (edgesVisited.contains(edge)) Colors.Red else Colors.Black))
  }

  def end(initialGraph: AbstractGraph): Unit = {
    /*
     * Write the evolution of the size + ids of the concrete nodes.
     */
    val fileWithoudIds =
      new java.io.File("Analysis/Concrete nodes/concrete_nodes_size.txt")
    val fileWithIds = new java.io.File(
      "Analysis/Concrete nodes/concrete_nodes_size_with_ids.txt")
    val bwWithoutIds =
      new java.io.BufferedWriter(new java.io.FileWriter(fileWithoudIds))
    val bwWithIds =
      new java.io.BufferedWriter(new java.io.FileWriter(fileWithIds))
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

    saveTraversedGraph(initialGraph, nodesVisited, edgesVisited)
  }

  case class ReachablesIntermediateResult(graph: AbstractGraph,
                                          nodesDone: Set[State],
                                          todoQueue: List[State])

  /*
   * Remove variable todo, let addReachableEdges return tuple of graph and todo-list
   */
  def addReachableEdges(reachables: ReachablesIntermediateResult,
                        node: State,
                        currentGraph: AbstractGraph): ReachablesIntermediateResult = {
    val graph = reachables.graph
    val todoQueue = reachables.todoQueue
    val nodesDone = reachables.nodesDone
    if (!nodesDone.contains(node)) {
      val edges = currentGraph.nodeEdges(node)
      val newGraphTodo =
        edges.foldLeft((graph, todoQueue))((graphTodo, edge) => {
          (graphTodo._1.addEdge(node, edge._1, edge._2),
            graphTodo._2 :+ edge._2)
        })
      ReachablesIntermediateResult(newGraphTodo._1,
        nodesDone + node,
        newGraphTodo._2)
    } else {
      /* If the current node was already placed in the graph, all of its edges should also
       * already have been placed in the graph, so we don't need to recursively call ourselves here. */
      reachables
    }
  }

  def breadthFirst(oldReachables: ReachablesIntermediateResult,
                   currentGraph: AbstractGraph): ReachablesIntermediateResult =
    oldReachables.todoQueue match {
      case Nil =>
        oldReachables
      case node :: rest =>
        val reachables = ReachablesIntermediateResult(oldReachables.graph, oldReachables.nodesDone, rest)
        val newReachables = addReachableEdges(reachables, node, currentGraph)
        breadthFirst(newReachables, currentGraph)
    }

  def filterReachable(stepCount: Int,
                      currentNodes: Set[State],
                      currentGraph: AbstractGraph): AbstractGraph = {
    val reachables = ReachablesIntermediateResult(new HyperlinkedGraph(), Set(), currentNodes.toList)
    val filteredGraph = breadthFirst(reachables, currentGraph).graph
    val newEdgesSize = currentGraph.edges.size
    graphSize = graphSize :+ (stepCount, newEdgesSize)
    filteredGraph
  }

}
