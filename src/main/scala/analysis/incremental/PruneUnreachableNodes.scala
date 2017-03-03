import ConcreteConcreteLattice.ConcreteValue

class PruneUnreachableNodes[Exp : Expression,
                            AbstL : IsSchemeLattice,
                            Addr : Address,
                            State <: StateTrait[Exp, AbstL, Addr, _] : Descriptor] {

  val usesGraph = new UsesGraph[Exp, AbstL, Addr, State]
  import usesGraph._

  val filterEdgeFilterAnnotations = new FilterEdgeFilterAnnotations[Exp, AbstL, Addr, State]

  var nodesVisited: Set[State] = Set()
  var edgesVisited: Set[(State, EdgeAnnotation2, State)] = Set()

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
      prunedGraph.nodeEdges(node).flatMap((edge) =>
        if (edge._1.filters.isSubsumptionAnnotation) {
          addEdgesVisited(node, Set(edge))
          followStateSubsumedEdges(edge._2, prunedGraph)
        } else {
          Set(node)
        })
    }

  def computeSuccNode(convertFrameFun: ConcreteFrame => AbstractFrame,
                      node: State,
                      concreteFilters: FilterAnnotations[Exp, AbstL, Addr],
                      prunedGraph: AbstractGraph): Set[State] = {
    val abstractEdges: Set[Edge] = prunedGraph.nodeEdges(node)
    Logger.log(s"abstractEdgeInfos = ${abstractEdges.map(_._1)}", Logger.D)
    val filteredAbstractEdges = filterEdgeFilterAnnotations.filterConcreteFilterEdge(abstractEdges, concreteFilters, convertFrameFun)
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
                       filters: FilterAnnotations[Exp, AbstL, Addr],
                       stepNumber: Int,
                       currentNodes: Set[State],
                       initialGraph: AbstractGraph,
                       prunedGraph: AbstractGraph): Set[State] = {
    Logger.log(s"In step $stepNumber before: currentNodes = " +
               s"${currentNodes.zip(currentNodes.map(initialGraph.nodeId))}\n" +
               s"concreteEdgeInfos = $filters, $filters", Logger.D)
    addNodesVisited(currentNodes)
    /* First follow all StateSubsumed edges before trying to use the concrete edge information */
    val nodesSubsumedEdgesFollowed: Set[State] = currentNodes.flatMap(followStateSubsumedEdges(_, prunedGraph))
    Logger.log(s"In step $stepNumber, followed subsumption edges: ${nodesSubsumedEdgesFollowed.map(initialGraph.nodeId)}", Logger.D)
    addNodesVisited(nodesSubsumedEdgesFollowed)
    val succNodes = nodesSubsumedEdgesFollowed.flatMap(computeSuccNode(convertFrameFun, _, filters, prunedGraph))
    addNodesVisited(succNodes)
    Logger.log(s"succNodes = ${succNodes.zip(succNodes.map(initialGraph.nodeId))}", Logger.D)
    Logger.log(s"In step $stepNumber, succNodes before subsumption edges: ${succNodes.map(initialGraph.nodeId)}", Logger.D)
    val newCurrentNodes = succNodes.flatMap(followStateSubsumedEdges(_, prunedGraph))
    concreteNodes = concreteNodes :+ (stepNumber, currentNodes.size, currentNodes.toList.map(initialGraph.nodeId))
    Logger.log(s"In step $stepNumber after: newCurrentNodes = ${newCurrentNodes.zip(currentNodes.map(initialGraph.nodeId))}", Logger.D)
    newCurrentNodes
  }

  private def saveTraversedGraph(initialGraph: AbstractGraph,
                                 nodesVisited: Set[State],
                                 edgesVisited: Set[(State, EdgeAnnotation2, State)]): Unit = {
    initialGraph.toDotFile(
      "Analysis/Traversed graph/traversed_graph.dot",
      node => List(scala.xml.Text(node.toString.take(40))),
      (s) => if (nodesVisited.contains(s)) Colors.Red else Colors.White,
      node => List(scala.xml.Text( (node.filters.machineFilters.mkString(", ") ++
                                    node.filters.semanticsFilters.mkString(", ") ++
                                    node.actions.mkString(", ")).take(300))),
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
                        prunedGraph: AbstractGraph): ReachablesIntermediateResult = {
    val graph = reachables.graph
    val todoQueue = reachables.todoQueue
    val nodesDone = reachables.nodesDone
    if (!nodesDone.contains(node)) {
      val edges = prunedGraph.nodeEdges(node)
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
                   prunedGraph: AbstractGraph): ReachablesIntermediateResult =
    oldReachables.todoQueue match {
      case Nil =>
        oldReachables
      case node :: rest =>
        val reachables = ReachablesIntermediateResult(oldReachables.graph, oldReachables.nodesDone, rest)
        val newReachables = addReachableEdges(reachables, node, prunedGraph)
        breadthFirst(newReachables, prunedGraph)
    }

  def filterReachable(stepCount: Int,
                      currentNodes: Set[State],
                      prunedGraph: AbstractGraph): AbstractGraph = {
    val reachables = ReachablesIntermediateResult(new HyperlinkedGraph(), Set(), currentNodes.toList)
    val filteredGraph = breadthFirst(reachables, prunedGraph).graph
    val newEdgesSize = prunedGraph.edges.size
    graphSize = graphSize :+ (stepCount, newEdgesSize)
    filteredGraph
  }

}
