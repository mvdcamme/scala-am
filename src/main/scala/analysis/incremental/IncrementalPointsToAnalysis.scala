class IncrementalPointsToAnalysis[Exp : Expression,
                                  AbstL : IsSchemeLattice,
                                  Addr : Address,
                                  State <: StateTrait[Exp, AbstL, Addr, _] : Descriptor]
                                 (implicit actionTApplier: ActionReplayApplier[Exp, AbstL, Addr, State]) {

  val usesGraph = new UsesGraph[Exp, AbstL, Addr, State]
  import usesGraph._

  val pruneUnreachableNodes = new PruneUnreachableNodes[Exp, AbstL, Addr, State]
  val propagateRunTimeInfo = new PropagateRunTimeInfo[Exp, AbstL, Addr, State]

  var initialGraph: Option[AbstractGraph] = None
  var currentGraph: Option[AbstractGraph] = initialGraph
  var currentNodes: Set[State] = Set()

  def hasInitialGraph: Boolean = currentGraph.isDefined
  def initializeGraph(graph: AbstractGraph) = {
    initialGraph = Some(graph)
    currentGraph = initialGraph
    val someStartNode = graph.getNode(0)
    assert(someStartNode.isDefined)
    currentNodes = Set(someStartNode.get)
  }

  def assertInitialized(): Unit = {
    assert(initialGraph.isDefined)
    assert(currentGraph.isDefined)
  }

  def containsNode(node: State): Boolean = {
    currentGraph.get.nodeId(node) != -1
  }

  def computeSuccNodes(convertFrameFun: ConcreteFrame => AbstractFrame,
                       edgeInfos: List[EdgeFilterAnnotation],
                       stepNumber: Int): Unit = {
    assertInitialized()
    currentNodes = pruneUnreachableNodes.computeSuccNodes(convertFrameFun, edgeInfos, stepNumber, currentNodes, initialGraph.get, currentGraph.get)
  }

  def end(): Unit = pruneUnreachableNodes.end(initialGraph.get)

  def filterReachable(stepCount: Int): Unit = {
    assertInitialized()
    currentGraph = Some(pruneUnreachableNodes.filterReachable(stepCount, currentNodes, currentGraph.get))
  }

  def applyEdgeActions(convertedState: State, stepCount: Int): AbstractGraph = {
    assertInitialized()
    val optionGraph = propagateRunTimeInfo.applyEdgeActions(convertedState,
                                                            stepCount,
                                                            currentNodes,
                                                            initialGraph.get,
                                                            currentGraph.get)
    optionGraph.get
//    val areEqual = graphsEqual(currentGraph.get, optionGraph.get)
//    Logger.log(s"Graphs equal? $areEqual", Logger.U)
//    assert(areEqual)
  }

  def graphsEqual(graph1: AbstractGraph, graph2: AbstractGraph): Boolean = {
    def nodeToString(node: State, graph: AbstractGraph): String =
      s"$node (id: ${graph.nodeId(node)})"

    def breadthFirst(todo: List[State], visited: Set[State]): Boolean = {
      if (todo.isEmpty) {
        true
      } else {
        val node = todo.head
        Logger.log(s"Checking node $node ${nodeToString(node, graph1)} ${nodeToString(node, graph2)}", Logger.D)
        /* Have to make sure that node1 and node2 are equal. */
        if (visited.contains(node)) {
          breadthFirst(todo.tail, visited)
        } else {
          val edges1 = graph1.edges.getOrElse(node, Set()) // .getOrElse as the node might not have any outgoing edges
          val edges2 = graph2.edges.getOrElse(node, Set())
          val edgesWithoutActionRs1 = edges1.map((edge) => (edge._1._1, edge._2))
          val edgesWithoutActionRs2 = edges2.map((edge) => (edge._1._1, edge._2))
          if (edges1.size == edges2.size) {
            val newStates = edgesWithoutActionRs1.foldLeft[List[State]](Nil)((states, edgeWithoutActionRs1) => {
              val edgeWithoutActionRs2 = edgesWithoutActionRs2.filter(edgeWithoutActionRs1 == _)
              assert(edgeWithoutActionRs2.size == 1,
                     s"Edges of node1 ${nodeToString(node, graph1)} don't match edges of node2 " +
                     s"${nodeToString(node, graph2)}")
              edgeWithoutActionRs1._2 :: states
            })
            breadthFirst(todo.tail ++ newStates, visited + node)
          } else {
            Logger.log(s"Number of edges of ${nodeToString(node, graph1)} does not match number of edges of " +
              s"${nodeToString(node, graph2)}", Logger.D)
            false
          }
        }
      }
    }
    val node1 = graph1.getNode(0)
    val node2 = graph2.getNode(0)
    assert(node1.isDefined && node2.isDefined)
    assert(actionTApplier.statesEqual(node1.get, node2.get), s"node1: ${node1.get}, node2: ${node2.get}")
//    assert(node1.get == node2.get, s"node1: ${node1.get}, node2: ${node2.get}")
    breadthFirst(List(node1.get), Set())
  }
}
