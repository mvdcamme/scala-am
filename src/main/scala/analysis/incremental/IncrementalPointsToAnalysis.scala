import ConcreteConcreteLattice.ConcreteValue

class IncrementalPointsToAnalysis[Exp : Expression,
                                  Abs : IsSchemeLattice,
                                  Addr : Address,
                                  Time : Timestamp,
                                  State <: StateTrait[Exp, Abs, Addr, Time] : Descriptor]
                                 (graphPrinter: GraphPrinter[Graph[State, EdgeAnnotation[Exp, Abs, Addr]]])
                                 (implicit actionRApplier: ActionReplayApplier[Exp, Abs, Addr, Time, State],
                                           analysisFlags: AnalysisFlags) {

  val usesGraph = new UsesGraph[Exp, Abs, Addr, State]
  import usesGraph._

  val pruneUnreachableNodes = new PruneUnreachableNodes[Exp, Abs, Addr, Time, State]
  val propagateRunTimeInfo = new PropagateRunTimeInfo[Exp, Abs, Addr, Time, State](graphPrinter)

  var initialGraph: Option[AbstractGraph] = None
  var lastPropagatedGraph: Option[AbstractGraph] = initialGraph
  var currentNodes: Set[State] = Set()

  def hasInitialGraph: Boolean = lastPropagatedGraph.isDefined
  def initializeGraph(graph: AbstractGraph) = {
    initialGraph = Some(graph)
    lastPropagatedGraph = initialGraph
    val someStartNode = graph.getNode(0)
    assert(someStartNode.isDefined)
    currentNodes = Set(someStartNode.get)
  }

  def assertInitialized(): Unit = {
    assert(initialGraph.isDefined)
    assert(lastPropagatedGraph.isDefined)
  }

  def containsNode(node: State): Boolean = {
    lastPropagatedGraph.get.nodeId(node) != -1
  }

  def computeSuccNodes(convertFrameFun: ConcreteFrame => AbstractFrame,
                       filters: FilterAnnotations[Exp, ConcreteValue, Addr],
                       stepNumber: Int): Unit = {
    assertInitialized()
    currentNodes = pruneUnreachableNodes.computeSuccNodes(convertFrameFun, filters, stepNumber, currentNodes, initialGraph.get, lastPropagatedGraph.get)
  }

  def end(): Unit = {
    pruneUnreachableNodes.end(initialGraph.get)
  }

  def filterReachable(stepCount: Int): Option[AbstractGraph] = {
    assertInitialized()
    lastPropagatedGraph = Some(pruneUnreachableNodes.filterReachable(stepCount, currentNodes, lastPropagatedGraph.get))
    lastPropagatedGraph
  }

  def applyEdgeActions(convertedState: State, stepCount: Int): Option[AbstractGraph] = {
    assertInitialized()
    Logger.log(s"Propagating run-time info for step $stepCount", Logger.U)
    lastPropagatedGraph = Some(propagateRunTimeInfo.applyEdgeActions(convertedState, stepCount, currentNodes,
                                                                     initialGraph.get, lastPropagatedGraph.get))
    currentNodes = Set(convertedState)
    lastPropagatedGraph
  }

  def subsumedGraphsEqual(graph1: AbstractGraph, graph2: AbstractGraph): Boolean = {
    if (graph1.size == 0 && graph2.size == 0) {
      true
    } else if (graph1.size == 0 || graph2.size == 0) {
      Logger.log(s"Graphs have a different size: graph1 ${graph1.nodes}, graph2: ${graph2.nodes}", Logger.U)
      false
    } else {
      val haltedStates1 = graph1.nodes.filter(actionRApplier.halted)
      val haltedStates2 = graph2.nodes.filter(actionRApplier.halted)
      Logger.log(s"haltedStates2: $haltedStates2", Logger.U)
      val joinedState1 = actionRApplier.joinStates(haltedStates1)
      val joinedState2 = actionRApplier.joinStates(haltedStates2)
      val result = joinedState1.finalValue == joinedState2.finalValue &&
                   joinedState1.store == joinedState2.store &&
                   joinedState1.errors == joinedState2.errors
      //      val result = joinedState1.store.subsumes(joinedState2.store) &&
//                   abs.subsumes(joinedState1.finalValue, joinedState2.finalValue)
      if (! result) {
        val diff1 = joinedState1.store.diff(joinedState2.store)
        val diff2 = joinedState2.store.diff(joinedState1.store)
        val kdiff1 = joinedState1.kstore.diff(joinedState2.kstore)
        val kdiff2 = joinedState2.kstore.diff(joinedState1.kstore)
        val errorsDiff1 = joinedState1.errors.diff(joinedState2.errors)
        val errorsDiff2 = joinedState2.errors.diff(joinedState1.errors)
//        Logger.log(s"Diff of kontstore:\nkstore1 - kstore2: $diff1\nkstore2 - kstore1: $diff2", Logger.U)
//        Logger.log(s"Diff of kontstore:\nkstore1 - kstore2: ${kdiff1.descriptor.describe(kdiff1)}\n\n\n\n##########" +
//                   s"\n\n\n\n\n\nkstore2 - kstore1: ${kdiff2.descriptor.describe(kdiff2)}", Logger.U)
//        Logger.log(s"Graphs are not the same:\n$joinedState1\n$joinedState2", Logger.U)
        Logger.log(s"Diff of store:\nstore1 - store2: $diff1\nstore2 - store1: $diff2", Logger.U)
        Logger.log(s"Diff of kontstore:\nkstore1 - kstore2: $kdiff1\nkstore2 - kstore1: $kdiff2", Logger.U)
        Logger.log(s"Diff of errors:\nerrors1 - errors2: $errorsDiff1\nstore2 - store1: $errorsDiff2", Logger.U)
        Logger.log(s"Graphs are not the same:\n$joinedState1\n$joinedState2", Logger.U)
      }
      result
    }
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
          /* .getOrElse as the node might not have any outgoing edges */
          val edges1 = graph1.edges.getOrElse(node, Set())
          val edges2 = graph2.edges.getOrElse(node, Set())
          /* To compare two edges, we only look at the target states and the machine filters. */
          val comparableEdges1: Set[(Set[MachineFilterAnnotation], State)] =
            edges1.map( (edge) => (edge._1.filters.machineFilters, edge._2))
          val comparableEdges2: Set[(Set[MachineFilterAnnotation], State)] =
            edges2.map( (edge) => (edge._1.filters.machineFilters, edge._2))
          if (edges1.size == edges2.size) {
            val newStates = comparableEdges1.foldLeft[List[State]](Nil)( (states, comparableEdge) => {
              val filterEdge1 = comparableEdge._1
              val state1 = comparableEdge._2
              val comparableEdge2 = comparableEdges2.filter( (edge2) => {
                val filterEdge2 = edge2._1
                val state2 = edge2._2
                state1 == state2 && filterEdge2.forall(filterEdge1.contains)
              })
              assert(comparableEdge2.size == 1,
                     s"Edges of node1 ${nodeToString(node, graph1)} don't match edges of node2 " +
                     s"${nodeToString(node, graph2)}")
              comparableEdge._2 :: states
            })
            breadthFirst(todo.tail ++ newStates, visited + node)
          } else {
            Logger.log(s"Number of edges of ${nodeToString(node, graph1)} does not match number of edges of " +
              s"${nodeToString(node, graph2)}", Logger.U)
            false
          }
        }
      }
    }
    if (graph1.size == 0 && graph2.size == 0) {
      true
    } else {
      val node1 = graph1.getNode(0)
      val node2 = graph2.getNode(0)
      assert(node1.isDefined && node2.isDefined)
      assert(actionRApplier.statesEqual(node1.get, node2.get), s"node1: ${node1.get}, node2: ${node2.get}")
//      assert(node1.get == node2.get, s"node1: ${node1.get}, node2: ${node2.get}")
      breadthFirst(List(node1.get), Set())
    }
  }
}
