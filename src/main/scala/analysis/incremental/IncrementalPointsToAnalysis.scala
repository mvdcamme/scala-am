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

  def filterReachable(stepCount: Int): Unit =
    pruneUnreachableNodes.filterReachable(stepCount, currentNodes, currentGraph.get)

  def applyEdgeActions(convertedState: State, stepCount: Int): Unit = {
    assertInitialized()
    propagateRunTimeInfo.applyEdgeActions(convertedState, stepCount, currentNodes, initialGraph.get, currentGraph.get)
  }
}
