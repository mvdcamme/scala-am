class PruneUnreachableNodes[Exp : Expression,
                            AbstL : IsSchemeLattice,
                            Addr : Address,
                            State <: StateTrait[Exp, AbstL, Addr, _] : Descriptor] {

  val usesGraph = new UsesGraph[Exp, AbstL, Addr, State]
  import usesGraph._

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

  class SubsumesOrdering[T](subsumes: (T, T) => Boolean)
    extends PartialOrdering[T] {

    def lteq(x: T, y: T): Boolean = {
      (subsumes(x, y), subsumes(y, x)) match {
        case (false, true) => true
        case _ => false
      }
    }

    def tryCompare(x: T, y: T): Option[Int] = {
      (subsumes(x, y), subsumes(y, x)) match {
        case _ if x == y => Some(0)
        case (true, true) => Some(0)
        case (true, false) => Some(1)
        case (false, true) => Some(-1)
        case (false, false) => None
      }
    }
  }

  def findMinimallySubsuming[T](edges: Set[(Edge, T)],
                                ordering: SubsumesOrdering[T]): Set[Edge] = {
    edges
      .filter(tuple1 => {
        /*
         * Only keep a value n if it holds that n does not subsume any other value m.
         * Only keep a value n if it holds that n is either smaller than (subsumed by), 'equal' to or incomparable with
         * every other node m.
         */
        val excluded = edges.filter(_ != tuple1)
        excluded.forall(tuple2 =>
          ordering.tryCompare(tuple1._2, tuple2._2) match {
            case Some(1) => false
            case _ => true
          })
      })
      .map(_._1)
  }

  private def frameUsedSubsumes(getFrameFromInfo: EdgeFilterAnnotation => Option[AbstractFrame])
                               (info: EdgeFilterAnnotation,
                                convertedFrame: AbstractFrame)
  : Option[AbstractFrame] =
    getFrameFromInfo(info) match {
      case Some(abstractFrame) =>
        val subsumes = abstractFrame.subsumes(convertedFrame)
        Logger.log(s"frameUsedSubsumes: $abstractFrame subsumes $convertedFrame ? $subsumes", Logger.D)
        if (subsumes) {
          Some(abstractFrame)
        } else {
          None
        }
      case _ =>
        None
    }

  def filterFrameEdges(convertedFrame: AbstractFrame,
                       subsumesFrame: (EdgeFilterAnnotation,
                         AbstractFrame) => Option[AbstractFrame],
                       abstractEdges: Set[Edge]): Set[Edge] = {
    if (!convertedFrame.meaningfullySubsumes) {
      // TODO hack: implement a proper subsumes method for every frame
      abstractEdges
    } else {
      /*
       * All edges containing a FrameFollowed annotation whose abstract value actually subsumes the abstracted
       * concrete value, zipped together with the abstract value that was reached.
       */
      val edgesContainingFrames: Set[(Edge, AbstractFrame)] =
        abstractEdges
          .flatMap[(Edge, AbstractFrame), Set[(Edge, AbstractFrame)]](
          (edge: Edge) => {
            val someFound: Option[AbstractFrame] = edge._1._1.map(subsumesFrame(_, convertedFrame)).foldLeft[Option[AbstractFrame]](None)({
              case (Some(x), _) =>
                Some(x)
              case (None, y) =>
                y
            })
            someFound.foldLeft[Set[(Edge, AbstractFrame)]](Set())({
              case (_, abstractFrame) => Set((edge, abstractFrame))
            })
          })
      val ordering = new SubsumesOrdering[AbstractFrame]((frame1, frame2) =>
        frame1.subsumes(frame2))
      val minFrameFollowedEdges: Set[Edge] =
        findMinimallySubsuming(edgesContainingFrames, ordering)
      Logger.log(s"minFrameFollowedEdges = $minFrameFollowedEdges", Logger.D)
      minFrameFollowedEdges
    }
  }

  def filterSingleEdgeInfo(convertValueFun: ConcreteConcreteLattice.L => AbstL,
                           convertFrameFun: ConcreteFrame => AbstractFrame,
                           abstractEdges: Set[Edge],
                           concreteEdgeInfo: EdgeFilterAnnotation): Set[Edge] =
    concreteEdgeInfo match {

      case info: FrameFollowed[ConcreteConcreteLattice.L] =>
        filterFrameEdges(convertFrameFun(info.frame), frameUsedSubsumes({
          case info: FrameFollowed[AbstL] =>
            Some(info.frame)
          case _ =>
            None}),
          abstractEdges)

      case _ =>
        abstractEdges.filter({
          case ((abstractEdgeInfos, _), _) =>
            concreteEdgeInfo match {
              case EvaluatingExpression(e) =>
                abstractEdgeInfos.contains(concreteEdgeInfo)
              case KontAddrPopped(oldA, newA) =>
                val KAConverter = new ConvertTimestampKontAddrConverter(ConvertTimeStampConverter)
                val convertedOlda = KAConverter.convertKontAddr(oldA)
                val convertedNewa = KAConverter.convertKontAddr(newA)
                abstractEdgeInfos.contains(KontAddrPopped(convertedOlda, convertedNewa))
              case KontAddrPushed(ka) =>
                val convertedKa = new ConvertTimestampKontAddrConverter(ConvertTimeStampConverter).convertKontAddr(ka)
                abstractEdgeInfos.contains(KontAddrPushed(convertedKa))
            }
        })
    }

  def computeSuccNode(convertValueFun: ConcreteConcreteLattice.L => AbstL,
                      convertFrameFun: ConcreteFrame => AbstractFrame,
                      node: State,
                      concreteEdgeInfos: List[EdgeFilterAnnotation],
                      currentGraph: AbstractGraph): Set[State] = {
    val abstractEdges = currentGraph.nodeEdges(node)
    Logger.log(s"abstractEdgeInfos = ${abstractEdges.map(_._1)}", Logger.D)
    val filteredAbstractEdges =
      concreteEdgeInfos.foldLeft[Set[Edge]](abstractEdges)(
        (filteredAbstractEdges, concreteEdgeInfo) =>
          filterSingleEdgeInfo(convertValueFun,
            convertFrameFun,
            filteredAbstractEdges,
            concreteEdgeInfo))
    addEdgesVisited(node, filteredAbstractEdges)
    filteredAbstractEdges.map(_._2)
  }

  var graphSize: List[(Int, Int)] = Nil
  var concreteNodes: List[(Int, Int, List[Int])] = Nil

  private def addNodesVisited(nodes: Set[State]): Unit =
    nodes.foreach(nodesVisited += _)

  private def addEdgesVisited(node: State, edges: Set[Edge]): Unit =
    edges.foreach((tuple) => edgesVisited += ((node, tuple._1, tuple._2)))

  /**
    *
    * @param convertValueFun
    * @param convertFrameFun
    * @param edgeInfos
    * @param stepNumber
    * @param currentNodes
    * @param initialGraph
    * @param currentGraph
    * @return The updated set of current nodes.
    */
  def computeSuccNodes(convertValueFun: ConcreteConcreteLattice.L => AbstL,
                       convertFrameFun: ConcreteFrame => AbstractFrame,
                       edgeInfos: List[EdgeFilterAnnotation],
                       stepNumber: Int,
                       currentNodes: Set[State],
                       initialGraph: AbstractGraph,
                       currentGraph: AbstractGraph): Set[State] = {
    Logger.log(s"In step $stepNumber before: currentNodes = ${currentNodes.zip(currentNodes.map(initialGraph.nodeId))} " +
      s"\nconcreteEdgeInfos = $edgeInfos", Logger.D)
    addNodesVisited(currentNodes)
    /* First follow all StateSubsumed edges before trying to use the concrete edge information */
    val nodesSubsumedEdgesFollowed = currentNodes.flatMap(followStateSubsumedEdges(_, currentGraph))
    Logger.log(s"In step $stepNumber, followed subsumption edges: ${nodesSubsumedEdgesFollowed.map(initialGraph.nodeId)}", Logger.D)
    addNodesVisited(nodesSubsumedEdgesFollowed)
    val succNodes = nodesSubsumedEdgesFollowed.flatMap(computeSuccNode(convertValueFun, convertFrameFun, _, edgeInfos, currentGraph))
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
