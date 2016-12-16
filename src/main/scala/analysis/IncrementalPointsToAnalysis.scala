class IncrementalPointsToAnalysis[Exp : Expression, AbstL : IsSchemeLattice, GraphNode](val aam: AAM[_, _, _, _]) {

  type AbstractGraph = Graph[GraphNode, List[EdgeAnnotation]]
  type Edge = (List[EdgeAnnotation], GraphNode)

  var initialGraph: Option[AbstractGraph] = None
  var currentGraph: Option[AbstractGraph] = initialGraph
  var currentNodes: Set[GraphNode] = Set()

  var nodesVisited: Set[GraphNode] = Set()
  var edgesVisited: Set[(GraphNode, List[EdgeAnnotation], GraphNode)] = Set()

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
    if (currentGraph.get.nodeEdges(node).isEmpty) {
      Set(node)
    } else {
      currentGraph.get.nodeEdges(node).flatMap((edge) =>
        if (edge._1.contains(StateSubsumed)) {
          /* Make sure that an edge is ONLY annotated with StateSubsumed. It should not be possible
           * to have a StateSubsumed edge with any other annotation. */
          assert(edge._1.size == 1,
            s"StateSubsumed edge contains more than 1 edge: ${edge._1}")
          addEdgesVisited(node, Set(edge))
          followStateSubsumedEdges(edge._2)
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

  def filterReachedConcreteValueEdges(
      convertValueFun: ConcreteConcreteLattice.L => AbstL,
      concreteValue: ConcreteConcreteLattice.L,
      abstractEdges: Set[Edge]): Set[Edge] = {
    /*
     * All edges containing a ReachedValue annotation whose abstract value actually subsumes the abstracted
     * concrete value, zipped together with the abstract value that was reached.
     */
    val edgesContainingReachedValue: Set[(Edge, AbstL)] = abstractEdges
      .flatMap[(Edge, AbstL), Set[(Edge, AbstL)]]((tuple: Edge) => {
        /*
         * The ReachedValue annotation (if any exists) containing an abstract value that subsumes the converted
         * concrete value.
         */
        val reachedValueFound: Option[EdgeAnnotation] = tuple._1.find({
          case info: ReachedValue[AbstL] =>
            /*
             * Immediately check whether the value on the abstract edge actually subsumes the converted concrete value.
             * If not, we won't take this edge anyway.
             */
            val isSchemeLattice = implicitly[IsSchemeLattice[AbstL]]
            val convertedValue = convertValueFun(concreteValue)
            isSchemeLattice.subsumes(info.v, convertedValue)
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
    val ordering =
      new SubsumesOrdering[AbstL](implicitly[IsSchemeLattice[AbstL]].subsumes)
    val minReachedValueEdges: Set[Edge] =
      findMinimallySubsuming(edgesContainingReachedValue, ordering)
    minReachedValueEdges
  }

  private def frameUsedSubsumes(getFrameFromInfo: EdgeAnnotation => Option[AbstractFrame])
                               (info: EdgeAnnotation,
                                convertedFrame: AbstractFrame)
  : Option[AbstractFrame] =
    getFrameFromInfo(info) match {
      case Some(abstractFrame) =>
        val subsumes = abstractFrame.subsumes(convertedFrame)
        Logger.log(s"frameUsedSubsumes: $abstractFrame subsumes $convertedFrame ? $subsumes", Logger.U)
        if (subsumes) {
          Some(abstractFrame)
        } else {
          None
        }
      case _ =>
        None
    }

  type ConcreteFrame =
    SchemeFrame[ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]
  type AbstractFrame = SchemeFrame[AbstL, HybridAddress.A, HybridTimestamp.T]

  def filterFrameEdges(convertedFrame: AbstractFrame,
                       subsumesFrame: (EdgeAnnotation,
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
              val someFound: Option[AbstractFrame] = edge._1.map(subsumesFrame(_, convertedFrame)).foldLeft[Option[AbstractFrame]](None)({
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
      Logger.log(s"minFrameFollowedEdges = $minFrameFollowedEdges", Logger.U)
      minFrameFollowedEdges
    }
  }

  def filterSingleEdgeInfo(convertValueFun: ConcreteConcreteLattice.L => AbstL,
                           convertFrameFun: ConcreteFrame => AbstractFrame,
                           abstractEdges: Set[Edge],
                           concreteEdgeInfo: EdgeAnnotation): Set[Edge] =
    concreteEdgeInfo match {
      case ReachedConcreteValue(concreteValue) =>
        filterReachedConcreteValueEdges(convertValueFun,
                                        concreteValue,
                                        abstractEdges)

      case info: FrameFollowed[ConcreteConcreteLattice.L] =>
        filterFrameEdges(convertFrameFun(info.frame), frameUsedSubsumes({
          case info: FrameFollowed[AbstL] =>
            Some(info.frame)
          case _ =>
            None}),
          abstractEdges)

      case info: FramePushed[ConcreteConcreteLattice.L] =>
        filterFrameEdges(convertFrameFun(info.frame), frameUsedSubsumes({
          case info: FramePushed[AbstL] =>
            Some(info.frame)
          case _ =>
            None}),
          abstractEdges)

      case _ =>
        abstractEdges.filter({
          case (abstractEdgeInfos, node) =>
            concreteEdgeInfo match {
              case ThenBranchTaken | ElseBranchTaken =>
                abstractEdgeInfos.contains(concreteEdgeInfo)
              case EvaluatingExpression(e) =>
                abstractEdgeInfos.contains(concreteEdgeInfo)
              case NextKontAddressNow(ka) =>
                val convertedKa = new ConvertTimestampKontAddrConverter(ConvertTimeStampConverter).convertKontAddr(ka)
                abstractEdgeInfos.contains(NextKontAddressNow(convertedKa))
            }
        })
    }

  def computeSuccNode(
      convertValueFun: ConcreteConcreteLattice.L => AbstL,
      convertFrameFun: ConcreteFrame => AbstractFrame,
      node: GraphNode,
      concreteEdgeInfos: List[EdgeAnnotation]): Set[GraphNode] = {
    assert(currentGraph.isDefined)
    val abstractEdges = currentGraph.get.nodeEdges(node)
    Logger.log(s"abstractEdgeInfos = ${abstractEdges.map(_._1)}", Logger.U)
    val filteredAbstractEdges =
      concreteEdgeInfos.foldLeft[Set[Edge]](abstractEdges)(
        (filteredAbstractEdges, concreteEdgeInfo) =>
          filterSingleEdgeInfo(convertValueFun,
                               convertFrameFun,
                               filteredAbstractEdges,
                               concreteEdgeInfo))
    addEdgesVisited(node, filteredAbstractEdges)
    Logger.log(s"filteredAbstractEdges = $filteredAbstractEdges", Logger.U)
    filteredAbstractEdges.map(_._2)
  }

  var graphSize: List[(Int, Int)] = Nil
  var concreteNodes: List[(Int, Int, List[Int])] = Nil

  private def addNodesVisited(nodes: Set[GraphNode]): Unit =
    nodes.foreach(nodesVisited += _)

  private def addEdgesVisited(node: GraphNode, edges: Set[Edge]): Unit =
    edges.foreach((tuple) => edgesVisited += ((node, tuple._1, tuple._2)))

  def computeSuccNodes(convertValueFun: ConcreteConcreteLattice.L => AbstL,
                       convertFrameFun: ConcreteFrame => AbstractFrame,
                       edgeInfos: List[EdgeAnnotation],
                       stepNumber: Int) = {
    Logger.log(s"in step $stepNumber \nbefore: currentNodes = ${currentNodes.zip(currentNodes.map(initialGraph.get.nodeId))} " +
               s"\nconcreteEdgeInfos = $edgeInfos", Logger.U)
    addNodesVisited(currentNodes)
    /* First follow all StateSubsumed edges before trying to use the concrete edge information */
    val nodesSubsumedEdgesFollowed =
      currentNodes.flatMap(followStateSubsumedEdges)
    addNodesVisited(nodesSubsumedEdgesFollowed)
    val succNodes = nodesSubsumedEdgesFollowed.flatMap(
      computeSuccNode(convertValueFun, convertFrameFun, _, edgeInfos))
    addNodesVisited(succNodes)
    Logger.log(
      s"succNodes = ${succNodes.zip(succNodes.map(initialGraph.get.nodeId))}",
      Logger.U)
    currentNodes = succNodes.flatMap(followStateSubsumedEdges)
    concreteNodes = concreteNodes :+ (stepNumber, currentNodes.size, currentNodes.toList
        .map(initialGraph.get.nodeId))
    Logger.log(s"in step $stepNumber, after: currentNodes = ${currentNodes.zip(
      currentNodes.map(initialGraph.get.nodeId))}", Logger.U)
  }

  private def saveTraversedGraph(): Unit = {
    initialGraph.get.toDotFile(
      "Analysis/Traversed graph/traversed_graph.dot",
      node => List(scala.xml.Text(node.toString.take(40))),
      (s) => if (nodesVisited.contains(s)) Colors.Red else Colors.White,
      node => List(scala.xml.Text(node.mkString(", ").take(300))),
      Some((edge) =>
        if (edgesVisited.contains(edge)) Colors.Red else Colors.Black))
  }

  def end(): Unit = {
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

    saveTraversedGraph()
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

  def breadthFirst(
      reachables: ReachablesIntermediateResult): ReachablesIntermediateResult =
    reachables.todoQueue match {
      case Nil =>
        reachables
      case node :: rest =>
        val newReachables = addReachableEdges(
          ReachablesIntermediateResult(reachables.graph,
                                       reachables.nodesDone,
                                       rest),
          node)
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
