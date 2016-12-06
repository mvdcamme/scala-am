import java.io.{FileWriter, BufferedWriter, File}

class PointsToAnalysis[
    Exp: Expression, L: JoinLattice, Addr: Address, Time: Timestamp] {

  private def joinStores[Machine <: KickstartEvalEvalKontMachine[Exp, L, Addr, Time]](machine: Machine)(
    stores: Set[Store[Addr, L]]): Set[(Addr, L)] = {
    val joinedStore = stores.foldLeft(Store.initial(Set()): Store[Addr, L]) {
      case (joinedStore, store) => joinedStore.join(store)
    }
    joinedStore.toSet
  }

  case class MetricsToWrite(max: Int,
                            median: Double,
                            average: Double,
                            sum: Int,
                            nrOfTops: Int)

  private def calculateMetrics(
      result: List[(Addr, Option[Int])]): MetricsToWrite = {
    val integerValues = result.map(_._2).filter(_.isDefined).map(_.get)
    val numberValues = integerValues.map(_.toDouble).sortWith(_ < _)
    val length = numberValues.length
    if (length == 0) {
      MetricsToWrite(0, 0, 0, 0, 0)
    } else {
      val max = integerValues.max
      val sum = integerValues.sum
      val median = if (length % 2 == 0) {
        (numberValues((length / 2) - 1) + numberValues(length / 2)) / 2
      } else {
        numberValues(length / 2)
      }
      val average = numberValues.sum / length
      val nrOfTops = result.map(_._2).count(_.isEmpty)
      MetricsToWrite(max, median, average, sum, nrOfTops)
    }
  }

  private def possiblyWriteMetrics(stepSwitched: Int,
                                   metrics: MetricsToWrite): Unit = {
    val output =
      s"$stepSwitched;${metrics.max};${metrics.median};${metrics.average};${metrics.sum};${metrics.nrOfTops}"
    if (GlobalFlags.ANALYSIS_RESULTS_OUTPUT.isDefined) {
      val file = new File(GlobalFlags.ANALYSIS_RESULTS_OUTPUT.get)
      val bw = new BufferedWriter(new FileWriter(file, true))
      bw.write(output ++ "\n")
      bw.close()
    }
  }

  private def analyzeOutput[Machine <: KickstartEvalEvalKontMachine[Exp, L, Addr, Time]](
                                                                                  machine: Machine,
                            pointsTo: L => Option[Int],
                            relevantAddress: Addr => Boolean)(
      output: Machine#MachineOutput): List[(Addr, Option[Int])] = {
    val storeValues = joinStores(machine)(output.finalStores)
    val initial: List[(Addr, Option[Int])] = Nil
    val result: List[(Addr, Option[Int])] = storeValues.foldLeft(initial)({
      case (result, (address, value)) =>
        val numberOfObjectsPointedTo = pointsTo(value)
        if (relevantAddress(address) && numberOfObjectsPointedTo
              .getOrElse(1) > 0) {
          /* List all addresses pointing to more than one value */
          (address, numberOfObjectsPointedTo) :: result
        } else {
          result
        }
    })
    Logger.log(
      s"Static points-to analysis completed, resulting set equals $result",
      Logger.U)
    val metrics = calculateMetrics(result)
    Logger.log(s"Static points-to analysis completed, metrics equals $metrics",
               Logger.D)
    possiblyWriteMetrics(output.stepSwitched.getOrElse(-1), metrics)
    result
  }

  def analyze[Machine <: KickstartEvalEvalKontMachine[Exp, L, Addr, Time]](
      toDot: Option[String],
      machine: Machine,
      sem: Semantics[Exp, L, Addr, Time],
      pointsTo: L => Option[Int],
      relevantAddress: Addr => Boolean)(
      startState: machine.MachineState,
      isInitial: Boolean,
      stepSwitched: Option[Int]): StaticAnalysisResult = {
    Logger.log(s"Starting static points-to analysis", Logger.I)
    val result =
      machine.kickstartEval(startState, sem, None, None, stepSwitched)
    toDot.map(result.toDotFile(_))
    analyzeOutput(machine, pointsTo, relevantAddress)(result)
    AnalysisGraph[machine.GraphNode](result.graph.get)
  }
}

class IncrementalAnalysisChecker[GraphNode](val aam: AAM[_, _, _, _]) {

  var initialGraph: Option[Graph[GraphNode, List[EdgeInformation]]] = None
  var currentNodes: Set[GraphNode] = Set()

  def hasInitialGraph: Boolean = initialGraph.isDefined
  def initializeGraph(graph: Graph[GraphNode, List[EdgeInformation]]) = {
    initialGraph = Some(graph)
    val someStartNode = graph.getNode(0)
    assert(someStartNode.isDefined)
    currentNodes = Set(someStartNode.get)
  }

  def containsNode(node: GraphNode): Boolean = {
    initialGraph.get.nodeId(node) != -1
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
    initialGraph.get.nodeEdges(node).flatMap( (edge) =>
      if (edge._1.contains(StateSubsumed)) {
        /* Make sure that an edge is ONLY annotated with StateSubsumed. It should not be possible
         * to have a StateSubsumed edge with any other annotation. */
        assert(edge._1.size == 1, s"StateSubsumed edge contains more than 1 edge: ${edge._1}")
        followStateSubsumedEdges(edge._2)
      } else {
        Set(node)
      } )

  def filterSingleEdgeInfo(abstractEdges: Set[(List[EdgeInformation], GraphNode)], edgeInfo: EdgeInformation): Set[(List[EdgeInformation], GraphNode)] =
    abstractEdges.filter({ case (abstractEdgeInfos, node) => edgeInfo match {
      case ThenBranchTaken | ElseBranchTaken =>
        abstractEdgeInfos.contains(edgeInfo)
      case EvaluatingExpression(e) =>
        abstractEdgeInfos.contains(edgeInfo)
      case OperatorTaken(_) | FrameFollowed(_) =>
        true
    } })

  def computeSuccNode(node: GraphNode, concreteEdgeInfos: List[EdgeInformation]): Set[GraphNode] = {
    Logger.log(s"Computing successor of node ${initialGraph.get.nodeId(node)}", Logger.U)
    assert(currentNodes.nonEmpty && initialGraph.isDefined)
    val abstractEdges = initialGraph.get.nodeEdges(node)
    val filteredAbstractEdges = concreteEdgeInfos.foldLeft[Set[(List[EdgeInformation], GraphNode)]](abstractEdges)(
      (filteredAbstractEdges,
      concreteEdgeInfo) => filterSingleEdgeInfo(filteredAbstractEdges, concreteEdgeInfo))
    filteredAbstractEdges.map(_._2)
  }

  def computeSuccNodes(edgeInfos: List[EdgeInformation]) = {
    /* First follow all StateSubsumed edges before trying to use the concrete edge information */
    val nodesSubsumedEdgesFollowed = currentNodes.flatMap(followStateSubsumedEdges)
    Logger.log(s"Skipping subsumed edges leads to ${nodesSubsumedEdgesFollowed.size} nodes with edge-info $edgeInfos",
      Logger.U)
    currentNodes = nodesSubsumedEdgesFollowed.flatMap(computeSuccNode(_, edgeInfos))
    Logger.log(s"Successor nodes ${currentNodes.map(initialGraph.get.nodeId)}", Logger.U)
  }
}

class PointsToAnalysisLauncher[
    Abs: IsConvertableLattice: PointsToableLatticeInfoProvider](
    concSem: ConvertableSemantics[SchemeExp,
                                  ConcreteConcreteLattice.L,
                                  HybridAddress.A,
                                  HybridTimestamp.T])
    extends AnalysisLauncher[Abs] {

  val aam: SpecAAM = new SpecAAM()

  val incrementalAnalysis = new IncrementalAnalysisChecker[aam.GraphNode](aam)

  val abs = implicitly[IsConvertableLattice[Abs]]
  val lip = implicitly[PointsToableLatticeInfoProvider[Abs]]

  val pointsToAnalysis =
    new PointsToAnalysis[SchemeExp, Abs, HybridAddress.A, HybridTimestamp.T]

  def runStaticAnalysisGeneric(currentProgramState: PS,
                               stepSwitched: Option[Int],
                               toDotFile: Option[String]): StaticAnalysisResult = {
    wrapRunAnalysis(
      () => {
        val startStates =
          convertStateAAM(aam, concSem, abstSem, currentProgramState)
        val result =
          pointsToAnalysis.analyze(toDotFile,
            aam,
            abstSem,
            lip.pointsTo,
            (addr) =>
              !HybridAddress.isAddress.isPrimitive(
                addr))(startStates, false, stepSwitched)
        Logger.log(s"Static points-to analysis result is $result", Logger.D)
        result
      })
  }

  def runStaticAnalysis(currentProgramState: PS,
                        stepSwitched: Option[Int]): StaticAnalysisResult = {
    assert(incrementalAnalysis.hasInitialGraph)
    val convertedState: aam.State = convertStateAAM(aam, concSem, abstSem, currentProgramState)
    Logger.log(s"Already contains current node: ${incrementalAnalysis.containsNode(convertedState)}", Logger.U)
    runStaticAnalysisGeneric(currentProgramState, stepSwitched, None)
  }

  def runInitialStaticAnalysis(currentProgramState: PS): Unit =
    runStaticAnalysisGeneric(currentProgramState, None, Some("initial_graph.dot")) match {
      case result: AnalysisGraph[aam.GraphNode] =>
        incrementalAnalysis.initializeGraph(result.graph)
      case other =>
        throw new Exception(
          s"Expected initial analysis to produce a graph, got $other instead")
    }

  def doConcreteStep(edgeInfos: List[EdgeInformation]) =
    incrementalAnalysis.computeSuccNodes(edgeInfos)

}
