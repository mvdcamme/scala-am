import java.io.{FileWriter, BufferedWriter, File}

case class CountedFunCalls(nrOfCalls: Int, totalNrOfClosuresPointedTo: Int)

class CountFunCalls[Exp : Expression,
                        Abs : IsSchemeLattice,
                        Addr : Address,
                        State <: StateTrait[Exp, Abs, Addr, _]] {

  val usesGraph = new UsesGraph[Exp, Abs, Addr, State]
  import usesGraph._

  private def constructClosureCallsMap(graph: AbstractGraph): Map[Exp, Set[Abs]] = {
    val initialMap: Map[Exp, Set[Abs]] = Map()
    graph.edges.foldLeft[Map[Exp, Set[Abs]]](initialMap)( (map, tuple) => {
      val edges = tuple._2
      /* Count number of ActionClosureCall in the actionEdges going outwards from the state. */
      edges.foldLeft(map)( (map, edge) => {
        val actionEdge = edge._1.actions
        actionEdge.foldLeft(map)( (map, actionR) => actionR match {
          case actionR: ActionFunCallMarkR[Exp, Abs, Addr] =>
            map + (actionR.fExp -> (map.getOrElse(actionR.fExp, Set()) + actionR.fValue))
          case _ =>
            map
        })
      })
    })
  }

  private def countClosureCalls(map: Map[Exp, Set[Abs]]): CountedFunCalls = {
    map.foreach( (tuple) => {
      Logger.log(s"Call site at ${tuple._1} with values ${tuple._2}", Logger.E)
    })
    CountedFunCalls(map.size, map.values.foldLeft(0)((sum, set) => sum + set.size))
  }

  def computeAndWriteMetrics(graph: AbstractGraph, stepCount: Int, path: String): Unit = {
    val map = constructClosureCallsMap(graph)
    val results = countClosureCalls(map)
    val totalNrOfClosuresCalled = ClosuresCalledMetric.getConcreteClosuresCalled() + results.nrOfCalls
    val totalNrOfClosuresPointedTo = ClosuresCalledMetric.getConcreteClosuresCalled() + results.totalNrOfClosuresPointedTo
    val averageNumberOfClosuresPerCall = (totalNrOfClosuresPointedTo : Double) / (totalNrOfClosuresCalled : Double)
    val bw = new BufferedWriter(new FileWriter(new File(path), true))
    bw.write(s"$stepCount; $averageNumberOfClosuresPerCall\n")
    bw.close()
  }

}
