import java.io.{BufferedWriter, File, FileWriter}

case class CountedFunCalls(nrOfCalls: Int, totalNrOfFunctionsPointedTo: Int)

class CountFunCalls[Exp : Expression,
                    Abs : IsSchemeLattice,
                    Addr : Address,
                    Time: Timestamp,
                    State <: StateTrait[Exp, Abs, Addr, Time]] extends Metric[Exp, Abs, Addr, Time, State] {

  val sabs = implicitly[IsSchemeLattice[Abs]]
  val usesGraph = new UsesGraph[Exp, Abs, Addr, State]
  import usesGraph._

  type FunctionsMap = Map[(Exp, Time), Set[Abs]]

  private def constructFunctionCallsMap(graph: AbstractGraph): FunctionsMap = {
    val initialMap: FunctionsMap = Map()
    graph.edges.foldLeft[FunctionsMap](initialMap)( (map, tuple) => {
      val edges = tuple._2
      /* Count number of ActionFunctionCallMarkR in the actionEdges going outwards from the state. */
      edges.foldLeft(map)( (map, edge) => {
        val machineFilters = edge._1.filters.machineFilters
        machineFilters.foldLeft(map)( (map, actionR) => actionR match {
          case filter: FunCallMark[Exp, Abs, Time] =>
            val flattenedClosures: Set[Abs] = sabs.getClosures(filter.fValue).map(triple => sabs.inject[Exp, Addr]((triple._1, triple._2), None))
            val flattenedPrimitives: Set[Abs] = sabs.getPrimitives(filter.fValue).map(sabs.inject[Addr, Abs])
            val allFunctions = flattenedClosures ++ flattenedPrimitives
            val key = (filter.fExp, filter.t)
            map + (key -> (map.getOrElse(key, Set()) ++ allFunctions))
          case _ =>
            map
        })
      })
    })
  }

  private def countFunctionCalls(map: FunctionsMap): CountedFunCalls = {
    map.foreach( (tuple) => {
      Logger.I(s"Call site at ${tuple._1} with values ${tuple._2}")
    })
    CountedFunCalls(map.size, map.values.foldLeft(0)( (sum, set) => sum + set.size) )
  }

  def computeAndWriteMetrics(graph: AbstractGraph, stepCount: Int, path: String, program: String): Unit = {
    val map = constructFunctionCallsMap(graph)
    val results = countFunctionCalls(map)
    val totalNrOfFunctionsCalled = FunctionsCalledMetric.getConcreteFunctionsCalled + results.nrOfCalls
    val totalNrOfFunctionsPointedTo = FunctionsCalledMetric.getConcreteFunctionsCalled + results.totalNrOfFunctionsPointedTo
    val averageNumberOfFunctionsPerCall = (totalNrOfFunctionsPointedTo : Double) / (totalNrOfFunctionsCalled : Double)
    val bw = new BufferedWriter(new FileWriter(new File(path), true))
    bw.write(s"$stepCount; $averageNumberOfFunctionsPerCall\n")
    bw.close()
  }

}
