import java.io.{FileWriter, BufferedWriter, File}

case class CountedClosureCalls(nrOfCalls: Int, totalNrOfClosuresPointedTo: Int)

class CountClosureCalls[Exp : Expression,
                        AbstL : IsSchemeLattice,
                        Addr : Address,
                        State <: StateTrait[Exp, AbstL, Addr, _]] {

  val usesGraph = new UsesGraph[Exp, AbstL, Addr, State]
  import usesGraph._

  def countClosureCalls(graph: AbstractGraph): CountedClosureCalls = {
    graph.edges.foldLeft[CountedClosureCalls](CountedClosureCalls(0, 0))( (acc, tuple) => {
      val edges = tuple._2
      /* Count number of ActionClosureCall in the actionEdges going outwards from the state. */
      val nrOfClosureCalls = edges.foldLeft(0)( (nrOfClosureCalls, edge) => {
        val actionEdge = edge._1._2
        nrOfClosureCalls + (if (actionEdge.contains(ActionClosureCall())) 1 else 0)
      })
      if (nrOfClosureCalls > 0) {
        CountedClosureCalls(acc.nrOfCalls + 1, acc.totalNrOfClosuresPointedTo + nrOfClosureCalls)
      } else {
        acc
      }
    })
  }

  def computeAndWriteMetrics(graph: AbstractGraph, stepCount: Int, path: String): Unit = {
    val results = countClosureCalls(graph)
    val averageNumberOfClosuresPerCall = (results.totalNrOfClosuresPointedTo : Double) / (results.nrOfCalls : Double)
    val bw = new BufferedWriter(new FileWriter(new File(path), true))
    bw.write(s"$stepCount; $averageNumberOfClosuresPerCall\n")
    bw.close()
  }

}
