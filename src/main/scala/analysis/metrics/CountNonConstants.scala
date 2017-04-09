import java.io.{FileWriter, BufferedWriter, File}

class CountNonConstants[Exp : Expression,
                        Abs : IsSchemeLattice,
                        Addr : Address,
                        Time: Timestamp,
                        State <: StateTrait[Exp, Abs, Addr, Time]]
                      (pointsTo: Abs => Option[Int])
                      (implicit stateInfoProvider: StateInfoProvider[Exp, Abs, Addr, Time, State])
  extends Metric[Exp, Abs, Addr, Time, State] {

  val sabs = implicitly[IsSchemeLattice[Abs]]
  val usesGraph = new UsesGraph[Exp, Abs, Addr, State]
  import usesGraph._

  private def finalStates(graph: AbstractGraph): Set[State] = {
    graph.nodes.filter(stateInfoProvider.halted)
  }

  private def finalStores(graph: AbstractGraph): Set[Store[Addr, Abs]] = {
    finalStates(graph).map(stateInfoProvider.store)
  }

  private def joinStores(stores: Set[Store[Addr, Abs]]): Set[(Addr, Abs)] = {
    val joinedStore = stores.foldLeft(Store.initial(Set()): Store[Addr, Abs])({
      case (joinedStore, store) => joinedStore.join(store)
    })
    joinedStore.toSet
  }

  case class MetricsToWrite(nrNonConstants: Int,
                            sum: Int,
                            nrOfTops: Int)

  private def calculateMetrics(storeTuples: Set[(Addr, Option[Int])]): MetricsToWrite = {
    val integerValues = storeTuples.map(_._2).filter(_.isDefined).map(_.get)
    if (storeTuples.isEmpty) {
      MetricsToWrite(0, 0, 0)
    } else {
      /* Sum of the number of tops and the number of abstract values pointing to more than one concrete value. */
      val nrNonConstants = storeTuples.count({
        case (_, pointsTo) =>
          pointsTo.map( (n) => { if (n > 1) true else false } ).getOrElse(true)
      })
      /* NUmber of values pointing to more than  */
      val sum = integerValues.sum
      val nrOfTops = storeTuples.map(_._2).count(_.isEmpty)
      MetricsToWrite(nrNonConstants, sum, nrOfTops)
    }
  }

  private def writeMetrics(stepSwitched: Int, metrics: MetricsToWrite, path: String, inputProgramName: String): Unit = {
    val output = s"$stepSwitched;${metrics.nrNonConstants};${metrics.sum};${metrics.nrOfTops}"
    val outputFileName = inputProgramName.replace('/', '_')
    val file = new File(s"$path$outputFileName.txt")
    val bw = new BufferedWriter(new FileWriter(file, true))
    bw.write(output ++ "\n")
    bw.close()
  }

  def computeAndWriteMetrics(graph: AbstractGraph, stepCount: Int, path: String, inputProgramName: String): Unit = {
    val finalStoreValues = joinStores(finalStores(graph))
    val finalValuesPointedTo = finalStoreValues.map({
      case (addr, value) =>
        (addr, pointsTo(value))
    })
    val metrics = calculateMetrics(finalValuesPointedTo)
    writeMetrics(stepCount, metrics, path, inputProgramName)
  }



}
