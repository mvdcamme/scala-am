import java.io.{FileWriter, BufferedWriter, File}

class CountNonConstants[Exp : Expression,
                        Abs : IsSchemeLattice,
                        Addr : Address,
                        Time: Timestamp,
                        State <: StateTrait[Exp, Abs, Addr, Time] : Descriptor]
                       (pointsTo: Abs => Option[Int])
                       (implicit stateInfoProvider: StateInfoProvider[Exp, Abs, Addr, Time, State],
                                 actionRApplier: ActionReplayApplier[Exp, Abs, Addr, Time, State])
  extends Metric[Exp, Abs, Addr, Time, State] {

  val abs = implicitly[JoinLattice[Abs]]
  val addr = implicitly[Address[Addr]]

  type ExpStack = List[Exp]

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

  private def getNonPrimitiveAddresses(stores: Set[Store[Addr, Abs]]): Set[(Addr, Abs)] = {
    joinStores(stores).filter( (tuple) => ! addr.isPrimitive(tuple._1) )
  }

  trait MetricsToWrite {
    def toCSVRow: String
    def addTop: MetricsToWrite
    def addNonTop(size: Int): MetricsToWrite
  }

  object MetricsToWrite {
    def init: MetricsToWriteImpl = MetricsToWriteImpl(0, 0, 0, 0, 0)

    /*
     * nrNonConstants: total number of addresses that don't point to exactly 1 value
     * sum: total nr of values each address points to: if x -> {1} and {1, 2}, sum for x alone would be 3
     * nrOfTops: total number of top values the addresses point to
     */
    case class MetricsToWriteImpl(nrOfAddresses: Int,
                                  nrOfConstants: Int,
                                  nrNonConstants: Int,
                                  sum: Int,
                                  nrOfTops: Int) extends MetricsToWrite {
      def toCSVRow: String = {
        s"$nrOfAddresses;$nrOfConstants;$nrNonConstants;$sum;$nrOfTops"
      }

      def addTop: MetricsToWriteImpl =
        MetricsToWriteImpl(nrOfAddresses + 1, nrOfConstants, nrNonConstants + 1, sum, nrOfTops + 1)

      def addNonTop(size: Int): MetricsToWriteImpl =
        if (size > 1) {
          MetricsToWriteImpl(nrOfAddresses + 1, nrOfConstants, nrNonConstants + 1, sum + size, nrOfTops)
        } else {
          MetricsToWriteImpl(nrOfAddresses + 1, nrOfConstants + 1, nrNonConstants, sum + size, nrOfTops)
        }
    }
  }

  private def calculateMetrics(values: Set[(Addr, Abs)]): MetricsToWrite = {
    if (values.isEmpty) {
      MetricsToWrite.init
    } else {
      val metrics = values.foldLeft(MetricsToWrite.init)({
        case (metrics, (a, value)) => pointsTo(value) match {
          case Some(x) =>
            if (x <= 0) Logger.log(s"Value $value of address $a points to $x separate values", Logger.U)
            metrics.addNonTop(x)
          case None =>
            metrics.addTop
        }
      })
      metrics
    }
  }

  private def writeMetrics(stepSwitched: Int, metrics: MetricsToWrite, directoryPath: String, inputProgramName: String): Unit = {
    val outputFileName = inputProgramName.replace('/', '_')
    val path = s"$directoryPath$outputFileName.txt"
    val output = s"$stepSwitched;" + metrics.toCSVRow
    Logger.log(output, Logger.U)
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file, true))
    bw.write(output ++ "\n")
    bw.close()
  }

  def computeAndWriteMetrics(graph: AbstractGraph, //For addresses
                             stepCount: Int,
                             path: String,
                             inputProgramName: String,
                             expStack: ExpStack,
                             expSet: Set[Exp]): Unit = {
    val valuesSet: Set[(Addr, Abs)] = getNonPrimitiveAddresses(finalStores(graph))
    val metrics = calculateMetrics(valuesSet)
    writeMetrics(stepCount, metrics, path, inputProgramName)
  }

}
