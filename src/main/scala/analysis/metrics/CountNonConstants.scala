import java.io.{FileWriter, BufferedWriter, File}

class CountNonConstants[Exp : Expression,
                        Abs : IsSchemeLattice,
                        Addr : Address,
                        Time: Timestamp,
                        State <: StateTrait[Exp, Abs, Addr, Time]]
                      (pointsTo: Abs => Option[Int])
                      (implicit stateInfoProvider: StateInfoProvider[Exp, Abs, Addr, Time, State])
  extends Metric[Exp, Abs, Addr, Time, State] {

  val abs = implicitly[JoinLattice[Abs]]

  type ExpStack = List[Exp]

  val sabs = implicitly[IsSchemeLattice[Abs]]
  val usesGraph = new UsesGraph[Exp, Abs, Addr, State]
  import usesGraph._

  def traverse(graph: AbstractGraph, expStack: ExpStack): Map[Exp, Abs] = {

    var map: Map[Exp, Abs] = Map()

    @scala.annotation.tailrec
    def loop(todo: Set[(State, ExpStack)], visited: Set[State]): Map[Exp, Abs] = todo.headOption match {
      case None =>
        map
      case Some((state, stack)) if visited.contains(state) =>
        loop(todo.tail, visited)
      case Some((state, stack)) =>
        val edges = graph.edges.getOrElse(state, Set())
        val newStack = if (stateInfoProvider.evalExp(state).isDefined) {
          stateInfoProvider.evalExp(state).get :: stack
        } else if (stateInfoProvider.valueReached(state).isDefined) {
          stack match {
            case exp :: rest =>
              map = map + (exp -> abs.join(map.getOrElse(exp, abs.bottom), stateInfoProvider.valueReached(state).get))
              rest
            case Nil =>
              assert(false) // Stack should not be empty
              Nil
          }
        } else {
          stack
        }
        val newStates = edges.map( (edge) => (edge._2, newStack) )
        loop(todo.tail ++ newStates, visited + state)
    }

    graph.getNode(0) match {
      case None =>
        Logger.log(s"ids of graph are ${graph.ids}", Logger.U)
        throw new Exception("Should not happen")
      case Some(state) =>
        loop(Set((state, expStack)), Set())
    }
  }

  /*
   * nrNonConstants: total number of expressions that don't point to exactly 1 value
   * sum: total nr of values each exp points to: if x -> {1} and {1, 2}, sum for x alone would be 3
   * nrOfTops: total number of top values the exps point to
   */
  case class MetricsToWrite(nrNonConstants: Int,
                            sum: Int,
                            nrOfTops: Int,
                            averageValuesPerExp: Double,
                            averageTopsPerExp: Double,
                            averageBothPerExp: Double) {
    def toCSVRow: String = {
      s"$nrNonConstants;$sum;$nrOfTops;$averageValuesPerExp;$averageTopsPerExp;$averageBothPerExp"
    }
  }

  private def calculateMetrics(expTuples: Set[(Exp, Boolean, Int, Boolean)]): MetricsToWrite = {
    if (expTuples.isEmpty) {
      MetricsToWrite(0, 0, 0, 0.0, 0.0, 0.0)
    } else {
      val nrOfExps = expTuples.size
      val (nrNonConstants, sum, nrOfTops) = expTuples.foldLeft((0, 0, 0))({
        case ((nrNonConstants, sum, nrOfTops), (_, isNonConstant, y, isTop)) =>
          (if (isNonConstant) nrNonConstants + 1 else nrNonConstants, sum + y, if (isTop) nrOfTops + 1 else nrOfTops)
      })
      MetricsToWrite(nrNonConstants,
                     sum,
                     nrOfTops,
                     (sum: Double) / nrOfExps,
                     (nrOfTops: Double) / nrOfExps,
                     (sum + nrOfTops : Double) / nrOfExps)
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

  def computeAndWriteMetrics(graph: AbstractGraph,
                             stepCount: Int,
                             path: String,
                             inputProgramName: String,
                             expStack: ExpStack,
                             expSet: Set[Exp]): Unit = {
    val valuesMap: Map[Exp, Abs] = traverse(graph, expStack)
    val valuesSet: Set[(Exp, Abs)] = valuesMap.toSet
    val valuesPointedTo: Set[(Exp, Boolean, Int, Boolean)] = valuesSet.map({
      case (exp, value) =>
        val (isNonConstant, sum, isTop): (Boolean, Int, Boolean) = pointsTo(value) match {
          case Some(x) =>
            if (x <= 0) Logger.log(s"Value $value points to $x separate values", Logger.U)
            (if (x > 1) true else false, x, false)
          case None =>
            (true, 1, true)
        }
        (exp, isNonConstant, sum, isTop)
    })
    val expsDisappearedSet: Set[(Exp, Boolean, Int, Boolean)] = expSet.flatMap( (exp: Exp) => {
      if (valuesMap.contains(exp)) {
        Set[(Exp, Boolean, Int, Boolean)]()
      } else {
        Set[(Exp, Boolean, Int, Boolean)]((exp, false, 1, false))
      }
    })
    val finalValuesPointedTo = valuesPointedTo ++ expsDisappearedSet
    Logger.log(s"Final values are $finalValuesPointedTo", Logger.U)
    val metrics = calculateMetrics(finalValuesPointedTo)
    writeMetrics(stepCount, metrics, path, inputProgramName)
  }

}
