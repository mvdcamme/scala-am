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

  def traverse(graph: AbstractGraph): Map[Exp, Set[Abs]] = {

    var map: Map[Exp, Set[Abs]] = Map()

    @scala.annotation.tailrec
    def loop(todo: Set[(State, List[Exp])], visited: Set[State]): Map[Exp, Set[Abs]] = todo.headOption match {
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
              map = map + (exp -> (map.getOrElse(exp, Set()) + stateInfoProvider.valueReached(state).get))
              rest
            case Nil =>
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
        throw new Exception("Should not happen")
      case Some(state) =>
        if (stateInfoProvider.evalExp(state).isDefined) {
          val exp = stateInfoProvider.evalExp(state).get
          loop(Set((state, List(exp))), Set())
        } else {
          loop(Set((state, Nil)), Set())
        }
    }
  }

//  def end(graph: Graph[State, EdgeAnnotation[Exp, Abs, Addr]]): Unit = {
//    val map = traverse(graph)
//    for ( (exp, values) <- map ) {
//      Logger.log(s"${values.size} elements for exp $exp at position ${implicitly[Expression[Exp]].pos(exp)}", Logger.U)
//      Logger.log(s"Values: $values", Logger.U)
//    }
//  }

  /*
   * nrNonConstants: total number of expressions that don't point to exactly 1 value
   * sum: total nr of values each exp points to: if x -> {1} and {1, 2}, sum for x alone would be 3
   * nrOfTops: total number of top values the exps point to
   */
  case class MetricsToWrite(nrNonConstants: Int, sum: Int, nrOfTops: Int)

  private def calculateMetrics(storeTuples: Set[(Exp, Boolean, Int, Boolean)]): MetricsToWrite = {
    if (storeTuples.isEmpty) {
      MetricsToWrite(0, 0, 0)
    } else {
      val (nrNonConstants, sum, nrOfTops) = storeTuples.foldLeft((0, 0, 0))({
        case ((nrNonConstants, sum, nrOfTops), (_, isNonConstant, y, isTop)) =>
          (if (isNonConstant) nrNonConstants + 1 else nrNonConstants, sum + y, if (isTop) nrOfTops + 1 else nrOfTops)
      })
      MetricsToWrite(nrNonConstants, sum, nrOfTops)
    }
  }

  private def writeMetrics(stepSwitched: Int, metrics: MetricsToWrite, directoryPath: String, inputProgramName: String): Unit = {
    val outputFileName = inputProgramName.replace('/', '_')
    val path = s"$directoryPath$outputFileName.txt"
    val output = s"$stepSwitched;${metrics.nrNonConstants};${metrics.sum};${metrics.nrOfTops}"
    Logger.log(output, Logger.U)
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file, true))
    bw.write(output ++ "\n")
    bw.close()
  }

  def computeAndWriteMetrics(graph: AbstractGraph, stepCount: Int, path: String, inputProgramName: String): Unit = {
    val values: Set[(Exp, Set[Abs])] = traverse(graph).toSet
    val finalValuesPointedTo: Set[(Exp, Boolean, Int, Boolean)] = values.map({
      case (exp, values) =>
        val (isNonConstant, sum, isTop) = values.foldLeft[(Boolean, Int, Boolean)]((false, 0, false))({
          case ((nrNonConstants, sum, isTop), value) =>
            pointsTo(value) match {
              case Some(x) =>
                (if (x > 1) true else false, sum + x, isTop)
              case None =>
                (true, sum, true)
            }
        })
        (exp, isNonConstant, sum, isTop)
    })
    val metrics = calculateMetrics(finalValuesPointedTo)
    writeMetrics(stepCount, metrics, path, inputProgramName)
  }



}
