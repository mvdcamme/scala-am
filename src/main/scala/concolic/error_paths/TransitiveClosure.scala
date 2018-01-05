import dk.brics.automaton._

/*
 * Computes the regular expressions that describe paths between specific nodes in a general, directed graph.
 * Technique is based on the transitive closure method.
 */
class TransitiveClosure[N, A, C, T](graph: Graph[N, A, C], isErrorState: N => Boolean, annotToChar: A => Option[Char], charToT: Char => T) {

  implicit val stateGraphNode: GraphNode[State, Unit] = new GraphNode[State, Unit] { }
  implicit val stateGraphAnnotation: GraphAnnotation[T, Unit] = new GraphAnnotation[T, Unit] { }

  /**
    * Returns the number of visited states
    */
  val numberOfStates: Int = graph.nodes.size

  def shortestPaths: Option[Set[Regex]] = {
      val annots = graph.getAnnotations
      val root = graph.getNode(0).get

//      val distinctFinalStates = graph.nodes.filter(isErrorState)

      var shortestpaths = List[(String, Set[String])]()

    var idx = 0
    if (annots.nonEmpty)  {
      val automaton = new Automaton()
      val initial = new dk.brics.automaton.State()
      automaton.setInitialState(initial)

      var startx = System.nanoTime()
      val epsilons = new java.util.HashSet[StatePair]()
      convert(Set((root, initial)), Set(), epsilons)

      println("Took " + ((System.nanoTime - startx) / Math.pow(10, 9)) + "s TIME")
      startx = System.nanoTime()
      automaton.addEpsilons(epsilons)
      val diff = (System.nanoTime - startx) / Math.pow(10, 9)
      println(s"Took ${diff}s TIME")

      Automaton.setMinimization(1) // brzozowski
      automaton.minimize()
      val shortestp = BasicOperations.getShortestExample(automaton, true)

      println("shortest = " + BasicOperations.getShortestExample(automaton, true))

      var grap = Graph.empty[State, T, Unit]
      automaton.getStates.forEach((st: State) => {
        grap = grap.addNode(st)
        var lt = Set[Transition]()
        st.getTransitions.forEach(x => lt = lt + new Transition(x.getMin, x.getDest))
        assert(lt.size == st.getTransitions.size)
        lt.foreach({ s => grap = grap.addEdge(st, charToT(s.getMin), s.getDest) })
      })

      val states = grap.nodes.toArray
      val initialState = automaton.getInitialState

      var finals = Set[State]()
      automaton.getAcceptStates.forEach((x: State) => finals = finals + x)

//      TODO Jonas used this; just return the automaton instead?
      val regex = new NFARegex2[T](grap, initialState, states, finals.toList)
      val regexes = regex.compute2()
      println(s"regexes are ${regexes.mkString(";;;")}")
      Some(regexes)
    } else {
      None
    }

//      TODO
//      shortestpaths

  }

  /**
    * Loops over the graph reachable from the nodes in the todo-set, converts the corresponding edges to Transitions
    * and adds these to the States.
    * Also returns the set of all StatePairs that denote epsilon-transitions, i.e., the set of all pairs where one
    * state can epsilon-transition to the other state.
    */
  @scala.annotation.tailrec
  final def convert(todo: Set[(N, State)], visited: Set[(N, State)], epsilons: java.util.HashSet[StatePair]): java.util.HashSet[StatePair] = todo.headOption match {
    // S = state
    case Some((s, ast)) if visited.exists(_._1 == s) =>
      convert(todo.tail, visited, epsilons)
    case Some((s, ast)) =>
      var newStates = Set[(N, State)]()

      // Set[(Annotation, State)] ,node == state
      graph.nodeEdges(s).foreach({
        case (annot, node) =>
          // new state for node
          val newState = visited.find(_._1 == node).map(_._2).getOrElse(new State())
          if (isErrorState(node)) {
            newState.setAccept(true)
          }

          annotToChar(annot) match {
            case None =>
              // ast = bricbk state
              epsilons.add(new StatePair(ast, newState))
            case Some(char) =>
              ast.addTransition(new Transition(char, newState))
          }
          // node = our state
          newStates = newStates + ((node, newState))
      })
//      val newT: (N, State) =
      convert(todo.tail ++ newStates, visited + ((s, ast): (N, State)), epsilons)
    case None => epsilons
  }



  type R = Array[Array[Array[Set[error_paths.RegExp]]]]
  def getR(r: R, i: Int, j: Int, k: Int): Set[error_paths.RegExp] = {
    r(i)(j)(k)
  }
  def setR(r: R, i: Int, j: Int, k: Int, newValue: Set[error_paths.RegExp]): Unit = {
    r(i)(j).update(k, newValue)
  }

  def computeRegularExpressions(g: Graph[N, A, C]): Unit = ()

}
