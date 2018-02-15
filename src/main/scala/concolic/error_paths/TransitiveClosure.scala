import dk.brics.automaton._

import backend.path_filtering.PartialRegexMatcher

/*
 * Computes the regular expressions that describe paths between specific nodes in a general, directed graph.
 * Technique is based on the transitive closure method.
 */
class TransitiveClosure[N, A, C](graph: Graph[N, A, C], isErrorState: N => Boolean, annotToChar: A => Option[Char]) {

  implicit val stateGraphNode: GraphNode[State, Unit] = new GraphNode[State, Unit] { }
  implicit val stateGraphAnnotation: GraphAnnotation[String, Unit] = new GraphAnnotation[String, Unit] { }

  private case class AutomatonToDot(automaton: Automaton) {
    def toDotFile(path: String): Unit = {
      val dotString = automaton.toDot
      val fileWriter = new java.io.FileWriter(path, false)
      fileWriter.write(dotString)
      fileWriter.close()
    }
  }
  import scala.language.implicitConversions
  implicit private def automatonToDot(automaton: Automaton): AutomatonToDot = AutomatonToDot(automaton)



  /**
    * Returns the number of visited states
    */
  val numberOfStates: Int = graph.nodes.size

  def computePartialMatcher: Option[PartialRegexMatcher] = {
    val annots = graph.getAnnotations
    val root = graph.getNode(0).get

    if (annots.nonEmpty)  {
      val automaton = new Automaton()
      val initial = new dk.brics.automaton.State()
      automaton.setInitialState(initial)

      val epsilons = new java.util.HashSet[StatePair]()
      convert(Set((root, initial)), Set(), epsilons)

      automaton.addEpsilons(epsilons)

      Automaton.setMinimization(1) // brzozowski
      automaton.minimize()
      automaton.toDotFile("minimized_DFA.dot")
      
      val partialMatcher = new PartialRegexMatcher(automaton)
      Some(partialMatcher)
    } else {
      None
    }
  }

  /**
    * Loops over the graph reachable from the nodes in the todo-set, converts the corresponding edges to Transitions
    * and adds these to the States.
    * Also returns the set of all StatePairs that denote epsilon-transitions, i.e., the set of all pairs where one
    * state can epsilon-transition to the other state.
    */
  @scala.annotation.tailrec
  private def convert(todo: Set[(N, State)], visited: Set[(N, State)], epsilons: java.util.HashSet[StatePair]): java.util.HashSet[StatePair] = todo.headOption match {
    // S = state
    case Some((state, _)) if visited.exists(_._1 == state) =>
      convert(todo.tail, visited, epsilons)
    case Some((state, ast)) =>
      var newStates = Set[(N, State)]()

      // Set[(Annotation, State)] ,node == state
      graph.nodeEdges(state).foreach({
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
      convert(todo.tail ++ newStates, visited + ((state, ast): (N, State)), epsilons)
    case None => epsilons
  }
}
