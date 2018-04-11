import dk.brics.automaton._
import dk.brics.automaton.{State => AutomState}

import backend.path_filtering.PartialRegexMatcher

/*
 * Computes the regular expressions that describe paths between specific nodes in a general, directed graph.
 * Technique is based on the transitive closure method.
 */
class TransitiveClosure[N, A, C](graph: Graph[N, A, C], isErrorState: N => Boolean, annotToChar: A => Option[Char], stepCount: Option[Int], concolicRun: Int) {

  implicit val stateGraphNode: GraphNode[N, Unit] = new GraphNode[N, Unit] { }
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
      val initial = new AutomState
      automaton.setInitialState(initial)

      import scala.collection.JavaConverters._
      val epsilons = convert(Set((root, initial)), Map(root -> initial), Map(initial -> root), Set(), Set[StatePair]())
      /* Converting an immutable scala collection to a Java collection will result in UnsupportedOperationExceptions being thrown when an operation is performed upon the Java collection. */
      val mutableEpsilons = collection.mutable.Set(epsilons.toSeq:_*)
      automaton.addEpsilons(mutableEpsilons.asJava)

      Automaton.setMinimization(1) // brzozowski
      automaton.minimize()
      
      val partialMatcher = new PartialRegexMatcher(automaton)
      Some(partialMatcher)
    } else {
      None
    }
  }

  /*
   * the toNode-parameter isn't actually used here: it only serves for debugging purposes, i.e., to make sure there is
   * a 1-to-1 mapping between nodes in the graph and automaton states.
   */
  @scala.annotation.tailrec
  private def convert(todo: Set[(N, AutomState)], toState: Map[N, AutomState], toNode: Map[AutomState, N], visited: Set[N], epsilons: Set[StatePair]): Set[StatePair] = {
    todo.headOption match {
      case None => epsilons
      case Some((node, state)) if visited.contains(node) =>
        assert(toState.contains(node))
        assert(toNode.contains(state))
        convert(todo.tail, toState, toNode, visited, epsilons)
      case Some((node, state)) =>
        state.setAccept(isErrorState(node))
        val (newStates, newEpsilons, newToState, newToNode) = graph.nodeEdges(node).foldLeft((Set[(N, AutomState)](), epsilons, toState, toNode))((acc, edge) => edge match {
          case (annot, destNode) =>
            val (destState, updatedToState) = acc._3.get(destNode) match {
              case None =>
                val newState = new AutomState
                (newState, acc._3 + (destNode -> newState))
              case Some(existingState) => (existingState, acc._3)
            }
            val updatedToNode = acc._4.get(destState) match {
              case None => acc._4 + (destState -> destNode)
              case Some(aNode) =>
                assert(aNode == destNode)
                acc._4
            }
            destState.setAccept(isErrorState(destNode))
            val newStates = acc._1 + ((destNode, destState))
            annotToChar(annot) match {
              case None =>
                val statePair = new StatePair(state, destState)
                (newStates, acc._2 + statePair, updatedToState, updatedToNode)
              case Some(char) =>
                state.addTransition(new Transition(char, destState))
                (newStates, acc._2, updatedToState, updatedToNode)
            }
        })
        convert(todo.tail ++ newStates, newToState, newToNode, visited + node, newEpsilons)
    }
  }
}
