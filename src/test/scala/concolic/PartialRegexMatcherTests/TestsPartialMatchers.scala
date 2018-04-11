import backend.path_filtering.PartialRegexMatcher
import dk.brics.automaton.{Automaton, State => AutomState}
import org.scalatest.PrivateMethodTester

trait TestsPartialMatchers extends PrivateMethodTester {

  private def resetRandom: scala.util.Random = {
    new scala.util.Random(System.nanoTime())
  }

  def generateRandomPaths(nrOfPaths: Int, maxPathSize: Int): List[String] = {
    val random = resetRandom
    1.to(nrOfPaths).map(_ => {
      val randomSize: Int = random.nextInt(maxPathSize) + 1
      1.to(randomSize).map(_ => if (random.nextInt(2) == 0) "t" else "e").mkString
    }).toList
  }

  def checkPartialMatchersEqual(pm1: PartialRegexMatcher, pm2: PartialRegexMatcher): Unit = {
    val randomPaths = generateRandomPaths(2000, 2000)
    randomPaths.foreach(path => {
      val (result1, _) = pm1.incrementalMatch(path)
      val (result2, _) = pm2.incrementalMatch(path)
      assert(result1 == result2)
    })
  }

  import scala.collection.convert.ImplicitConversionsToScala._
  def toGraph(partialMatcher: PartialRegexMatcher): Graph[AutomState, Char, Unit] = {

    implicit val graphNode = new GraphNode[AutomState, Unit] {}
    implicit val graphAnnotation = new GraphAnnotation[Char, Unit] {}

    val getAutomatonMethod = PrivateMethod[Automaton]('automaton)
    val automaton = partialMatcher.invokePrivate(getAutomatonMethod())
    val initialState = automaton.getInitialState
    @scala.annotation.tailrec
    def loop(todo: List[AutomState], visited: Set[AutomState], graph: Graph[AutomState, Char, Unit]): Graph[AutomState, Char, Unit] = todo.headOption match {
      case None => graph
      case Some(state) if visited.contains(state) => loop(todo.tail, visited, graph)
      case Some(state) =>
        val transitions = state.getTransitions
        val edges: Set[(AutomState, Char)] = transitions.map(transition => (transition.getDest, transition.getMin)).toSet
        val newGraph = graph.addEdges(edges.map(tuple => (state, tuple._2, tuple._1)))
        loop(todo.tail ++ edges.map(_._1), visited + state, newGraph)
    }
    loop(List(initialState), Set(), Graph.empty[AutomState, Char, Unit])
  }

  def checkPartialMatcherGraphsEqual(pm1: PartialRegexMatcher, pm2: PartialRegexMatcher): Unit = {
    /*
     * Can't just check whether the set of nodes and the edges of the graphs are equal, because the automaton's States
     * are Java classes and they hence rely on reference equality.
     * Instead, we loop over the graph and check whether the transitions for each node are similar.
     */
    @scala.annotation.tailrec
    def loop(todo1: List[AutomState], todo2: List[AutomState], visited1: Set[AutomState], visited2: Set[AutomState]): Unit = {
      (todo1.headOption, todo2.headOption) match {
        case (None, None) => /* Done, no mismatches found */
        case (None, Some(_)) => assert(false)
        case (Some(_), None) => assert(false)
        case (Some(state1), Some(state2)) if visited1.contains(state1) && visited2.contains(state2) => loop(todo1.tail, todo2.tail, visited1, visited2)
        case (Some(state1), Some(state2)) if visited1.contains(state1) || visited2.contains(state2) => assert(false)
        case (Some(state1), Some(state2)) =>
          val transitions1 = state1.getTransitions
          val transitions2 = state2.getTransitions
          assert(transitions1.size == transitions2.size)
          /* Check that every transition of the first graph has a corresponding transition in the second graph that uses the same character annotation. */
          assert(transitions1.forall(transition1 => transitions2.exists(transition2 => transition1.getMin == transition2.getMin)))
          /* Check whether, for all automaton transitions of the current state, there is only one transition containing that character annotation. */
          assert(transitions1.forall(transition => transitions1.count(otherTransition => transition.getMin == otherTransition.getMin) == 1))
          assert(transitions2.forall(transition => transitions2.count(otherTransition => transition.getMin == otherTransition.getMin) == 1))
          /* Make sure both collections of edges have the same ordering: sort them by their character-annotation */
          val edges1: List[(AutomState, Char)] = transitions1.map(transition => (transition.getDest, transition.getMin)).toList.sortBy(_._2)
          val edges2: List[(AutomState, Char)] = transitions2.map(transition => (transition.getDest, transition.getMin)).toList.sortBy(_._2)
          loop(todo1 ++ edges1.map(_._1), todo2 ++ edges2.map(_._1), visited1 + state1, visited2 + state2)
      }
    }
    loop(List(pm1.initialState), List(pm2.initialState), Set(), Set())
  }

}
