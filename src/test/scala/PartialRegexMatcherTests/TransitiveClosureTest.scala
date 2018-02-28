import org.scalatest.{FunSuite, PrivateMethodTester}
import ConcreteConcreteLattice.{L => ConcreteValue}
import dk.brics.automaton.{State, StatePair, Transition}

class TransitiveClosureTest extends FunSuite with PrivateMethodTester with TestCommon {

  val (machine: ConcolicMachine[pointsToLattice.L], sem: ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T]) = makeConcolicMachineAndSemantics(ConcolicRunTimeFlags())

  type N = machine.analysisLauncher.aam.State
  type A = EdgeAnnotation[SchemeExp, pointsToLattice.L, HybridAddress.A]
  type G = Graph[N, A, Set[N]]

  val (launchAnalyses: LaunchAnalyses[pointsToLattice.L],
  initialState: machine.State,
  analysisResult: AnalysisOutputGraph[SchemeExp, pointsToLattice.L, HybridAddress.A, N]) = Util.runOnFile(connect4Program, (program) => {
    val launchAnalyses = new LaunchAnalyses[pointsToLattice.L](machine.analysisLauncher, machine.reporter)
    val initialState: machine.State = machine.inject(sem.parse(program), Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial[HybridAddress.A, ConcreteValue](sem.initialStore))
    val analysisResult = machine.analysisLauncher.runInitialStaticAnalysis(initialState, connect4Program)
    (launchAnalyses, initialState, analysisResult)
  })

  test("Test whether the convert method returns sets of epsilon transitions where the sets have the same size") {
    val tc1 = new TransitiveClosure[N, A, Set[N]](
      analysisResult.graph, _.isUserErrorState, annotToOptChar, None, 1)
    val tc2 = new TransitiveClosure[N, A, Set[N]](
      analysisResult.graph, _.isUserErrorState, annotToOptChar, None, 1)
    val root = analysisResult.graph.getNode(0).get
    val convertMethod = PrivateMethod[java.util.HashSet[StatePair]]('convert)
    val epsilons1 = new java.util.HashSet[StatePair]()
    val epsilons2 = new java.util.HashSet[StatePair]()
    tc1.invokePrivate(convertMethod(Set((root, new dk.brics.automaton.State())), Set(), epsilons1))
    tc2.invokePrivate(convertMethod(Set((root, new dk.brics.automaton.State())), Set(), epsilons2))
    assert(epsilons1.size == epsilons2.size)
  }

  private def annotToOptChar(annot: A): Option[Char] = {
    val (elseBranch, thenBranch) = (annot.filters.semanticsFilters.contains(ElseBranchFilter), annot.filters.semanticsFilters.contains(ThenBranchFilter))
    /* Edge cannot indicate that both the else- and the then-branch have been taken. */
    assert(! (elseBranch && thenBranch), "Should not happen")
    if (elseBranch) {
      Some('e')
    } else if (thenBranch) {
      Some('t')
    } else {
      None
    }
  }

  type Path = List[Char]

  private class Converter(graph: G, annotToChar: A => Option[Char], isErrorState: N => Boolean) {

    @scala.annotation.tailrec
    final def convert(todo: Set[(N, State)], toState: Map[N, State], toNode: Map[State, N], visited: Set[N], epsilons: Set[StatePair]): (Map[N, State], Map[State, N], Set[StatePair]) = {
      todo.headOption match {
        case None => (toState, toNode, epsilons)
        case Some((node, state)) if visited.contains(node) =>
          assert(toState.contains(node))
          assert(toNode.contains(state))
          convert(todo.tail, toState, toNode, visited, epsilons)
        case Some((node, state)) =>
          state.setAccept(isErrorState(node))
          val (newStates, newEpsilons, newToState, newToNode) = graph.nodeEdges(node).foldLeft((Set[(N, State)](), epsilons, toState, toNode))((acc, edge) => edge match {
            case (annot, destNode) =>
              val (destState, updatedToState) = acc._3.get(destNode) match {
                case None =>
                  val newState = new State
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

  private def automatonEpsilonsToGraphEpsilons(epsilons: Set[StatePair], toNode: Map[State, N]): Set[(N, N)] = {
    epsilons.map(statePair => {
      val maybeNode1 = toNode.get(statePair.getFirstState)
      val maybeNode2 = toNode.get(statePair.getSecondState)
      assert(maybeNode1.isDefined)
      assert(maybeNode2.isDefined)
      (maybeNode1.get, maybeNode2.get)
    })
  }

  test("More thorough check whether the convert method produces same results") {
    val root = analysisResult.graph.getNode(0).get
    val converter = new Converter(analysisResult.graph, annotToOptChar, _.isUserErrorState)
    val state1 = new dk.brics.automaton.State()
    val (toState1, toNode1, epsilons1) = converter.convert(Set((root, state1)), Map(root -> state1), Map(state1 -> root), Set(), Set[StatePair]())
    val state2 = new dk.brics.automaton.State()
    val (toState2, toNode2, epsilons2) = converter.convert(Set((root, state2)), Map(root -> state2), Map(state2 -> root), Set(), Set[StatePair]())
    assert(toState1.size == toState2.size)
    assert(toNode1.size == toNode2.size)
    assert(epsilons1.size == epsilons2.size)
    val graphEpsilons1 = automatonEpsilonsToGraphEpsilons(epsilons1, toNode1)
    val graphEpsilons2 = automatonEpsilonsToGraphEpsilons(epsilons2, toNode2)
    graphEpsilons1.foreach(pair1 => {
      if (! graphEpsilons2.contains(pair1)) {
        println(pair1)
        assert(false)
      }
    })
    graphEpsilons2.foreach(pair2 => {
      if (! graphEpsilons1.contains(pair2)) {
        println(pair2)
        assert(false)
      }
    })
    assert(graphEpsilons1 == graphEpsilons2)
  }
}
