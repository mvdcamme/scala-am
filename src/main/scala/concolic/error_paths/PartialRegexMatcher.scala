import dk.brics.automaton.{ Automaton, State }

case class PartialRegexMatcher(automaton: Automaton) {

  private val initialState: State = automaton.getInitialState

  def partialMatch(string: String): Boolean = {
    @scala.annotation.tailrec
    def loop(currentState: State, currentString: String): Boolean = currentString.headOption match {
      case None =>
        /* Full or partial match: we don't check whether currentState is an accepting state because we also accept partial matches */
        true
      case Some(head) =>
        val transitions = scala.collection.JavaConverters.asScalaSet(currentState.getTransitions)
        transitions.find(_.getMin == head) match {
          case None =>
            /* Transition required that does not exist in the automaton */
            false
          case Some(transition) =>
            loop(transition.getDest, currentString.tail)
        }
    }
    loop(initialState, string)
  }

}
