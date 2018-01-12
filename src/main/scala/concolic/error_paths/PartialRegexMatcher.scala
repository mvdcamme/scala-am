import dk.brics.automaton.{ Automaton, State }

case class PartialRegexMatcher(automaton: Automaton) {

  sealed trait MatchResult
  case class PartialMatch(terminatingState: State) extends MatchResult
  case class CompleteMatch(terminatingState: State) extends MatchResult
  case object NoMatch extends MatchResult

  private val initialState: State = automaton.getInitialState

  private def tryPartialMatch(startState: State, string: String): MatchResult = {
    @scala.annotation.tailrec
    def loop(currentState: State, currentString: String): MatchResult = currentString.headOption match {
      case None =>
        if (currentState.isAccept) {
          CompleteMatch(currentState)
        } else {
          PartialMatch(currentState)
        }
      case Some(head) =>
        val transitions = scala.collection.JavaConverters.asScalaSet(currentState.getTransitions)
        transitions.find(_.getMin == head) match {
          case None =>
            /* Transition required that does not exist in the automaton */
            NoMatch
          case Some(transition) =>
            loop(transition.getDest, currentString.tail)
        }
    }
    loop(startState, string)
  }


  def partialMatch(string: String): Boolean = {
    tryPartialMatch(initialState, string) match {
      case PartialMatch(_) | CompleteMatch(_) => true
      case NoMatch => false
    }
  }

  private var lastState: Option[State] = None

  /**
    * Similar to [[partialMatch]], but starts checking the string from the final, cached state that was reached in the last incremental match.
    * If no final state was cached yet, e.g., because this is the first incremental match, starts from the automaton's initial state.
    *
    * If the string does not match, the final state is not cached.
    */
  def incrementalMatch(string: String): Boolean = {
    /* If lastState is None, incrementalMatch hasn't yet been called before, so we start from the initial state. */
    val startState = lastState.getOrElse(initialState)
    tryPartialMatch(startState, string) match {
      case PartialMatch(endState) =>
        lastState = Some(endState)
        true
      case CompleteMatch(endState) =>
        lastState = Some(endState)
        true
      case NoMatch =>
        /* Don't reassign lastState */
        false
    }
  }

}
