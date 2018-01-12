import dk.brics.automaton.{ Automaton, State }

class PartialRegexMatcher(val automaton: Automaton) {

  sealed trait MatchResult {
    def representsMatch: Boolean
  }
  case class PartialMatch(terminatingState: State) extends MatchResult {
    override def representsMatch: Boolean = true
  }
  case class CompleteMatch(terminatingState: State) extends MatchResult {
    override def representsMatch: Boolean = true
  }
  case object NoMatch extends MatchResult {
    override def representsMatch: Boolean = false
  }

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
    tryPartialMatch(initialState, string).representsMatch
  }

  private var lastState: Option[State] = None

  /**
    * Similar to [[partialMatch]], but starts checking the string from the final, cached state that was reached in the
    * last incremental match.
    * If the updateState-parameter is true, the final state reached during this match is cached so subsequent
    * incremental matches start from this state.
    *
    * If the string does not match, the final state is not cached.
    */
  private def incrementalMatchAndMaybeUpdateState(string: String, updateState: Boolean): MatchResult = {
    /* If lastState is None, incrementalMatch hasn't yet been called before, so we start from the initial state. */
    val startState = lastState.getOrElse(initialState)
    tryPartialMatch(startState, string) match {
      case PartialMatch(endState) =>
        if (updateState) { lastState = Some(endState) }
        PartialMatch(endState)
      case CompleteMatch(endState) =>
        if (updateState) { lastState = Some(endState) }
        CompleteMatch(endState)
      case NoMatch =>
        /* Don't reassign lastState */
        NoMatch
    }
  }

  /**
    * Similar to [[partialMatch]], but starts checking the string from the final, cached state that was reached in the
    * last incremental match.
    * If no final state was cached yet, e.g., because this is the first incremental match, starts from the automaton's
    * initial state.
    *
    * If the string does not match, the final state is not cached.
    */
  def incrementalMatch(string: String): Boolean = {
    incrementalMatchAndMaybeUpdateState(string, updateState = true).representsMatch
  }

  def tentativeIncrementalMatch(string: String): Boolean = {
    incrementalMatchAndMaybeUpdateState(string, updateState = false).representsMatch
  }

}
