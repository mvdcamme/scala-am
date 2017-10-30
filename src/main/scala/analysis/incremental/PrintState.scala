object PrintState {

  def stateToString[State]
                   (state: State, graph: Graph[State, _, _]): String = {
    s"($state, ${graph.nodeId(state)}"
  }

}
