trait AbstractGraphTracker[AbstractStateToTrack] {
  def getInitialStateGraph: Option[Graph[AbstractStateToTrack, EdgeAnnotation[_, _, HybridAddress.A], _]]
  def getCurrentAbstractStateNode: Option[Set[AbstractStateToTrack]]
}
