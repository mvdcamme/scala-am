class AnalysisResultCache[State] {
  private var cache: Map[State, AnalysisResult] = Map()
  def addAnalysisResult(abstractedState: State, result: AnalysisResult): Unit = {
    cache += abstractedState -> result
  }
  def getAnalysisResult(abstractedState: State): Option[AnalysisResult] = cache.get(abstractedState)

}
