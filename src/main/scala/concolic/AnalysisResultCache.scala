class AnalysisResultCache[KeyState, ValueState] {
  private var cache: Map[KeyState, AnalysisResult[ValueState]] = Map()
  def addAnalysisResult(abstractedState: KeyState, result: AnalysisResult[ValueState]): Unit = {
    cache += abstractedState -> result
  }
  def getAnalysisResult(abstractedState: KeyState): Option[AnalysisResult[ValueState]] = cache.get(abstractedState)

}
