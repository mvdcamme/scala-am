class StaticAnalysisResultCache[State] {
  private var cache: Map[State, StaticAnalysisResult] = Map()
  def get(state: State): Option[StaticAnalysisResult] = cache.get(state)
  def add(state: State, analysisResult: StaticAnalysisResult): Unit = {
    cache += state -> analysisResult
  }
}
