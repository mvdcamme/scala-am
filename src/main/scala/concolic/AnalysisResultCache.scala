import backend.PathConstraint

class AnalysisResultCache {
  private var cache: Map[PathConstraint, AnalysisResult] = Map()
  def addAnalysisResult(path: PathConstraint, result: AnalysisResult): Unit = {
    cache += path -> result
  }
  def getAnalysisResult(path: PathConstraint): Option[AnalysisResult] = cache.get(path)

}
