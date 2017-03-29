object TracesMetrics {

  private var numberOfTraces = 0
  private var tracesLength = 0

  def incNumberOfTraces(): Unit = {
    numberOfTraces += 1
  }

  def addTraceLength(traceLength: Int): Unit = {
    tracesLength += traceLength
  }

  def getNumberOfTraces: Int =
    numberOfTraces

  def getTracesLength: Int =
    tracesLength

}
