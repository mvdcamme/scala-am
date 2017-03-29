object TracesMetrics {

  private var numberOfTraces = 0
  private var tracesLength = 0
  private var numberOfVarLookups = 0
  private var numberOfGenericPrimitivesApplied = 0
  private var numberOfGuardsApplied = 0

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

  def getNumberOfVarLookups: Int =
    numberOfVarLookups

  def incNumberOfVarLookups(): Unit = {
    numberOfVarLookups += 1
  }

  def getNumberOfGenericPrimitivesApplied: Int =
    numberOfGenericPrimitivesApplied

  def incNumberOfGenericPrimitivesApplied(): Unit = {
    numberOfGenericPrimitivesApplied += 1
  }

  def getNumberOfGuardsApplied: Int =
    numberOfGuardsApplied

  def incNumberOfGuardsApplied(): Unit = {
    numberOfGuardsApplied += 1
  }

}
