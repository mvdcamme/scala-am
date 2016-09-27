class PointsToAnalysis[Exp: Expression, L : JoinLattice, Addr : Address, Time : Timestamp] {

  private def joinStores(free: Free[Exp, L, Addr, Time])
                        (stores: Set[Store[Addr, L]]): Set[(Addr, L)] = {
    val joinedStore = stores.foldLeft(Store.initial(Set()): Store[Addr, L])
    { case (joinedStore, store) => joinedStore.join(store) }
    joinedStore.toSet
  }

  private def analyzeOutput(free: Free[Exp, L, Addr, Time], pointsTo: L => Int)
                           (output: free.FreeOutput): Set[(Addr, Int)] = {
    val storeValues = joinStores(free)(output.finalStores)
    val initial = Set[(Addr, Int)]()
    val result: Set[(Addr, Int)] = storeValues.foldLeft(initial)({ case (result, (address, value)) =>
      val numberOfObjectsPointedTo: Int = pointsTo(value)
      if (numberOfObjectsPointedTo > 1) {
        /* List all addresses pointing to more than one value */
        result + ((address, numberOfObjectsPointedTo))
      } else {
        result
      } })
    Logger.log(s"Static points-to analysis completed, resulting set equals $result", Logger.D)
    result
  }

  def analyze(free: Free[Exp, L, Addr, Time], sem: Semantics[Exp, L, Addr, Time], pointsTo: L => Int)
             (startState: free.States, isInitial: Boolean): Set[(Addr, Int)] = {
    Logger.log(s"Starting static points-to analysis", Logger.I)
    val output = free.kickstartEval(startState, sem, None, None)
    analyzeOutput(free, pointsTo)(output)
  }
}

class PointsToAnalysisLauncher[Exp : Expression]
  (sem: SemanticsTraced[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T]) extends AnalysisLauncher[Exp](sem) {

  val pointsToAnalysis = new PointsToAnalysis[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T]

  def runStaticAnalysis(currentProgramState: PS): StaticAnalysisResult =
    wrapRunAnalysis(() => {
      val free: SpecFree = new SpecFree()
      val startStates = convertState(free, currentProgramState)
      val result = pointsToAnalysis.analyze(free, sem.absSem, HybridLattice.pointsTo)(startStates, false)
      Logger.log(s"Static points-to analysis result is $result", Logger.U)
      PointsToSet(result)
    })

}