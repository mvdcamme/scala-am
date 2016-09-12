class PointsToAnalysis[Exp: Expression, L : JoinLattice, Addr : Address, Time : Timestamp] {

  private def joinStores(aam: AAM[Exp, L, Addr, Time])
                        (stores: Set[Store[Addr, L]]): Set[(Addr, L)] = {
    val joinedStore = stores.foldLeft(Store.initial(Set()): Store[Addr, L])
    { case (joinedStore, store) => joinedStore.join(store) }
    joinedStore.toSet
  }

  private def analyzeOutput(aam: AAM[Exp, L, Addr, Time], pointsTo: L => Int)
                           (output: aam.AAMOutput): Set[(Addr, Int)] = {
    val storeValues = joinStores(aam)(output.finalStores)
    val initial = Set[(Addr, Int)]()
    val result: Set[(Addr, Int)] = storeValues.foldLeft(initial)({ case (result, (address, value)) =>
      val numberOfObjectsPointedTo: Int = pointsTo(value)
      if (numberOfObjectsPointedTo > 0) {
        result + ((address, numberOfObjectsPointedTo))
      } else {
        result
      } })
    Logger.log(s"Static points-to analysis completed, resulting set equals $result", Logger.D)
    result
  }

  def analyze(aam: AAM[Exp, L, Addr, Time], sem: Semantics[Exp, L, Addr, Time], pointsTo: L => Int)
             (startState: aam.State, isInitial: Boolean): Set[(Addr, Int)] = {
    Logger.log(s"Starting static points-to analysis", Logger.I)
    val output = aam.kickstartEval(startState, sem, None, None, false)
    analyzeOutput(aam, pointsTo)(output)
  }
}

class PointsToAnalysisLauncher[Exp : Expression]
  (sem: SemanticsTraced[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T]) extends AnalysisLauncher[Exp](sem) {

  val pointsToAnalysis = new PointsToAnalysis[Exp, HybridLattice.L, HybridAddress.A, HybridTimestamp.T]

  def runStaticAnalysis(currentProgramState: PS): Set[(HybridAddress.A, Int)] = {
    val aam: SpecAAM = new SpecAAM()
    val startState = convertState(aam, currentProgramState)
    val result = pointsToAnalysis.analyze(aam, sem.absSem, HybridLattice.pointsTo)(startState, false)
    Logger.log(s"Static points-to analysis result is $result", Logger.U)
    result
  }

}