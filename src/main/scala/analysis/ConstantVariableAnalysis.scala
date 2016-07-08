/* An analysis for finding variables whose value never changes throughout the lifetime of a program. */

class ConstantVariableAnalysis[Exp: Expression, L : JoinLattice, Addr : Address, Time : Timestamp] {

  var initialAnalysisResults: Option[Set[Addr]] = None

  private def joinStores(aam: AAM[Exp, L, Addr, Time])(stores: Set[Store[Addr, L]]): Set[(Addr, L)] = {
    val joinedStore = stores.foldLeft(Store.initial(Set()): Store[Addr, L])
      { case (joinedStore, store) => joinedStore.join(store) }
    joinedStore.toSet
  }

  private def analyzeOutput(aam: AAM[Exp, L, Addr, Time], isConstantValue: L => Boolean)(output: aam.AAMOutput): Set[Addr] = {
    val storeValues = joinStores(aam)(output.finalStores)
    storeValues.flatMap({ case (address, value) =>
        if (! isConstantValue(value)) Set[Addr](address) else Set[Addr]()
    })
  }

  def analyze(aam: AAM[Exp, L, Addr, Time], sem: Semantics[Exp, L, Addr, Time], isConstantValue: L => Boolean)
    (startState: aam.State, initialEnv: Environment[Addr]): Set[Addr] = {
    val output = aam.kickstartEval(startState, sem, None, false)
    analyzeOutput(aam, isConstantValue)(output)
  }

  /**
    * Performs an initial global analysis of the entire program and saves the results for later use.
    */
  def initialAnalyze(aam: AAM[Exp, L, Addr, Time], sem: Semantics[Exp, L, Addr, Time], isConstantValue: L => Boolean)
  (startState: aam.State, initialEnv: Environment[Addr]): Set[Addr] = {
    val results = analyze(aam, sem, isConstantValue)(startState, initialEnv)
    initialAnalysisResults = Some(results)
    results
  }
}
