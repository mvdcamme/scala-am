/* An analysis for finding variables whose value never changes throughout the lifetime of a program. */

case class ConstantAnalysis[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
  (aam: AAM[Exp, HybridLattice.L, Addr, Time], initialEnv: Environment[Addr])
  extends BaseAnalysis[(Set[Addr], Set[Addr]), Exp, Abs, Addr, Time] {

  private def envToAddressSet(env: Environment[Addr]): Set[Addr] = {
    var envAddresses: Set[Addr] = Set()
    env.forall({ case (_, a) => envAddresses = envAddresses + a; true})
    envAddresses
  }

  def stepEval(e: Exp, env: Environment[Addr], store: Store[Addr, Abs], t: Time, current: (Set[Addr], Set[Addr])) = {
    val (previousAddresses, nonConstants) = current
    val newAddresses = envToAddressSet(env)
    /* The addresses that were present in the previous, but not in the new environment. */
    val addressesRemoved = previousAddresses -- newAddresses
    (newAddresses, nonConstants ++ addressesRemoved)
  }
  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time, current: (Set[Addr], Set[Addr])) = {
    val someNewEnv = frame.savesEnv
    val (previousAddresses, nonConstants) = current
    val newAddresses = someNewEnv match {
      case Some(env) => envToAddressSet(env.asInstanceOf[Environment[Addr]])
      case None => previousAddresses
    }
    val addressesRemoved = previousAddresses -- newAddresses
    val writtenAddresses = frame.writeEffectsFor()
    (newAddresses, nonConstants ++ writtenAddresses.asInstanceOf[Set[Addr]] ++ addressesRemoved)
  }
  def error(error: SemanticError, current: (Set[Addr], Set[Addr])) = current
  def join(x: (Set[Addr], Set[Addr]), y: (Set[Addr], Set[Addr])) = {
    val (previousAddressesX, nonConstantsX) = x
    val (previousAddressesY, nonConstantsY) = y
    /* The addresses that were present in the previous, but not in the new environment. */
    val addressesRemoved = (previousAddressesX -- previousAddressesY) ++ (previousAddressesY -- previousAddressesX)
    val intersection = previousAddressesX.intersect(previousAddressesY)
    (intersection, nonConstantsX ++ nonConstantsY ++ addressesRemoved)
  }
  def init = (envToAddressSet(initialEnv), Set())
}

object ConstantVariableAnalysis {

  private def joinStores[Exp: Expression, L : JoinLattice, Addr : Address, Time : Timestamp]
    (aam: AAM[Exp, L, Addr, Time])(stores: Set[Store[Addr, L]]): Set[(Addr, L)] = {
    val joinedStore = stores.foldLeft(Store.initial(Set()): Store[Addr, L])
      { case (joinedStore, store) => joinedStore.join(store) }
    joinedStore.toSet
  }

  private def analyzeOutput[Exp: Expression, L : JoinLattice, Addr : Address, Time : Timestamp]
    (aam: AAM[Exp, L, Addr, Time], isConstantValue: L => Boolean)(output: aam.AAMOutput): Set[Addr] = {
    val storeValues = joinStores[Exp, L, Addr, Time](aam)(output.finalStores)
    storeValues.flatMap({ case (address, value) =>
        if (! isConstantValue(value)) Set[Addr](address) else Set[Addr]()
    })
  }

  def analyze[Exp: Expression, L : JoinLattice, Addr : Address, Time : Timestamp]
    (aam: AAM[Exp, HybridLattice.L, Addr, Time], sem: Semantics[Exp, HybridLattice.L, Addr, Time], isConstantValue: HybridLattice.L => Boolean)
    (startState: aam.State, initialEnv: Environment[Addr]): Set[Addr] = {
    val analysis = ConstantAnalysis[Exp, HybridLattice.L, Addr, Time](aam, initialEnv)
    val output = aam.kickstartEval(startState, sem, None, false)
    analyzeOutput(aam, isConstantValue)(output)
  }
}
