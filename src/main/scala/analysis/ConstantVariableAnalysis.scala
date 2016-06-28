/* An analysis for finding variables whose value never changes throughout the lifetime of a program. */

case class ConstantAnalysis[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
  (aam: AAM[Exp, HybridLattice.L, Addr, ZeroCFA.T], initialEnv: Environment[Addr])
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
    val (previousAddresses, nonConstants) = current
    val writtenAddresses = frame.writeEffectsFor()
    (previousAddresses, nonConstants ++ writtenAddresses.asInstanceOf[Set[Addr]])
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
  def analyze[Exp: Expression, L : JoinLattice, Addr : Address]
    (aam: AAM[Exp, HybridLattice.L, Addr, ZeroCFA.T], sem: Semantics[Exp, HybridLattice.L, Addr, ZeroCFA.T])
    (startState: aam.State, initialEnv: Environment[Addr]): Option[(Set[Addr], Set[Addr])] = {
    val analysis = ConstantAnalysis[Exp, HybridLattice.L, Addr, ZeroCFA.T](aam, initialEnv)
    aam.kickstartAnalysis(analysis, startState, sem, None)
  }
}
