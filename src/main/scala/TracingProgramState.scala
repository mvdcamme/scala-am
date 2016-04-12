/**
  * Created by mvdcamme on 12/04/16.
  */
trait TracingProgramState[Exp] {
  type HybridValue = HybridLattice.Hybrid
  def applyAction(action: Action[Exp, HybridValue, HybridAddress]): TracingProgramState[Exp]
}

case class ProgramState[Exp : Expression, Time : Timestamp]
  (control: TracingControl[Exp, HybridLattice.Hybrid, HybridAddress],
   ρ : Environment[HybridAddress],
   σ: Store[HybridAddress, HybridLattice.Hybrid],
   kstore: KontStore[KontAddr],
   a: KontAddr,
   t: Time,
   v : HybridLattice.Hybrid,
   vStack : List[Storable]) extends TracingProgramState[Exp]{

  def applyAction(action: Action[Exp, HybridValue, HybridAddress]): ProgramState[Exp, Time] = this

}