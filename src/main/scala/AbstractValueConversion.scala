import lattice.scheme._

class AbstractConcreteToAbstractType {
  
  def convertAddress(address: Address[HybridAddress.A]) = address match {
    case HybridAddress.Left(concreteAddress, classicalAddress) => HybridAddress.Right(classicalAddress)
    case HybridAddress.Right(classicalAddress) => HybridAddress.Right(classicalAddress)
  }
  
  def convert[Exp : Expression](value: ConcreteLattice, store: Store[HybridAddress.A, HybridLattice.L]): AbstractType = value match {
    /*
     * Does not convert AbstractTid
     */
    case AbstractConcrete.AbstractBool(v) => AbstractType.AbstractBool
    case AbstractConcrete.AbstractBottom => AbstractType.AbstractBottom
    case AbstractConcrete.AbstractChar(v) => AbstractType.AbstractChar
    case AbstractConcrete.AbstractCons(car: HybridAddress.A, cdr: HybridAddress.A) => AbstractType.AbstractCons[HybridAddress.A](convertAddress(car), convertAddress(cdr))
    case AbstractConcrete.AbstractFloat(v) => AbstractType.AbstractFloat
    case AbstractConcrete.AbstractInt(v) => AbstractType.AbstractInt
    case AbstractConcrete.AbstractNil => AbstractType.AbstractNil
    case AbstractConcrete.AbstractString(v) => AbstractType.AbstractString
    case AbstractConcrete.AbstractSymbol(v) => AbstractType.AbstractSymbol
    case v: AbstractConcrete.AbstractClosure[Exp, HybridAddress.A] => v match {
      case AbstractConcrete.AbstractClosure(λ, ρ) => AbstractType.AbstractClosures(Set[(Exp, Environment[HybridAddress.A])]((λ, ρ.map(HybridAddress.convertAddress))))
    }
    case v: AbstractConcrete.AbstractPrimitive[HybridAddress.A, HybridLattice.Hybrid] => v match {
      case AbstractConcrete.AbstractPrimitive(p: Primitive[HybridAddress.A, HybridLattice.Hybrid]) => AbstractType.AbstractPrimitive(p)
    }
  }
}