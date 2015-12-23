class AbstractConcreteToAbstractType {
  
  def convertAddress(address : HybridAddress) = address match {
    case HybridAddress.Left(concreteAddress, classicalAddress) => HybridAddress.Right(classicalAddress)
    case HybridAddress.Right(classicalAddress) => HybridAddress.Right(classicalAddress)
  }
  
  def convert[Exp : Expression](value : AbstractConcrete, store : Store[HybridAddress, HybridLattice.Hybrid]) : AbstractType = value match {
    case AbstractConcrete.AbstractBool(v) => AbstractType.AbstractBool
    case AbstractConcrete.AbstractBottom => AbstractType.AbstractBottom
    case AbstractConcrete.AbstractChar(v) => AbstractType.AbstractChar
    case AbstractConcrete.AbstractCons(car: HybridAddress, cdr: HybridAddress) => AbstractType.AbstractCons[HybridAddress](convertAddress(car), convertAddress(cdr))
    case AbstractConcrete.AbstractInt(v) => AbstractType.AbstractInt
    case AbstractConcrete.AbstractNil => AbstractType.AbstractNil
    //case AbstractConcrete.AbstractPrimitive(p : Primitive) => AbstractType.AbstractPrimitive(p) TODO shouldn't need any conversion: handled by
    case AbstractConcrete.AbstractString(v) => AbstractType.AbstractString
    case AbstractConcrete.AbstractSymbol(v) => AbstractType.AbstractSymbol
    case v: AbstractConcrete.AbstractClosure[Exp, HybridAddress] => v match {
      case AbstractConcrete.AbstractClosure(λ, ρ) => AbstractType.AbstractClosures(Set[(Exp, Environment[HybridAddress])]((λ, ρ.map(HybridAddress.convertAddress))))
    }
    case v: AbstractConcrete.AbstractPrimitive[HybridAddress, HybridLattice.Hybrid] => v match {
      case AbstractConcrete.AbstractPrimitive(p: Primitive[HybridAddress, HybridLattice.Hybrid]) => AbstractType.AbstractPrimitive(p)
    }
  }
}