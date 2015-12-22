trait AbstractValueConversion[Abs1, Abs2] {
  def convert[Addr : Address](value : Abs1, store : Store[Addr, Abs1]) : Abs2
}

class AbstractConcreteToAbstractType extends AbstractValueConversion[AbstractConcrete, AbstractType] {
  def convert(value : AbstractConcrete, store : HybridStore) : AbstractType = value match {
    case AbstractConcrete.AbstractBool(v) => AbstractType.AbstractBool
    case AbstractConcrete.AbstractBottom => AbstractType.AbstractBottom
    case AbstractConcrete.AbstractChar(v) => AbstractType.AbstractChar
    //case AbstractConcrete.AbstractClosure(λ, ρ) => AbstractType.AbstractClosures(Set((λ, ρ))) TODO type fails, can't infer λ : Exp and ρ : Environment[Addr] ???
    case AbstractConcrete.AbstractCons(car : HybridAddress, cdr : HybridAddress) => AbstractType.AbstractCons[ClassicalAddress](store.convertAddress(car), store.convertAddress(cdr))
    case AbstractConcrete.AbstractInt(v) => AbstractType.AbstractInt
    case AbstractConcrete.AbstractNil => AbstractType.AbstractNil
    // case AbstractConcrete.AbstractPrimitive(p : Primitive[Addr, Abs]) => AbstractType.AbstractPrimitive[Addr, AbstractType](p) TODO convert p
    case AbstractConcrete.AbstractString(v) => AbstractType.AbstractString
    case AbstractConcrete.AbstractSymbol(v) => AbstractType.AbstractSymbol
  }
}