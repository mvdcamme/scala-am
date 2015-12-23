import scalaz.Scalaz._

class Store[Addr : Address, Abs : AbstractValue](content: Map[Addr, (Int, Abs)], counting: Boolean) {
  val abs = implicitly[AbstractValue[Abs]]
  val addr = implicitly[Address[Addr]]

  override def toString = content.filterKeys(a => !addr.isPrimitive(a)).toString

  def getContent : Map[Addr, (Int, Abs)] = content

//  def convertAddress(address : Addr) : ClassicalAddress = {
//    return ClassicalAddress.PrimitiveAddress("ERROR: CONVERSION NOT IMPLEMENTED")
//  }

//  def convertStore[NewAbs : AbstractValue](converter : AbstractValueConversion[Abs, Addr, NewAbs, ClassicalAddress]) : Store[ClassicalAddress, NewAbs] = {
//    var newStore = new Store[ClassicalAddress, NewAbs](Map[ClassicalAddress, (Int, NewAbs)](), counting)
//    def addToNewStore(tuple: (Addr, Abs)): Boolean = {
//      val newAddress = convertAddress(tuple._1)
//      val newValue = converter.convert(tuple._2, this)
//      newStore = newStore.extend(newAddress, newValue)
//      return true
//    }
//    forall(addToNewStore)
//    return newStore
//  }

  def convertStore[Abs : AbstractValue] : Store[ClassicalAddress, Abs] = {
    return new Store[ClassicalAddress, Abs](Map[ClassicalAddress, (Int, Abs)](), counting)
  }

  def keys: collection.Iterable[Addr] = content.keys
  /** Checks if a predicate is true for all elements of the store */
  def forall(p: ((Addr, Abs)) => Boolean) = content.forall({
    case (a, (_, v)) => p(a, v)
  })
  def lookup(a: Addr): Abs = content.get(a) match {
    case None => throw new Exception(s"Unbound address (should not happen): $a")
    case Some(v) => v._2
  }
  /** Looks up a value in the store (returning bottom if value not present) */
  def lookupBot(a: Addr): Abs = content.getOrElse(a, (0, abs.bottom))._2
  /** Adds a new element to the store */
  def extend(a: Addr, v: Abs): Store[Addr, Abs] = content.get(a) match {
    case None => new Store(content + (a -> (0, v)), counting)
    case Some((n, v2)) => new Store(content + (a -> (if (counting) { n+1 } else { n }, abs.join(v2, v))), counting)
  }
  /** Updates an element in the store. Might perform a strong update if this store supports strong updates */
  def update(a: Addr, v: Abs): Store[Addr, Abs] =
    if (counting) {
      content.get(a) match {
        case None => throw new RuntimeException("Updating store at an adress not used")
        case Some((0, _)) => new Store(content + (a -> (0, v)), counting)
        case _ => extend(a, v)
      }
    } else {
      extend(a, v)
    }
  /** Joins two stores */
  def join(that: Store[Addr, Abs]): Store[Addr, Abs] = new Store(this.getContent |+| that.getContent, counting)
  /** Checks whether this store subsumes another store */
  def subsumes(that: Store[Addr, Abs]): Boolean =
    that.forall((binding: (Addr, Abs)) => abs.subsumes(lookupBot(binding._1), binding._2))
  /** Returns a store containing items that are not equal with the other store */
  def diff(that: Store[Addr, Abs]): Store[Addr, Abs] = {
    new Store(content.filter({ case (a, (n, v)) => that.getContent.get(a) match {
      case Some((n2, v2)) => n != n2 && v != v2
      case None => true
    }}), counting)
  }
}

//class HybridStore(content: Map[HybridAddress, (Int, AbstractConcrete)], counting: Boolean) extends Store[HybridAddress, AbstractConcrete](content, counting) {
//
//  override def convertAddress(address : HybridAddress) : HybridAddress = address match {
//    case HybridAddress.PrimitiveAddress(name) => HybridAddress.PrimitiveAddress(name)
//    case HybridAddress.Left(address1, address2) => HybridAddress.Right(address2)
//  }
//}

object Store {
  /* TODO: have abstract counting as a parameter of the analysis. Also, when it is
   * turned on, it prevents AAC and Free from converging. For now, it's only
   * enabled with the AbstractConcrete lattice. */
  def empty[Addr : Address, Abs : AbstractValue] =
    new Store(Map[Addr, (Int, Abs)](), implicitly[AbstractValue[Abs]].name == "Concrete")
  def initial[Addr : Address, Abs : AbstractValue](values: List[(Addr, Abs)]): Store[Addr, Abs] =
    new Store(values.map({ case (a, v) => (a, (0, v)) }).toMap, implicitly[AbstractValue[Abs]].name == "Concrete")
}

//object HybridStore {
//  def empty =
//    new HybridStore(Map[HybridAddress, (Int, AbstractConcrete)](), implicitly[AbstractValue[AbstractConcrete]].name == "Hybrid")
//  def initial(values: List[(HybridAddress, AbstractConcrete)]): Store[HybridAddress, AbstractConcrete] =
//    new HybridStore(values.map({ case (a, v) => (a, (0, v)) }).toMap, implicitly[AbstractValue[AbstractConcrete]].name == "Hybrid")
//}
