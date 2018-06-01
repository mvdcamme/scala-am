import ConcreteConcreteLattice.{L => ConcreteValue}

abstract class Environment[Addr: Address] {

  /** Gets all the keys of the environment */
  def keys: Iterable[String]

  /** Checks if a predicate is true for all elements of the environment */
  def forall(p: ((String, Addr)) => Boolean): Boolean

  /** Looks up a value in the environment */
  def lookup(name: String): Option[Addr]

  def lookupMF(name: Identifier): MayFail[Addr] = lookup(name.name) match {
    case Some(v) => v
    case None => UnboundVariable(name)
  }
  /** Extend the environment */
  def extend(name: String, a: Addr): Environment[Addr]

  /** Extend the environment with multiple values */
  def extend(values: Iterable[(String, Addr)]): Environment[Addr]

  /** Checks whether this environment subsumes another */
  def subsumes(that: Environment[Addr]) =
    that.forall((binding: (String, Addr)) => lookup(binding._1).contains(binding._2))

  /** Maps over all addresses in the environment */
  def map(f: Addr => Addr): Environment[Addr]

  /** Checks whether this environment equals the other, modulo exact values for timestamps */
  def equalModuloTimestamp[T: CompareTimestampsWithMapping](other: Environment[Addr], shouldCheck: ShouldCheckAddress[Addr], store: Store[Addr, ConcreteValue],
                                                            otherStore: Store[Addr, ConcreteValue], mapping: Mapping[T]): Option[Mapping[T]] = {
    if (keys != other.keys) {
      None
    } else {
      keys.foldLeft[Option[Mapping[T]]](Some(mapping))({
        case (accMapping, variable) =>
          for {
            address <- lookup(variable) /* Should definitely exist because we're looping over its key. */
            otherAddress <- other.lookup(variable)
            addressMapping <- accMapping.flatMap(mapping => implicitly[Address[Addr]].equalModuloTimestamp[T](address, otherAddress, mapping))
            finalMapping <- if (shouldCheck.hasCheckedAddress(address)) {
              Some(addressMapping)
            } else {
              shouldCheck.addCheckedAddress(address)
              for {
                value <- store.lookup(address)
                otherValue <- otherStore.lookup(otherAddress)
                valueMapping <- implicitly[IsSchemeLattice[ConcreteValue]].equalModuloTimestamp(value, otherValue, shouldCheck, store, otherStore, addressMapping)
              } yield valueMapping
            }
          } yield finalMapping
      })
    }
  }
}

/** Basic mapping from names to addresses */
case class BasicEnvironment[Addr : Address](content: Map[String, Addr]) extends Environment[Addr] {
  override def toString = content.filter({ case (_, a) => !Address[Addr].isPrimitive(a) }).toString
  def keys = content.keys
  def forall(p: ((String, Addr)) => Boolean) = content.forall(p)
  def lookup(name: String) = content.get(name)
  def extend(name: String, a: Addr) =
    this.copy(content = content + (name -> a))
  def extend(values: Iterable[(String, Addr)]) =
    this.copy(content = content ++ values)
  def map(f: Addr => Addr): Environment[Addr] = {
    val newMap: Map[String, Addr] = content.mapValues(f)
    new BasicEnvironment[Addr](newMap)
  }
}

/** Environment that combines a default read-only environment with a writable environment */
case class CombinedEnvironment[Addr : Address](ro: Environment[Addr], w: Environment[Addr]) extends Environment[Addr] {
  def keys = ro.keys.toSet ++ w.keys.toSet
  def forall(p: ((String, Addr)) => Boolean) = keys.forall(name => lookup(name) match {
    case Some(a) => p(name, a)
    case None => throw new Exception(s"shouldn't happen: an existing key is not bound in the environment (key: $name, env: $this)")
  })
  def lookup(name: String) = w.lookup(name).orElse(ro.lookup(name))
  def extend(name: String, a: Addr) = this.copy(w = w.extend(name, a))
  def extend(values: Iterable[(String, Addr)]) =
    this.copy(w = w.extend(values))
  def map(f: Addr => Addr): Environment[Addr] = {
    CombinedEnvironment(ro.map(f), w.map(f))
  }
}

object Environment {
  def empty[Addr: Address]: Environment[Addr] =
    BasicEnvironment(Map[String, Addr]())
  def initial[Addr: Address](
      values: Iterable[(String, Addr)]): Environment[Addr] =
    BasicEnvironment(values.toMap)
}
