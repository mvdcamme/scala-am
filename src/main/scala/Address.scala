trait Address[A] {
  def name: String
  def subsumes(x: A, y: A): Boolean = x.equals(y)
  def isPrimitive(x: A): Boolean
  def primitive(name: String): A
  def variable[Time: Timestamp, Abs: JoinLattice](name: String,
                                                  value: Abs,
                                                  t: Time): A
  def cell[Exp: Expression, Time: Timestamp](exp: Exp, t: Time): A
}

trait AddressWrapper {
  type A
  val isAddress: Address[A]
}

object ClassicalAddress extends AddressWrapper {
  trait A
  case class VariableAddress[Time: Timestamp](name: String, t: Time)
      extends A {
    override def toString = s"@$name"
  }
  case class PrimitiveAddress(name: String) extends A {
    override def toString = s"@$name"
  }
  case class CellAddress[Exp: Expression, Time: Timestamp](exp: Exp, t: Time)
      extends A {
    override def toString = s"@$exp"
  }

  implicit val isAddress = new Address[A] {
    def name = "Classical"
    def isPrimitive(x: A) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
    def primitive(name: String) = PrimitiveAddress(name)
    def variable[Time: Timestamp, Abs: JoinLattice](name: String,
                                                    value: Abs,
                                                    t: Time) =
      VariableAddress(name, t)
    def cell[Exp: Expression, Time: Timestamp](exp: Exp, t: Time) =
      CellAddress(exp, t)
  }
}

object ValueSensitiveAddress extends AddressWrapper {
  trait A
  case class VariableAddress[Time: Timestamp, Abs: JoinLattice](name: String,
                                                                value: Abs,
                                                                t: Time)
      extends A {
    override def toString = s"@($name,$value)"
  }
  case class PrimitiveAddress(name: String) extends A {
    override def toString = s"@$name"
  }
  case class CellAddress[Exp: Expression, Time: Timestamp](exp: Exp, t: Time)
      extends A {
    override def toString = s"@$exp"
  }

  implicit val isAddress = new Address[A] {
    def name = "ValueSensitive"
    def isPrimitive(x: A) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
    def primitive(name: String) = PrimitiveAddress(name)
    def variable[Time: Timestamp, Abs: JoinLattice](name: String,
                                                    value: Abs,
                                                    t: Time) = {
      val abs = implicitly[JoinLattice[Abs]]
      /* To ensure finiteness, value should be a primitive value that doesn't contain addresses (i.e., no cons cell etc.) */
      VariableAddress(name,
                      if (abs.isPrimitiveValue(value)) value else abs.bottom,
                      t)
    }
    def cell[Exp: Expression, Time: Timestamp](exp: Exp, t: Time) =
      CellAddress(exp, t)
  }
}

object HybridAddress extends AddressWrapper {
  trait A

  val abstractAddress = ClassicalAddress.isAddress

  case class HybridAddr(a: ClassicalAddress.A) extends A {
    override def equals(that: Any): Boolean = that match {
      case v: HybridAddr =>
        a == v.a
      case _ => super.equals(that)
    }

    override def hashCode() = a.hashCode()
  }
  case class PrimitiveAddress(name: String) extends A

  implicit val isAddress = new Address[A] {
    def name = "Hybrid"
    def isPrimitive(x: A) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
    def variableAbstract[Time: Timestamp, Abs: JoinLattice](name: String,
                                                            value: Abs,
                                                            t: Time) =
      abstractAddress.variable[Time, Abs](name, value, t)
    def cellAbstract[Exp: Expression, Time: Timestamp](exp: Exp, t: Time) =
      abstractAddress.cell(exp, t)

    def primitive(name: String) = { PrimitiveAddress(name) }
    def variable[Time: Timestamp, Abs: JoinLattice](name: String,
                                                    value: Abs,
                                                    t: Time) =
      HybridAddr(variableAbstract[Time, Abs](name, value, t))
    def cell[Exp: Expression, Time: Timestamp](exp: Exp, t: Time) =
      HybridAddr(cellAbstract[Exp, Time](exp, t))
  }

  def isPrimitiveAddress(address: A): Boolean = address match {
    case PrimitiveAddress(_) => true
    case _ => false
  }
}
