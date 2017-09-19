trait Address[A] {
  def name: String
  def isPrimitive(x: A): Boolean
  def primitive(name: String): A
  def variable[Time : Timestamp, Abs : JoinLattice](id: Identifier, value: Abs, t: Time): A
  def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time): A
  def botAddress: A = primitive("__bottom__")
  def allocationSite[Exp : Expression](a: A): Option[Either[Position, Position]]
}

object Address {
  def apply[A : Address]: Address[A] = implicitly
}

trait AddressWrapper {
  type A
  val isAddress: Address[A]
}

object ClassicalAddress extends AddressWrapper {
  trait A
  case class VariableAddress[Time : Timestamp](id: Identifier, t: Time) extends A {
    override def toString = s"@$id-$t"
  }
  case class PrimitiveAddress(name: String) extends A {
    override def toString = s"@$name"
  }
  case class CellAddress[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) extends A {
    override def toString = s"@$exp-$t"
  }

  implicit val isAddress = new Address[A] {
    def name = "Classical"
    def isPrimitive(x: A) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
    def primitive(name: String) = PrimitiveAddress(name)
    def variable[Time : Timestamp, Abs : JoinLattice](id: Identifier, value: Abs, t: Time) = VariableAddress(id, t)
    def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = CellAddress(exp, t)
    def allocationSite[Exp : Expression](a: A) = a match {
      case PrimitiveAddress(_) => None
      case VariableAddress(id, _) => Some(Left(id.pos))
      case CellAddress(exp: Exp @unchecked, _) => Some(Right(Expression[Exp].pos(exp)))
    }
  }
}

object ValueSensitiveAddress extends AddressWrapper {
  trait A
  case class VariableAddress[Time : Timestamp, Abs : JoinLattice](id: Identifier, value: Abs, t: Time) extends A {
    override def toString = s"@($id,$value)"
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
    def variable[Time : Timestamp, Abs : JoinLattice](id: Identifier, value: Abs, t: Time) = {
      /* To ensure finiteness, value should be a primitive value that doesn't contain addresses (i.e., no cons cell etc.) */
      VariableAddress(id, if (JoinLattice[Abs].isPrimitiveValue(value)) value else JoinLattice[Abs].bottom, t)
    }
    def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = CellAddress(exp, t)
    def allocationSite[Exp : Expression](a: A) = a match {
      case PrimitiveAddress(_) => None
      case VariableAddress(id, _, _) => Some(Left(id.pos))
      case CellAddress(exp: Exp @unchecked, _) => Some(Right(Expression[Exp].pos(exp)))
    }
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
    def variableAbstract[Time: Timestamp, Abs: JoinLattice](id: Identifier, value: Abs, t: Time) =
      abstractAddress.variable[Time, Abs](id, value, t)
    def cellAbstract[Exp: Expression, Time: Timestamp](exp: Exp, t: Time) =
      abstractAddress.cell(exp, t)

    def primitive(name: String) = { PrimitiveAddress(name) }
    def variable[Time: Timestamp, Abs: JoinLattice](id: Identifier, value: Abs, t: Time) = {
      HybridAddr(variableAbstract[Time, Abs](id, value, t))
    }
    def cell[Exp: Expression, Time: Timestamp](exp: Exp, t: Time) = HybridAddr(cellAbstract[Exp, Time](exp, t))
    def allocationSite[Exp : Expression](a: A) = a match {
      case PrimitiveAddress(_) => None
      case HybridAddr(a) =>
        abstractAddress.allocationSite(a)
    }
  }
}
