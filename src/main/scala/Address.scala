trait Address[A] {
  def name: String
  def subsumes(x: A, y: A): Boolean = x.equals(y)
  def isPrimitive(x: A): Boolean
  def primitive(name: String): A
  def variable[Time : Timestamp, Abs : JoinLattice](name: String, value: Abs, t: Time): A
  def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time): A
}

trait AddressWrapper {
  type A
  val isAddress: Address[A]
}

object ClassicalAddress extends AddressWrapper {
  trait A
  case class VariableAddress[Time : Timestamp](name: String, t: Time) extends A {
    override def toString = s"@$name"
  }
  case class PrimitiveAddress(name: String) extends A {
    override def toString = s"@$name"
  }
  case class CellAddress[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) extends A {
    override def toString = s"@$exp"
  }

  implicit val isAddress = new Address[A] {
    def name = "Classical"
    def isPrimitive(x: A) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
    def primitive(name: String) = PrimitiveAddress(name)
    def variable[Time : Timestamp, Abs : JoinLattice](name: String, value: Abs, t: Time) = VariableAddress(name, t)
    def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = CellAddress(exp, t)
  }
}

object ValueSensitiveAddress extends AddressWrapper {
  trait A
  case class VariableAddress[Time : Timestamp, Abs : JoinLattice](name: String, value: Abs, t: Time) extends A {
    override def toString = s"@($name,$value)"
  }
  case class PrimitiveAddress(name: String) extends A {
    override def toString = s"@$name"
  }
  case class CellAddress[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) extends A {
    override def toString = s"@$exp"
  }

  implicit val isAddress = new Address[A] {
    def name = "ValueSensitive"
    def isPrimitive(x: A) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
    def primitive(name: String) = PrimitiveAddress(name)
    def variable[Time : Timestamp, Abs : JoinLattice](name: String, value: Abs, t: Time) = {
      val abs = implicitly[JoinLattice[Abs]]
      /* To ensure finiteness, value should be a primitive value that doesn't contain addresses (i.e., no cons cell etc.) */
      VariableAddress(name, if (abs.isPrimitiveValue(value)) value else abs.bottom, t)
    }
    def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = CellAddress(exp, t)
  }
}

object HybridAddress extends AddressWrapper {
  trait A

  var id = 0
  
  var useConcrete = true
  
  def switchToAbstract() = {
    useConcrete = false
  }

  def switchToConcrete = {
    useConcrete = true
  }

  val classicalAddress = ClassicalAddress.isAddress

  case class IntAddress(name: String, id: Int)

  case class Left(address1: IntAddress, address2: ClassicalAddress.A) extends A
  case class Right(address: ClassicalAddress.A) extends A {
    override def equals(that: Any): Boolean = that match {
      case v: Right =>
        address == v.address
      case _ => super.equals(that)
    }

    override def hashCode() = address.hashCode()
  }
  case class PrimitiveAddress(name: String) extends A

  def convertAddress(address: A): A = address match {
    case HybridAddress.PrimitiveAddress(name) => HybridAddress.PrimitiveAddress(name)
    case HybridAddress.Left(address1, address2) => HybridAddress.Right(address2)
    case HybridAddress.Right(_) => address
    case _ => throw new Exception(s"Cannot reconvert an abstract address: $address")
  }

  implicit val isAddress = new Address[A] {
    def name = "Hybrid"
    def isPrimitive(x: A) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
    def variableConcrete[Time : Timestamp, Abs: JoinLattice](name: String, t: Time) = { id += 1; IntAddress(name, id) }
    def cellConcrete[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = { id += 1; IntAddress(s"cell-$exp", id) }
    def variableAbstract[Time : Timestamp, Abs: JoinLattice](name: String, value: Abs, t: Time) = classicalAddress.variable[Time, Abs](name, value, t)
    def cellAbstract[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = classicalAddress.cell(exp, t)

    def primitive(name: String) = { PrimitiveAddress(name) }
    def variable[Time : Timestamp, Abs : JoinLattice](name: String, value: Abs, t: Time) = {
      if (useConcrete) {
        Left(variableConcrete[Time, Abs](name, t), variableAbstract[ZeroCFA.T, Abs](name, value, ZeroCFA.isTimestamp.initial("")))
      } else {
        Right(variableAbstract[Time, Abs](name, value, t))
      }
    }
      
    def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = {
      if (useConcrete) {
        Left(cellConcrete[Exp, Time](exp, t), cellAbstract[Exp, ZeroCFA.T](exp, ZeroCFA.isTimestamp.initial("")))
      } else {
        Right(cellAbstract[Exp, Time](exp, t))
      }
    }
  }
}
