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

trait HybridAddress

object HybridAddress {
  var id = 0
  
  var useConcrete = true
  
  def switchToAbstract() = {
    useConcrete = false
  }

  def switchToConcrete() = {
    useConcrete = true
  }

  case class Left(address1 : ConcreteAddress, address2 : ClassicalAddress) extends HybridAddress
  case class Right(address : ClassicalAddress) extends HybridAddress
  case class PrimitiveAddress(name: String) extends HybridAddress

  case class IntAddress(name: String, id: Int) extends ConcreteAddress
  case class VariableAddress[Time : Timestamp](name: String, t : Time) extends ClassicalAddress
  case class CellAddress[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) extends ClassicalAddress

  def convertAddress(address : HybridAddress) : HybridAddress = address match {
    case HybridAddress.PrimitiveAddress(name) => HybridAddress.PrimitiveAddress(name)
    case HybridAddress.Left(address1, address2) => HybridAddress.Right(address2)
    case HybridAddress.Right(_) => address
    case _ => throw new Exception(s"Cannot reconvert an abstract address: $address")
  }

  implicit object HybridAddressAddress extends Address[HybridAddress] {
    def name = "Hybrid"
    def isPrimitive(x: HybridAddress) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
    def variableConcrete[Time : Timestamp](name: String, t: Time) = { id += 1; IntAddress(name, id) }
    def cellConcrete[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = { id += 1; IntAddress(s"cell-$exp", id) }
    def variableAbstract[Time : Timestamp](name: String, t: Time) = VariableAddress(name, t)
    def cellAbstract[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = CellAddress(exp, t)

    def primitive(name: String) = { PrimitiveAddress(name) }
    def variable[Time : Timestamp](name: String, t: Time) = {
      if (useConcrete) {
        Left(variableConcrete[Time](name, t), variableAbstract[Time](name, t))
      } else {
        Right(variableAbstract[Time](name, t))
      }
    }
      
    def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = {
      if (useConcrete) {
        Left(cellConcrete[Exp, Time](exp, t), cellAbstract[Exp, Time](exp, t))
      } else {
        Right(cellAbstract[Exp, Time](exp, t))
      }
    }
  }
}
