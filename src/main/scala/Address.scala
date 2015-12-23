trait Address[A] {
  def name: String
  def subsumes(x: A, y: A): Boolean = x.equals(y)
  def isPrimitive(x: A): Boolean
  def primitive(name: String): A
  def variable[Time : Timestamp](name: String, t: Time): A
  def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time): A
}

trait ClassicalAddress

object ClassicalAddress {
  case class VariableAddress[Time : Timestamp](name: String, t: Time) extends ClassicalAddress
  case class PrimitiveAddress(name: String) extends ClassicalAddress
  case class CellAddress[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) extends ClassicalAddress

  implicit object ClassicalAddressAddress extends Address[ClassicalAddress] {
    def name = "Classical"
    def isPrimitive(x: ClassicalAddress) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
    def primitive(name: String) = PrimitiveAddress(name)
    def variable[Time : Timestamp](name: String, t: Time) = VariableAddress(name, t)
    def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = CellAddress(exp, t)
  }
}

trait ConcreteAddress

object ConcreteAddress {
  var id = 0
  case class PrimitiveAddress(name: String) extends ConcreteAddress
  case class IntAddress(name: String, id: Int) extends ConcreteAddress
  implicit object ConcreteAddressAddress extends Address[ConcreteAddress] {
    def name = "Concrete"
    def isPrimitive(x: ConcreteAddress) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
    def primitive(name: String) = { PrimitiveAddress(name) }
    def variable[Time : Timestamp](name: String, t: Time) = { id += 1; IntAddress(name, id) }
    def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = { id += 1; IntAddress(s"cell-$exp", id) }
  }
}

trait HybridAddress

object HybridAddress {
  var id = 0
  
  var useConcrete = true
  
  def switchToAbstract = {
    useConcrete = false
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
