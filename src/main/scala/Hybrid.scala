import UnaryOperator._
import BinaryOperator._
  
object HybridLattice {
  
  var doConcrete = true
  
  def switchToAbstract = {
    doConcrete = false
  }

  def switchToConcrete = {
    doConcrete = true
  }
  
  trait Hybrid  
  
  val concreteValue = implicitly[AbstractValue[AbstractConcrete]]
  val abstractType = implicitly[AbstractValue[AbstractType]]

  case class Left(c: AbstractConcrete) extends Hybrid
  case class Right(a: AbstractType) extends Hybrid
  case class Prim[Addr, Abs](prim : Primitive[Addr, Abs]) extends Hybrid

  implicit object HybridAbstractValue extends AbstractValue[Hybrid] {
    def name = s"(${concreteValue.name} | ${abstractType.name})"

    private def err(reason: String) = {
      print(s"ERROR: ${reason}\n")
      error(inject(reason))
    }

    def isTrue(s: Hybrid) = s match {
      case Left(x) => concreteValue.isTrue(x)
      case Right(y) => abstractType.isTrue(y)
      case Prim(_) => true
    }
    def isFalse(s: Hybrid) = s match {
      case Left(x) => concreteValue.isFalse(x)
      case Right(y) => abstractType.isFalse(y)
      case Prim(_) => false
    }
    def isError(s: Hybrid) = s match {
      case Left(x) => concreteValue.isError(x)
      case Right(y) => abstractType.isError(y)
      case Prim(_) => false
    }
    def unaryOp(op: UnaryOperator)(s: Hybrid) = {
      s match {
        case Left(x) => Left(concreteValue.unaryOp(op)(x))
        case Right(y) => Right(abstractType.unaryOp(op)(y))
        case Prim(_) => err("unary operation ($op) performed on primitive $s")
      }
    }
    def binaryOp(op: BinaryOperator)(s1: Hybrid, s2: Hybrid) = {
      (s1, s2) match {
        case (Left(x1), Left(x2)) => Left(concreteValue.binaryOp(op)(x1, x2))
        case (Right(y1), Right(y2)) => Right(abstractType.binaryOp(op)(y1, y2))
        case _ => err(s"binary operation ${op} on hybrid lattice cannot mix elements from two lattices: ${s1} and ${s2}")
      }
    }
    def foldValues[B](s: Hybrid, f: Hybrid => Set[B]) = s match {
      case Left(x) => concreteValue.foldValues(x, (x) => f(Left(x)))
      case Right(y) => abstractType.foldValues(y, (y) => f(Right(y)))
      case Prim(_) => f(s)
    }
    def join(s1: Hybrid, s2: Hybrid) = (s1, s2) match {
      case (Left(x1), Left(x2)) => Left(concreteValue.join(x1, x2))
      case (Right(y1), Right(y2)) => Right(abstractType.join(y1, y2))
      case _ => err("cannot join different elements of a hybrid lattice: $s1 and $s2")
    }
    def meet(s1: Hybrid, s2: Hybrid) = (s1, s2) match {
      case (Left(x1), Left(x2)) => Left(concreteValue.meet(x1, x2))
      case (Right(y1), Right(y2)) => Right(abstractType.meet(y1, y2))
      case _ => bottom
    }
    def subsumes(s1: Hybrid, s2: Hybrid) = (s1, s2) match {
      case (Left(x1), Left(x2)) => concreteValue.subsumes(x1, x2)
      case (Right(y1), Right(y2)) => abstractType.subsumes(y1, y2)
      case (Prim(p1), Prim(p2)) => p1 == p2
      case _ => s1.equals(s2)
    }
    def and(s1: Hybrid, s2: => Hybrid) = (s1, s2) match {
      case (Left(x1), Left(x2)) => Left(concreteValue.and(x1, x2))
      case (Right(y1), Right(y2)) => Right(abstractType.and(y1, y2))
      case _ => err("and used on two different element of a hybrid lattice: $s1 and $s2")
    }
    def or(s1: Hybrid, s2: => Hybrid) = (s1, s2) match {
      case (Left(x1), Left(x2)) => Left(concreteValue.or(x1, x2))
      case (Right(y1), Right(y2)) => Right(abstractType.or(y1, y2))
      case _ => err("or used on two different element of a hybrid lattice: $s1 and $s2")
    }
    def car[Addr : Address](s: Hybrid) = s match {
      case Left(x) => concreteValue.car[Addr](x)
      case Right(y) => abstractType.car[Addr](y)
      case Prim(_) => Set[Addr]()
    }
    def cdr[Addr : Address](s: Hybrid) = s match {
      case Left(x) => concreteValue.cdr[Addr](x)
      case Right(y) => abstractType.cdr[Addr](y)
      case Prim(_) => Set[Addr]()
    }
    def toString[Addr : Address](s: Hybrid, store: Store[Addr, Hybrid]) = s.toString

    def getClosures[Exp : Expression, Addr : Address](s: Hybrid) = s match {
      case Left(x) => concreteValue.getClosures[Exp, Addr](x)
      case Right(y) => abstractType.getClosures[Exp, Addr](y)
      case Prim(_) => Set()
    }

    def getPrimitive[Addr : Address, Abs : AbstractValue](s: Hybrid) = s match {
      case Prim(p: Primitive[Addr, Abs]) => Some(p)
      case _ => None
    }
    def getTids[TID : ThreadIdentifier](s: Hybrid) = s match {
      case Left(x) => concreteValue.getTids[TID](x)
      case Right(y) => abstractType.getTids[TID](y)
      case Prim(_) => Set()
    }

    def bottom = if (doConcrete) { Left(concreteValue.bottom) } else { Right(abstractType.bottom) }

    def error(s: Hybrid) = s match {
      case Left(x) => Left(concreteValue.error(x))
      case Right(y) => Right(abstractType.error(y))
    }
    
    def injectCorrectValue[A](x : A, f : A => AbstractConcrete, g : A => AbstractType) : Hybrid = {
      if (doConcrete) {
        return Left(f(x))
      } else {
        return Right(g(x))
      }
    }
    
    def inject(x: Int) = injectCorrectValue[Int](x, concreteValue.inject, abstractType.inject)
    def inject(x: String) = injectCorrectValue[String](x, concreteValue.inject, abstractType.inject)
    def inject(x: Char) = injectCorrectValue[Char](x, concreteValue.inject, abstractType.inject)
    def inject(x: Boolean) = injectCorrectValue[Boolean](x, concreteValue.inject, abstractType.inject)
    def inject[Addr : Address, Abs : AbstractValue](x: Primitive[Addr, Abs]) = Prim[Addr, Abs](x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = {
      if (doConcrete) {
        Left(concreteValue.inject[Exp, Addr](x))
      } else {
        Right(abstractType.inject[Exp, Addr](x))
      }
    }
    def injectTid[TID : ThreadIdentifier](t: TID) = injectCorrectValue[TID](t, concreteValue.injectTid[TID], abstractType.injectTid[TID])
    def injectSymbol(x: String) = injectCorrectValue[String](x, concreteValue.injectSymbol, abstractType.injectSymbol)
    def nil = {
      if (doConcrete) {
        Left(concreteValue.nil)
      } else {
        Right(abstractType.nil)
      }
    }
    def cons[Addr : Address](car: Addr, cdr: Addr) = {
      if (doConcrete) {
        Left(concreteValue.cons[Addr](car, cdr))
      } else {
        Right(abstractType.cons[Addr](car, cdr))
      }
    }
  }
}
