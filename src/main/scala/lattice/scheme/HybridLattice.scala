

import scalaz.ISet

object HybridLattice extends SchemeLattice {

  var doConcrete = true

  def switchToAbstract = {
    doConcrete = false
  }

  def switchToConcrete = {
    doConcrete = true
  }

  sealed trait L

  val concreteLattice = new ConcreteLattice(true)
  val abstractLattice = new TypeSetLattice(true)

  val concreteSchemeLattice = concreteLattice.isSchemeLattice
  val abstractSchemeLattice = abstractLattice.isSchemeLattice

  type ConcL = concreteLattice.L
  type AbstL = abstractLattice.L

//  import Type._
//
//  def getOperandType(operand: LSet): IsLatticeElement[T] = operand match {
//    case Concrete(AbstractConcrete.AbstractFloat(_)) => AbstractType.AbstractFloat
//    case Concrete(AbstractConcrete.AbstractInt(_)) => AbstractType.AbstractInt
//    case _ => AbstractType.AbstractTop
//  }
//
//  def checkValuesTypes(operands: List[Hybrid]): AbstractType = {
//    operands.foldLeft(AbstractType.AbstractBottom: AbstractType)({ (operandsTypes, operand) =>
//      if (operandsTypes == AbstractType.AbstractBottom) {
//        getOperandType(operand)
//      } else if (operandsTypes == getOperandType(operand)) {
//        operandsTypes
//      } else {
//        AbstractType.AbstractTop
//      }
//    })
//  }

  case class Concrete(c: concreteLattice.L) extends L
  case class Abstract(a: abstractLattice.L) extends L

  def convert[Exp : Expression, Addr : Address](value: L, store: Store[HybridAddress.A, L]): L = {

    import ConcreteString._
    import ConcreteBoolean._
    import ConcreteInteger._
    import ConcreteFloat._
    import ConcreteChar._
    import ConcreteSymbol._

    def convertValue(lat: MakeSchemeLattice[S, B, I, F, C, Sym])(v: lat.Value): abstractLattice.lattice.Value = v match {
      case lat.Bot =>
        abstractLattice.lattice.Bot
      case lat.Str(s) =>
        abstractLattice.lattice.isSchemeLattice.inject(s.asInstanceOf[ISet[String]].toList.head)
      case lat.Bool(b) =>
        abstractLattice.lattice.isSchemeLattice.inject(b.asInstanceOf[ISet[Boolean]].toList.head)
      case lat.Int(i) =>
        abstractLattice.lattice.isSchemeLattice.inject(i.asInstanceOf[ISet[Int]].toList.head)
      case lat.Float(f) =>
        abstractLattice.lattice.isSchemeLattice.inject(f.asInstanceOf[ISet[Float]].toList.head)
      case lat.Symbol(s) =>
        abstractLattice.lattice.isSchemeLattice.injectSymbol(s.asInstanceOf[ISet[String]].toList.head)
      case lat.Prim(prim) =>
        abstractLattice.lattice.isSchemeLattice.inject(prim)
      case lat.Closure(lambda, env) =>
        abstractLattice.lattice.isSchemeLattice.inject((lambda, env))
      case c: lat.Cons[Addr] =>
        abstractLattice.lattice.Cons[Addr](c.car, c.cdr)
      case lat.Nil =>
        abstractLattice.lattice.Nil
      case lat.Vec(size, elements, init) =>
        val actualSize = size.asInstanceOf[ISet[Int]].toList.head
        val abstractSize = Type.typeIsInteger.inject(actualSize)
        var abstractElements = collection.immutable.Map[Type.T, Addr]()
        elements.foreach({ case (i, address: Addr) => abstractElements = abstractElements + (Type.typeIsInteger.inject(i.asInstanceOf[ISet[Int]].toList.head) -> address) })
        abstractLattice.lattice.Vec[Addr](abstractSize, abstractElements, init.asInstanceOf[Addr])
    }
    value match {
      case Abstract(a) => throw new Exception(s"Cannot convert an abstract value into a concrete value: $a")
      case Concrete(c) => c match {
        case concreteLattice.lattice.Element(v) =>
          val convertedValue = convertValue(concreteLattice.lattice)(v)
          Abstract(abstractLattice.lattice.Element(convertedValue))
      }
      case concreteLattice.lattice.Elements(set) =>
        Abstract(abstractLattice.lattice.Elements(set.map(convertValue(concreteLattice.lattice))))
    }
  }


//  def convert[Exp : Expression, Addr : Address](value: L, store: Store[HybridAddress.A, L]): L = value match {
//    case Abstract(a) => throw new Exception(s"Cannot convert an abstract value into a concrete value: $a")
//    case Concrete(c) => c match {
//      case concreteLattice.lattice.Element(v) => v match {
//        case concreteLattice.lattice.Bot =>
//          Abstract(abstractLattice.lattice.Element(abstractLattice.lattice.Bot))
//        case concreteLattice.lattice.Str(s) =>
//          Abstract(abstractLattice.lattice.Element(abstractLattice.lattice.isSchemeLattice.inject(s.asInstanceOf[ISet[String]].toList.head)))
//        case concreteLattice.lattice.Bool(b) =>
//          Abstract(abstractLattice.lattice.Element(abstractLattice.lattice.isSchemeLattice.inject(b.asInstanceOf[ISet[Boolean]].toList.head)))
//        case concreteLattice.lattice.Int(i) =>
//          Abstract(abstractLattice.lattice.Element(abstractLattice.lattice.isSchemeLattice.inject(i.asInstanceOf[ISet[Int]].toList.head)))
//        case concreteLattice.lattice.Float(f) =>
//          Abstract(abstractLattice.lattice.Element(abstractLattice.lattice.isSchemeLattice.inject(f.asInstanceOf[ISet[Float]].toList.head)))
//        case concreteLattice.lattice.Symbol(s) =>
//          Abstract(abstractLattice.lattice.Element(abstractLattice.lattice.isSchemeLattice.injectSymbol(s.asInstanceOf[ISet[String]].toList.head)))
//        case concreteLattice.lattice.Prim(prim) =>
//          Abstract(abstractLattice.lattice.Element(abstractLattice.lattice.isSchemeLattice.inject(prim)))
//        case concreteLattice.lattice.Closure(lambda, env) =>
//          Abstract(abstractLattice.lattice.Element(abstractLattice.lattice.isSchemeLattice.inject((lambda, env))))
//        case c: concreteLattice.lattice.Cons[Addr] =>
//          Abstract(abstractLattice.lattice.Element(abstractLattice.lattice.Cons[Addr](c.car, c.cdr)))
//        case concreteLattice.lattice.Nil =>
//          Abstract(abstractLattice.lattice.Element(abstractLattice.lattice.Nil))
//        case concreteLattice.lattice.Vec(size, elements, init) =>
//          val actualSize = size.asInstanceOf[ISet[Int]].toList.head
//          val abstractSize = Type.typeIsInteger.inject(actualSize)
//          var abstractElements = collection.immutable.Map[Type.T, Addr]()
//          elements.foreach({ case (i, address: Addr) => abstractElements = abstractElements + (Type.typeIsInteger.inject(i.asInstanceOf[ISet[Int]].toList.head) -> address) })
//          Abstract(abstractLattice.lattice.Element(abstractLattice.lattice.Vec[Addr](abstractSize, abstractElements, init.asInstanceOf[Addr])))
//      }
//      case concreteLattice.lattice.Elements(set) =>
//        Abstract(abstractLattice.lattice.Elements(set.map({ case concreteLattice.lattice.Bot =>
//          abstractLattice.lattice.Bot })))
//    }
//  }

//  def convertValue(v: MakeSchemeLattice[S, B, I, F, C, Sym]#Value): MakeSchemeLattice[T, B, T, T, T, T]#Value = v match {
//    case concreteLattice.lattice.Bot =>
//      abstractLattice.lattice.Bot
//    case concreteLattice.lattice.Str(s) =>
//      abstractLattice.lattice.isSchemeLattice.inject(s.asInstanceOf[ISet[String]].toList.head)
//    case concreteLattice.lattice.Bool(b) =>
//      abstractLattice.lattice.isSchemeLattice.inject(b.asInstanceOf[ISet[Boolean]].toList.head)
//    case concreteLattice.lattice.Int(i) =>
//      abstractLattice.lattice.isSchemeLattice.inject(i.asInstanceOf[ISet[Int]].toList.head)
//    case concreteLattice.lattice.Float(f) =>
//      abstractLattice.lattice.isSchemeLattice.inject(f.asInstanceOf[ISet[Float]].toList.head)
//    case concreteLattice.lattice.Symbol(s) =>
//      abstractLattice.lattice.isSchemeLattice.injectSymbol(s.asInstanceOf[ISet[String]].toList.head)
//    case concreteLattice.lattice.Prim(prim) =>
//      abstractLattice.lattice.isSchemeLattice.inject(prim)
//    case concreteLattice.lattice.Closure(lambda, env) =>
//      abstractLattice.lattice.isSchemeLattice.inject((lambda, env))
//    case c: concreteLattice.lattice.Cons[Addr] =>
//      abstractLattice.lattice.Cons[Addr](c.car, c.cdr)
//    case concreteLattice.lattice.Nil =>
//      abstractLattice.lattice.Nil
//    case concreteLattice.lattice.Vec(size, elements, init) =>
//      val actualSize = size.asInstanceOf[ISet[Int]].toList.head
//      val abstractSize = Type.typeIsInteger.inject(actualSize)
//      var abstractElements = collection.immutable.Map[Type.T, Addr]()
//      elements.foreach({ case (i, address: Addr) => abstractElements = abstractElements + (Type.typeIsInteger.inject(i.asInstanceOf[ISet[Int]].toList.head) -> address) })
//      abstractLattice.lattice.Vec[Addr](abstractSize, abstractElements, init.asInstanceOf[Addr])
//  }
//  value match {
//    case Abstract(a) => throw new Exception(s"Cannot convert an abstract value into a concrete value: $a")
//    case Concrete(c) => c match {
//      case concreteLattice.lattice.Element(v) =>
//        val convertedValue = convertValue(v)
//        Abstract(abstractLattice.lattice.Element(convertedValue))
//    }



//    /*
//     * Does not convert AbstractTid
//     */
//    case AbstractConcrete.AbstractBool(v) => AbstractType.AbstractBool
//    case AbstractConcrete.AbstractBottom => AbstractType.AbstractBottom
//    case AbstractConcrete.AbstractChar(v) => AbstractType.AbstractChar
//    case AbstractConcrete.AbstractCons(car: HybridAddress.A, cdr: HybridAddress.A) => AbstractType.AbstractCons[HybridAddress.A](convertAddress(car), convertAddress(cdr))
//    case AbstractConcrete.AbstractFloat(v) => AbstractType.AbstractFloat
//    case AbstractConcrete.AbstractInt(v) => AbstractType.AbstractInt
//    case AbstractConcrete.AbstractNil => AbstractType.AbstractNil
//    case AbstractConcrete.AbstractString(v) => AbstractType.AbstractString
//    case AbstractConcrete.AbstractSymbol(v) => AbstractType.AbstractSymbol
//    case v: AbstractConcrete.AbstractClosure[Exp, HybridAddress.A] => v match {
//      case AbstractConcrete.AbstractClosure(λ, ρ) => AbstractType.AbstractClosures(Set[(Exp, Environment[HybridAddress.A])]((λ, ρ.map(HybridAddress.convertAddress))))
//    }
//    case v: AbstractConcrete.AbstractPrimitive[HybridAddress.A, HybridLattice.Hybrid] => v match {
//      case AbstractConcrete.AbstractPrimitive(p: Primitive[HybridAddress.A, HybridLattice.Hybrid]) => AbstractType.AbstractPrimitive(p)
//    }
//  }





  implicit val isSchemeLattice = HybridAbstractValue

  implicit object HybridAbstractValue extends IsSchemeLattice[L] {

    def delegateToLattice[Result](x: L, concreteFun: (ConcL) => Result,
                                  abstractFun: (AbstL) => Result): Result = x match {
      case Concrete(concreteX) => concreteFun(concreteX)
      case Abstract(abstractX) => abstractFun(abstractX)
    }

    def delegateToLattice[Result](x: L, y: L,
                                  concreteFun: (ConcL, ConcL) => Result,
                                  abstractFun: (AbstL, AbstL) => Result): Option[Result] = (x, y) match {
      case (Concrete(concreteX), Concrete(concreteY)) => Some(concreteFun(concreteX, concreteY))
      case (Abstract(abstractX), Abstract(abstractY)) => Some(abstractFun(abstractX, abstractY))
      case _ => None
    }

    def delegateToLattice[Result](x: L, y: L, z: L,
                                  concreteFun: (ConcL, ConcL, ConcL) => Result,
                                  abstractFun: (AbstL, AbstL, AbstL) => Result): Option[Result] = (x, y, z) match {
      case (Concrete(concreteX), Concrete(concreteY), Concrete(concreteZ)) => Some(concreteFun(concreteX, concreteY, concreteZ))
      case (Abstract(abstractX), Abstract(abstractY), Abstract(abstractZ)) => Some(abstractFun(abstractX, abstractY, abstractZ))
      case _ => None
    }

    def isTrue(x: L): Boolean = delegateToLattice[Boolean](x,
      (x) => concreteSchemeLattice.isTrue(x),
      (x) => abstractSchemeLattice.isTrue(x))

    def isFalse(x: L): Boolean = delegateToLattice[Boolean](x,
      (x) => concreteSchemeLattice.isFalse(x),
      (x) => abstractSchemeLattice.isFalse(x))

    def unaryOp(op: SchemeOps.UnaryOperator)(x: L): MayFail[L] = delegateToLattice[MayFail[L]](x,
      (x) => concreteSchemeLattice.unaryOp(op)(x).map(Concrete(_)),
      (x) => abstractSchemeLattice.unaryOp(op)(x).map(Abstract(_)))

    def binaryOp(op: SchemeOps.BinaryOperator)(x: L, y: L): MayFail[L] = delegateToLattice[MayFail[L]](x, y,
      (x, y) => concreteSchemeLattice.binaryOp(op)(x, y).map(Concrete(_)),
      (x, y) => abstractSchemeLattice.binaryOp(op)(x, y).map(Abstract(_))) match {
      case Some(result) => result
      case None => throw new Exception(s"binary operation $op on hybrid lattice cannot mix elements from two lattices: $x and $y")
    }

    def and(x: L, y: => L): L = delegateToLattice[L](x, y,
      (x, y) => Concrete(concreteSchemeLattice.and(x, y)),
      (x, y) => Abstract(abstractSchemeLattice.and(x, y))) match {
      case Some(result) => result
      case None => throw new Exception(s"and used on two different element of a hybrid lattice: $x and $y")
    }

    def or(x: L, y: => L): L = delegateToLattice[L](x, y,
      (x, y) => Concrete(concreteSchemeLattice.or(x, y)),
      (x, y) => Abstract(abstractSchemeLattice.or(x, y))) match {
      case Some(result) => result
      case None => throw new Exception(s"or used on two different element of a hybrid lattice: $x and $y")
    }

    def getClosures[Exp : Expression, Addr : Address](x: L): Set[(Exp, Environment[Addr])] = delegateToLattice[Set[(Exp, Environment[Addr])]](x,
      (x) => concreteSchemeLattice.getClosures[Exp, Addr](x),
      (y) => abstractSchemeLattice.getClosures[Exp, Addr](y))

    def getPrimitives[Addr : Address, Abs : JoinLattice](x: L): Set[Primitive[Addr, Abs]] = delegateToLattice[Set[Primitive[Addr, Abs]]](x,
      (x) => concreteSchemeLattice.getPrimitives[Addr, Abs](x),
      (y) => abstractSchemeLattice.getPrimitives[Addr, Abs](y))

    def applyEither(f: () => ConcL, g: () => AbstL): L =
      if (doConcrete) {
        Concrete(f())
      } else {
        Abstract(g())
      }

    def injectEither[A](x: A, f: A => ConcL, g: A => AbstL): L =
      applyEither(() => f(x), () => g(x))

    def inject(x: Int): L = injectEither[Int](x,
      (x) => concreteSchemeLattice.inject(x),
      (x) => abstractSchemeLattice.inject(x))

    def inject(x: Float): L = injectEither[Float](x,
      (x) => concreteSchemeLattice.inject(x),
      (x) => abstractSchemeLattice.inject(x))

    def inject(x: String): L = injectEither[String](x,
      (x) => concreteSchemeLattice.inject(x),
      (x) => abstractSchemeLattice.inject(x))

    def inject(x: Boolean): L = injectEither[Boolean](x,
      (x) => concreteSchemeLattice.inject(x),
      (x) => abstractSchemeLattice.inject(x))

    def inject(x: Char): L = injectEither[Char](x,
      (x) => concreteSchemeLattice.inject(x),
      (x) => abstractSchemeLattice.inject(x))

    def inject[Addr : Address, Abs : JoinLattice](x: Primitive[Addr, Abs]): L = injectEither[Primitive[Addr, Abs]](x,
      (x) => concreteSchemeLattice.inject(x),
      (x) => abstractSchemeLattice.inject(x))

    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): L = injectEither[(Exp, Environment[Addr])](x,
      (x) => concreteSchemeLattice.inject(x),
      (x) => abstractSchemeLattice.inject(x))

    def injectSymbol(x: String): L = injectEither[String](x,
      (x) => concreteSchemeLattice.injectSymbol(x),
      (x) => abstractSchemeLattice.injectSymbol(x))

    def cons[Addr : Address](car: Addr, cdr: Addr): L =
      applyEither(() => concreteSchemeLattice.cons(car, cdr), () => abstractSchemeLattice.cons(car, cdr))

    def nil: L = applyEither(() => concreteSchemeLattice.nil, () => abstractSchemeLattice.nil)

    def car[Addr : Address](x: L): Set[Addr] = delegateToLattice[Set[Addr]](x,
      (x) => concreteSchemeLattice.car(x),
      (x) => abstractSchemeLattice.car(x))

    def cdr[Addr : Address](x: L): Set[Addr] = delegateToLattice[Set[Addr]](x,
      (x) => concreteSchemeLattice.cdr(x),
      (x) => abstractSchemeLattice.cdr(x))

    def vectorRef[Addr : Address](vector: L, index: L): MayFail[Set[Addr]] = delegateToLattice[MayFail[Set[Addr]]](vector, index,
      (x, y) => concreteSchemeLattice.vectorRef(x, y),
      (x, y) => abstractSchemeLattice.vectorRef(x, y)) match {
        case Some(result) => result
        case None => throw new Exception(s"vector-ref used on two different element of a hybrid lattice: $vector and $index")
    }

    def vectorSet[Addr : Address](vector: L, index: L, addr: Addr): MayFail[(L, Set[Addr])] = delegateToLattice[MayFail[(L, Set[Addr])]](vector, index,
      (x, y) => concreteSchemeLattice.vectorSet(x, y, addr).map( (tuple) => (Concrete(tuple._1), tuple._2) ),
      (x, y) => abstractSchemeLattice.vectorSet(x, y, addr).map( (tuple) => (Abstract(tuple._1), tuple._2) )) match {
        case Some(result) => result
        case None => throw new Exception(s"vector-set! used on two different element of a hybrid lattice: $vector, $index and $addr")
    }

    def getVectors[Addr : Address](x: L): Set[Addr] = delegateToLattice[Set[Addr]](x,
      (x) => concreteSchemeLattice.getVectors(x),
      (x) => abstractSchemeLattice.getVectors(x))

    def vector[Addr : Address](addr: Addr, size: L, init: Addr): MayFail[(L, L)] = delegateToLattice[MayFail[(L, L)]](size,
      (x) => concreteSchemeLattice.vector(addr, x, init).map( (tuple) => (Concrete(tuple._1), Concrete(tuple._2))),
      (x) => abstractSchemeLattice.vector(addr, x, init).map( (tuple) => (Abstract(tuple._1), Abstract(tuple._2))))

    def bottom = applyEither(() => concreteSchemeLattice.bottom, () => abstractSchemeLattice.bottom)
    def name = "Hybrid"
    def join(x: L, y: L): L = delegateToLattice[L](x, y,
      (x, y) => Concrete(concreteSchemeLattice.join(x, y)),
      (x, y) => Abstract(abstractSchemeLattice.join(x, y))) match {
        case Some(result) => result
        case None => throw new Exception(s"Cannot join elements from two different lattices: $x and $y")
    }

    def subsumes(x: L, y: L): Boolean = delegateToLattice[Boolean](x, y,
      (x, y) => concreteSchemeLattice.subsumes(x, y),
      (x, y) => abstractSchemeLattice.subsumes(x, y)) match {
        case Some(result) => result
        case None => throw new Exception(s"Values from different lattices cannot subsume each other: $x and $y")
    }

    def counting = true

    def isPrimitiveValue(x: L): Boolean = delegateToLattice[Boolean](x,
      (x) => concreteSchemeLattice.isPrimitiveValue(x),
      (x) => abstractSchemeLattice.isPrimitiveValue(x))
  }

}
