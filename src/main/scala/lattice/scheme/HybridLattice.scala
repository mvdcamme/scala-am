import scalaz.ISet

object SimpleTypes extends Enumeration {
  val Boolean, Bottom, Char, Closure, Cons, Float, Integer, Nil,
      Primitive, String, Symbol, Top, Vector, VectorAddress = Value
}

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
        abstractLattice.lattice.isSchemeLattice.inject(prim.asInstanceOf[Primitive[Addr, HybridLattice.L]])
      case lat.Closure(lambda, env) =>
        abstractLattice.lattice.isSchemeLattice.inject((lambda, env).asInstanceOf[(Exp, Environment[Addr])])
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
        case concreteLattice.lattice.Elements(set) =>
          Abstract(abstractLattice.lattice.Elements(set.map(convertValue(concreteLattice.lattice))))
      }
    }
  }

  implicit val isTracableLattice = LatticeConverter

  object LatticeConverter extends TracableLattice[L] {

    import Type._

    def getValueType(value: L): SimpleTypes.Value = value match {
      case Concrete(c) => c match {
        case concreteLattice.lattice.Element(concreteLattice.lattice.Bool(_)) => SimpleTypes.Boolean
        case concreteLattice.lattice.Element(concreteLattice.lattice.Bot) => SimpleTypes.Bottom
        case concreteLattice.lattice.Element(concreteLattice.lattice.Char(_)) => SimpleTypes.Char
        case concreteLattice.lattice.Element(concreteLattice.lattice.Closure(_, _)) => SimpleTypes.Closure
        case concreteLattice.lattice.Element(concreteLattice.lattice.Cons(_, _)) => SimpleTypes.Cons
        case concreteLattice.lattice.Element(concreteLattice.lattice.Float(_)) => SimpleTypes.Float
        case concreteLattice.lattice.Element(concreteLattice.lattice.Int(_)) => SimpleTypes.Integer
        case concreteLattice.lattice.Element(concreteLattice.lattice.Nil) => SimpleTypes.Nil
        case concreteLattice.lattice.Element(concreteLattice.lattice.Prim(_)) => SimpleTypes.Primitive
        case concreteLattice.lattice.Element(concreteLattice.lattice.Str(_)) => SimpleTypes.String
        case concreteLattice.lattice.Element(concreteLattice.lattice.Symbol(_)) => SimpleTypes.Symbol
        case concreteLattice.lattice.Element(concreteLattice.lattice.Vec(_, _, _)) => SimpleTypes.Vector
        case concreteLattice.lattice.Element(concreteLattice.lattice.VectorAddress(_)) => SimpleTypes.VectorAddress
        case _ => SimpleTypes.Top
      }
      case Abstract(_) =>
        throw new Exception(s"Cannot check type of an abstract value: $value")
    }

    def getValuesTypes(values: List[L]): SimpleTypes.Value = {
      values.foldLeft(SimpleTypes.Bottom)({ (previousValuesTypes, value) =>
        if (previousValuesTypes == SimpleTypes.Bottom) {
          getValueType(value)
        } else if (previousValuesTypes == getValueType(value)) {
          previousValuesTypes
        } else {
          SimpleTypes.Top
        }})
    }

    def asPrimitive[Addr: Address](supposedPrimitive: L): Option[Primitive[Addr, L]] = supposedPrimitive match {
      case Abstract(_) => None
      case Concrete(c) => c match {
        case concreteLattice.lattice.Element(concreteLattice.lattice.Prim(prim) => prim match {
          case prim: Primitive[Addr, L] => Some(prim)
          case _ => None
        }
        case _ => None
      }
    }
  }

  implicit val isSchemeLattice = HybridAbstractValue

  object HybridAbstractValue extends IsSchemeLattice[L] {

    def delegateToLattice1[Result](x: L, concreteFun: (ConcL) => Result,
                                  abstractFun: (AbstL) => Result): Result = x match {
      case Concrete(concreteX) => concreteFun(concreteX)
      case Abstract(abstractX) => abstractFun(abstractX)
    }

    def delegateToLattice2[Result](x: L, y: L,
                                  concreteFun: (ConcL, ConcL) => Result,
                                  abstractFun: (AbstL, AbstL) => Result): Option[Result] = (x, y) match {
      case (Concrete(concreteX), Concrete(concreteY)) => Some(concreteFun(concreteX, concreteY))
      case (Abstract(abstractX), Abstract(abstractY)) => Some(abstractFun(abstractX, abstractY))
      case _ => None
    }

    def delegateToLattice3[Result](x: L, y: L, z: L,
                                  concreteFun: (ConcL, ConcL, ConcL) => Result,
                                  abstractFun: (AbstL, AbstL, AbstL) => Result): Option[Result] = (x, y, z) match {
      case (Concrete(concreteX), Concrete(concreteY), Concrete(concreteZ)) => Some(concreteFun(concreteX, concreteY, concreteZ))
      case (Abstract(abstractX), Abstract(abstractY), Abstract(abstractZ)) => Some(abstractFun(abstractX, abstractY, abstractZ))
      case _ => None
    }

    def isTrue(x: L): Boolean = delegateToLattice1[Boolean](x,
      (x) => concreteSchemeLattice.isTrue(x),
      (x) => abstractSchemeLattice.isTrue(x))

    def isFalse(x: L): Boolean = delegateToLattice1[Boolean](x,
      (x) => concreteSchemeLattice.isFalse(x),
      (x) => abstractSchemeLattice.isFalse(x))

    def unaryOp(op: SchemeOps.UnaryOperator)(x: L): MayFail[L] = delegateToLattice1[MayFail[L]](x,
      (x: ConcL) => concreteSchemeLattice.unaryOp(op)(x).map(Concrete(_)),
      (x: AbstL) => abstractSchemeLattice.unaryOp(op)(x).map(Abstract(_)))

    def binaryOp(op: SchemeOps.BinaryOperator)(x: L, y: L): MayFail[L] = delegateToLattice2[MayFail[L]](x, y,
      (x: ConcL, y: ConcL) => concreteSchemeLattice.binaryOp(op)(x, y).map(Concrete(_)),
      (x: AbstL, y: AbstL) => abstractSchemeLattice.binaryOp(op)(x, y).map(Abstract(_))) match {
      case Some(result) => result
      case None => throw new Exception(s"binary operation $op on hybrid lattice cannot mix elements from two lattices: $x and $y")
    }

    def and(x: L, y: => L): L = delegateToLattice2[L](x, y,
      (x: ConcL, y: ConcL) => Concrete(concreteSchemeLattice.and(x, y)),
      (x: AbstL, y: AbstL) => Abstract(abstractSchemeLattice.and(x, y))) match {
      case Some(result) => result
      case None => throw new Exception(s"and used on two different element of a hybrid lattice: $x and $y")
    }

    def or(x: L, y: => L): L = delegateToLattice2[L](x, y,
      (x: ConcL, y: ConcL) => Concrete(concreteSchemeLattice.or(x, y)),
      (x: AbstL, y: AbstL) => Abstract(abstractSchemeLattice.or(x, y))) match {
      case Some(result) => result
      case None => throw new Exception(s"or used on two different element of a hybrid lattice: $x and $y")
    }

    def getClosures[Exp : Expression, Addr : Address](x: L): Set[(Exp, Environment[Addr])] = delegateToLattice1[Set[(Exp, Environment[Addr])]](x,
      (x: ConcL) => concreteSchemeLattice.getClosures[Exp, Addr](x),
      (y: AbstL) => abstractSchemeLattice.getClosures[Exp, Addr](y))

    def getPrimitives[Addr : Address, Abs : JoinLattice](x: L): Set[Primitive[Addr, Abs]] = delegateToLattice1[Set[Primitive[Addr, Abs]]](x,
      (x: ConcL) => concreteSchemeLattice.getPrimitives[Addr, Abs](x),
      (y: AbstL) => abstractSchemeLattice.getPrimitives[Addr, Abs](y))

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

    def car[Addr : Address](x: L): Set[Addr] = delegateToLattice1[Set[Addr]](x,
      (x: ConcL) => concreteSchemeLattice.car(x),
      (x: AbstL) => abstractSchemeLattice.car(x))

    def cdr[Addr : Address](x: L): Set[Addr] = delegateToLattice1[Set[Addr]](x,
      (x: ConcL) => concreteSchemeLattice.cdr(x),
      (x: AbstL) => abstractSchemeLattice.cdr(x))

    def vectorRef[Addr : Address](vector: L, index: L): MayFail[Set[Addr]] = delegateToLattice2[MayFail[Set[Addr]]](vector, index,
      (x: ConcL, y: ConcL) => concreteSchemeLattice.vectorRef(x, y),
      (x: AbstL, y: AbstL) => abstractSchemeLattice.vectorRef(x, y)) match {
        case Some(result) => result
        case None => throw new Exception(s"vector-ref used on two different element of a hybrid lattice: $vector and $index")
    }

    def vectorSet[Addr : Address](vector: L, index: L, addr: Addr): MayFail[(L, Set[Addr])] = delegateToLattice2[MayFail[(L, Set[Addr])]](vector, index,
      (x: ConcL, y: ConcL) => concreteSchemeLattice.vectorSet(x, y, addr).map( (tuple) => (Concrete(tuple._1), tuple._2) ),
      (x: AbstL, y: AbstL) => abstractSchemeLattice.vectorSet(x, y, addr).map( (tuple) => (Abstract(tuple._1), tuple._2) )) match {
        case Some(result) => result
        case None => throw new Exception(s"vector-set! used on two different element of a hybrid lattice: $vector, $index and $addr")
    }

    def getVectors[Addr : Address](x: L): Set[Addr] = delegateToLattice1[Set[Addr]](x,
      (x: ConcL) => concreteSchemeLattice.getVectors(x),
      (x: AbstL) => abstractSchemeLattice.getVectors(x))

    def vector[Addr : Address](addr: Addr, size: L, init: Addr): MayFail[(L, L)] = delegateToLattice1[MayFail[(L, L)]](size,
      (x: ConcL) => concreteSchemeLattice.vector(addr, x, init).map( (tuple) => (Concrete(tuple._1), Concrete(tuple._2))),
      (x: AbstL) => abstractSchemeLattice.vector(addr, x, init).map( (tuple) => (Abstract(tuple._1), Abstract(tuple._2))))

    def bottom = applyEither(() => concreteSchemeLattice.bottom, () => abstractSchemeLattice.bottom)
    def name = "Hybrid"
    def join(x: L, y: L): L = delegateToLattice2[L](x, y,
      (x: ConcL, y: ConcL) => Concrete(concreteSchemeLattice.join(x, y)),
      (x: AbstL, y: AbstL) => Abstract(abstractSchemeLattice.join(x, y))) match {
        case Some(result) => result
        case None => throw new Exception(s"Cannot join elements from two different lattices: $x and $y")
    }

    def subsumes(x: L, y: L): Boolean = delegateToLattice2[Boolean](x, y,
      (x: ConcL, y: ConcL) => concreteSchemeLattice.subsumes(x, y),
      (x: AbstL, y: AbstL) => abstractSchemeLattice.subsumes(x, y)) match {
        case Some(result) => result
        case None => throw new Exception(s"Values from different lattices cannot subsume each other: $x and $y")
    }

    def counting = true

    def isPrimitiveValue(x: L): Boolean = delegateToLattice1[Boolean](x,
      (x: ConcL) => concreteSchemeLattice.isPrimitiveValue(x),
      (x: AbstL) => abstractSchemeLattice.isPrimitiveValue(x))
  }

}
