import scalaz.ISet

object SimpleTypes extends Enumeration {
  val Boolean, Bottom, Char, Closure, Cons, Float, Integer, Nil,
      Primitive, String, Symbol, Top, Vector, VectorAddress = Value
}

class HybridLattice
  (val abstractLattice: HasMakeSchemeLattice) extends SchemeLattice {

  var useConcrete = true

  def switchToAbstract() = {
    useConcrete = false
  }

  def switchToConcrete() = {
    useConcrete = true
  }

  sealed trait L

  val abstractLatticeCounting = false

  val concreteLattice = new ConcreteLattice(true)
//  val abstractLattice = new ConstantPropagationLattice(abstractLatticeCounting)

  val concreteSchemeLattice = concreteLattice.isSchemeLattice
  val abstractSchemeLattice = abstractLattice.isSchemeLattice

  type ConcL = concreteLattice.L
  type AbstL = abstractLattice.L

  case class Concrete(c: ConcL) extends L
  case class Abstract(a: AbstL) extends L

  /**
    * If the given value matches to an abstract value, calls the given function f on the wrapped abstract value.
    * If the value does not match an abstract value, an exception is thrown.
    * @param value The value to be matched.
    * @param f The function to be called on the abstract value inside 'value'.
    * @tparam ResultType The return type of f.
    * @return The result of f called on the wrapped abstract value inside 'value'.
    */
  private def handleAbstractValue[ResultType](value: L, f: AbstL => ResultType): ResultType = value match {
    case Concrete(_) =>
      throw new Exception(s"Value expected to be abstract; was instead: $value")
    case Abstract(v) => f(v)
  }

  def isConstantValue(value: L): Boolean = handleAbstractValue[Boolean](value, abstractLattice.lattice.latticeInfoProvider.isConstantValue)

  def pointsTo(value: L): Int = handleAbstractValue[Int](value, abstractLattice.lattice.latticeInfoProvider.pointsTo)

  def convert[Exp : Expression, Addr : Address](value: L,
                                                addressConverter: AddressConverter[Addr],
                                                convertEnv: Environment[Addr] => Environment[Addr]): L = {

    def convertValue(v: concreteLattice.lattice.Value): abstractLattice.lattice.Value = v match {
      case concreteLattice.lattice.Bot =>
        abstractLattice.lattice.Bot
      case concreteLattice.lattice.Str(s) =>
        abstractLattice.lattice.isSchemeLattice.inject(s.asInstanceOf[ISet[String]].toList.head)
      case concreteLattice.lattice.Bool(b) =>
        abstractLattice.lattice.isSchemeLattice.inject(b.asInstanceOf[ISet[Boolean]].toList.head)
      case concreteLattice.lattice.Int(i) =>
        abstractLattice.lattice.isSchemeLattice.inject(i.asInstanceOf[ISet[Int]].toList.head)
      case concreteLattice.lattice.Float(f) =>
        abstractLattice.lattice.isSchemeLattice.inject(f.asInstanceOf[ISet[Float]].toList.head)
      case concreteLattice.lattice.Char(c) =>
        abstractLattice.lattice.isSchemeLattice.inject(c.asInstanceOf[ISet[Char]].toList.head)
      case concreteLattice.lattice.Symbol(s) =>
        abstractLattice.lattice.isSchemeLattice.injectSymbol(s.asInstanceOf[ISet[String]].toList.head)
      case concreteLattice.lattice.Prim(prim) =>
        abstractLattice.lattice.isSchemeLattice.inject(prim.asInstanceOf[Primitive[Addr, this.L]])
      case concreteLattice.lattice.Closure(lambda, env) =>
        val convertedEnv = convertEnv(env.asInstanceOf[Environment[Addr]])
        abstractLattice.lattice.isSchemeLattice.inject((lambda, convertedEnv).asInstanceOf[(Exp, Environment[Addr])])
      case c: concreteLattice.lattice.Cons[Addr] =>
        abstractLattice.lattice.Cons[Addr](addressConverter.convertAddress(c.car), addressConverter.convertAddress(c.cdr))
      case concreteLattice.lattice.Nil =>
        abstractLattice.lattice.Nil
      case concreteLattice.lattice.Vec(size, elements, init) =>
        val actualSize = size.asInstanceOf[ISet[Int]].toList.head
        val abstractSize: abstractLattice.In = abstractLattice.lattice.int.inject(actualSize)
        var abstractElements = collection.immutable.Map[abstractLattice.In, Addr]()
        elements.foreach({ case (i, address: Addr) => {
          val index = i.asInstanceOf[ISet[Int]].toList.head
          val newIndex: abstractLattice.In = abstractLattice.lattice.int.inject(index)
          abstractElements = abstractElements + (newIndex -> address)
        } })
        abstractLattice.lattice.Vec[Addr](abstractSize, abstractElements, init.asInstanceOf[Addr])
    }
    value match {
      case Abstract(a) => throw new Exception(s"Cannot convert an abstract value into a concrete value: $a")
      case Concrete(c) => c match {
        case e: concreteLattice.lattice.Element =>
          val convertedValue = convertValue(e.v)
          Abstract(abstractLattice.lattice.Element(convertedValue))
        case es: concreteLattice.lattice.Elements =>
          Abstract(abstractLattice.lattice.Elements(es.vs.map(convertValue)))
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
      if (useConcrete) {
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

    def counting = if (useConcrete) { true } else { abstractLatticeCounting }

    def isPrimitiveValue(x: L): Boolean = delegateToLattice1[Boolean](x,
      (x: ConcL) => concreteSchemeLattice.isPrimitiveValue(x),
      (x: AbstL) => abstractSchemeLattice.isPrimitiveValue(x))
  }

}
