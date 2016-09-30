import scalaz.{Plus => _, _}
import scalaz.Scalaz._
import SchemeOps._

trait LatticeInfoProvider[L] {

  def isConstantValue(x: L): Boolean

  def pointsTo(x: L): Int

}

class MakeSchemeLattice[S, B, I, F, C, Sym](supportsCounting: Boolean)(implicit str: IsString[S],
  bool: IsBoolean[B], val int: IsInteger[I], float: IsFloat[F], char: IsChar[C],
  sym: IsSymbol[Sym]) {

  sealed trait Value
  case object Bot extends Value {
    override def toString = "⊥"
  }
  case class Str(s: S) extends Value {
    override def toString = str.shows(s)
  }
  case class Bool(b: B) extends Value {
    override def toString = bool.shows(b)
  }
  case class Int(i: I) extends Value {
    override def toString = int.shows(i)
  }
  case class Float(f: F) extends Value {
    override def toString = float.shows(f)
  }
  case class Char(c: C) extends Value {
    override def toString = char.shows(c)
  }
  case class Symbol(s: Sym) extends Value {
    override def toString = sym.shows(s)
  }
  case class Prim[Addr : Address, Abs : JoinLattice](prim: Primitive[Addr, Abs]) extends Value {
    override def toString = s"#<prim ${prim.name}>"
  }
  case class Closure[Exp : Expression, Addr : Address](lambda: Exp, env: Environment[Addr]) extends Value {
    override def toString = "#<clo>"
  }
  case class Cons[Addr : Address](car: Addr, cdr: Addr) extends Value
  case object Nil extends Value {
    override def toString = "()"
  }
  case class Vec[Addr : Address](size: I, elements: Map[I, Addr], init: Addr) extends Value {
    override def toString = {
      val els = elements.toList.map({ case (k, v) => s"${int.shows(k)}: $v" }).mkString(", ")
      s"Vec(${int.shows(size)}, {$els}, $init)"
    }
  }
  case class VectorAddress[Addr : Address](a: Addr) extends Value

  val True = Bool(bool.inject(true))
  val False = Bool(bool.inject(false))

  type L = Value

  /* TODO: don't use exceptions */
  case class CannotJoin[Abs](values: Set[Abs]) extends Exception {
    override def toString = "CannotJoin(" + values.mkString(", ") + ")"
  }

  val isSchemeLattice: IsSchemeLattice[L] = new IsSchemeLattice[L] {
    def bottom = Bot
    def join(x: L, y: L): L = if (x == y) { x } else {
      (x, y) match {
        case (Bot, _) => y
        case (_, Bot) => x
        case (Str(s1), Str(s2)) => Str(str.join(s1, s2))
        case (Bool(b1), Bool(b2)) => Bool(bool.join(b1, b2))
        case (Int(i1), Int(i2)) => Int(int.join(i1, i2))
        case (Float(f1), Float(f2)) => Float(float.join(f1, f2))
        case (Char(c1), Char(c2)) => Char(char.join(c1, c2))
        case _ => throw new CannotJoin[L](Set(x, y))
      }
    }
    def subsumes(x: L, y: L): Boolean = if (x == y) { true } else {
      (x, y) match {
        case (_, Bot) => true
        case (Str(s1), Str(s2)) => str.subsumes(s1, s2)
        case (Bool(b1), Bool(b2)) => bool.subsumes(b1, b2)
        case (Int(i1), Int(i2)) => int.subsumes(i1, i2)
        case (Float(f1), Float(f2)) => float.subsumes(f1, f2)
        case (Char(c1), Char(c2)) => char.subsumes(c1, c2)
        case _ => false
      }
    }
    val name = s"Lattice(${str.name}, ${bool.name}, ${int.name}, ${float.name}, ${char.name}, ${sym.name})"
    val counting = supportsCounting

    def isPrimitiveValue(x: L): Boolean = x match {
      case Bot | Str(_) | Bool(_) | Int(_) | Float(_) | Char(_) | Symbol(_) | Nil => true
      case Closure(_, _) | Prim(_) | Cons(_, _) | VectorAddress(_) | Vec(_, _, _) => false
    }

    def isTrue(x: L): Boolean = x match {
      case Bool(b) => bool.isTrue(b)
      case Bot => false
      case _ => true
    }
    def isFalse(x: L): Boolean = x match {
      case Bool(b) => bool.isFalse(b)
      case Bot => true
      case _ => false
    }

    import scala.language.implicitConversions
    implicit def mayFailSuccess(l: L): MayFail[L] = MayFailSuccess(l)
    implicit def mayFailError(err: SemanticError): MayFail[L] = MayFailError(List(err))
    def unaryOp(op: UnaryOperator)(x: L): MayFail[L] = if (x == Bot) { Bot } else { op match {
      case IsNull => x match {
        case Nil => True
        case _ => False
      }
      case IsCons => x match {
        case Cons(_, _) => True
        case _ => False
      }
      case IsChar => x match {
        case Char(_) => True
        case _ => False
      }
      case IsSymbol => x match {
        case Symbol(_) => True
        case _ => False
      }
      case IsString => x match {
        case Str(_) => True
        case _ => False
      }
      case IsInteger => x match {
        case Int(_) => True
        case _ => False
      }
      case IsFloat => x match {
        case Float(_) => True
        case _ => False
      }
      case IsBoolean => x match {
        case Bool(_) => True
        case _ => False
      }
      case IsVector => x match {
        case Vec(_, _, _) => True
        case VectorAddress(_) => True
        case _ => False
      }
      case Not => x match {
        case Bool(b) => Bool(bool.not(b))
        case _ => False /* any value is true */
      }
      case Ceiling => x match {
        case Int(n) => Int(int.ceiling(n))
        case Float(n) => Float(float.ceiling(n))
        case _ => OperatorNotApplicable("ceiling", List(x.toString))
      }
      case Log => x match {
        case Int(n) => Float(float.log(int.toFloat(n)))
        case Float(n) => Float(float.log(n))
        case _ => OperatorNotApplicable("log", List(x.toString))
      }
      case Random => x match {
        case Int(n) => Int(int.random(n))
        case Float(n) => Float(float.random(n))
        case _ => OperatorNotApplicable("random", List(x.toString))
      }
      case VectorLength => x match {
        case Vec(size, _, _) => Int(size)
        case _ => OperatorNotApplicable("vector-length", List(x.toString))
      }
      case StringLength => x match {
        case Str(s) => Int(str.length(s))
        case _ => OperatorNotApplicable("string-length", List(x.toString))
      }
      case NumberToString => x match {
        case Int(n) => Str(int.toString(n))
        case Float(n) => Str(float.toString(n))
        case _ => OperatorNotApplicable("number->string", List(x.toString))
      }
    }}

    def binaryOp(op: BinaryOperator)(x: L, y: L): MayFail[L] = op match {
      case Plus => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.plus(n1, n2))
        case (Int(n1), Float(n2)) => Float(float.plus(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Float(float.plus(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Float(float.plus(n1, n2))
        case _ => OperatorNotApplicable("+", List(x.toString, y.toString))
      }
      case PlusFloat => (x, y) match {
        case (Float(n1), Float(n2)) => Float(float.plus(n1, n2))
        case _ => throw new Exception(s"Cannot apply Float addition on non-float values: $x, $y")
      }
      case PlusInteger => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.plus(n1, n2))
        case _ => throw new Exception(s"Cannot apply Integer addition on non-integer values: $x, $y")
      }
      case Minus => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.minus(n1, n2))
        case (Int(n1), Float(n2)) => Float(float.minus(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Float(float.minus(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Float(float.minus(n1, n2))
        case _ => OperatorNotApplicable("-", List(x.toString, y.toString))
      }
      case MinusFloat => (x, y) match {
        case (Float(n1), Float(n2)) => Float(float.minus(n1, n2))
        case _ => throw new Exception(s"Cannot apply Float subtraction on non-float values: $x, $y")
      }
      case MinusInteger => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.plus(n1, n2))
        case _ => throw new Exception(s"Cannot apply Minus subtraction on non-integer values: $x, $y")
      }
      case Times => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.times(n1, n2))
        case (Int(n1), Float(n2)) => Float(float.times(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Float(float.times(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Float(float.times(n1, n2))
        case _ => OperatorNotApplicable("*", List(x.toString, y.toString))
      }
      /* TODO: have a div for integer division (i.e., Scheme's quotient), and one for real division (/)). Also, handle division by zero. */
      case Div => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.div(n1, n2))
        case (Int(n1), Float(n2)) => Float(float.div(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Float(float.div(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Float(float.div(n1, n2))
        case _ => OperatorNotApplicable("/", List(x.toString, y.toString))
      }
      case Modulo => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.modulo(n1, n2))
        case _ => OperatorNotApplicable("modulo", List(x.toString, y.toString))
      }
      case Lt => (x, y) match {
        case (Int(n1), Int(n2)) => Bool(int.lt(n1, n2))
        case (Int(n1), Float(n2)) => Bool(float.lt(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Bool(float.lt(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Bool(float.lt(n1, n2))
        case _ => OperatorNotApplicable("<", List(x.toString, y.toString))
      }
      case NumEq => (x, y) match {
        case (Int(n1), Int(n2)) => Bool(int.eql(n1, n2))
        case (Int(n1), Float(n2)) => Bool(float.eql(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Bool(float.eql(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Bool(float.eql(n1, n2))
        case _ => OperatorNotApplicable("number=", List(x.toString, y.toString))
      }
      case Eq => (x, y) match {
        case (Str(s1), Str(s2)) => Bool(str.eql(s1, s2)) /* TODO: this isn't really physical equality for strings */
        case (Bool(b1), Bool(b2)) => Bool(bool.eql(b1, b2))
        case (Int(n1), Int(n2)) => Bool(int.eql(n1, n2))
        case (Float(n1), Float(n2)) => Bool(float.eql(n1, n2))
        case (Char(c1), Char(c2)) => Bool(char.eql(c1, c2))
        case (Symbol(s1), Symbol(s2)) => Bool(sym.eql(s1, s2))
        case (Nil, Nil) => True
        case (Prim(_), Prim(_)) => Bool(bool.inject(x == y))
        case (Closure(_, _), Closure(_, _)) => Bool(bool.inject(x == y))
        case (Cons(_, _), Cons(_, _)) => Bool(bool.inject(x == y))
        case (VectorAddress(_), VectorAddress(_)) => Bool(bool.inject(x == y))
        case _ => False
      }
      case StringAppend => (x, y) match {
        case (Str(s1), Str(s2)) => Str(str.append(s1, s2))
        case _ => OperatorNotApplicable("string-append", List(x.toString, y.toString))
      }
    }

    def and(x: L, y: => L): L = x match {
      case Bot => False
      case Bool(b) => (bool.isTrue(b), bool.isFalse(b)) match {
        case (true, false) => y
        case (false, _) => False
        case (true, true) => join(False, y)
      }
      case _ => y
    }

    def or(x: L, y: => L): L = x match {
      case Bot => y
      case Bool(b) => (bool.isTrue(b), bool.isFalse(b)) match {
        case (true, false) => x
        case (false, _) => y
        case (true, true) => join(x, y)
      }
      case _ => x
    }
    def inject(x: scala.Int): L = Int(int.inject(x))
    def inject(x: scala.Float): L = Float(float.inject(x))
    def inject(x: String): L = Str(str.inject(x))
    def inject(x: scala.Char): L = Char(char.inject(x))
    def inject(x: Boolean): L = Bool(bool.inject(x))
    def inject[Addr : Address, Abs : JoinLattice](x: Primitive[Addr, Abs]): L = Prim(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): L = Closure(x._1, x._2)
    def injectSymbol(x: String): L = Symbol(sym.inject(x))
    def nil: L = Nil
    def cons[Addr : Address](car: Addr, cdr: Addr): L = Cons(car, cdr)

    def getClosures[Exp : Expression, Addr : Address](x: L) = x match {
      case Closure(lam: Exp @unchecked, env: Environment[Addr] @unchecked) => Set((lam, env))
      case _ => Set()
    }
    def getPrimitives[Addr : Address, Abs : JoinLattice](x: L) = x match {
      case Prim(p: Primitive[Addr, Abs] @unchecked) => Set(p)
      case _ => Set()
    }

    def car[Addr : Address](x: L): Set[Addr] = x match {
      case Cons(car: Addr @unchecked, cdr: Addr @unchecked) => Set(car)
      case _ => Set()
    }

    def cdr[Addr : Address](x: L): Set[Addr] = x match {
      case Cons(car: Addr @unchecked, cdr: Addr @unchecked) => Set(cdr)
      case _ => Set()
    }

    def vectorRef[Addr : Address](vector: L, index: L): MayFail[Set[Addr]] = (vector, index) match {
      case (Vec(size, content: Map[I, Addr] @unchecked, init: Addr @unchecked), Int(index)) => {
        val comp = int.lt(index, size)
        val t: Set[Addr] = if (bool.isTrue(comp)) {
          content.get(index) match {
            case Some(a: Addr @unchecked) =>
              if (bool.isTrue(int.eql(index, index)) && !bool.isFalse(int.eql(index, index))) {
                /* we know index represents a concrete integer, we can return only one address */
                Set(a)
              } else {
                /* otherwise, init should be returned as well for soundness */
                Set(a, init)
              }
            case None => Set(init)
          }
        } else { Set() }
        /* Don't perform bound checks here because we would get too many spurious flows */
        val f: Set[Addr] = Set()
        MayFailSuccess(t ++ f)
      }
      case (_: Vec[Addr] @unchecked, _) => MayFailError(List(OperatorNotApplicable("vector-ref", List(vector.toString, index.toString))))
      case _ => MayFailError(List(OperatorNotApplicable("vector-ref", List(vector.toString, index.toString))))
    }

    def vectorSet[Addr : Address](vector: L, index: L, addr: Addr): MayFail[(L, Set[Addr])] = (vector, index) match {
      case (Vec(size, content: Map[I, Addr] @unchecked, init: Addr @unchecked), Int(index)) => {
        val comp = int.lt(index, size)
        val t: (L, Set[Addr]) = if (bool.isTrue(comp)) {
          content.get(index) match {
            case Some(a: Addr @unchecked) => (vector, Set(a))
            case None => (Vec(size, content + (index -> addr), init), Set(addr))
          }
        } else { (Bot, Set()) }
        val f: (L, Set[Addr]) = (Bot, Set())
        MayFailSuccess((join(t._1, f._1), t._2 ++ f._2))
      }
      case (_: Vec[Addr] @unchecked, _) => MayFailError(List(OperatorNotApplicable("vector-set!", List(vector.toString, index.toString, addr.toString))))
      case _ => MayFailError(List(OperatorNotApplicable("vector-set!", List(vector.toString, index.toString, addr.toString))))
    }

    def getVectors[Addr : Address](x: L) = x match {
      case VectorAddress(a: Addr @unchecked) => Set(a)
      case _ => Set()
    }

    def vector[Addr : Address](addr: Addr, size: L, init: Addr): MayFail[(L, L)] = size match {
      case Int(size) => MayFailSuccess((VectorAddress(addr), Vec(size, Map[I, Addr](), init)))
      case _ => MayFailError(List(OperatorNotApplicable("vector", List(addr.toString, size.toString, init.toString))))
    }
  }

  sealed trait LSet
  case class Element(v: Value) extends LSet {
    override def toString = v.toString
  }
  case class Elements(vs: Set[Value]) extends LSet {
    override def toString = "{" + vs.mkString(",") + "}"
  }
  val boolOrMonoid = new Monoid[Boolean] {
    def append(x: Boolean, y: => Boolean): Boolean = x || y
    def zero: Boolean = false
  }
  val boolAndMonoid = new Monoid[Boolean] {
    def append(x: Boolean, y: => Boolean): Boolean = x && y
    def zero: Boolean = true
  }
  private def wrap(x: => Value): LSet = try { Element(x) } catch {
    case err: CannotJoin[Value] @unchecked => Elements(err.values)
  }
  implicit val lsetMonoid = new Monoid[LSet] {
    def append(x: LSet, y: => LSet): LSet = x match {
      case Element(Bot) => y
      case Element(a) => y match {
        case Element(Bot) => x
        case Element(b) => wrap(isSchemeLattice.join(a, b))
        case _: Elements => append(Elements(Set(a)), y)
      }
      case Elements(as) => y match {
        case Element(Bot) => x
        case Element(b) => append(x, Elements(Set(b)))
        case Elements(bs) =>
          /* every element in the other set has to be joined in this set */
          Elements(as.foldLeft(bs)((acc, x2) =>
            if (acc.exists(x1 => isSchemeLattice.subsumes(x1, x2))) {
              /* the set already contains an element that subsumes x2, don't add it to the set */
              acc
            } else {
              /* remove all elements subsumed by x2 and add x2 to the set */
              val subsumed = acc.filter(x1 => isSchemeLattice.subsumes(x2, x1))
              (acc -- subsumed) + x2
            }))
      }
    }
    def zero: LSet = Element(Bot)
  }
  implicit def mayFailMonoid[A](implicit monoid: Monoid[A]): Monoid[MayFail[A]] =
    new Monoid[MayFail[A]] {
      def append(x: MayFail[A], y: => MayFail[A]): MayFail[A] = (x, y) match {
        case (MayFailSuccess(x), MayFailSuccess(y)) => MayFailSuccess(monoid.append(x, y))
        case (MayFailSuccess(x), MayFailError(errs)) => MayFailBoth(x, errs)
        case (MayFailSuccess(x), MayFailBoth(y, errs)) => MayFailBoth(monoid.append(x, y), errs)
        case (MayFailError(errs1), MayFailError(errs2)) => MayFailError(errs1 ++ errs2)
        case (MayFailError(errs1), MayFailBoth(x, errs2)) => MayFailBoth(x, errs1 ++ errs2)
        case (MayFailBoth(x, errs1), MayFailBoth(y, errs2)) => MayFailBoth(monoid.append(x, y), errs1 ++ errs2)
      }
      def zero: MayFail[A] = MayFailSuccess(monoid.zero)
    }
  implicit def setMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def append(x: Set[A], y: => Set[A]): Set[A] = x ++ y
    def zero: Set[A] = Set[A]()
  }
  private def foldMapLSet[B](x: LSet, f: L => B)(implicit b: Monoid[B]): B = x match {
    case Element(x) => f(x)
    case Elements(xs) => xs.foldMap(x => f(x))(b)
  }
  val isSchemeLatticeSet = new IsConvertableLattice[LSet] {
    val name = s"SetLattice(${str.name}, ${bool.name}, ${int.name}, ${float.name}, ${char.name}, ${sym.name})"
    val counting = supportsCounting

    def isTrue(x: LSet): Boolean = foldMapLSet(x, isSchemeLattice.isTrue(_))(boolOrMonoid)
    def isFalse(x: LSet): Boolean = foldMapLSet(x, isSchemeLattice.isFalse(_))(boolOrMonoid)
    def isPrimitiveValue(x: LSet): Boolean = foldMapLSet(x, isSchemeLattice.isPrimitiveValue(_))(boolAndMonoid)
    def unaryOp(op: UnaryOperator)(x: LSet): MayFail[LSet] = foldMapLSet(x, x => isSchemeLattice.unaryOp(op)(x).map(x => wrap(x)))
    def binaryOp(op: BinaryOperator)(x: LSet, y: LSet): MayFail[LSet] = foldMapLSet(x, x => foldMapLSet(y, y => isSchemeLattice.binaryOp(op)(x, y).map(x => wrap(x))))
    def join(x: LSet, y: LSet): LSet = implicitly[Monoid[LSet]].append(x, y)
    /* if we need to define meet at some point, a different representation might be
     * more practical. Using a product of all the domains used is probably thea
     * best, i.e., Value(int: I, bool: B, ..., prims: Set[Primitive]) */
    def meet(x: LSet, y: LSet): LSet = ???
    def subsumes(x: LSet, y: LSet): Boolean = foldMapLSet(y, y =>
      /* For every element in y, there exists an element of x that subsumes it */
      foldMapLSet(x, x => isSchemeLattice.subsumes(x, y))(boolOrMonoid))(boolAndMonoid)
    def and(x: LSet, y: => LSet): LSet = foldMapLSet(x, x => foldMapLSet(y, y => wrap(isSchemeLattice.and(x, y))))
    def or(x: LSet, y: => LSet): LSet = foldMapLSet(x, x => foldMapLSet(y, y => wrap(isSchemeLattice.or(x, y))))
    def car[Addr : Address](x: LSet): Set[Addr] = foldMapLSet(x, x => isSchemeLattice.car(x))
    def cdr[Addr : Address](x: LSet): Set[Addr] = foldMapLSet(x, x => isSchemeLattice.cdr(x))

    def vectorRef[Addr : Address](vector: LSet, index: LSet): MayFail[Set[Addr]] = foldMapLSet(vector, vector => foldMapLSet(index, index =>
      isSchemeLattice.vectorRef(vector, index)))
    def vectorSet[Addr : Address](vector: LSet, index: LSet, addr: Addr): MayFail[(LSet, Set[Addr])] = foldMapLSet(vector, vector => foldMapLSet(index, index =>
      isSchemeLattice.vectorSet(vector, index, addr).map({ case (v, addrs) => (wrap(v), addrs) })))

    def getClosures[Exp : Expression, Addr : Address](x: LSet): Set[(Exp, Environment[Addr])] = foldMapLSet(x, x => isSchemeLattice.getClosures(x))
    def getPrimitives[Addr : Address, Abs : JoinLattice](x: LSet): Set[Primitive[Addr, Abs]] = foldMapLSet(x, x => isSchemeLattice.getPrimitives(x))
    def getVectors[Addr : Address](x: LSet): Set[Addr] = foldMapLSet(x, x => isSchemeLattice.getVectors(x))

    def bottom: LSet = Element(isSchemeLattice.bottom)
    def inject(x: scala.Int): LSet = Element(isSchemeLattice.inject(x))
    def inject(x: scala.Float): LSet = Element(isSchemeLattice.inject(x))
    def inject(x: String): LSet = Element(isSchemeLattice.inject(x))
    def inject(x: scala.Char): LSet = Element(isSchemeLattice.inject(x))
    def inject(x: Boolean): LSet = Element(isSchemeLattice.inject(x))
    def inject[Addr : Address, Abs : JoinLattice](x: Primitive[Addr, Abs]): LSet = Element(isSchemeLattice.inject(x))
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): LSet = Element(isSchemeLattice.inject(x))
    def injectSymbol(x: String): LSet = Element(isSchemeLattice.injectSymbol(x))
    def cons[Addr : Address](car: Addr, cdr: Addr): LSet = Element(isSchemeLattice.cons(car, cdr))
    def vector[Addr : Address](addr: Addr, size: LSet, init: Addr): MayFail[(LSet, LSet)] = foldMapLSet(size, size =>
      isSchemeLattice.vector(addr, size, init).map({ case (a, v) => (Element(a), Element(v)) }))
    def nil: LSet = Element(isSchemeLattice.nil)

    def injectVector[Addr : Address](size: scala.Int, elements: Map[scala.Int, Addr], init: Addr): LSet = {
      val newMap = elements.mapKeys[I]( (i) => int.inject(i) )
      Element(Vec(int.inject(size), newMap, init))
    }
    def injectVectorAddress[Addr : Address](addr: Addr): LSet = Element(VectorAddress[Addr](addr))

    val latticeInfoProvider = lsetInfoProvider
  }

  object lsetInfoProvider extends LatticeInfoProvider[LSet] {

    def isConstantValue(x: LSet): Boolean = x match {
      case Element(e) => e match {
        case Bot => false
        case Str(StringConstantPropagation.Constant(_)) => true
        case Bool(_) => true
        case Int(IntegerConstantPropagation.Constant(_)) => true
        case Float(FloatConstantPropagation.Constant(_)) => true
        case Char(CharConstantPropagation.Constant(_)) => true
        case Symbol(SymbolConstantPropagation.Constant(_)) => true
        case Closure(_, _) => true
        case Prim(_) => true
        case Cons(_, _) => true
        case Nil => true
        case Vec(_, _, _) => true
        case VectorAddress(_) => true
        case _ => false
      }
      /* If the value consists of a set of abstract values (i.e., Elements), the value is not a constant */
      case _ => false
    }

    def pointsTo(x: LSet): scala.Int = {

      def pointsTo(value: Value): Boolean = value match {
        case Symbol(_) | Prim(_) | Closure(_, _) |
             Cons(_, _) | Vec(_, _, _) | VectorAddress(_) => true
        case _ => false
      }

      x match {
        case Element(value) =>
          if (pointsTo(value)) 1 else 0
        case Elements(values) =>
          values.foldLeft[scala.Int](0)((acc, value) => acc + (if (pointsTo(value)) 1 else 0))
      }
    }

  }

}

class ConcreteLattice(counting: Boolean) extends SchemeLattice {
  import ConcreteString._
  import ConcreteBoolean._
  import ConcreteInteger._
  import ConcreteFloat._
  import ConcreteChar._
  import ConcreteSymbol._

  val lattice = new MakeSchemeLattice[S, B, I, F, C, Sym](counting)
  type L = lattice.LSet
  implicit val isSchemeLattice: IsConvertableLattice[L] = lattice.isSchemeLatticeSet
}

object ConcreteConcreteLattice extends SchemeLattice {
  val concreteLattice = new ConcreteLattice(true)
  type L = concreteLattice.L
  implicit val isSchemeLattice: IsConvertableLattice[L] = concreteLattice.isSchemeLattice
}

class TypeSetLattice(counting: Boolean) extends SchemeLattice {
  import Type._
  import ConcreteBoolean._
  val lattice = new MakeSchemeLattice[T, B, T, T, T, T](counting)
  type L = lattice.LSet
  implicit val isSchemeLattice: IsConvertableLattice[L] = lattice.isSchemeLatticeSet
}

class BoundedIntLattice(bound: Int, counting: Boolean) extends SchemeLattice {
  import Type._
  import ConcreteBoolean._
  val bounded = new BoundedInteger(bound)
  import bounded._
  val lattice = new MakeSchemeLattice[T, B, I, T, T, T](counting)
  type L = lattice.LSet
  implicit val isSchemeLattice: IsConvertableLattice[L] = lattice.isSchemeLatticeSet
}

class ConstantPropagationLattice(counting: Boolean) extends SchemeLattice {
  import StringConstantPropagation._
  import ConcreteBoolean._
  import IntegerConstantPropagation._
  import FloatConstantPropagation._
  import CharConstantPropagation._
  import SymbolConstantPropagation._
  val lattice = new MakeSchemeLattice[S, B, I, F, C, Sym](counting)
  type L = lattice.LSet
  implicit val isSchemeLattice: IsConvertableLattice[L] = lattice.isSchemeLatticeSet


}
