import scalaz.{Plus => _, _}
import scalaz.Scalaz._
import SchemeOps._

case class CannotAccessVector(vector: String) extends SemanticError
case class CannotAccessCar(v: String) extends SemanticError
case class CannotAccessCdr(v: String) extends SemanticError

<<<<<<< HEAD
trait MayFail[L] {
  def map[A](f: L => A): MayFail[A]
  def bind[A](f: L => MayFail[A]): MayFail[A]
  def addError(err: SemanticError): MayFail[L]
  def extract: L
  def value: Option[L]
  def errors: List[SemanticError]
  def collect[A](success: L => Set[A], error: SemanticError => Set[A]): Set[A]
}
case class MayFailSuccess[L](l: L) extends MayFail[L] {
  def map[A](f: L => A) = MayFailSuccess[A](f(l))
  def bind[A](f: L => MayFail[A]) = f(l)
  def addError(err: SemanticError) = MayFailBoth[L](l, List(err))
  def extract = l
  def value = Some(l)
  def errors = List()
  def collect[A](success: L => Set[A], error: SemanticError => Set[A]) =
    success(l)
}
case class MayFailError[L](errs: List[SemanticError]) extends MayFail[L] {
  def map[A](f: L => A) = MayFailError[A](errs)
  def bind[A](f: L => MayFail[A]) = MayFailError[A](errs)
  def addError(err: SemanticError) = MayFailError[L](errs :+ err)
  def extract = throw new Exception("Cannot extract value from MayFailError")
  def value = None
  def errors = errs
  def collect[A](success: L => Set[A], error: SemanticError => Set[A]) =
    errs.toSet.foldMap(error)
}
case class MayFailBoth[L](l: L, errs: List[SemanticError]) extends MayFail[L] {
  def map[A](f: L => A) = MayFailBoth(f(l), errs)
  def bind[A](f: L => MayFail[A]) = f(l) match {
    case MayFailSuccess(a) => MayFailBoth[A](a, errs)
    case MayFailError(errs2) => MayFailError(errs ++ errs2)
    case MayFailBoth(a, errs2) => MayFailBoth[A](a, errs ++ errs2)
  }
  def addError(err: SemanticError) = MayFailBoth[L](l, errs :+ err)
  def extract = l
  def value = Some(l)
  def errors = errs
  def collect[A](success: L => Set[A], error: SemanticError => Set[A]) =
    success(l) ++ errs.toSet.foldMap(error)
}

object MayFail {
  implicit def monoid[A](implicit monoid: Monoid[A]): Monoid[MayFail[A]] =
    new Monoid[MayFail[A]] {
      def append(x: MayFail[A], y: => MayFail[A]): MayFail[A] = (x, y) match {
        case (MayFailSuccess(x), MayFailSuccess(y)) =>
          MayFailSuccess(monoid.append(x, y))
        case (MayFailSuccess(x), MayFailError(errs)) => MayFailBoth(x, errs)
        case (MayFailSuccess(x), MayFailBoth(y, errs)) =>
          MayFailBoth(monoid.append(x, y), errs)
        case (MayFailError(errs1), MayFailError(errs2)) =>
          MayFailError(errs1 ++ errs2)
        case (MayFailError(errs1), MayFailBoth(x, errs2)) =>
          MayFailBoth(x, errs1 ++ errs2)
        case (MayFailBoth(x, errs1), MayFailBoth(y, errs2)) =>
          MayFailBoth(monoid.append(x, y), errs1 ++ errs2)
        case (x, y) => append(y, x)
      }
      def zero: MayFail[A] = MayFailSuccess(monoid.zero)
    }
}

=======
>>>>>>> 9de48f824fa56370876d922b957948f007216898
/** A lattice for Scheme should support the following operations */
trait IsSchemeLattice[L] extends JoinLattice[L] {

  /** Can this value be considered true for conditionals? */
  def isTrue(x: L): Boolean

  /** Can this value be considered false for conditionals? */
  def isFalse(x: L): Boolean

  /** Performs a unary operation on the abstract value x */
  def unaryOp(op: SchemeOps.UnaryOperator)(x: L): MayFail[L]

  /** Performs a binary operation on abstract values x and y */
  def binaryOp(op: SchemeOps.BinaryOperator)(x: L, y: L): MayFail[L]

  /** Conjunction */
<<<<<<< HEAD
  def and(x: L, y: => L): L

  /** Disjunction */
  def or(x: L, y: => L): L

=======
  def and(x: L, y: => L): L = (isTrue(x), isFalse(x)) match {
    case (true, false) => y /* x is true: return y */
    case (false, true) => inject(false) /* x is false: return false */
    case (false, false) => bottom /* x is not true nor false, it is therefore bottom */
    case (true, true) => join(y, inject(false)) /* either x is true, and we have y, or x is false, and we have false */
  }
  /** Disjunction */
  def or(x: L, y: => L): L = (isTrue(x), isFalse(x)) match {
    case (true, false) => x /* x is true, return x */
    case (false, true) => y /* x is false, return y */
    case (false, false) => bottom /* x is not true nor false, it is therefore bottom */
    case (true, true) => join(x, y) /* either x is true, and we have x, or x is false, and we have y */
  }
>>>>>>> 9de48f824fa56370876d922b957948f007216898
  /** Extract closures contained in this value */
  def getClosures[Exp: Expression, Addr: Address](
      x: L): Set[(Exp, Environment[Addr])]

  /** Extract primitives contained in this value */
  def getPrimitives[Addr: Address, Abs: JoinLattice](
      x: L): Set[Primitive[Addr, Abs]]

  /** Injection of an integer */
  def inject(x: Int): L
<<<<<<< HEAD

  /** Injection of a float */
  def inject(x: Float): L

=======
  /** The top integer */
  def intTop: L
  /** Injection of a float */
  def inject(x: Double): L
>>>>>>> 9de48f824fa56370876d922b957948f007216898
  /** Injection of a string */
  def inject(x: String): L

  /** Injection of a boolean */
  def inject(x: Boolean): L
<<<<<<< HEAD

=======
  /** The top boolean */
  def boolTop: L = join(inject(true), inject(false))
>>>>>>> 9de48f824fa56370876d922b957948f007216898
  /** Injection of a character */
  def inject(x: Char): L

  /** Injection of a primitive function */
  def inject[Addr: Address, Abs: JoinLattice](x: Primitive[Addr, Abs]): L

  /** Injection of a closure */
  def inject[Exp: Expression, Addr: Address](x: (Exp, Environment[Addr])): L

  /** Injection of a symbol */
  def injectSymbol(x: String): L

  /** Creates a cons cell */
  def cons[Addr: Address](car: Addr, cdr: Addr): L

  /** Nil value */
  def nil: L

  /** Takes the car of a cons cell */
  def car[Addr: Address](x: L): Set[Addr]

  /** Takes the cdr of a cons cell */
  def cdr[Addr: Address](x: L): Set[Addr]

  /** Get a value from a vector. Returns the addresses where to look for the values */
  def vectorRef[Addr: Address](vector: L, index: L): MayFail[Set[Addr]]

  /** Changes a value inside a vector. The address given is an address where the
    * value can be stored if needed. Returns the vector value, as well as the
    * addresses to update in the store. The value stored is not passed to
    * vectorSet, but will be stored in the returned addresses. */
  def vectorSet[Addr: Address](vector: L,
                               index: L,
                               addr: Addr): MayFail[(L, Set[Addr])]

  /** Extract vector addresses contained in this value */
  def getVectors[Addr: Address](x: L): Set[Addr]

  /** Creates a vector of the given size, where the initial value lies at
    * address. Return the vector address wrapped in a lattice value, as well as
    * the vector value itsel */
  def vector[Addr: Address](addr: Addr, size: L, init: Addr): MayFail[(L, L)]

  trait SchemeLatticeLaw extends MonoidLaw {
    import scalaz.std.boolean.conditional
    def bottomSubsumesItself: Boolean = subsumes(bottom, bottom)
    def bottomAlwaysSubsumed(x: L): Boolean = subsumes(x, bottom) && conditional(x != bottom, !subsumes(bottom, x))
    def joinedWithSubsumesRemainsEqual(x: L, y: L): Boolean = conditional(subsumes(x, y), join(x, y) == x)
    def joinedWithBottomRemainsEqual(x: L): Boolean = join(x, bottom) == x
    def joinedSubsumes(x: L, y: L): Boolean = {
      val xy = join(x, y)
      subsumes(xy, x) && subsumes(xy, y)
    }
    def joinedSubsumes3(x: L, y: L, z: L): Boolean = {
      /* Due to a bug detected on commit 7546a519, where {#t, Str, Int} did not subsume Str */
      val xyz = join(x, join(y, z))
      subsumes(xyz, x) && subsumes(xyz, y) && subsumes(xyz, z)
    }
    def injectBoolPreservesTruth: Boolean = isTrue(inject(true)) && isFalse(inject(false))
    def bottomNeitherTrueNorFalse: Boolean = !isTrue(bottom) && !isFalse(bottom)
    def boolTopIsTrueAndFalse: Boolean = {
      val boolTop = join(inject(true), inject(false))
      isTrue(boolTop) && isFalse(boolTop)
    }
    def unaryOpPreservesBottom(op: SchemeOps.UnaryOperator): Boolean =
      unaryOp(op)(bottom).extract == Some(bottom)
    def binaryOpPreservesBottom(op: SchemeOps.BinaryOperator, v: L): Boolean = {
      binaryOp(op)(bottom, bottom).extract == Some(bottom) &&
      binaryOp(op)(bottom, v).extract == Some(bottom) &&
      binaryOp(op)(v, bottom).extract == Some(bottom)
    }
    def notIsCorrect: Boolean = {
      unaryOp(UnaryOperator.Not)(inject(true)).map(isFalse).extract == Some(true) &&
      unaryOp(UnaryOperator.Not)(inject(false)).map(isTrue).extract == Some(true)
    }
    def andIsCorrect(b1: Boolean, b2: Boolean): Boolean = {
      val v1 = inject(b1)
      val v2 = inject(b2)
      if (b1 && b2) isTrue(and(v1, v2)) else isFalse(and(v1, v2))
    }
    def orIsCorrect(b1: Boolean, b2: Boolean): Boolean = {
      val v1 = inject(b1)
      val v2 = inject(b2)
      if (b1 || b2) isTrue(or(v1, v2)) else isFalse(or(v1, v2))
    }
    def ltIsCorrect(n1: Int, n2: Int): Boolean = {
      val v1 = inject(n1)
      val v2 = inject(n2)
      conditional(n1 < n2,
        (binaryOp(BinaryOperator.Lt)(v1, v2).extract.map(isTrue).getOrElse(false)) &&
          (binaryOp(BinaryOperator.Lt)(v2, v1).extract.map(isFalse).getOrElse(false)))
    }
    def eqIsCorrect(n1: Int, n2: Int): Boolean = {
      val v1 = inject(n1)
      val v2 = inject(n2)
      if (n1 == n2) {
        (binaryOp(BinaryOperator.Eq)(v1, v2).extract.map(isTrue).getOrElse(false) &&
          binaryOp(BinaryOperator.Eq)(v2, v1).extract.map(isTrue).getOrElse(false))
      } else {
        (binaryOp(BinaryOperator.Eq)(v1, v2).extract.map(isFalse).getOrElse(false) &&
          binaryOp(BinaryOperator.Eq)(v2, v1).extract.map(isFalse).getOrElse(false))
      }
    }
    /* TODO: more properties */
  }
  val schemeLatticeLaw = new SchemeLatticeLaw {}
}

object IsSchemeLattice {
  def apply[L : IsSchemeLattice]: IsSchemeLattice[L] = implicitly
}

/* TODO: something like that:
import scala.language.higherKinds
trait Wrapper[F[_]] {
  type T
  val isInstance: F[T]
}
 */
trait SchemeLattice {
  type L
  val isSchemeLattice: IsSchemeLattice[L]
}
<<<<<<< HEAD

trait SchemeConvertableLattice extends SchemeLattice {
  override val isSchemeLattice: IsConvertableLattice[L]
  val latticeInfoProvider: LatticeInfoProvider[L]
}

/** A lattice for Concurrent Scheme */
trait IsConcurrentSchemeLattice[L] extends IsSchemeLattice[L] {

  /** Extract thread ids contained in this value */
  def getTids[TID: ThreadIdentifier](x: L): Set[TID]

  /** Extract lock addresses contained in this value */
  def getLocks[Addr: Address](x: L): Set[Addr]

  /** Inject a thread id */
  def injectTid[TID: ThreadIdentifier](tid: TID): L

  /** Creates a lock wrapper (that contains the address of the lock) */
  def lock[Addr: Address](addr: Addr): L

  /** The locked value */
  def lockedValue: L

  /** The unlocked value */
  def unlockedValue: L
}
=======
>>>>>>> 9de48f824fa56370876d922b957948f007216898
