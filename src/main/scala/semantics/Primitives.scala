import backend.expression._

/**
 * Each primitive has to implement this trait.
 */
trait Primitive[Addr, Abs] {
  type Arg = (Abs, Option[ConcolicExpression])

  def mapSymbolicArgs(args: List[Arg]): Option[List[ConcolicExpression]] = {
    args.foldLeft[Option[List[ConcolicExpression]]](Some(Nil))((optAcc, arg) => optAcc.flatMap(acc => arg._2.map(arg => acc :+ arg)))
  }
  def mapJustSymbolicArgs(args: List[Option[ConcolicExpression]]): Option[List[ConcolicExpression]] = {
    args.foldLeft[Option[List[ConcolicExpression]]](Some(Nil))((optAcc, arg) => optAcc.flatMap(acc => arg.map(arg => acc :+ arg)))
  }

  /** The name of the primitive */
  val name: String
  /** Calls the primitive.
   * @param fexp: the expression with which the primitive has been called
   * @param args: the arguments with which the primitive has been called, both their expression and their value
   * @param store: the store
   * @return either an error, or the value returned by the primitive along with the updated store
   */
  def call[Exp : Expression, Time : Timestamp](
    fexp : Exp,
    args: List[(Exp, Arg)],
    store: Store[Addr, Abs],
    t: Time): (MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])], Option[ConcolicExpression])
  def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs]
}

abstract class Primitives[Addr : Address, Abs : JoinLattice] {
  def all: List[Primitive[Addr, Abs]]
  def toVal(prim: Primitive[Addr, Abs]): Abs

  /** Modify a primitive to trace it: output will be printed when the primitive is
    * called. This is for debugging purposes. */
  def traced(prim: Primitive[Addr, Abs]): Primitive[Addr, Abs] = new Primitive[Addr, Abs] {
    val name = prim.name
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Arg)], store: Store[Addr, Abs], t: Time) = {
      val argsstr = args.map({ case (_, (v, cv)) => v.toString }).mkString(" ")
      val res = prim.call(fexp, args, store, t)
      print("($name $argsstr) -> ")
      res match {
        case (MayFailError(errs), _) => "ERROR: " + errs.mkString(" | ERROR: ")
        case (MayFailSuccess((v, store, effs)), _) => v.toString
        case (MayFailBoth((v, store, effs), errs), _) => v.toString + " | ERROR: " + errs.mkString(" | ERROR: ")
      }
      res
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.traced(prim.convert(prims))
  }

  lazy val bindings = ("bottom", Address[Addr].botAddress, JoinLattice[Abs].bottom) :: all.map({ prim => (prim.name, Address[Addr].primitive(prim.name), toVal(prim)) })
}
