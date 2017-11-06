import scalaz._
import scalaz.Scalaz._

class CSchemePrimitives[Addr : Address, Abs : IsCSchemeLattice] extends SchemePrimitives[Addr, Abs] {
  object NewLock extends Primitive[Addr, Abs] {
    val name = "new-lock"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case Nil =>
        val a = Address[Addr].cell(fexp, t)
        MayFailSuccess((IsCSchemeLattice[Abs].lock(a), store.extend(a, IsCSchemeLattice[Abs].unlockedValue), Set()))
      case l => MayFailError(List(ArityError(name, 0, l.size)))
    }
    def convert[Addr: Address, Abs: IsConvertableLattice](prims: SchemePrimitives[Addr, Abs]): Primitive[Addr, Abs] =
      prims.asInstanceOf[CSchemePrimitives[Addr, Abs]].NewLock
  }

  override def all = NewLock :: super.all
}
