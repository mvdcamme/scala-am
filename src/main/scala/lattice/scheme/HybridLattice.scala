import scalaz.ISet

object SimpleTypes extends Enumeration {
  val Boolean, Bottom, Char, Closure, Cons, Float, Integer, Nil,
      Primitive, String, Symbol, Top, Vector, VectorAddress = Value
}

//trait IsConvertableLattice[L] extends IsSchemeLattice[L] {
//
//  def injectVector[Addr : Address](size: Int, elements: Map[Int, Addr], init: Addr): L
//  def injectVectorAddress[Addr : Address](a: Addr): L
//
//  val latticeInfoProvider: LatticeInfoProvider[L] TODO
//
//}