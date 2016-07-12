/**
  * Created by mvdcamme on 21/03/16.
  */
trait TraceInformation[Abs, Addr]

case class AddressesAllocated[Abs : JoinLattice, Addr : Address]
(addresses: List[Addr]) extends TraceInformation[Abs, Addr]
case class AddressesReassigned[Abs : JoinLattice, Addr : Address]
(addresses: List[Addr]) extends TraceInformation[Abs, Addr]
case class PrimitiveAppliedInfo[Abs : JoinLattice, Addr : Address]
  (v: Abs, vStack: List[Storable[Abs, Addr]]) extends TraceInformation[Abs, Addr]
case class VariableLookedUp[Abs : JoinLattice, Addr : Address]
  (variable: String, address: Addr, value: Abs) extends TraceInformation[Abs, Addr]

class CombinedInfos[Abs : JoinLattice, Addr : Address](val infos: Set[TraceInformation[Abs, Addr]]) {

  def join(other: CombinedInfos[Abs, Addr]): CombinedInfos[Abs, Addr] = new CombinedInfos[Abs, Addr](infos ++ other.infos)

  def filter[SpecificInfo] = new CombinedInfos[Abs, Addr](infos.filter(_.isInstanceOf[SpecificInfo]))

  def find[Result](pred: (TraceInformation[Abs, Addr]) => Boolean,
                   todoIfFound: (TraceInformation[Abs, Addr]) => Result): Option[Result] =
  infos.find(pred) match {
    case Some(found) =>
      val result = todoIfFound(found)
      Some(result)
    case None => None
  }
  def flatMap[Result](pred: (TraceInformation[Abs, Addr]) => Boolean,
                      todoIfFound: (TraceInformation[Abs, Addr]) => Option[Result]): Option[Result] =
    find[Option[Result]](pred, todoIfFound) match {
      case Some(result) => result
      case None => None
    }
}

object TraceInfos {
  def nil[Abs : JoinLattice, Addr : Address]: CombinedInfos[Abs, Addr] =
    new CombinedInfos(Set())
  def multiple[Abs : JoinLattice, Addr : Address](infos: Set[TraceInformation[Abs, Addr]]): CombinedInfos[Abs, Addr] =
    new CombinedInfos(infos)
  def single[Abs : JoinLattice, Addr : Address](info: TraceInformation[Abs, Addr]): CombinedInfos[Abs, Addr] =
    new CombinedInfos(Set(info))
}