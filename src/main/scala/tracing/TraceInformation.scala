/**
  * Created by mvdcamme on 21/03/16.
  */
trait TraceInformation[Abs, Addr]

case class PrimitiveAppliedInfo[Abs : JoinLattice, Addr : Address]
  (v: Abs, vStack: List[Storable[Abs, Addr]])
  extends TraceInformation[Abs, Addr]
case class VariablesAllocated[Abs : JoinLattice, Addr : Address]
  (addresses: List[Addr])
  extends TraceInformation[Abs, Addr]
