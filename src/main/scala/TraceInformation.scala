/**
  * Created by mvdcamme on 21/03/16.
  */
trait TraceInformation[Abs]

case class PrimitiveAppliedInfo[Abs: AbstractValue, Addr : Address](v: Abs, vStack: List[Storable[Abs, Addr]]) extends TraceInformation[Abs]
case class DefaultInfo[Abs: AbstractValue, Addr : Address](vStack: List[Storable[Abs, Addr]]) extends TraceInformation[Abs]
