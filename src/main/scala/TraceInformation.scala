/**
  * Created by mvdcamme on 21/03/16.
  */
trait TraceInformation[Abs]

case class PrimitiveAppliedInfo[Abs: AbstractValue](v: Abs, vStack: List[Storable]) extends TraceInformation[Abs]
