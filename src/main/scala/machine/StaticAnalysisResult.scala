trait StaticAnalysisResult

case object NoStaticisAnalysisResult extends StaticAnalysisResult
case class ConstantAddresses[Addr : Address](
    constants: Set[Addr],
    nonConstants: Set[Addr])
  extends StaticAnalysisResult