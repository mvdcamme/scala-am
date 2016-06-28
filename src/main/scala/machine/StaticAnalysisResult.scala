trait StaticAnalysisResult

case object NoStaticisAnalysisResult extends StaticAnalysisResult
case class NonConstantAddresses(addresses: Set[HybridAddress.A]) extends StaticAnalysisResult