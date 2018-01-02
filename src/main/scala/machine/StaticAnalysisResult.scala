trait StaticAnalysisResult

case object NoStaticisAnalysisResult extends StaticAnalysisResult
case class ConstantAddresses[Addr: Address](constants: Set[Addr], nonConstants: Set[Addr]) extends StaticAnalysisResult
case class PointsToSet[Addr: Address](pointsTo: List[(Addr, Option[Int])]) extends StaticAnalysisResult
case class AnalysisOutputGraph[Exp, Abs, Addr, State <: StateTrait[Exp, Abs, Addr, _]](hasGraph: HasGraph[Exp, Abs, Addr, State] with HasFinalStores[Addr, Abs]) extends StaticAnalysisResult