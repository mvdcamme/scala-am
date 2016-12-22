trait StaticAnalysisResult

case object NoStaticisAnalysisResult extends StaticAnalysisResult
case class ConstantAddresses[Addr: Address](constants: Set[Addr],
                                            nonConstants: Set[Addr])
    extends StaticAnalysisResult
case class PointsToSet[Addr: Address](pointsTo: List[(Addr, Option[Int])])
    extends StaticAnalysisResult
case class AnalysisGraph[State, Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
  (graph:Graph[State, (List[EdgeAnnotation], List[StateChangeEdge[Exp, Abs, Addr, Time]])])
  extends StaticAnalysisResult