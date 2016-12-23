trait StaticAnalysisResult

case object NoStaticisAnalysisResult extends StaticAnalysisResult
case class ConstantAddresses[Addr: Address](constants: Set[Addr],
                                            nonConstants: Set[Addr])
    extends StaticAnalysisResult
case class PointsToSet[Addr: Address](pointsTo: List[(Addr, Option[Int])])
    extends StaticAnalysisResult
case class AnalysisGraph[State <: StateTrait[_, _, _, _]]
  (graph:Graph[State, (List[EdgeAnnotation], List[StateChangeEdge[State]])])
  extends StaticAnalysisResult