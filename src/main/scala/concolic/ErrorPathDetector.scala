import backend.path._

class ErrorPathDetector[Exp : Expression, Abs : IsSchemeLattice, Addr : Address, Time : Timestamp]
  (val aam: KickstartAAM[Exp, Abs, Addr, Time]) {

  type RelevantGraph =  Graph[aam.State, EdgeAnnotation[Exp, Abs, Addr], Unit]
  type Path = List[Binding]

  case class Binding(edge: EdgeAnnotation[Exp, Abs, Addr], state: aam.State) {
    override def toString: String = state.toString
  }

  def detectErrors(graph: RelevantGraph): List[List[ExecutionTreeEdge]] = {

    @scala.annotation.tailrec
    def loop(visited: Set[aam.State], worklist: List[Path], acc: List[Path]): List[Path] = worklist.headOption match {
      case None =>
        acc
      case Some(path) if visited.contains(path.last.state) =>
        loop(visited, worklist.tail, acc)
      case Some(path) if path.last.state.isErrorState  =>
        loop(visited + path.last.state, worklist.tail, acc :+ path)
      case Some(path) if path.last.state.halted =>
        loop(visited + path.last.state, worklist.tail, acc)
      case Some(path) =>
        val lastState = path.last.state
        val outgoingEdges = graph.nodeEdges(lastState)
        val newWork: List[Path] = outgoingEdges.toList.map( (tuple) => path :+ Binding(tuple._1, tuple._2) )
        val newWorklist: List[Path] = worklist.tail ++ newWork
        loop(visited + lastState, newWorklist, acc)
    }

    graph.getNode(0) match {
      case None =>
        Logger.log("Graph is empty", Logger.U)
        Nil
      case Some(root) =>
        val start = List(Binding(EdgeAnnotation.dummyEdgeAnnotation, root))
        val errorPaths = loop(Set(), List(start), Nil)
        errorPaths.map(filterBranchesTaken)
    }
  }

  private def filterBranchesTaken(path: Path): List[ExecutionTreeEdge] = {
    path.flatMap({
      case Binding(edge, _) =>
        if (edge.filters.semanticsFilters.contains(ThenBranchTaken)) {
          List(backend.path.ThenBranchTaken)
        } else if (edge.filters.semanticsFilters.contains(ElseBranchTaken)) {
          List(backend.path.ElseBranchTaken)
        } else {
          List()
        }
    })
  }

}
