import backend._
import backend.tree.path._

class ErrorPathDetector[Exp : Expression, Abs : IsSchemeLattice, Addr : Address, Time : Timestamp]
  (val aam: KickstartAAMGlobalStore[Exp, Abs, Addr, Time]) {

  type RelevantGraph =  Graph[aam.State, EdgeAnnotation[Exp, Abs, Addr], Unit]
  type Bindings = List[Binding]

  case class Binding(edge: EdgeAnnotation[Exp, Abs, Addr], state: aam.State) {
    override def toString: String = state.toString
  }

  def detectErrors(graph: RelevantGraph): Option[Set[Regex]] = {

    @scala.annotation.tailrec
    def loop(visited: Set[aam.State], worklist: List[Bindings], acc: Set[Bindings]): Set[Bindings] = worklist.headOption match {
      case None =>
        acc
      case Some(path) if visited.contains(path.last.state) =>
        loop(visited, worklist.tail, acc)
      case Some(path) if path.last.state.isErrorState && ! acc.exists(binding => binding.last.state == path.last.state) =>
        /* Only consider this path if this exact error state does not already appear in any other error path that was collected */
        loop(visited + path.last.state, worklist.tail, acc + path)
      case Some(path) if path.last.state.halted =>
        /* Note that an error state is also a halted state */
        loop(visited + path.last.state, worklist.tail, acc)
      case Some(path) =>
        val lastState = path.last.state
        val outgoingEdges = graph.nodeEdges(lastState)
        val newWork: List[Bindings] = outgoingEdges.toList.map( (tuple) => path :+ Binding(tuple._1, tuple._2) )
        val newWorklist: List[Bindings] = worklist.tail ++ newWork
        loop(visited + lastState, newWorklist, acc)
    }

    val elseBranchChar = 'e'
    val thenBranchChar = 't'
    def annotToOptChar(annot: EdgeAnnotation[Exp, Abs, Addr]): Option[Char] = {
      val (elseBranch, thenBranch) = (annot.filters.semanticsFilters.contains(ElseBranchFilter), annot.filters.semanticsFilters.contains(ThenBranchFilter))
      /* Edge cannot indicate that both the else- and the then-branch have been taken. */
      assert(! (elseBranch && thenBranch), "Should not happen")
      if (elseBranch) {
        Some(elseBranchChar)
      } else if (thenBranch) {
        Some(thenBranchChar)
      } else {
        None
      }
    }
    def charToT(char: Char): SymbolicTreeEdge = char match {
      case c if c == elseBranchChar => ElseBranchTaken
      case c if c == thenBranchChar => ThenBranchTaken
    }

    graph.getNode(0) match {
      case None =>
        Logger.log("Graph is empty", Logger.U)
        None
      case Some(root) =>
        // TODO Using automaton approach now...
//        val start = List(Binding(EdgeAnnotation.dummyEdgeAnnotation, root))
//        val errorPaths = loop(Set(), List(start), Set())
//        errorPaths.map(filterBranchesTaken)
        val regexes = new TransitiveClosure(graph, (state: aam.State) => state.isErrorState, annotToOptChar, charToT).shortestPaths
        regexes
    }
  }

  private def filterBranchesTaken(path: Bindings): Path = {
    path.flatMap({
      case Binding(edge, _) =>
        if (edge.filters.semanticsFilters.contains(ThenBranchFilter)) {
          List(backend.tree.path.ThenBranchTaken)
        } else if (edge.filters.semanticsFilters.contains(ElseBranchFilter)) {
          List(backend.tree.path.ElseBranchTaken)
        } else {
          List()
        }
    })
  }

}
