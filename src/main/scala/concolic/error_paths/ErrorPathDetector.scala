import backend._
import backend.tree.path._

class ErrorPathDetector[Exp : Expression, Abs : IsSchemeLattice, Addr : Address, Time : Timestamp]
  (val aam: KickstartAAMGlobalStore[Exp, Abs, Addr, Time]) {

  type RelevantGraph =  Graph[aam.State, EdgeAnnotation[Exp, Abs, Addr], Unit]

  private val elseBranchChar = 'e'
  private val thenBranchChar = 't'

  /**
    * Converts a given EdgeAnnotation to its character-representation, if applicable.
    * An EdgeAnnotation that represents a else-branch that is taken is converted to the character 'e'.
    * An EdgeAnnotation that represents a true-branch that is taken is converted to the character 't'.
    * @param annot The EdgeAnnotation to convert.
    * @return
    */
  private def annotToOptChar(annot: EdgeAnnotation[Exp, Abs, Addr]): Option[Char] = {
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

  def detectErrors(graph: RelevantGraph): Option[Set[Regex]] = graph.getNode(0) match {
    case None =>
      Logger.log("Graph is empty", Logger.U)
      None
    case Some(_) =>
      val transitiveClosure = new TransitiveClosure(graph, (state: aam.State) => state.isErrorState, annotToOptChar)
      val regexes = transitiveClosure.computeRegexes
      regexes
  }

}