import backend.tree.path._

package object error_paths {

  sealed trait RegExp

  sealed trait AtomicRegExp extends RegExp
  case object AtomicRegExpElse extends AtomicRegExp {
    override def toString: String = "Else"
  }
  case object AtomicRegExpThen extends AtomicRegExp {
    override def toString: String = "Then"
  }
  case object AtomicRegExpEmpty extends AtomicRegExp {
    override def toString: String = "ε"
  }

  case class RegExpKleeneStar(regExp: RegExp) extends RegExp
  case class RegExpPath(path: List[RegExp]) extends RegExp

  import scala.language.implicitConversions
  implicit def symbolicTreeEdgeToRegExpSymbol(symbolicTreeEdge: SymbolicTreeEdge): AtomicRegExp = symbolicTreeEdge match {
    case ElseBranchTaken => AtomicRegExpElse
    case ThenBranchTaken => AtomicRegExpThen
  }
  implicit def regExpSymbolToSymbolicTreeEdge(regExpSymbol: AtomicRegExp): SymbolicTreeEdge = regExpSymbol match {
    case AtomicRegExpElse => ElseBranchTaken
    case AtomicRegExpThen => ThenBranchTaken
  }

}
