object RegexPartialMatcher {

  type M[T] = Option[Set[Regex[T]]]

  def symbolMatchesRegex[T](symbol: T, regex: Regex[T]): M[T] = regex match {
    case Atomic(s) if s == symbol => Some(Set())
    case Atomic(_) => None
    case EmptySet() => None // throw new Exception("Should not happen")
    case EmptyWord() => Some(Set()) // throw new Exception("Should not happen")
    case Concat(s, r) =>
      symbolMatchesRegex(symbol, s).map(_ => Set(r))
    case Or(r1, r2) =>
      val m1 = symbolMatchesRegex(symbol, r1)
      val m2 = symbolMatchesRegex(symbol, r2)
      combineOptSets(m1, m2)
  }

  private def combineOptSets[T](optSet1: M[T], optSet2: M[T]): M[T] = (optSet1, optSet2) match {
    case (None, None) => None
    case (None, Some(set)) => Some(set)
    case (Some(set), None) => Some(set)
    case (Some(set1), Some(set2)) => Some(set1 ++ set2)
  }

  def symbolMatchesRegexes[T](symbol: T, regexes: Set[Regex[T]]): M[T] = {
    regexes.foldLeft[M[T]](None)((acc, regex) => {
      val singleResult = symbolMatchesRegex(symbol, regex)
      combineOptSets(acc, singleResult)
    })
  }

}
