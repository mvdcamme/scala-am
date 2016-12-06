trait EdgeInformation

case object NoEdgeInformation extends EdgeInformation {
  override def toString = "void"
}
case object TODOEdgeInformation extends EdgeInformation

/*
 * Control-flow split due to if: took else or then.
 */
case object ElseBranchTaken extends EdgeInformation {
  override def toString = "Else"
}
case object ThenBranchTaken extends EdgeInformation {
  override def toString = "Then"
}

/*
 * Control-flow split due to operator-value: check operator's AST.
 */
case class OperatorTaken[Exp : Expression](body: List[Exp]) extends EdgeInformation {
  override def toString = body.toString()
}

/*
 * Control-flow split due to spurious return: check continuation frame used.
 */
case class FrameFollowed(frame: Frame) extends EdgeInformation {
  override def toString = frame.toString
}

case object StateSubsumed extends EdgeInformation