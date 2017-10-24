case class BranchConstraint(exp: RelationalConcolicExpression) {
  def negate: BranchConstraint = {
    BranchConstraint(exp.negate)
  }
}

trait ConcolicExpression

case class ArithmeticalConcolicExpression(op: String, exps: List[ConcolicExpression]) extends ConcolicExpression {
  override def toString: String = {
    exps.mkString(s" $op ")
  }
}

case class RelationalConcolicExpression(left: ConcolicExpression, op: String, right: ConcolicExpression) extends ConcolicExpression {
  override def toString: String = {
    s"$left $op $right"
  }
  def getOp = Some(op)
  def getRhs = Some(right.toString)

  def negate: RelationalConcolicExpression = op match {
    case "<" =>
      RelationalConcolicExpression(left, ">=", right)
    case "<=" =>
      RelationalConcolicExpression(left, ">", right)
    case ">" =>
      RelationalConcolicExpression(left, "<=", right)
    case ">=" =>
      RelationalConcolicExpression(left, "<", right)
    case "=" =>
      RelationalConcolicExpression(left, "!=", right)
  }
}

case class ConcolicInt(i: Int) extends ConcolicExpression {
  override def toString: String = {
    i.toString
  }
}

//case class ConcolicVariable(symbolicName: String, originalName: String) extends ConcolicAtom {
//  override def toString: String = {
//    s"$symbolicName"
//  }
//}

case class ConcolicInput(id: Int, fexp: SchemeExp) extends ConcolicExpression {
  override def toString: String = {
    s"i$id"
  }
}

object ConcolicIdGenerator {
  private var id: Int = 0

  def resetId(): Unit = {
    id = 0
  }

  def newConcolicInput(fexp: SchemeExp): ConcolicInput = {
    val current = id
    id += 1
    ConcolicInput(current, fexp)
  }
}
