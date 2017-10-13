trait ConcolicConstraint {
  override def toString: String
  def getOp: Option[String]
  def getRhs: Option[String]
}

case class StatementConstraint(variableName: String, exp: ConcolicExpression) extends ConcolicConstraint {
  override def toString: String = {
    s"Statement: $variableName = $exp"
  }
  def getOp = {
    Some("=")
  }
  def getRhs = {
    Some(exp.toString)
  }
}

/**
  *
  * @par trueExp The concolic expression that was true when this constraint was generated.
  * @par originalExp The original expression corresponding to the if-expression that was generated. This expression
  *                    may or may not have evaluated to #t. If it did, this expression is equal to trueExp. Otherwise,
  *                    trueExp is the negated form of this expression.
  */
case class BranchConstraint(exp: RelationalConcolicExpression) extends ConcolicConstraint {
  override def toString: String = {
    s"Branch: $exp"
  }
  def getOp = exp.getOp
  def getRhs = exp.getRhs

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

case class ConcolicInput(id: Int) extends ConcolicExpression {
  override def toString: String = {
    s"i$id"
  }
}

object ConcolicIdGenerator {
  private var id: Int = 0

  def resetId(): Unit = {
    id = 0
  }

  def newConcolicInput: ConcolicInput = {
    val current = id
    id += 1
    ConcolicInput(current)
  }

  def newVariable(variableName: String, exp: ConcolicExpression): StatementConstraint = {
    StatementConstraint(variableName, exp)
  }
}
