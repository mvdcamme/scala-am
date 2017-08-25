trait ConcolicConstraint {
  override def toString: String
  def getLhs: Option[String]
  def getOp: Option[String]
  def getRhs: Option[String]
}

case class StatementConstraint(symbolicVariable: String, exp: ConcolicExpression, originalName: String) extends ConcolicConstraint {
  override def toString: String = {
    s"$symbolicVariable = $exp"
  }
  def getLhs = {
    Some(symbolicVariable)
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
case class BranchConstraint(exp: ConcolicExpression) extends ConcolicConstraint {
  override def toString: String = {
    exp.toString
  }
  def getLhs = exp.getLhs
  def getOp = exp.getOp
  def getRhs = exp.getRhs

  def negate: BranchConstraint = {
    BranchConstraint(exp.negate)
  }
}

trait ConcolicExpression {
  def getLhs: Option[String]
  def getOp: Option[String]
  def getRhs: Option[String]

  def negate: ConcolicExpression
}

case class BinaryConcolicExpression(left: ConcolicAtom, op: String, right: ConcolicAtom) extends ConcolicExpression {
  override def toString: String = {
    s"$left $op $right"
  }
  def getLhs = Some(left.toString)
  def getOp = Some(op)
  def getRhs = Some(right.toString)

  override def negate: ConcolicExpression = op match {
    case "<" =>
      BinaryConcolicExpression(left, ">=", right)
    case "<=" =>
      BinaryConcolicExpression(left, ">", right)
    case ">" =>
      BinaryConcolicExpression(left, "<=", right)
    case ">=" =>
      BinaryConcolicExpression(left, "<", right)
    case "=" =>
      BinaryConcolicExpression(left, "!=", right)
  }
}

trait ConcolicAtom extends ConcolicExpression {
  def getLhs = None
  def getOp = None
  def getRhs = None

  def negate: ConcolicExpression = this
}

case class ConcolicInt(i: Int) extends ConcolicAtom {
  override def toString: String = {
    i.toString
  }
}

case class ConcolicVariable(symbolicName: String, originalName: String) extends ConcolicAtom {
  override def toString: String = {
    s"$symbolicName"
  }
}

case class ConcolicInput(id: Int) extends ConcolicAtom {
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
    val current = id
    id += 1
    StatementConstraint(s"s$current", exp, variableName)
  }
}
