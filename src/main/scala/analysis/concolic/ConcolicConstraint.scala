trait ConcolicConstraint {
  override def toString: String
}

case class StatementConstraint(symbolicVariable: String, exp: ConcolicExpression, originalName: String) extends ConcolicConstraint {
  override def toString: String = {
    s"$symbolicVariable = $exp"
  }
}

case class BranchConstraint(exp: ConcolicExpression) extends ConcolicConstraint {
  override def toString: String = {
    exp.toString
  }
}

trait ConcolicExpression

case class BinaryConcolicExpression(left: ConcolicAtom, op: String, right: ConcolicAtom) extends ConcolicExpression {
  override def toString: String = {
    s"$left $op $right"
  }
}

trait ConcolicAtom extends ConcolicExpression

case class ConcolicInt(i: Int) extends ConcolicAtom {
  override def toString: String = {
    i.toString
  }
}

case class ConcolicVariable(symbolicName: String, originalName: String) extends ConcolicAtom {
  override def toString: String = {
    s"$symbolicName($originalName)"
  }
}

case class ConcolicInput(id: Int) extends ConcolicAtom {
  override def toString: String = {
    s"i$id"
  }
}

object ConcolicIdGenerator {
  private var id: Int = 0

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
