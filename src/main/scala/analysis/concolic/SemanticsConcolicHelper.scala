object SemanticsConcolicHelper {

  /**
    * Tries to convert the given expression to a ConcolicExpression, if at all possible. Returns None if this
    * is not possible.
    * @param exp The expression to convert.
    * @return
    */
  private def generateConcolicExpression(exp: SchemeExp): Option[ConcolicExpression] = {
    val validBinaryOperators: List[String] = List("<", ">", "=", "<=", ">=", "+", "*", "-", "/")
    exp match {
      case SchemeIdentifier(name, _) =>
        Reporter.lookupVariable(name) match {
          case Some(symbolicVariable) =>
            Some(ConcolicVariable(symbolicVariable, name))
          case None =>
            None
        }
      case SchemeValue(ValueInteger(i), _) =>
        Some(ConcolicInt(i))
      case SchemeFuncall(SchemeIdentifier(operatorVar, _), operands, _) =>
        if (validBinaryOperators.contains(operatorVar) && operands.size == 2) {
          val operandExpressions = operands.map(generateConcolicExpression)
          (operandExpressions.head, operandExpressions(1)) match {
            case (Some(a: ConcolicAtom), Some(b: ConcolicAtom)) =>
              Some(BinaryConcolicExpression(a, operatorVar, b))
            case _ =>
              None
          }
        } else {
          None
        }
      case _ =>
        None
    }
  }

  private def isRandomExpression(exp: SchemeExp): Boolean = exp match {
    case SchemeFuncall(SchemeIdentifier("random", _), _, _) =>
      true
    case _ =>
      false
  }

  def handleDefine(variableName: String, exp: SchemeExp): Unit = {
    if (isRandomExpression(exp)) {
      val concolicStatement = ConcolicIdGenerator.newVariable(variableName, ConcolicIdGenerator.newConcolicInput)
      Reporter.addConstraint(concolicStatement)
    } else {
      val optionConcolicExpression = generateConcolicExpression(exp)
      optionConcolicExpression match {
        case Some(concolicExpression) =>
          val concolicStatement = ConcolicIdGenerator.newVariable(variableName, concolicExpression)
          Reporter.addConstraint(concolicStatement)
        case None =>
      }
    }
  }

  def handleIf(exp: SchemeIf, thenBranchTaken: Boolean): Unit = {
    val optionConcolicExpression = generateConcolicExpression(exp.cond)
    optionConcolicExpression match {
      case Some(exp) =>
        val baseConstraint = BranchConstraint(exp)
        val actualConstraint = if (thenBranchTaken) baseConstraint else baseConstraint.negate
        Reporter.addConstraint(actualConstraint)
      case None =>
    }
  }

}
