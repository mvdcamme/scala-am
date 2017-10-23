object SemanticsConcolicHelper {

  /**
    * Tries to convert the given expression to a ConcolicExpression, if at all possible. Returns None if this
    * is not possible.
    * @param exp The expression to convert.
    * @return
    */
  private def generateConcolicExpression(exp: SchemeExp): Option[ConcolicExpression] = {
    val validArithmeticalOperators: List[String] = List("+", "*", "-", "/")
    val validRelationalOperators: List[String] = List("<", ">", "=", "<=", ">=")
    exp match {
      case SchemeIdentifier(name, _) =>
        Reporter.lookupVariable(name) match {
          case Some(concolicExp) =>
            Some(concolicExp)
          case None =>
            None
        }
      case SchemeValue(ValueInteger(i), _) =>
        Some(ConcolicInt(i))
      case SchemeFuncall(SchemeIdentifier(operatorVar, _), operands, _) =>
        lazy val operandExpressions = operands.map(generateConcolicExpression)
        if (validRelationalOperators.contains(operatorVar) && operands.size == 2) {
          (operandExpressions.head, operandExpressions(1)) match {
            case (Some(a), Some(b)) =>
              Some(RelationalConcolicExpression(a, operatorVar, b))
            case _ =>
              None
          }
        } else if (validArithmeticalOperators.contains(operatorVar) && operandExpressions.forall(_.isDefined)) {
          val arithmeticalExp = ArithmeticalConcolicExpression(operatorVar, operandExpressions.map(_.get))
          Some(arithmeticalExp)
        } else {
          None
        }
      case _ =>
        None
    }
  }

  def handleIf(concolicExpression: ConcolicExpression, thenBranchTaken: Boolean): Unit = {
    if (Reporter.isConcolicEnabled) {
      concolicExpression match {
        case b: RelationalConcolicExpression =>
          val baseConstraint = BranchConstraint(b)
          //        val actualConstraint = if (thenBranchTaken) baseConstraint else baseConstraint.negate
          ConcolicRunTimeFlags.setIfEncountered()
          Reporter.addBranchConstraint(baseConstraint, thenBranchTaken)
        case _ =>
          Logger.log(s"Using a non-BinaryConcolicExpression in a branch constraint: $concolicExpression", Logger.E)
      }
    }
  }

}
