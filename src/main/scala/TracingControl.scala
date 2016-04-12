/**
  * The control component of the machine
  */
trait TracingControl[Exp, Abs, Addr] {
  def subsumes(that: TracingControl[Exp, Abs, Addr]): Boolean
  def toString(store: Store[Addr, Abs]): String = toString()
}

/**
  * It can either be an eval component, where an expression needs to be
  * evaluated in an environment
  */
case class TracingControlEval[Exp : Expression, Abs : AbstractValue, Addr : Address](exp: Exp) extends TracingControl[Exp, Abs, Addr] {
  override def toString = s"ev($exp)"
  def subsumes(that: TracingControl[Exp, Abs, Addr]) = that match {
    case TracingControlEval(exp2) => exp.equals(exp2)
    case _ => false
  }
}

/**
  * Or an error component, in case an error is reached (e.g., incorrect number
  * of arguments in a function call)
  */
case class TracingControlError[Exp : Expression, Abs : AbstractValue, Addr : Address](reason: String) extends TracingControl[Exp, Abs, Addr] {
  override def toString = s"err($reason)"
  def subsumes(that: TracingControl[Exp, Abs, Addr]) = that.equals(this)
}

/**
  * Or it can be a continuation component, where a value has been reached and a
  * continuation should be popped from the stack to continue the evaluation
  */
case class TracingControlKont[Exp : Expression, Abs : AbstractValue, Addr : Address](ka : KontAddr) extends TracingControl[Exp, Abs, Addr] {
  override def toString = s"ko($ka)"
  override def toString(store: Store[Addr, Abs]) = s"ko($ka)"
  def subsumes(that: TracingControl[Exp, Abs, Addr]) = that match {
    case TracingControlKont(ka2) => ka == ka2
    case _ => false
  }
}
