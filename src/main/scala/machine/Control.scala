/**
  * The control component of the machine
  */
trait Control[Exp, Abs, Addr] {
  def subsumes(that: Control[Exp, Abs, Addr]): Boolean
}

object Control {
  import org.json4s._
  import org.json4s.JsonDSL._
  import org.json4s.jackson.JsonMethods._
  import scala.language.implicitConversions
  import JSON._
  implicit def controlToJSON[Exp: Expression, Abs: JoinLattice, Addr: Address](c: Control[Exp, Abs, Addr]): JValue = c match {
    case ControlEval(exp, env) =>
      ("type" -> "ev") ~ ("exp" -> exp.toString) ~ ("env" -> env)
    case ControlKont(v) =>
      ("type" -> "kont") ~ ("value" -> v.toString)
    case ControlError(err) =>
      ("type" -> "err") ~ ("error" -> err.toString)
  }
}

/**
  * It can either be an eval component, where an expression needs to be
  * evaluated in an environment
  */
case class ControlEval[Exp: Expression, Abs: JoinLattice, Addr: Address](exp: Exp, env: Environment[Addr]) extends Control[Exp, Abs, Addr] {
  override def toString = s"ev(${exp})"
  def subsumes(that: Control[Exp, Abs, Addr]) = that match {
    case ControlEval(exp2, env2) => exp.equals(exp2) && env.subsumes(env2)
    case _ => false
  }
}

/**
  * Or it can be a continuation component, where a value has been reached and a
  * continuation should be popped from the stack to continue the evaluation
  */
case class ControlKont[Exp: Expression, Abs: JoinLattice, Addr: Address](v: Abs) extends Control[Exp, Abs, Addr] {
  override def toString = s"ko(${v})"
  def subsumes(that: Control[Exp, Abs, Addr]) = that match {
    case ControlKont(v2) => JoinLattice[Abs].subsumes(v, v2)
    case _ => false
  }
}

/**
  * Or an error component, in case an error is reached (e.g., incorrect number
  * of arguments in a function call)
  */
case class ControlError[Exp: Expression, Abs: JoinLattice, Addr: Address](err: SemanticError) extends Control[Exp, Abs, Addr] {
  override def toString = s"err($err)"
  def subsumes(that: Control[Exp, Abs, Addr]) = that.equals(this)
}
