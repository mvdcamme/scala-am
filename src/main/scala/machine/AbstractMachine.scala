/**
  * These are the traits that should be implemented by an abstract
  * machine. Example abstract machines that implement these are AAM.scala,
  * AAC.scala and Free.scala.
  */
/**
  * The output of the abstract machine
  */
trait Output[Abs] {

  /**
    * Returns the set of final values that can be reached by the abstract machine.
    * Example: the Scheme program (+ 1 2) has as final values the set {3} , in the concrete case.
    */
  def finalValues: Set[Abs]

  /**
    * Checks if the set of final values contains a value that subsumes @param v
    */
  def containsFinalValue(v: Abs): Boolean

  /**
    * Returns the number of states visited to evaluate the program
    */
  def numberOfStates: Int

  /**
    * Returns the time it took to evaluate the program
    */
  def time: Double

  /**
    * Does this output comes from a computation that timed out?
    */
  def timedOut: Boolean

  /**
    * Outputs the graph computed by the machine in a dot file
    */
  def toDotFile(path: String): Unit

  /**
    * Inspects a specific state
    */
  def inspect(state: Int, query: String): Unit =
    println("TODO: Inspection not implemented for this machine")
}

/**
  * The interface of the abstract machine itself
  */
trait BasicAbstractMachine[Exp, Abs, Addr, Time] {

  /**
    * The abstract machine is parameterized by abstract values, addresses and
    * expressions. Look into AAM.scala for an example of how to define these
    * parameters */
  implicit def abs: JoinLattice[Abs]
  implicit def addr: Address[Addr]
  implicit def exp: Expression[Exp]
  implicit def time: Timestamp[Time]

  /** The name of the abstract machine */
  def name: String
}

/**
  * The interface of the abstract machine itself
  */
trait AbstractMachine[Exp, Abs, Addr, Time]
    extends BasicAbstractMachine[Exp, Abs, Addr, Time] {

  /**
    * Evaluates a program, given a semantics. If @param graph is true, the state
    * graph will be computed and stored in the output. Returns an object
    * implementing the Output trait, containing information about the
    * evaluation. @param timeout is the timeout in ns, when reached, the
    * evaluation stops and the currently computed results are returned.
    */
  def eval(exp: Exp,
           sem: Semantics[Exp, Abs, Addr, Time],
           graph: Boolean = false,
           timeout: Option[Long] = None): Output[Abs]

  def analyze[L](exp: Exp,
                 sem: Semantics[Exp, Abs, Addr, Time],
                 analysis: Analysis[L, Exp, Abs, Addr, Time],
                 timeout: Option[Long] = None): Option[L] =
    throw new Exception(
      s"analyze method not handled by the following machine: $name")
}

/**
  * The interface of the abstract machine itself
  */
trait AbstractMachineTraced[Exp, Abs, Addr, Time]
    extends BasicAbstractMachine[Exp, Abs, Addr, Time] {
  def sem: SemanticsTraced[Exp, Abs, Addr, Time]

  /**
    * Evaluates a program, given a semantics. If @param graph is true, the state
    * graph will be computed and stored in the output. Returns an object
    * implementing the Output trait, containing information about the
    * evaluation.
    */
  def eval(exp: Exp,
           graph: Boolean = false,
           timeout: Option[Long] = None): Output[Abs]
}

/**
  * Abstract machine with a control component that works in an eval-kont way: it
  * can either be evaluating something, or have reached a value and will pop a
  * continuation.
  */
abstract class EvalKontMachine[
    Exp: Expression, Abs: JoinLattice, Addr: Address, Time: Timestamp]
    extends AbstractMachine[Exp, Abs, Addr, Time] {
  def abs = implicitly[JoinLattice[Abs]]
  def addr = implicitly[Address[Addr]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[Time]]

  /**
    * The control component of the machine
    */
  trait Control {
    def subsumes(that: Control): Boolean

    /** Generates a descriptor for this control. */
    def descriptor: Descriptor[Control] = new ControlDescriptor
  }

  /**
    * It can either be an eval component, where an expression needs to be
    * evaluated in an environment
    */
  case class ControlEval(exp: Exp, env: Environment[Addr]) extends Control {
    override def toString() = s"ev(${exp})"
    def subsumes(that: Control) = that match {
      case ControlEval(exp2, env2) => exp.equals(exp2) && env.subsumes(env2)
      case _ => false
    }
  }

  class ControlDescriptor extends BasicDescriptor[Control] {
    override def describe[U >: Control](control: U): String = control match {
      case ControlEval(exp, env) =>
        putIntoCollapsableList(List(exp.toString, env.descriptor.describe(env)), control.toString, Some("eval"))
      case _ =>
        control.toString
    }
  }

  /**
    * Or it can be a continuation component, where a value has been reached and a
    * continuation should be popped from the stack to continue the evaluation
    */
  case class ControlKont(v: Abs) extends Control {
    override def toString() = s"ko(${v})"
    def subsumes(that: Control) = that match {
      case ControlKont(v2) => abs.subsumes(v, v2)
      case _ => false
    }
  }

  /**
    * Or an error component, in case an error is reached (e.g., incorrect number
    * of arguments in a function call)
    */
  case class ControlError(err: SemanticError) extends Control {
    override def toString() = s"err($err)"
    def subsumes(that: Control) = that.equals(this)
  }
}

abstract class EvalKontMachineTraced[Exp: Expression, Abs: JoinLattice,
Addr: Address, Time: Timestamp](sem: SemanticsTraced[Exp, Abs, Addr, Time])
    extends AbstractMachineTraced[Exp, Abs, Addr, Time] {
  def abs = implicitly[JoinLattice[Abs]]
  def addr = implicitly[Address[Addr]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[Time]]
}

trait HasGraph[Exp, Abs, Addr, Node <: StateTrait[Exp, Abs, Addr, _]] {
  def graph: Graph[Node, (List[EdgeAnnotation], List[ActionT[Exp, Abs, Addr]])]
  def toDotFile(path: String): Unit
}

trait HasFinalStores[Addr, Abs] {

  def finalStores: Set[Store[Addr, Abs]]
  def stepSwitched: Option[Int]

}

trait StateTrait[Exp, Abs, Addr, Time]

trait KickstartEvalEvalKontMachine[Exp, Abs, Addr, Time] {
  type MachineState
  type GraphNode
  type MachineOutput <: Output[Abs] with HasFinalStores[Addr, Abs]

  def kickstartEval(initialState: MachineState,
                    sem: Semantics[Exp, Abs, Addr, Time],
                    stopEval: Option[MachineState => Boolean],
                    timeout: Option[Long],
                    stepSwitched: Option[Int]): MachineOutput
}

trait ProducesStateGraph[Exp, Abs, Addr, Time] extends KickstartEvalEvalKontMachine[Exp, Abs, Addr, Time] {
  override type GraphNode <: StateTrait[Exp, Abs, Addr, Time]
  override type MachineOutput <: Output[Abs] with HasGraph[Exp, Abs, Addr, GraphNode] with HasFinalStores[Addr, Abs]
}
