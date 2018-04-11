/**
  * These are the traits that should be implemented by an abstract
  * machine. Example abstract machines that implement these are AAM.scala,
  * AAC.scala and Free.scala.
  */
/**
 * The interface of the abstract machine itself.
 * The abstract machine is parameterized by abstract values, addresses and
 * expressions. Look into AAM.scala for an example of how to define these
 * parameters
 */
abstract class AbstractMachine[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp] {
  /** The name of the abstract machine */
  def name: String

  /**
   * The output of the abstract machine
   */
  trait Output {
    /**
     * Returns the set of final values that can be reached by the abstract machine.
     * Example: the Scheme program (+ 1 2) has as final values the set {3} , in the concrete case.
     */
    def finalValues: Set[Abs]

    /**
     * Checks if the set of final values contains a value that subsumes @param v
     */
    def containsFinalValue(v: Abs): Boolean =
      finalValues.exists(v2 => JoinLattice[Abs].subsumes(v2, v))

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
     * Outputs the graph computed by the machine in a file, according to the given output format
     */
    def toFile(path: String)(output: GraphOutput): Unit
  }

  /**
   * Evaluates a program, given a semantics. If @param graph is true, the state
   * graph will be computed and stored in the output. Returns an object
   * implementing the Output trait, containing information about the
   * evaluation. @param timeout is the timeout in ns, when reached, the
   * evaluation stops and the currently computed results are returned.
   */
  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean = false, timeout: Timeout = Timeout.start(None)): Output
}

/**
  * Abstract machine with a control component that works in an eval-kont way: it
  * can either be evaluating something, or have reached a value and will pop a
  * continuation.
  */
abstract class EvalKontMachine[Exp: Expression, Abs: JoinLattice, Addr: Address, Time: Timestamp]
    extends AbstractMachine[Exp, Abs, Addr, Time] {
}

trait GraphPrinter[Graph] {
  def printGraph(graph: Graph, path: String): Unit
}

trait StateTrait[Exp, Abs, Addr, Time] {
  def isErrorState: Boolean
  def isUserErrorState: Boolean
}

trait KickstartEvalEvalKontMachine[Exp, Abs, Addr, Time] extends AbstractMachine[Exp, Abs, Addr, Time] {
  type InitialState
  type MachineState <: StateTrait[Exp, Abs, Addr, _]

  def kickstartEval(initialState: InitialState, sem: ConvertableSemantics[Exp, Abs, Addr, Time], stopEval: Option[MachineState => Boolean],
    timeout: Timeout, stepSwitched: Option[Int]): AnalysisOutputGraph[Exp, Abs, Addr, MachineState]
}
