trait TracingProgramState[Exp, Abs, Addr, Time] {

  implicit def abs: AbstractValue[Abs]
  implicit def addr: Address[Addr]
  implicit def time: Timestamp[Time]

  def halted: Boolean

  def finalValues(): Set[Abs]

  /**
    * Returns the color that the node representing this state in a graph should have.
    */
  def graphNodeColor: String

  /**
    * Checks whether a states subsumes another, i.e., if it is "bigger". This
    * is used to perform subsumption checking when exploring the state space,
    * in order to avoid exploring states for which another state that subsumes
    * them has already been explored.
    */
  def subsumes(that: TracingProgramState[Exp, Abs, Addr, Time]): Boolean
}
