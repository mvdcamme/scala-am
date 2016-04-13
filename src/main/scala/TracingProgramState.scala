/**
  * Created by mvdcamme on 12/04/16.
  */
trait TracingProgramState[Exp, Abs, Addr, Time] {

  def finalValues(): Set[Abs]

  /**
    * Outputs the graph in a dot file
    */
  def toDotFile(path: String)

  def halted: Boolean

  /**
    * Checks whether a states subsumes another, i.e., if it is "bigger". This
    * is used to perform subsumption checking when exploring the state space,
    * in order to avoid exploring states for which another state that subsumes
    * them has already been explored.
    */
  def subsumes(that: ConcreteTracingProgramState[Exp, Abs, Addr, Time]): Boolean
}
