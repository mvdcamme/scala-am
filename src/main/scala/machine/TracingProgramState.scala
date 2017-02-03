import ConcreteConcreteLattice.ConcreteValue

trait TracingProgramState[Exp, Addr, Time]
  extends ConcreteTracingProgramState[Exp, Addr, Time] {

  implicit def abs: JoinLattice[ConcreteConcreteLattice.L]
  implicit def addr: Address[Addr]
  implicit def time: Timestamp[Time]

  def halted: Boolean

  def finalValues(): Set[ConcreteConcreteLattice.L]

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
  def subsumes(that: TracingProgramState[Exp, Addr, Time]): Boolean

  def control: TracingControl[Exp, ConcreteValue, Addr]
  def ρ: Environment[Addr]
  def σ: Store[Addr, ConcreteValue]
  def kstore: KontStore[KontAddr]
  def a: KontAddr
  def t: Time
  def v: ConcreteValue
  def vStack: List[Storable[ConcreteValue, Addr]]

  def step(sem: SemanticsTraced[Exp, ConcreteValue, Addr, Time])
  : Option[InterpreterStep[Exp, ConcreteValue, Addr]]
  def applyAction(sem: SemanticsTraced[Exp, ConcreteValue, Addr, Time],
                  action: ActionTrace[Exp, ConcreteValue, Addr])
  : ActionReturn[Exp,
    ConcreteValue,
    Addr,
    Time,
    TracingProgramState[Exp, Addr, Time]]
  def restart(sem: SemanticsTraced[Exp, ConcreteValue, Addr, Time],
              restartPoint: RestartPoint[Exp, ConcreteValue, Addr])
  : TracingProgramState[Exp, Addr, Time]

  def runHeader(
                 sem: SemanticsTraced[Exp, ConcreteValue, HybridAddress.A, Time],
                 assertions: List[ActionTrace[Exp, ConcreteValue, Addr]])
  : Option[TracingProgramState[Exp, Addr, Time]]

  def generateTraceInformation(action: ActionTrace[Exp, ConcreteValue, Addr])
  : CombinedInfos[ConcreteValue, HybridAddress.A]
}
