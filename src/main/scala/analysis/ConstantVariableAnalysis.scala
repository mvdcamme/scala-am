/* We develop an analysis for finding variables whose value never changes throughout the lifetime of a program. */

/* The analysis itself only collects the error strings starting with "sink: " */
case class ConstantAnalysis[Abs : JoinLattice, Addr : Address, Time : Timestamp](initialEnv: Environment[Addr])
  extends BaseAnalysis[(Environment[Addr], Set[Addr]), SchemeExp, Abs, Addr, Time] {
  def stepEval(e: SchemeExp, env: Environment[Addr], store: Store[Addr, Abs], t: Time, current: (Environment[Addr], Set[Addr])) = current
  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time, current: (Environment[Addr], Set[Addr])) = current
  def error(error: SemanticError, current: (Environment[Addr], Set[Addr])) = current
  def join(x: (Environment[Addr], Set[Addr]), y: (Environment[Addr], Set[Addr])) = x ++ y
  def init = (initialEnv, Set())
}

/* We can finally run the analysis and detect when a tanted value flows to a sink */
object ConstantVariableAnalysis {
  def analyze[L : JoinLattice, Addr : Address](program: String): (Environment[Addr], Set[Addr]) = {
    val sem = new SchemeSemantics[L, ClassicalAddress.A, ZeroCFA.T](new TSchemePrimitives[ClassicalAddress.A, L])
    val machine = new AAM[SchemeExp, L, ClassicalAddress.A, ZeroCFA.T]
    val analysis = TaintAnalysis[L, ClassicalAddress.A, ZeroCFA.T]
    machine.analyze(sem.parse(program), sem, analysis, None) match {
      case Some(v) => v
      case None => println("Analysis did not succeed..."); Set()
    }
  }
  def main(args: Array[String]) {
    if (args.length >= 1) {
      val cpLattice = new ConstantPropagationLattice(false)
      implicit val isSchemeLattice = cpLattice.isSchemeLattice
      val taintLattice = new TaintLattice[cpLattice.L]()
      implicit val isTaintLattice = taintLattice.isTaintLattice
      val errors = analyze[taintLattice.L](args(0))
      if (errors.isEmpty) {
        println("No taint errors detected")
      } else {
        errors.foreach({ case (source, sink) => println(s"tainted value flows from source at position $source to sink at position $sink") })
      }
    } else {
      println("Please provide input program as argument")
    }
  }
}
