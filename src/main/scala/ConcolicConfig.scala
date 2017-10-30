//case class Config(machine: Machine.Value = Machine.Free,
//  lattice: Lattice.Value = Lattice.TypeSet,
//  concrete: Boolean = false,
//  file: Option[String] = None,
//  dotfile: Option[String] = None,
//  address: Address.Value = Address.Classical,
//  inspect: Boolean = false,
//  counting: Boolean = false,
//  bound: Int = 100,
//  timeout: Option[Long] = None,
//  resultsPath: String = "benchmark_times.txt",
//  analysisPath: Option[String] = None,
//  analysisFlags: AnalysisFlags = AnalysisFlags())

//val parser = new scopt.OptionParser[Config]("scala-am") {
//  head("scala-am", "0.0")
//  opt[Machine.Value]('m', "machine") action { (x, c) =>
//    c.copy(machine = x)
//  } text ("Abstract machine to use (AAM, AAMGlobalStore, AAC, Free, ConcreteMachine, Hybrid)")
//  opt[Lattice.Value]('l', "lattice") action { (x, c) =>
//    c.copy(lattice = x)
//  } text ("Lattice to use (Concrete, Type, TypeSet)")
//  opt[Unit]('c', "concrete") action { (_, c) =>
//    c.copy(concrete = true)
//  } text ("Run in concrete mode")
//  opt[String]('d', "dotfile") action { (x, c) =>
//    c.copy(dotfile = Some(x))
//  } text ("Dot file to output graph to")
//  opt[String]('f', "file") action { (x, c) =>
//    c.copy(file = Some(x))
//  } text ("File to read program from")
//  opt[String]("result") action { (x, c) =>
//    c.copy(resultsPath = x)
//  } text ("File to print benchmarks results to")
//  opt[String]("analysis_output") action { (x, c) =>
//    c.copy(analysisPath = Some(x))
//  } text ("File to print analysis results to")
//  opt[String]("inc_analysis") action { (s, c) => {
//    val incrAnalysisInterval = s match {
//      case "N" | "n" | "None" | "none" => NoIncrementalAnalysis
//      case _ => IncrementalAnalysisEvery(Integer.parseInt(s))
//    }
//    c.copy(analysisFlags = c.analysisFlags.copy(incrementalAnalysisInterval = incrAnalysisInterval))
//  }
//  } text ("Launch an incremental analysis every x execution steps")
//  opt[String]("rt_analysis") action { (s, c) => {
//    val runTimeAnalysisInterval: RunTimeAnalysisInterval = s match {
//      case "N" | "n" | "None" | "none" => NoRunTimeAnalysis
//      case _ => RunTimeAnalysisEvery(Integer.parseInt(s))
//    }
//    c.copy(analysisFlags = c.analysisFlags.copy(runTimeAnalysisInterval = runTimeAnalysisInterval))
//  }
//  } text ("Launch an incremental analysis every x execution steps")
//  opt[Unit]("disable_skip_iteration_optimisation") action { (_, c) =>
//    c.copy(analysisFlags = c.analysisFlags.copy(skipIterationOptimisation = false))
//  } text ("Turn optimisation of incremental analysis OFF")
//  opt[Unit]("disable_delta_optimisation") action { (_, c) =>
//    c.copy(analysisFlags = c.analysisFlags.copy(deltaOptimisation = false))
//  } text ("Turn extra optimisation of incremental analysis OFF")
//  opt[Unit]("disable_propagation") action { (_, c) =>
//    c.copy(analysisFlags = c.analysisFlags.copy(doPropagationPhase = false))
//  } text ("Disable the run-time info propagation phase in the incremental analysis")
//  opt[Time]('t', "timeout") action { (x, c) =>
//    c.copy(timeout = Some(x.nanoSeconds))
//  } text ("Timeout (none by default)")
//  opt[Unit]('i', "inspect") action { (x, c) =>
//    c.copy(inspect = true)
//  } text ("Launch inspection REPL (disabled by default)")
//  opt[Address.Value]('a', "address") action { (x, c) =>
//    c.copy(address = x)
//  } text ("Addresses to use (Classical, ValueSensitive)")
//  opt[Unit]("counting") action { (x, c) =>
//    c.copy(counting = true)
//  } text ("Use abstract counting (on for concrete lattices)")
//  opt[Int]('b', "bound") action { (x, c) =>
//    c.copy(bound = x)
//  } text ("Bound for bounded lattice (default to 100)")
//}

case class ConcolicConfig(resultsPath: String = "benchmark_times.txt",
  analysisPath: Option[String] = None,
  analysisFlags: AnalysisFlags = AnalysisFlags(),
  dotfile: Option[String] = None,
  file: Option[String] = None,
) {

  val parser = new scopt.OptionParser[ConcolicConfig]("scala-am") {
    head("scala-am", "0.0")
    opt[String]('d', "dotfile") action { (x, c) => c.copy(dotfile = Some(x)) } text("Dot file to output graph to")
    opt[String]('f', "file") action { (x, c) => c.copy(file = Some(x)) } text("File to read program from")

  }

}
