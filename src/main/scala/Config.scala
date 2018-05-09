import scala.concurrent.duration.Duration

/**
 * This is where we parse the arguments given to the implementation
 */
object Config {
  object Machine extends Enumeration {
    val AAC, AAM, AAMGlobalStore, Free, ConcreteMachine = Value
  }
  implicit val machineRead: scopt.Read[Machine.Value] = scopt.Read.reads(Machine withName _)

  object Lattice extends Enumeration {
    val Concrete, TypeSet, BoundedInt, ConstantPropagation, RunTime = Value
  }
  implicit val latticeRead: scopt.Read[Lattice.Value] = scopt.Read.reads(Lattice withName _)

  object Language extends Enumeration {
    val Scheme, AScheme, CScheme, ConcolicScheme = Value
  }
  implicit val languageRead: scopt.Read[Language.Value] = scopt.Read.reads(Language withName _)

  object Address extends Enumeration {
    val Classical, ValueSensitive = Value
  }
  implicit val addressRead: scopt.Read[Address.Value] = scopt.Read.reads(Address withName _)

  object Mbox extends Enumeration {
    val Powerset, BoundedList, BoundedMultiset, Graph = Value
  }
  implicit val MboxRead: scopt.Read[Mbox.Value] = scopt.Read.reads(Mbox withName _)

  case class Config(machine: Machine.Value = Machine.Free,
    language: Language.Value = Language.Scheme,
    lattice: Lattice.Value = Lattice.TypeSet, concrete: Boolean = false,
    file: Option[String] = None, dotfile: Option[String] = None, jsonfile: Option[String] = None,
    address: Address.Value = Address.Classical,
    inspect: Boolean = false,
    counting: Boolean = false,
    bound: Int = 100,
    timeout: Option[Duration] = None,
    workers: Int = 1,
    mbox: Mbox.Value = Mbox.BoundedList,
    mboxBound: Int = 1,
    resultsPath: String = "benchmark_times.txt",
    analysisFlags: AnalysisFlags = AnalysisFlags()
  )

  private val separator = ", "
  val parser = new scopt.OptionParser[Config]("scala-am") {
    head("scala-am", "0.0")
    opt[Machine.Value]('m', "machine") action { (x, c) => c.copy(machine = x) } text(s"Abstract machine to use (${Machine.values.mkString(separator)})")
    opt[Lattice.Value]('l', "lattice") action { (x, c) => c.copy(lattice = x) } text(s"Lattice to use (${Lattice.values.mkString(separator)})")
    opt[Language.Value]("lang") action { (x, c) => c.copy(language = x) } text(s"Language to analyze (${Language.values.mkString(separator)})")
    opt[Unit]('c', "concrete") action { (_, c) => c.copy(concrete = true, counting = true) } text("Run in concrete mode")
    opt[String]('d', "dotfile") action { (x, c) => c.copy(dotfile = Some(x)) } text("Dot file to output graph to")
    opt[String]('j', "jsonfile") action { (x, c) => c.copy(jsonfile = Some(x)) } text("JSON file to output graph to")
    opt[String]('f', "file") action { (x, c) => c.copy(file = Some(x)) } text("File to read program from")
    opt[Duration]('t', "timeout") action { (x, c) => c.copy(timeout = if (x.isFinite) Some(x) else None) } text("Timeout (none by default)")
    opt[Unit]('i', "inspect") action { (x, c) => c.copy(inspect = true) } text("Launch inspection REPL (disabled by default)")
    opt[Address.Value]('a', "address") action { (x, c) => c.copy(address = x) } text(s"Addresses to use (${Address.values.mkString(separator)})")
    opt[Mbox.Value]("mbox") action { (x, c) => c.copy(mbox = x) } text(s"Mailbox to use (${Mbox.values.mkString(separator)})")
    opt[Int]("mbox-bound") action { (x, c) => c.copy(mboxBound = x) } text("Mailbox bound to use (defaults to 1)")
    opt[Unit]("counting") action { (x, c) => c.copy(counting = true) } text("Use abstract counting (on for concrete lattices)")
    opt[Int]('b', "bound") action { (x, c) => c.copy(bound = x) } text("Bound for bounded lattice (defaults to 100)")
    opt[Int]('w', "workers") action { (x, c) => c.copy(workers = x) } text("Number of workers (defaults to 1)")
  }
}
