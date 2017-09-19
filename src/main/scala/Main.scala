import scala.io.StdIn
<<<<<<< HEAD
import java.io._

/**
  * Before looking at this, we recommend seeing how to use this framework. A
  * detailed example is available in LambdaCalculus.scala.
  *
  * This is the entry point. It parses the arguments, parses the input file and
  * launch an abstract machine on the parsed expression (or launches a REPL if no
  * input file is given). The code in this file isn't very clean and I'd like to
  * improve it at some point, but my scala-fu isn't good enough to do it now. The
  * pipeline goes as follows:
  *   1. The input program is parsed. For Scheme programs, it is done by:
  *      - Parsing the file as a list of s-expressions (exp/SExp.scala,
  *        exp/SExpParser.scala)
  *      - Compiling these s-expressions into Scheme expressions
  *        (exp/scheme/Scheme.scala)
  *      - Optionally, converting Scheme expressions into ANF form
  *        (exp/anf/ANF.scala) to have a simpler interpreter (but longer
  *        programs)

  *   2. To run the program, we need an abstract machine and some semantics. For
  *      now, the only semantics available are for ANF Scheme and Scheme
  *      (semantics/anf/ANFSemantics.scala,
  *      semantics/scheme/SchemeSemantics.scala). Semantics definitions have to
  *      implement the Semantics interface (semantics/Semantics.scala).

  *   3. Once the abstract machine is created and we have a semantics for the
  *      program we want to analyze, the abstract machine can perform its
  *      evaluation, relying on methods of the semantics class to know how to
  *      evaluate expressions. The abstract machine only deals with which states
  *      to evaluate in which order, where to store values, where to store
  *      continuations, how to push and pop continuations, etc. The semantics
  *      encode what to do when encountering a program construct. For example,
  *      the semantics can tell what to evaluate next, that a continuation needs
  *      to be pushed, or that a variable needs to be updated. The abstract
  *      machine will then respectively evaluate the expression needed, push the
  *      continuation, or update the variable.
  *
  *      Multiple abstract machine implementations are available, including:
  *      - The classical Abstracting Abstract Machine of Might and Van Horn (machine/AAM.scala)
  *      - Johnson's Abstracting Abstract Control (machine/AAC.scala)
  *      - Gilrey's Pushdown Control-Flow Analysis for Free (machine/Free.scala)
  *      - A fast concrete interpreter (machine/ConcreteMachine.scala)
  *      Every abstract machine implementation has to implement the AbstractMachine
  *      interface (machine/AbstractMachine.scala).
  *
  *      The abstract machine also uses a lattice to represent values. Lattices
  *      should implement the AbstractValue trait that can be found in
  *      AbstractValue.scala. The following lattices are available:
  *      - A modular lattice (lattice/scheme/ModularLattice.scala) where every component
  *        (numbers, strings, ...) can be specified independently of each
  *        other. It can then automatically transform a lattice into a powerset
  *        lattice. There are implementations for concrete values, type
  *        representations of a value, and bounded integers.
  *      - A product lattice, combining two lattices together as a cartesian
  *        product. Example: one could combine the type lattice with a sign
  *        lattice, getting abstract values such as (Int, +), (String, bottom),
  *        ...
  *
  *  If you want to:
  *  - Support a new language: you will need:
  *    - A parser, you can look into exp/SExpParser.scala as an inspiration. If your
  *      language is s-expression based, you can use this parser and compile
  *      s-expressions into your abstract grammar. To do so, look at exp/scheme/Scheme.scala.
  *    - An abstract grammar, look at exp/SExp.scala or the SchemeExp class in exp/scheme/Scheme.scala.
  *    - A semantics, look at semantics/anf/ANFSemantics.scala for a simple example.
  *    - Support for your language operations at the lattice level. For this,
  *      you'll probably need to extend the lattices (lattice/AbstractValue.scala,
  *      lattice/ModularLattice.scala, ...)
  *  - Play with abstract machines, you can look into AAM.scala, AAC.scala or
  *    Free.scala (AAM is the simplest machine).
  *  - Implement some kind of analysis, you'll probably need to design a lattice
  *    that is suited for your analysis. You can use an existing lattice as an
  *    inspiration.
  */
/**
  * This is where we parse the arguments given to the implementation
  */
object Config {

  object Machine extends Enumeration {
    val AAC, AAM, AAMGlobalStore, Free, ConcreteMachine, Hybrid, HybridConcrete = Value
  }

  implicit val machineRead: scopt.Read[Machine.Value] =
    scopt.Read.reads(Machine withName _)

  object Lattice extends Enumeration {
    val Concrete, ConcreteNew, TypeSet, BoundedInt, Constant = Value
  }

  implicit val latticeRead: scopt.Read[Lattice.Value] =
    scopt.Read.reads(Lattice withName _)

  def parseBool(boolString: String): Option[Boolean] = boolString match {
    case "true" | "t" => Some(true)
    case "false" | "f" => Some(false)
    case _ => None
  }

  def readBoolStringForTraceFlag(
      config: Config,
      boolString: String,
      genTracingFlags: (Boolean) => TracingFlags): Config = {
    val someBool = parseBool(boolString)
    /* If no argument is passed, or if the argument could not be properly parsed,
     * the default flag is used, i.e. we don't change config. */
    someBool.fold(config)(bool =>
      config.copy(tracingFlags = genTracingFlags(bool)))
  }

  object Address extends Enumeration {
    val Classical, ValueSensitive = Value
  }

  implicit val addressRead: scopt.Read[Address.Value] =
    scopt.Read.reads(Address withName _)

  trait Time {
    def nanoSeconds: Long
  }

  case class Hours(n: Long) extends Time {
    def nanoSeconds = n * 60 * 60 * Math.pow(10, 9).toLong

    override def toString = if (n == 1) "1 hour" else s"$n hours"
  }

  case class Minutes(n: Long) extends Time {
    def nanoSeconds = n * 60 * Math.pow(10, 9).toLong

    override def toString = if (n == 1) "1 minute" else s"$n minutes"
  }

  case class Seconds(n: Long) extends Time {
    def nanoSeconds = n * Math.pow(10, 9).toLong

    override def toString = if (n == 1) "1 second" else s"$n seconds"
  }

  case class Milliseconds(n: Long) extends Time {
    def nanoSeconds = n * Math.pow(10, 6).toLong

    override def toString = if (n == 1) "1 millisecond" else s"$n milliseconds"
  }

  case class Nanoseconds(nanoSeconds: Long) extends Time {
    override def toString =
      if (nanoSeconds == 1) "1 nanosecond" else s"$nanoSeconds nanoseconds"
  }

  object TimeParser extends scala.util.parsing.combinator.RegexParsers {
    val number = "[0-9]+".r

    def hours: Parser[Time] = (number <~ "h") ^^ ((s) => Hours(s.toLong))

    def minutes: Parser[Time] = (number <~ "min") ^^ ((s) => Minutes(s.toLong))

    def seconds: Parser[Time] = (number <~ "s") ^^ ((s) => Seconds(s.toLong))

    def milliseconds: Parser[Time] =
      (number <~ "ms") ^^ ((s) => Milliseconds(s.toLong))

    def nanoseconds: Parser[Time] =
      (number <~ "ns") ^^ ((s) => Nanoseconds(s.toLong))

    def time: Parser[Time] =
      hours | minutes | seconds | milliseconds | nanoseconds

    def parse(s: String): Time = parseAll(time, s) match {
      case Success(res, _) => res
      case Failure(msg, _) => throw new Exception(s"cannot parse time: $msg")
      case Error(msg, _) => throw new Exception(s"cannot parse time: $msg")
    }
  }

  implicit val timeRead: scopt.Read[Time] =
    scopt.Read.reads(TimeParser.parse _)

  case class Config(machine: Machine.Value = Machine.Free,
                    lattice: Lattice.Value = Lattice.TypeSet,
                    concrete: Boolean = false,
                    file: Option[String] = None,
                    dotfile: Option[String] = None,
                    address: Address.Value = Address.Classical,
                    inspect: Boolean = false,
                    counting: Boolean = false,
                    bound: Int = 100,
                    timeout: Option[Long] = None,
                    amb: Boolean = false,
                    resultsPath: String = "benchmark_times.txt",
                    analysisPath: Option[String] = None,
                    analysisFlags: AnalysisFlags = AnalysisFlags(),
                    tracingFlags: TracingFlags = TracingFlags())

  val parser = new scopt.OptionParser[Config]("scala-am") {
    head("scala-am", "0.0")
    opt[Machine.Value]('m', "machine") action { (x, c) =>
      c.copy(machine = x)
    } text ("Abstract machine to use (AAM, AAMGlobalStore, AAC, Free, ConcreteMachine, Hybrid)")
    opt[Lattice.Value]('l', "lattice") action { (x, c) =>
      c.copy(lattice = x)
    } text ("Lattice to use (Concrete, Type, TypeSet)")
    opt[Unit]('c', "concrete") action { (_, c) =>
      c.copy(concrete = true)
    } text ("Run in concrete mode")
    opt[String]('d', "dotfile") action { (x, c) =>
      c.copy(dotfile = Some(x))
    } text ("Dot file to output graph to")
    opt[String]('f', "file") action { (x, c) =>
      c.copy(file = Some(x))
    } text ("File to read program from")
    opt[String]("result") action { (x, c) =>
      c.copy(resultsPath = x)
    } text ("File to print benchmarks results to")
    opt[String]("analysis_output") action { (x, c) =>
      c.copy(analysisPath = Some(x))
    } text ("File to print analysis results to")
    opt[String]('o', "Optimization") action { (x, c) =>
      {
        val optimization: ShouldApplyOptimization = x match {
          case "A" | "a" | "All" | "all" =>
            ApplyAllOptimizations
          case "N" | "n" | "None" | "none" =>
            ApplyNoOptimizations
          case string =>
            val chars = string.toList
            ApplySpecificOptimizations(chars.map(_.asDigit))
        }
        c.copy(tracingFlags = c.tracingFlags.copy(OPTIMIZATION = optimization))
      }
    } text ("Apply (dynamic) optimizations")
    opt[String]("inc_analysis") action { (s, c) => {
        val incrAnalysisInterval = s match {
          case "N" | "n" | "None" | "none" => NoIncrementalAnalysis
          case _ => IncrementalAnalysisEvery(Integer.parseInt(s))
        }
        c.copy(analysisFlags = c.analysisFlags.copy(incrementalAnalysisInterval = incrAnalysisInterval))
      }
    } text ("Launch an incremental analysis every x execution steps")
    opt[String]("rt_analysis") action { (s, c) => {
      val runTimeAnalysisInterval: RunTimeAnalysisInterval = s match {
        case "N" | "n" | "None" | "none" => NoRunTimeAnalysis
        case _ => RunTimeAnalysisEvery(Integer.parseInt(s))
      }
      c.copy(analysisFlags = c.analysisFlags.copy(runTimeAnalysisInterval = runTimeAnalysisInterval))
    }
    } text ("Launch an incremental analysis every x execution steps")
    opt[Unit]("disable_skip_iteration_optimisation") action { (_, c) =>
      c.copy(analysisFlags = c.analysisFlags.copy(skipIterationOptimisation = false))
    } text ("Turn optimisation of incremental analysis OFF")
    opt[Unit]("disable_delta_optimisation") action { (_, c) =>
      c.copy(analysisFlags = c.analysisFlags.copy(deltaOptimisation = false))
    } text ("Turn extra optimisation of incremental analysis OFF")
    opt[Unit]("disable_propagation") action { (_, c) =>
      c.copy(analysisFlags = c.analysisFlags.copy(doPropagationPhase = false))
    } text ("Disable the run-time info propagation phase in the incremental analysis")
    opt[String]("tracing") action { (b, c) =>
      readBoolStringForTraceFlag(
        c,
        b,
        bool => c.tracingFlags.copy(DO_TRACING = bool))
    } text ("Record and execute traces")
    opt[String]("threshold") action { (x, c) =>
      c.copy(
        tracingFlags =
          c.tracingFlags.copy(TRACING_THRESHOLD = Integer.parseInt(x)))
    } text ("The minimum threshold required to consider a loop hot")

    opt[String]("initial") action { (b, c) =>
      readBoolStringForTraceFlag(
        c,
        b,
        bool => c.tracingFlags.copy(DO_INITIAL_ANALYSIS = bool))
    } text ("Perform an initial static analysis over the program")
    opt[String]("switch") action { (b, c) =>
      /* Make sure, for the moment at least, that if runtime analyses are enabled, an initial analysis is also performed. */
      readBoolStringForTraceFlag(
        c,
        b,
        bool =>
          if (bool) {
            c.tracingFlags.copy(SWITCH_ABSTRACT = true,
                                DO_INITIAL_ANALYSIS = true)
          } else {
            c.tracingFlags.copy(SWITCH_ABSTRACT = false)
        })
    } text ("Switch to abstract (type) interpretation after recording a trace and use this abstract information to optimize traces.")
    opt[Unit]("amb") action { (_, c) =>
      c.copy(amb = true)
    } text ("Execute ambiguous Scheme instead of normal Scheme")
    opt[Time]('t', "timeout") action { (x, c) =>
      c.copy(timeout = Some(x.nanoSeconds))
    } text ("Timeout (none by default)")
    opt[Unit]('i', "inspect") action { (x, c) =>
      c.copy(inspect = true)
    } text ("Launch inspection REPL (disabled by default)")
    opt[Address.Value]('a', "address") action { (x, c) =>
      c.copy(address = x)
    } text ("Addresses to use (Classical, ValueSensitive)")
    opt[Unit]("counting") action { (x, c) =>
      c.copy(counting = true)
    } text ("Use abstract counting (on for concrete lattices)")
    opt[Int]('b', "bound") action { (x, c) =>
      c.copy(bound = x)
    } text ("Bound for bounded lattice (default to 100)")
  }
}

object Main {

  var currentProgram: String = ""

  def printExecutionTimes[Abs: JoinLattice](benchmarks_results_file: String): Unit = {
    val file = new File(benchmarks_results_file)
    val bw = new BufferedWriter(new FileWriter(file, true))
    bw.write(s"$currentProgram: ${Stopwatch.time}\n")
    bw.close()
  }

  def runBasic[Exp: Expression,
               Abs: JoinLattice,
               Addr: Address,
               Time: Timestamp](
      machine: BasicAbstractMachine[Exp, Abs, Addr, Time],
      output: Option[String],
      calcResult: () => Output[Abs],
      benchmarks_results_file: String,
      timeout: Option[Long],
      inspect: Boolean): Unit = {

    def handleJITWarmUp(): Unit = {
      val warmUpIterations = 2
      val doWarmUp = false

      if (doWarmUp) {
        var i = 1
        while (i < warmUpIterations) {
          i += 1
          calcResult()
        }
      }

    }

    val abs = implicitly[JoinLattice[Abs]]
    val addr = implicitly[Address[Addr]]
    println(
      s"Running ${machine.name} with lattice ${abs.name} and address ${addr.name}")

    handleJITWarmUp()

    val result: Output[Abs] = Stopwatch.doTimed({
      val result = calcResult()

      output match {
        case Some(f) =>
          result.toDotFile(f)
        case None =>
      }
      result
    })
    if (result.timedOut) {
      println(
        s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
    } else if (GlobalFlags.PRINT_EXECUTION_TIME) {
      printExecutionTimes(benchmarks_results_file)
    }
    println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")

    if (inspect) {
      try {
        do {
          import scala.util.{Try, Success, Failure}
          val input = StdIn.readLine(">>> ")
          if (input == null) throw Done
          if (input.size > 0) {
            input.indexOf(".") match {
              case -1 => println(s"Unknown inspection query: $input")
              case n =>
                Try(input.subSequence(0, n).toString.toInt) match {
                  case Success(state) =>
                    result.inspect(
                      state,
                      input.subSequence(n + 1, input.size).toString)
                  case Failure(e) =>
                    println(
                      s"Cannot parse state number (${input.subSequence(0, n)}): $e")
                }
            }
          }
        } while (true);
      } catch {
        case Done => ()
      }
    }
  }

  /** Run a machine on a program with the given semantics. If @param output is
    * set, generate a dot graph visualizing the computed graph in the given
    * file. */
  def run[Exp: Expression, Abs: JoinLattice, Addr: Address, Time: Timestamp](
      machine: AbstractMachine[Exp, Abs, Addr, Time],
      sem: Semantics[Exp, Abs, Addr, Time])(
      program: String,
      output: Option[String],
      timeout: Option[Long],
      inspect: Boolean,
      benchmarks_results_file: String): Unit = {
    def calcResult() = {
      machine.eval(currentProgram, sem.parse(program), sem, output.isDefined, timeout)
    }
    runBasic[Exp, Abs, Addr, Time](machine,
                                   output,
                                   calcResult,
                                   benchmarks_results_file,
                                   timeout,
                                   inspect)
  }

  def runTraced[Exp: Expression,
                Abs: JoinLattice,
                Addr: Address,
                Time: Timestamp](
      machine: AbstractMachineTraced[Exp, Abs, Addr, Time])(
      programName: String,
      output: Option[String],
      timeout: Option[Long],
      inspect: Boolean,
      benchmarks_results_file: String): Unit = {
    def calcResult() = {
      machine.eval(programName, machine.sem.parse(programName), output.isDefined, timeout)
    }
    runBasic[Exp, Abs, Addr, Time](machine,
                                   output,
                                   calcResult,
                                   benchmarks_results_file,
                                   timeout,
                                   inspect)
  }

  object Done extends Exception

  def fileContent(file: String): String = {
    val f = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    content
  }

  def main(args: Array[String]) {
    Config.parser.parse(args, Config.Config()) match {
      case Some(config) => {

        def handleOptimization(): Unit =
          config.tracingFlags.OPTIMIZATION match {
            case ApplyNoOptimizations =>
            case ApplyAllOptimizations =>
              GlobalFlags.APPLY_OPTIMIZATION_CONSTANT_FOLDING = true
              GlobalFlags.APPLY_OPTIMIZATION_TYPE_SPECIALIZED_ARITHMETICS =
                true
              GlobalFlags.APPLY_OPTIMIZATION_VARIABLE_FOLDING = true
              GlobalFlags.APPLY_OPTIMIZATION_MERGE_ACTIONS = true
              GlobalFlags.APPLY_OPTIMIZATION_ENVIRONMENTS_LOADING = true
              GlobalFlags.APPLY_OPTIMIZATION_CONTINUATIONS_LOADING = true
            case ApplySpecificOptimizations(ids) =>
              ids.foreach({
                case 1 =>
                  GlobalFlags.APPLY_OPTIMIZATION_CONSTANT_FOLDING = true
                case 2 =>
                  GlobalFlags.APPLY_OPTIMIZATION_TYPE_SPECIALIZED_ARITHMETICS =
                    true
                case 3 =>
                  GlobalFlags.APPLY_OPTIMIZATION_VARIABLE_FOLDING = true
                case 4 =>
                  GlobalFlags.APPLY_OPTIMIZATION_MERGE_ACTIONS = true
                case 5 =>
                  GlobalFlags.APPLY_OPTIMIZATION_ENVIRONMENTS_LOADING = true
                case 6 =>
                  GlobalFlags.APPLY_OPTIMIZATION_CONTINUATIONS_LOADING = true
              })
          }

        GlobalFlags.ANALYSIS_RESULTS_OUTPUT = config.analysisPath
        handleOptimization()

        val lattice: SchemeLattice = config.lattice match {
          case Config.Lattice.Concrete =>
            if (config.counting) { ConcreteConcreteLattice } else {
              new ConcreteLattice(true)
            }
          case Config.Lattice.TypeSet => new TypeSetLattice(config.counting)
          case Config.Lattice.BoundedInt =>
            new BoundedIntLattice(config.bound, config.counting)
          case Config.Lattice.Constant =>
            new ConstantPropagationLattice(config.counting)
=======
import Util._
import scala.util.{Try, Success, Failure}

/**
 * Before looking at this, we recommend seeing how to use this framework. A
 * detailed example is available in LambdaCalculus.scala.
 *
 * This is the entry point. It parses the arguments, parses the input file and
 * launches an abstract machine on the parsed expression (or launches a REPL if no
 * input file is given). The pipeline goes as follows:
 *   1. The input program is parsed. For Scheme programs, it is done by:
 *      - Parsing the file as a list of s-expressions (exp/SExp.scala,
 *        exp/SExpParser.scala)
 *      - Compiling these s-expressions into Scheme expressions
 *        (exp/scheme/Scheme.scala)

 *   2. To run the program, we need an abstract machine and some
 *      semantics. Semantics definitions have to implement the Semantics
 *      interface (semantics/Semantics.scala).

 *   3. Once the abstract machine is created and we have a semantics for the
 *      program we want to analyze, the abstract machine can perform its
 *      evaluation, relying on methods of the semantics class to know how to
 *      evaluate expressions. The abstract machine only deals with which states
 *      to evaluate in which order, where to store values, where to store
 *      continuations, how to push and pop continuations, etc. The semantics
 *      encode what to do when encountering a program construct. For example,
 *      the semantics can tell what to evaluate next, that a continuation needs
 *      to be pushed, or that a variable needs to be updated. The abstract
 *      machine will then respectively evaluate the expression needed, push the
 *      continuation, or update the variable.
 *
 *      Multiple abstract machine implementations are available, defined in the
 *      machine/ directory. Every abstract machine implementation has to
 *      implement the AbstractMachine interface (machine/AbstractMachine.scala).
 *
 *      The abstract machine also uses a lattice to represent values. Lattices
 *      should implement the JoinLattice trait that can be found in
 *      JoinLattice.scala, which provides the basic features of a lattice.
 *
 *  If you want to:
 *  - Support a new language: you will need:
 *    - A parser, you can look into exp/SExpParser.scala as an inspiration. If
 *      your language is s-expression based, you can use this parser and compile
 *      s-expressions into your abstract grammar. To do so, look at
 *      exp/scheme/Scheme.scala.
 *    - An abstract grammar, look at exp/SExp.scala or the SchemeExp class in
 *      exp/scheme/Scheme.scala.
 *    - A semantics, look at semantics/anf/ANFSemantics.scala for a simple example.
 *    - Support for your language operations at the lattice level. For this,
 *      you'll probably need to extend the lattices (see
 *      lattice/scheme/SchemeLattice.scala, lattice/scheme/ModularLattice.scala)
 *  - Play with abstract machines, you can look into AAM.scala.
 *  - Implement some kind of analysis, look at examples/LambdaCalculus.scala and
 *    examples/TaintAnalysis.scala.
 */
object Main {
  /** Run a machine on a program with the given semantics. If @param output is
    * set, generate a dot graph visualizing the computed graph in the given
    * file. Return the number of states and time taken. */
  def run[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp](machine: AbstractMachine[Exp, Abs, Addr, Time], sem: Semantics[Exp, Abs, Addr, Time])(program: String, outputDot: Option[String], outputJSON: Option[String], timeout: Option[Long], inspect: Boolean): (Int, Double) = {
    println(s"Running ${machine.name} with lattice ${JoinLattice[Abs].name} and address ${Address[Addr].name}")
    val result = machine.eval(sem.parse(program), sem, !outputDot.isEmpty || !outputJSON.isEmpty, Timeout.start(timeout))
    outputDot.foreach(result.toFile(_)(GraphDOTOutput))
    outputJSON.foreach(result.toFile(_)(GraphJSONOutput))
    if (result.timedOut) println(s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
    println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")
    (result.numberOfStates, result.time)
  }

  def main(args: Array[String]) {
    import scala.util.control.Breaks._
    Config.parser.parse(args, Config.Config()).foreach(config => {
        val lattice: SchemeLattice = config.lattice match {
          case Config.Lattice.Concrete => new MakeSchemeLattice[Concrete.S, Concrete.B, Concrete.I, Concrete.F, Concrete.C, Concrete.Sym](config.counting)
          case Config.Lattice.TypeSet => new MakeSchemeLattice[Type.S, Concrete.B, Type.I, Type.F, Type.C, Type.Sym](config.counting)
          case Config.Lattice.BoundedInt =>
            val bounded = new BoundedInteger(config.bound)
                new MakeSchemeLattice[Type.S, Concrete.B, bounded.I, Type.F, Type.C, Type.Sym](config.counting)
          case Config.Lattice.ConstantPropagation => new MakeSchemeLattice[ConstantPropagation.S, Concrete.B, ConstantPropagation.I, ConstantPropagation.F, ConstantPropagation.C, ConstantPropagation.Sym](config.counting)
>>>>>>> 9de48f824fa56370876d922b957948f007216898
        }
        implicit val isSchemeLattice: IsSchemeLattice[lattice.L] = lattice.isSchemeLattice

<<<<<<< HEAD
        val time: TimestampWrapper =
          if (config.concrete) ConcreteTimestamp else ZeroCFA
        implicit val isTimestamp = time.isTimestamp

        val address: AddressWrapper = config.address match {
          case Config.Address.Classical => ClassicalAddress
          case Config.Address.ValueSensitive => ValueSensitiveAddress
        }
        implicit val isAddress = address.isAddress

        val sem = new SchemeSemantics[lattice.L, address.A, time.T](
          new SchemePrimitives[address.A, lattice.L])

        /*
         * Takes a non-tracing abstract machine as input and creates a function that, given a program string, runs
         * the abstract machine on that string.
         */
        def genNonTracingMachineStartFun(
            machine: AbstractMachine[SchemeExp, lattice.L, address.A, time.T])
          : (String) => Unit =
          (program: String) =>
            run(machine, sem)(program,
                              config.dotfile,
                              config.timeout,
                              config.inspect,
                              config.resultsPath)

        val startMachineFun: (String) => Unit = config.machine match {
          case Config.Machine.AAM =>
            genNonTracingMachineStartFun(
              new AAM[SchemeExp, lattice.L, address.A, time.T])
          case Config.Machine.AAMGlobalStore =>
            genNonTracingMachineStartFun(
              new AAMGlobalStore[SchemeExp, lattice.L, address.A, time.T])
          case Config.Machine.ConcreteMachine =>
            genNonTracingMachineStartFun(
              new ConcreteMachine[SchemeExp, lattice.L, address.A, time.T])
          case Config.Machine.AAC =>
            genNonTracingMachineStartFun(
              new AAC[SchemeExp, lattice.L, address.A, time.T])
          case Config.Machine.Free =>
            genNonTracingMachineStartFun(
              new Free[SchemeExp, lattice.L, address.A, time.T])
          case Config.Machine.Hybrid =>

            implicit val sabsCCLattice =
              ConcreteConcreteLattice.isSchemeLattice

            val sem = new BaseSchemeSemantics[ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T](
              new SchemePrimitives[HybridAddress.A, ConcreteConcreteLattice.L])

            val pointsLattice = new PointsToLattice(false)
            implicit val pointsConvLattice = pointsLattice.isSchemeLattice
            implicit val pointsLatInfoProv = pointsLattice.latticeInfoProvider

            implicit val CCLatInfoProv = ConcreteConcreteLattice.latticeInfoProvider

            val pointsToAnalysisLauncher = new PointsToAnalysisLauncher[pointsLattice.L](sem)(
                                                                                         pointsConvLattice,
                                                                                         pointsLatInfoProv,
                                                                                         config.analysisFlags)

            val machine = new HybridConcreteMachine[pointsLattice.L](pointsToAnalysisLauncher, config.analysisFlags)

            def calcResult(program: String)() = {
              machine.eval(currentProgram, sem.parse(program), sem, config.dotfile.isDefined, config.timeout)
            }

            (program: String) =>
              runBasic[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T](machine,
                config.dotfile,
                calcResult(program),
                config.resultsPath,
                config.timeout,
                config.inspect)

//          case Config.Machine.Hybrid => {
//
//            implicit val sabsCCLattice =
//              ConcreteConcreteLattice.isSchemeLattice
//            implicit val tracingFlags: TracingFlags = config.tracingFlags
//
//            val constLattice = new ConstantPropagationLattice(false)
//            implicit val constConvLattice = constLattice.isSchemeLattice
//            implicit val constLatInfoProv = constLattice.latticeInfoProvider
//
//            val pointsLattice = new PointsToLattice(false)
//            implicit val pointsConvLattice = pointsLattice.isSchemeLattice
//            implicit val pointsLatInfoProv = pointsLattice.latticeInfoProvider
//
//            implicit val CCLatInfoProv =
//              ConcreteConcreteLattice.latticeInfoProvider
//
//            val sabs =
//              implicitly[IsConvertableLattice[ConcreteConcreteLattice.L]]
//
//            /**
//              * A factory method for the construction of several components required for running the HybridMachine:
//              * the semantics, the ConstantsAnalysisLauncher, (possibly, if one can be created) an optimizer for the traces,
//              * and an injection function for creating an initial state for the machine.
//              * Construction of these components depends on whether the machine will execute regular Scheme programs
//              * or Amb-Scheme programs (in which case other semantics and another injection function should be used,
//              * and no optimizer can be created).
//              * @param createConstantsAnalysisLauncher A factory-function that, given some SchemeSemantics, creates a ConstantsAnalysisLauncher.
//              * @return A four-tuple consisting of the four components mentioned above.
//              */
//            def constructComponents(
//                createConstantsAnalysisLauncher: SemanticsTraced[
//                  SchemeExp,
//                  ConcreteConcreteLattice.L,
//                  HybridAddress.A,
//                  HybridTimestamp.T] with ConvertableSemantics[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A,
//                  HybridTimestamp.T] =>
//                  ConstantsAnalysisLauncher[
//                  constLattice.L])
//              : (SchemeSemanticsTraced[ConcreteConcreteLattice.L,
//                                       HybridAddress.A,
//                                       HybridTimestamp.T],
//                 ConstantsAnalysisLauncher[constLattice.L],
//                 Option[SchemeTraceOptimizer[constLattice.L]],
//                 SchemeExp => TracingProgramState[
//                   SchemeExp,
//                   HybridAddress.A,
//                   HybridTimestamp.T]) = {
//
//              if (config.amb) {
//                val sem =
//                  new AmbSchemeSemanticsTraced[ConcreteConcreteLattice.L,
//                                               HybridAddress.A,
//                                               HybridTimestamp.T](
//                    new SchemePrimitives[HybridAddress.A,
//                                         ConcreteConcreteLattice.L])
//                val constantsAnalysisLauncher =
//                  createConstantsAnalysisLauncher(sem)
//                val injectState = { (exp: SchemeExp) =>
//                  val normalState = new ProgramState[SchemeExp](sem, exp)
//                  AmbProgramState[SchemeExp](normalState,
//                                             List(HaltFailFrame()))
//                }
//                (sem, constantsAnalysisLauncher, None, injectState)
//              } else {
//                val sem = new SchemeSemanticsTraced[ConcreteConcreteLattice.L,
//                                                    HybridAddress.A,
//                                                    HybridTimestamp.T](
//                  new SchemePrimitives[HybridAddress.A,
//                                       ConcreteConcreteLattice.L])
//                val constantsAnalysisLauncher =
//                  createConstantsAnalysisLauncher(sem)
//                val someOptimizer = Some(
//                  new SchemeTraceOptimizer[constLattice.L](
//                    sem,
//                    constantsAnalysisLauncher))
//                val injectState = { (exp: SchemeExp) =>
//                  new ProgramState[SchemeExp](sem, exp)
//                }
//                (sem, constantsAnalysisLauncher, someOptimizer, injectState)
//              }
//            }
//
//            val (sem, constantsAnalysisLauncher, someOptimizer, injectState) =
//              constructComponents(
//                new ConstantsAnalysisLauncher[constLattice.L](_))
//            val pointsToAnalysisLauncher =
//              new PointsToAnalysisLauncher[pointsLattice.L](sem)(
//                pointsConvLattice,
//                pointsLatInfoProv)
//            val tracerContext =
//              new SchemeTracer[ConcreteConcreteLattice.L,
//                               HybridAddress.A,
//                               HybridTimestamp.T](sem, someOptimizer)
//            val machine = new HybridMachine[constLattice.L, pointsLattice.L](
//              sem,
//              constantsAnalysisLauncher,
//              pointsToAnalysisLauncher,
//              tracerContext,
//              injectState)
//            (program: String) =>
//              runTraced[SchemeExp,
//                        ConcreteConcreteLattice.L,
//                        HybridAddress.A,
//                        HybridTimestamp.T](machine)(program,
//                                                    config.dotfile,
//                                                    config.timeout,
//                                                    config.inspect,
//                                                    config.resultsPath)
//          }
        }

        try {
          do {
            val program = config.file match {
              case Some(file) => currentProgram = file; fileContent(file)
              case None => StdIn.readLine(">>> ")
            }
            if (program == null) throw Done
            if (program.size > 0) startMachineFun(program)
            //              config.machine match {
            //                case Config.Machine.Hybrid => runTraced(machine, sem)(program, config.dotfile, config.timeout, config.inspect, config.resultsPath)
            //                case _ => run(machine, sem)(program, config.dotfile, config.timeout, config.inspect, config.resultsPath)
            //              }
          } while (config.file.isEmpty);
        } catch {
          case Done => ()
=======
        config.language match {
          case Config.Language.Scheme =>
            val time: TimestampWrapper = if (config.concrete) ConcreteTimestamp else ZeroCFA
            implicit val isTimestamp = time.isTimestamp

            val address: AddressWrapper = config.address match {
              case Config.Address.Classical => ClassicalAddress
              case Config.Address.ValueSensitive => ValueSensitiveAddress
            }
            implicit val isAddress = address.isAddress

            val machine = config.machine match {
              case Config.Machine.AAM => new AAM[SchemeExp, lattice.L, address.A, time.T]
              case Config.Machine.AAMGlobalStore => new AAMAACP4F[SchemeExp, lattice.L, address.A, time.T](AAMKAlloc)
              case Config.Machine.ConcreteMachine => new ConcreteMachine[SchemeExp, lattice.L, address.A, time.T]
              case Config.Machine.AAC => new AAMAACP4F[SchemeExp, lattice.L, address.A, time.T](AACKAlloc)
              case Config.Machine.Free => new AAMAACP4F[SchemeExp, lattice.L, address.A, time.T](P4FKAlloc)
            }

            val sem = new SchemeSemantics[lattice.L, address.A, time.T](new SchemePrimitives[address.A, lattice.L])

            replOrFile(config.file, program => run(machine, sem)(program, config.dotfile, config.jsonfile, config.timeout.map(_.toNanos), config.inspect))
          case Config.Language.CScheme =>
            val clattice: CSchemeLattice = new MakeCSchemeLattice[lattice.L]
            implicit val isCSchemeLattice = clattice.isCSchemeLattice

            val time: TimestampWrapper = if (config.concrete) ConcreteTimestamp else ZeroCFA
            implicit val isTimestamp = time.isTimestamp

            val address: AddressWrapper = config.address match {
              case Config.Address.Classical => ClassicalAddress
              case Config.Address.ValueSensitive => ValueSensitiveAddress
            }
            implicit val isAddress = address.isAddress

            val machine = config.machine match {
              case Config.Machine.AAM => new ConcurrentAAM[SchemeExp, clattice.L, address.A, time.T, ContextSensitiveTID](AllInterleavings)
              case _ => throw new Exception(s"unsupported machine for CScheme: ${config.machine}")
            }

            val sem = new CSchemeSemantics[clattice.L, address.A, time.T, ContextSensitiveTID](new CSchemePrimitives[address.A, clattice.L])
            replOrFile(config.file, program => run(machine, sem)(program, config.dotfile, config.jsonfile, config.timeout.map(_.toNanos), config.inspect))
          case Config.Language.AScheme =>
            val alattice: ASchemeLattice = new MakeASchemeLattice[lattice.L]
            implicit val isASchemeLattice = alattice.isASchemeLattice

            import ActorTimestamp.fromTime
            val time: ActorTimestampWrapper = if (config.concrete) ConcreteTimestamp else KMessageTagSensitivity(1)
            implicit val isTimestamp = time.isActorTimestamp

            val address: AddressWrapper = config.address match {
              case Config.Address.Classical => ClassicalAddress
              case Config.Address.ValueSensitive => ValueSensitiveAddress
            }
            implicit val isAddress = address.isAddress

            val mbox = config.mbox match {
              case Config.Mbox.Powerset => new PowersetMboxImpl[ContextSensitiveTID, alattice.L]
              case Config.Mbox.BoundedList => new BoundedListMboxImpl[ContextSensitiveTID, alattice.L](config.mboxBound)
              case Config.Mbox.BoundedMultiset => new BoundedMultisetMboxImpl[ContextSensitiveTID, alattice.L](config.mboxBound)
              case Config.Mbox.Graph => new GraphMboxImpl[ContextSensitiveTID, alattice.L]
            }

            val machine = config.machine match {
              case Config.Machine.AAM => new ActorsAAM[SchemeExp, alattice.L, address.A, time.T, ContextSensitiveTID](mbox)
              case Config.Machine.AAMGlobalStore => new ActorsAAMGlobalStore[SchemeExp, alattice.L, address.A, time.T, ContextSensitiveTID](mbox)
              case _ => throw new Exception(s"unsupported machine for AScheme: ${config.machine}")
            }

            val visitor = new RecordActorVisitor[SchemeExp, alattice.L, address.A]
            val sem = new ASchemeSemanticsWithVisitorAndOptimization[alattice.L, address.A, time.T, ContextSensitiveTID](new SchemePrimitives[address.A, alattice.L], visitor)
            val N = 1
            val warmup = if (N > 1) 2 else 0 // 2 runs that are ignored to warm up
            val (states, times) = (1 to N+warmup).map(i =>
              runOnFile(config.file.get, program => run(machine, sem)(program, config.dotfile, config.jsonfile, config.timeout.map(_.toNanos), config.inspect))).unzip
            println("States: " + states.mkString(", "))
            println("Time: " + times.drop(warmup).mkString(","))
            if (N == 1) visitor.print
>>>>>>> 9de48f824fa56370876d922b957948f007216898
        }
      })
    Profiler.print
  }
}

object ScalaAM {
  /** Simply run a program and return the result. Compute the graph is @param graph is true. */
  def run[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp](machine: AbstractMachine[Exp, Abs, Addr, Time], sem: Semantics[Exp, Abs, Addr, Time])(program: String, graph: Boolean = true, timeout: Option[Long] = None): AbstractMachine[Exp, Abs, Addr, Time]#Output = {
    val result = machine.eval(sem.parse(program), sem, graph, Timeout.start(timeout))
    if (result.timedOut) println(s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
    result
  }
  /** Some lattice instanciations */
  val typeLattice = new MakeSchemeLattice[Type.S, Concrete.B, Type.I, Type.F, Type.C, Type.Sym](false)
  val concreteLattice = new MakeSchemeLattice[Concrete.S, Concrete.B, Concrete.I, Concrete.F, Concrete.C, Concrete.Sym](false)
  val cpLattice = new MakeSchemeLattice[ConstantPropagation.S, Concrete.B, ConstantPropagation.I, ConstantPropagation.F, ConstantPropagation.C, ConstantPropagation.Sym](false)

  /* You can then launch a console to load ScalaAM (sbt console), and perform the following:
   * > import ScalaAM._
   * > run(new AAM[SchemeExp, cpLattice.L, ClassicalAddress.A, ZeroCFA.T], new SchemeSemantics[cpLattice.L, ClassicalAddress.A, ZeroCFA.T](new SchemePrimitives[ClassicalAddress.A, cpLattice.L]))("(* 2 3)")
   * From there on, you can inspect the result.
   */

  /* Or you can use one of the preinstantiated machines:
   * > ScalaAM.FastConcrete.eval("(+ 1 2 3)")
   * Or with a REPL:
   * > ScalaAM.repl(ScalaAM.FastConcrete.eval _)
   */
  object FastConcrete {
    def eval(program: String, timeout: Option[Long] = None): Option[concreteLattice.L] = {
      val output = run[SchemeExp, concreteLattice.L, ClassicalAddress.A, ConcreteTimestamp.T](
        new ConcreteMachine[SchemeExp, concreteLattice.L, ClassicalAddress.A, ConcreteTimestamp.T],
        new SchemeSemantics[concreteLattice.L, ClassicalAddress.A, ConcreteTimestamp.T](new SchemePrimitives[ClassicalAddress.A, concreteLattice.L]))(program, false, timeout)
      assert(output.finalValues.size <= 1)
      output.finalValues.headOption
    }
  }

  object ConstantPropagationAAM {
    def eval(program: String, timeout: Option[Long] = None): Set[cpLattice.L] = {
      val output = run[SchemeExp, cpLattice.L, ClassicalAddress.A, ZeroCFA.T](
        new AAM[SchemeExp, cpLattice.L, ClassicalAddress.A, ZeroCFA.T],
        new SchemeSemantics[cpLattice.L, ClassicalAddress.A, ZeroCFA.T](new SchemePrimitives[ClassicalAddress.A, cpLattice.L]))(program, false, timeout)
      output.finalValues
    }
  }

  object TypeAAM {
    def eval(program: String, timeout: Option[Long] = None): Set[typeLattice.L] = {
      val output = run[SchemeExp, typeLattice.L, ClassicalAddress.A, ZeroCFA.T](
        new AAM[SchemeExp, typeLattice.L, ClassicalAddress.A, ZeroCFA.T],
        new SchemeSemantics[typeLattice.L, ClassicalAddress.A, ZeroCFA.T](new SchemePrimitives[ClassicalAddress.A, typeLattice.L]))(program, false, timeout)
      output.finalValues
    }
  }

  def repl[A](eval: (String, Option[Long]) => A): Unit = {
    Util.replOrFile(None, p => println(eval(p, None)))
  }
}
