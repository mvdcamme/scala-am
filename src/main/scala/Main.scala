import scala.io.StdIn
import java.io._

import ConcreteConcreteLattice.ConcreteValue

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
                    resultsPath: String = "benchmark_times.txt",
                    analysisPath: Option[String] = None,
                    analysisFlags: AnalysisFlags = AnalysisFlags())

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
    opt[Unit]("disable_skip_iteration_optimisation") action { (_, c) =>
      c.copy(analysisFlags = c.analysisFlags.copy(skipIterationOptimisation = false))
    } text ("Turn optimisation of incremental analysis OFF")
    opt[Unit]("disable_delta_optimisation") action { (_, c) =>
      c.copy(analysisFlags = c.analysisFlags.copy(deltaOptimisation = false))
    } text ("Turn extra optimisation of incremental analysis OFF")
    opt[Unit]("disable_propagation") action { (_, c) =>
      c.copy(analysisFlags = c.analysisFlags.copy(doPropagationPhase = false))
    } text ("Disable the run-time info propagation phase in the incremental analysis")
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

    val abs = implicitly[JoinLattice[Abs]]
    val addr = implicitly[Address[Addr]]
    println(s"Running ${machine.name} with lattice ${abs.name} and address ${addr.name}")

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

        GlobalFlags.ANALYSIS_RESULTS_OUTPUT = config.analysisPath

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
        }
        implicit val isSchemeLattice = lattice.isSchemeLattice

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

            implicit val joinLattice: JoinLattice[ConcreteValue] = ConcreteConcreteLattice.isSchemeLattice
            implicit val sabsCCLattice: IsSchemeLattice[ConcreteValue] = ConcreteConcreteLattice.isSchemeLattice
            val sem = new ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T](new SchemePrimitives[HybridAddress.A, ConcreteConcreteLattice.L])
            val pointsLattice = new TypeSetLattice(false)
            implicit val pointsConvLattice = pointsLattice.isSchemeLattice
            implicit val pointsLatInfoProv = pointsLattice.latticeInfoProvider

            implicit val CCLatInfoProv = ConcreteConcreteLattice.latticeInfoProvider

            val pointsToAnalysisLauncher = new PointsToAnalysisLauncher[pointsLattice.L](sem)(
                                                                                         pointsConvLattice,
                                                                                         pointsLatInfoProv,
                                                                                         config.analysisFlags)

            val machine = new ConcolicMachine[pointsLattice.L](pointsToAnalysisLauncher, config.analysisFlags)

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
        }

        try {
          do {
            val program = config.file match {
              case Some(file) => currentProgram = file; fileContent(file)
              case None => StdIn.readLine(">>> ")
            }
            if (program == null) throw Done
            if (program.nonEmpty) startMachineFun(program)
          } while (config.file.isEmpty);
        } catch {
          case Done => ()
        }
      }
      case None => ()
    }
    Profiler.print
  }
}
