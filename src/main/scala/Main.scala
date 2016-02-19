import scala.io.StdIn
import Timestamps._

/**
 * This is the entry point. It parses the arguments, parses the input file and
 * launch an abstract machine on the parsed expression (or launches a REPL if no
 * input file is given). The code in this file isn't very clean and I'd like to
 * improve it at some point, but my scala-fu isn't good enough to do it now. The
 * pipeline goes as follows:
 *   1. The input program is parsed. For Scheme programs, it is done by:
 *      - Parsing the file as a list of s-expressions (SExp.scala, SExpParser.scala)
 *      - Compiling these s-expressions into Scheme expressions (Scheme.scala)
 *      - Optionally, converting Scheme expressions into ANF form (ANF.scala) to
 *        have a simpler interpreter (but longer programs)

 *   2. To run the program, we need an abstract machine and some semantics. For
 *      now, the only semantics available are for ANF Scheme and Scheme
 *      (ANFSemantics.scala, SchemeSemantics.scala). Semantics definitions have
 *      to implement the Semantics interface (Semantics.scala).

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
 *      Three abstract machine implementations are available:
 *      - The classical Abstracting Abstract Machine of Might and Van Horn (AAM.scala)
 *      - Johnson's Abstracting Abstract Control (AAC.scala)
 *      - Gilrey's Pushdown Control-Flow Analysis for Free (Free.scala) Every
 *      abstract machine implementation has to implement the AbstractMachine
 *      interface (AbstractMachine.scala).
 *
 *      The abstract machine also uses a lattice to represent values. Lattices
 *      should implement some traits that can be found in
 *      AbstractValue.scala. The following lattices are available:
 *      - A concrete lattice, AbstractConcrete.scala
 *      - A type lattice, representing each value by its type, AbstractType.scala
 *      - A type set lattice, representing each value by a set of its possible
 *        types (to avoid having a top element that loses all precision),
 *        AbstractTypeSet.scala
 *
 *  If you want to:
 *  - Support a new language: you will need:
 *    - A parser, you can look into SExpParser.scala as an inspiration. If your
 *      language is s-expression based, you can use this parser and compile
 *      s-expressions into your abstract grammar. To do so, look at Scheme.scala
 *    - An abstract grammar, look at SExp.scala or the SchemeExp class in Scheme.scala
 *    - A semantics, look at ANFSemantics.scala for a simple example
 *    - Support for your language operations at the lattice level. For this,
 *      you'll probably need to extend the lattices (AbstractValue.scala,
 *      AbstractConcrete.scala, AbstractType.scala, AbstractTypeSet.scala)
 *  - Play with abstract machines, you can look into AAM.scala, AAC.scala or
 *    Free.scala (AAM is the simplest machine).
 *  - Implement some kind of analysis, you'll probably need to design a lattice
 *    that is suited for your analysis. You can use an existing lattice as an
 *    inspiration (AbstractType.scala is a good lattice to start with, even
 *    though its very imprecise, it's simple. You can then look into
 *    AbstractTypeSet.scala).
 */

/**
 * This is where we parse the arguments given to the implementation
 */
object Config {
  object Machine extends Enumeration {
    type Machine = Value
    val AAC, AAM, Free, ConcurrentAAM, Hybrid = Value
  }
  implicit val machineRead: scopt.Read[Machine.Value] = scopt.Read.reads(Machine withName _)

  object Lattice extends Enumeration {
    type Lattice = Value
    val Concrete, Type, TypeSet, Test = Value
  }
  implicit val latticeRead: scopt.Read[Lattice.Value] = scopt.Read.reads(Lattice withName _)

  case class Config(machine: Machine.Value = Machine.Free, lattice: Lattice.Value = Lattice.TypeSet, concrete: Boolean = false, file: Option[String] = None, dotfile: Option[String] = None, anf: Boolean = false, diff: Option[(Int, Int)] = None)

  val parser = new scopt.OptionParser[Config]("scala-am") {
    head("scala-ac", "0.0")
    opt[Machine.Value]('m', "machine") action { (x, c) => c.copy(machine = x) } text("Abstract machine to use (AAM, AAC, Free, ConcurrentAAM, Hybrid)")
    opt[Lattice.Value]('l', "lattice") action { (x, c) => c.copy(lattice = x) } text("Lattice to use (Concrete, Type, TypeSet)")
    opt[Unit]('c', "concrete") action { (_, c) => c.copy(concrete = true) } text("Run in concrete mode")
    opt[String]('d', "dotfile") action { (x, c) => c.copy(dotfile = Some(x)) } text("Dot file to output graph to")
    opt[Unit]("anf") action { (_, c) => c.copy(anf = true) } text("Desugar program into ANF")
    // opt[(Int, Int)]("diff") action { (x, c) => c.copy(diff = Some(x)) } text("States to diff") /* TODO: take this into account */
    opt[String]('f', "file") action { (x, c) => c.copy(file = Some(x)) } text("File to read program from")
  }
}

object Main {

  /** Run a machine on a program with the given semantics. If @param output is
    * set, generate a dot graph visualizing the computed graph in the given
    * file. */
  def run[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp](machine: AbstractMachine[Exp, Abs, Addr, Time], sem: Semantics[Exp, Abs, Addr, Time])(program: String, output: Option[String]): Unit = {
    val abs = implicitly[AbstractValue[Abs]]
    val addr = implicitly[Address[Addr]]
    println(s"Running ${machine.name} with lattice ${abs.name} and address ${addr.name}")
    val result = machine.eval(sem.parse(program), sem, !output.isEmpty)
    output match {
      case Some(f) => result.toDotFile(f)
      case None => ()
    }
    println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")
  }

  /** Run a machine on a program with the given semantics. If @param output is
    * set, generate a dot graph visualizing the computed graph in the given
    * file. */
  def runTraced[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp](machine: AbstractMachineTraced[Exp, Abs, Addr, Time])(program: String, output: Option[String]): Unit = {
    val abs = implicitly[AbstractValue[Abs]]
    val addr = implicitly[Address[Addr]]
    println(s"Running ${machine.name} with lattice ${abs.name} and address ${addr.name}")
    val result = machine.eval(machine.sem.parse(program), !output.isEmpty)
    output match {
      case Some(f) => result.toDotFile(f)
      case None => ()
    }
    println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")
  }

  object Done extends Exception

  def fileContent(file: String): String = {
    val f = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    content
  }

  def main(args: Array[String]) {
    import scala.util.control.Breaks._
    Config.parser.parse(args, Config.Config()) match {
      case Some(config) => {
        /* ugly as fuck, but I don't find a simpler way to pass type parameters that are computed at runtime */
        val f = (config.anf, config.machine, config.lattice, config.concrete) match {
          case (false, Config.Machine.Hybrid, Config.Lattice.Concrete, true) =>
            val semantics = new SchemeSemanticsTraced[HybridLattice.Hybrid, HybridAddress, ZeroCFA]
            runTraced(new HybridMachine[SchemeExp, ZeroCFA](semantics)) _

          case (true, Config.Machine.AAM, Config.Lattice.Concrete, true) => run(new AAM[ANFExp, AbstractConcrete, ConcreteAddress, ZeroCFA], new ANFSemantics[AbstractConcrete, ConcreteAddress, ZeroCFA]) _
          case (false, Config.Machine.AAM, Config.Lattice.Concrete, true) => run(new AAM[SchemeExp, AbstractConcrete, ConcreteAddress, ZeroCFA], new SchemeSemantics[AbstractConcrete, ConcreteAddress, ZeroCFA]) _
          case (true, Config.Machine.AAM, Config.Lattice.Concrete, false) => run(new AAM[ANFExp, AbstractConcrete, ClassicalAddress, ZeroCFA], new ANFSemantics[AbstractConcrete, ClassicalAddress, ZeroCFA]) _
          case (false, Config.Machine.AAM, Config.Lattice.Concrete, false) => run(new AAM[SchemeExp, AbstractConcrete, ClassicalAddress, ZeroCFA], new SchemeSemantics[AbstractConcrete, ClassicalAddress, ZeroCFA]) _
          case (true, Config.Machine.AAM, Config.Lattice.Type, true) => run(new AAM[ANFExp, AbstractType, ConcreteAddress, ZeroCFA], new ANFSemantics[AbstractType, ConcreteAddress, ZeroCFA]) _
          case (false, Config.Machine.AAM, Config.Lattice.Type, true) => run(new AAM[SchemeExp, AbstractType, ConcreteAddress, ZeroCFA], new SchemeSemantics[AbstractType, ConcreteAddress, ZeroCFA]) _
          case (true, Config.Machine.AAM, Config.Lattice.Type, false) => run(new AAM[ANFExp, AbstractType, ClassicalAddress, ZeroCFA], new ANFSemantics[AbstractType, ClassicalAddress, ZeroCFA]) _
          case (false, Config.Machine.AAM, Config.Lattice.Type, false) => run(new AAM[SchemeExp, AbstractType, ClassicalAddress, ZeroCFA], new SchemeSemantics[AbstractType, ClassicalAddress, ZeroCFA]) _
          case (true, Config.Machine.AAM, Config.Lattice.TypeSet, true) => run(new AAM[ANFExp, AbstractTypeSet, ConcreteAddress, ZeroCFA], new ANFSemantics[AbstractTypeSet, ConcreteAddress, ZeroCFA]) _
          case (false, Config.Machine.AAM, Config.Lattice.TypeSet, true) => run(new AAM[SchemeExp, AbstractTypeSet, ConcreteAddress, ZeroCFA], new SchemeSemantics[AbstractTypeSet, ConcreteAddress, ZeroCFA]) _
          case (true, Config.Machine.AAM, Config.Lattice.TypeSet, false) => run(new AAM[ANFExp, AbstractTypeSet, ClassicalAddress, ZeroCFA], new ANFSemantics[AbstractTypeSet, ClassicalAddress, ZeroCFA]) _
          case (false, Config.Machine.AAM, Config.Lattice.TypeSet, false) => run(new AAM[SchemeExp, AbstractTypeSet, ClassicalAddress, ZeroCFA], new SchemeSemantics[AbstractTypeSet, ClassicalAddress, ZeroCFA]) _
          case (true, Config.Machine.AAC, Config.Lattice.Concrete, true) => run(new AAC[ANFExp, AbstractConcrete, ConcreteAddress, ZeroCFA], new ANFSemantics[AbstractConcrete, ConcreteAddress, ZeroCFA]) _
          case (false, Config.Machine.AAC, Config.Lattice.Concrete, true) => run(new AAC[SchemeExp, AbstractConcrete, ConcreteAddress, ZeroCFA], new SchemeSemantics[AbstractConcrete, ConcreteAddress, ZeroCFA]) _
          case (true, Config.Machine.AAC, Config.Lattice.Concrete, false) => run(new AAC[ANFExp, AbstractConcrete, ClassicalAddress, ZeroCFA], new ANFSemantics[AbstractConcrete, ClassicalAddress, ZeroCFA]) _
          case (false, Config.Machine.AAC, Config.Lattice.Concrete, false) => run(new AAC[SchemeExp, AbstractConcrete, ClassicalAddress, ZeroCFA], new SchemeSemantics[AbstractConcrete, ClassicalAddress, ZeroCFA]) _
          case (true, Config.Machine.AAC, Config.Lattice.Type, true) => run(new AAC[ANFExp, AbstractType, ConcreteAddress, ZeroCFA], new ANFSemantics[AbstractType, ConcreteAddress, ZeroCFA]) _
          case (false, Config.Machine.AAC, Config.Lattice.Type, true) => run(new AAC[SchemeExp, AbstractType, ConcreteAddress, ZeroCFA], new SchemeSemantics[AbstractType, ConcreteAddress, ZeroCFA]) _
          case (true, Config.Machine.AAC, Config.Lattice.Type, false) => run(new AAC[ANFExp, AbstractType, ClassicalAddress, ZeroCFA], new ANFSemantics[AbstractType, ClassicalAddress, ZeroCFA]) _
          case (false, Config.Machine.AAC, Config.Lattice.Type, false) => run(new AAC[SchemeExp, AbstractType, ClassicalAddress, ZeroCFA], new SchemeSemantics[AbstractType, ClassicalAddress, ZeroCFA]) _
          case (true, Config.Machine.AAC, Config.Lattice.TypeSet, true) => run(new AAC[ANFExp, AbstractTypeSet, ConcreteAddress, ZeroCFA], new ANFSemantics[AbstractTypeSet, ConcreteAddress, ZeroCFA]) _
          case (false, Config.Machine.AAC, Config.Lattice.TypeSet, true) => run(new AAC[SchemeExp, AbstractTypeSet, ConcreteAddress, ZeroCFA], new SchemeSemantics[AbstractTypeSet, ConcreteAddress, ZeroCFA]) _
          case (true, Config.Machine.AAC, Config.Lattice.TypeSet, false) => run(new AAC[ANFExp, AbstractTypeSet, ClassicalAddress, ZeroCFA], new ANFSemantics[AbstractTypeSet, ClassicalAddress, ZeroCFA]) _
          case (false, Config.Machine.AAC, Config.Lattice.TypeSet, false) => run(new AAC[SchemeExp, AbstractTypeSet, ClassicalAddress, ZeroCFA], new SchemeSemantics[AbstractTypeSet, ClassicalAddress, ZeroCFA]) _
          case (true, Config.Machine.Free, Config.Lattice.Concrete, true) => run(new Free[ANFExp, AbstractConcrete, ConcreteAddress, ZeroCFA], new ANFSemantics[AbstractConcrete, ConcreteAddress, ZeroCFA]) _
          case (false, Config.Machine.Free, Config.Lattice.Concrete, true) => run(new Free[SchemeExp, AbstractConcrete, ConcreteAddress, ZeroCFA], new SchemeSemantics[AbstractConcrete, ConcreteAddress, ZeroCFA]) _
          case (true, Config.Machine.Free, Config.Lattice.Concrete, false) => run(new Free[ANFExp, AbstractConcrete, ClassicalAddress, ZeroCFA], new ANFSemantics[AbstractConcrete, ClassicalAddress, ZeroCFA]) _
          case (false, Config.Machine.Free, Config.Lattice.Concrete, false) => run(new Free[SchemeExp, AbstractConcrete, ClassicalAddress, ZeroCFA], new SchemeSemantics[AbstractConcrete, ClassicalAddress, ZeroCFA]) _
          case (true, Config.Machine.Free, Config.Lattice.Type, true) => run(new Free[ANFExp, AbstractType, ConcreteAddress, ZeroCFA], new ANFSemantics[AbstractType, ConcreteAddress, ZeroCFA]) _
          case (false, Config.Machine.Free, Config.Lattice.Type, true) => run(new Free[SchemeExp, AbstractType, ConcreteAddress, ZeroCFA], new SchemeSemantics[AbstractType, ConcreteAddress, ZeroCFA]) _
          case (true, Config.Machine.Free, Config.Lattice.Type, false) => run(new Free[SchemeExp, AbstractType, ClassicalAddress, ZeroCFA], new SchemeSemantics[AbstractType, ClassicalAddress, ZeroCFA]) _
          case (true, Config.Machine.Free, Config.Lattice.TypeSet, true) => run(new Free[ANFExp, AbstractTypeSet, ConcreteAddress, ZeroCFA], new ANFSemantics[AbstractTypeSet, ConcreteAddress, ZeroCFA]) _
          case (false, Config.Machine.Free, Config.Lattice.TypeSet, true) => run(new Free[SchemeExp, AbstractTypeSet, ConcreteAddress, ZeroCFA], new SchemeSemantics[AbstractTypeSet, ConcreteAddress, ZeroCFA]) _
          case (true, Config.Machine.Free, Config.Lattice.TypeSet, false) => run(new Free[ANFExp, AbstractTypeSet, ClassicalAddress, ZeroCFA], new ANFSemantics[AbstractTypeSet, ClassicalAddress, ZeroCFA]) _
          case (false, Config.Machine.Free, Config.Lattice.TypeSet, false) => run(new Free[SchemeExp, AbstractTypeSet, ClassicalAddress, ZeroCFA], new SchemeSemantics[AbstractTypeSet, ClassicalAddress, ZeroCFA]) _
          case (false, Config.Machine.ConcurrentAAM, Config.Lattice.Concrete, true) => run(new ConcurrentAAM[SchemeExp, AbstractConcrete, ConcreteAddress, ZeroCFA, ConcreteTID], new ConcurrentSchemeSemantics[AbstractConcrete, ConcreteAddress, ZeroCFA, ConcreteTID]) _
          case (false, Config.Machine.ConcurrentAAM, Config.Lattice.Concrete, false) => run(new ConcurrentAAM[SchemeExp, AbstractConcrete, ClassicalAddress, ZeroCFA, ConcreteTID], new ConcurrentSchemeSemantics[AbstractConcrete, ClassicalAddress, ZeroCFA, ConcreteTID]) _
          case (false, Config.Machine.ConcurrentAAM, Config.Lattice.Type, true) => run(new ConcurrentAAM[SchemeExp, AbstractType, ConcreteAddress, ZeroCFA, ConcreteTID], new ConcurrentSchemeSemantics[AbstractType, ConcreteAddress, ZeroCFA, ConcreteTID]) _
          case (false, Config.Machine.ConcurrentAAM, Config.Lattice.Type, false) => run(new ConcurrentAAM[SchemeExp, AbstractType, ClassicalAddress, ZeroCFA, ConcreteTID], new ConcurrentSchemeSemantics[AbstractType, ClassicalAddress, ZeroCFA, ConcreteTID]) _
          case (false, Config.Machine.ConcurrentAAM, Config.Lattice.TypeSet, true) => run(new ConcurrentAAM[SchemeExp, AbstractTypeSet, ConcreteAddress, ZeroCFA, ConcreteTID], new ConcurrentSchemeSemantics[AbstractTypeSet, ConcreteAddress, ZeroCFA, ConcreteTID]) _
          case (false, Config.Machine.ConcurrentAAM, Config.Lattice.TypeSet, false) => run(new ConcurrentAAM[SchemeExp, AbstractTypeSet, ClassicalAddress, ZeroCFA, ConcreteTID], new ConcurrentSchemeSemantics[AbstractTypeSet, ClassicalAddress, ZeroCFA, ConcreteTID]) _
          /* Example of how to use the product lattice */
          case (false, Config.Machine.Free, Config.Lattice.Test, false) => {
            val prod = new ProductLattice[AbstractType, AbstractTypeSet]
            import prod._
            run(new Free[SchemeExp, Product, ClassicalAddress, ZeroCFA], new SchemeSemantics[Product, ClassicalAddress, ZeroCFA]) _
          }
          case _ => throw new Exception(s"Impossible configuration: $config")
        }
        try {
          do {
            val program = config.file match {
              case Some(file) => fileContent(file)
              case None => StdIn.readLine(">>> ")
            }
            if (program == null) throw Done
            f(program, config.dotfile)
          } while (config.file.isEmpty)
        } catch {
          case Done => ()
        }
      }
      case None => ()
    }
  }
}
