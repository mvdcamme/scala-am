import scala.concurrent.duration.Duration
import Util._

case class MachineConfig(
    program: String,
    machine: Config.Machine.Value = Config.Machine.AAM,
    address: Config.Address.Value = Config.Address.Classical,
    lattice: Config.Lattice.Value = Config.Lattice.TypeSet,
    concrete: Boolean = false) {
  override def toString = s"[$program, $machine, $address, $lattice]"
}

abstract class Benchmarks(dir: String,
                          inputs: Iterable[MachineConfig],
                          classify: MachineConfig => String) {
  val now = java.util.Calendar.getInstance.getTime
  val timeformat = new java.text.SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
  val stdout = scala.Console.out
  val fileout = new java.io.FileOutputStream(
    new java.io.File(s"benchmarks-${timeformat.format(now)}.log"))
  val logging = new java.io.OutputStream {
    override def write(b: Int) { stdout.write(b); fileout.write(b) }
  }

  case class MachineOutput(time: Double, states: Int, timedOut: Boolean) {
    override def toString =
      if (timedOut) { s"/, ${states}+" } else { f"$time%.2f, $states" }
  }

  import akka.actor.{ActorRef, ActorSystem, Props, Actor, Inbox}
  import scala.concurrent.duration._

  case class Computation(config: MachineConfig)
  case class Result(in: MachineConfig, out: MachineOutput)
  case class AddWork(items: Iterable[MachineConfig])
  case class SendWork(actor: ActorRef)

  class Worker(timeout: Option[Long]) extends Actor {
    def compute(config: MachineConfig): MachineOutput = {
      val lattice: SchemeLattice = config.lattice match {
        case Config.Lattice.Concrete => new MakeSchemeLattice[Concrete.S, Concrete.B, Concrete.I, Concrete.F, Concrete.C, Concrete.Sym](true)
        case Config.Lattice.TypeSet => new MakeSchemeLattice[Type.S, Concrete.B, Type.I, Type.F, Type.C, Type.Sym](false)
        case Config.Lattice.BoundedInt =>
          val bounded = new BoundedInteger(1000)
          new MakeSchemeLattice[Type.S, Concrete.B, bounded.I, Type.F, Type.C, Type.Sym](false)
        case Config.Lattice.ConstantPropagation => new MakeSchemeLattice[ConstantPropagation.S, Concrete.B, ConstantPropagation.I, ConstantPropagation.F, ConstantPropagation.C, ConstantPropagation.Sym](false)
      }
      implicit val isSchemeLattice: IsSchemeLattice[lattice.L] = lattice.isSchemeLattice

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

      val sem = new SchemeSemantics[lattice.L, address.A, time.T](
        new SchemePrimitives[address.A, lattice.L])

      fileContent(s"$dir/${config.program}.scm") match {
        case Some(program) if program.size > 0 =>
          try {
            val output = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) { } }) {
              machine.eval(sem.parse(program), sem, false, Timeout.start(timeout))
            }
            MachineOutput(output.time, output.numberOfStates, output.timedOut)
          } catch {
            case e: Throwable => {
              println(s"Benchmark ${config.program} failed! (config: $config, error: $e)")
              MachineOutput(0, 0, false)
            }
          }
        case _ => MachineOutput(0, 0, false) // TODO: error output
      }
    }

    def receive = {
      case Computation(config) =>
        val answer = compute(config)
        sender ! Result(config, answer)
    }
  }

  val system = ActorSystem("scala-am-benchmarks")

  case class Results(
      results: Map[String, Map[String, MachineOutput]], /* maps program name to input classification to program name to output */
      seen: Set[String] /* classes seen */
  ) {
    def add(in: MachineConfig, out: MachineOutput): Results =
      this
        .copy(
          results = results + (in.program -> (results(in.program) + (classify(
              in) -> out))),
          seen = seen + classify(in))
    def print {
      import scala.math.Ordering.String._
      if (!seen.isEmpty) {
        val keys = seen.toList.sortBy(x => x)
        val tab = ("benchmarks" :: keys) :: results.toList
            .sortBy({ case (name, _) => name })
            .map({
            case (name, res) =>
              name :: keys.map(k =>
                res.get(k) match {
                  case Some(out) => out.toString
                  case None => "x"
              })
          })
        println(Tabulator.format(tab))
      }
    }
  }
  object Results {
    def apply(): Results =
      Results(Map[String, Map[String, MachineOutput]]()
                .withDefaultValue(Map[String, MachineOutput]()),
              Set())
  }

  /* Avoids deprecated warning when using Java 8, and call the shutdown method if an older version is used */
  def terminate(system: ActorSystem) = try {
    system.getClass.getMethod("terminate").invoke(system)
  } catch {
    case _: NoSuchMethodException =>
      system.getClass.getMethod("shutdown").invoke(system)
  }

  class Dispatcher(bound: Option[Int]) extends Actor {
    private case class State(computing: Int, work: List[MachineConfig], results: Results)

    private def sendWork(actor: ActorRef, state: State): Receive = state.work match {
      case item :: remainingWork =>
        actor ! Computation(item)
        val newState = state.copy(computing = state.computing + 1, work = state.work.tail)
        active(newState)
      case Nil =>
        if (state.computing == 0) {
          scala.Console.withOut(logging) {
            /* no more work to do, nothing is computing, stop */
            state.results.print
          }
          terminate(system)
        }
    }

    private def active(state: State): Receive = {
      case AddWork(items) =>
        context.become(active(state.copy(work = state.work ++ items)))
      case SendWork(actor) => context.become(sendWork(actor, state))
      case Result(in, out) =>
        scala.Console.withOut(logging) {
          println(
            s"$in: $out (${state.work.size + state.computing} remaining)")
        }
        val newState = state.copy(computing = state.computing - 1,
                                  results = state.results.add(in, out))
        scala.Console.withOut(logging) { newState.results.print }
        context.become(sendWork(sender, newState))
    }

    def receive = active(State(0, List[MachineConfig](), Results()))
  }

  def run(nworkers: Int, timeout: Option[Long]) {
    import sys.process._
    import scala.util.{Try, Success, Failure}
    import scala.language.postfixOps

    val work = inputs
    val commit = Try(("git log -1" !!).split('\n').head.split(' ')(1))
      .getOrElse("unknown")
    scala.Console.withOut(logging) {
      println(s"Running benchmarks for commit $commit with timeout $timeout")
    }
    println(s"Scheduling ${work.size} items of work")
    val workers = (1 to nworkers).map(i =>
      system.actorOf(Props(new Worker(timeout)), s"worker-$i"))
    val dispatcher = system.actorOf(Props(new Dispatcher(None)), "dispatcher")
    dispatcher ! AddWork(work.toSet)
    workers.foreach(dispatcher ! SendWork(_))
  }
  def main(args: Array[String]) {
    Config.parser.parse(args, Config.Config()) match {
      case Some(config) => run(config.workers, config.timeout.map(_.toNanos))
      case None => terminate(system)
    }
  }
}

object SimpleBenchmarks extends Benchmarks("test", {
  val programs = List("fact", "fib", "church", "church-2", "church-6", "count", "mceval")
  import Config._
  programs.flatMap(p => Set(MachineConfig(p, machine = Machine.AAM)))
  }, (config => config.machine match {
    case Config.Machine.AAM => "AAM"
  }))

object MonoBenchmarks extends Benchmarks("test", {
  val programs = List("ack", "blur", "church", "collatz", "count", "cpstak", "dderiv", "divrec", "eta", "fact", "fib", "gcipd", "grid", "inc", "kcfa2", "kcfa3", "loop2", "mceval", "mut-rec", "mj09", "nqueens", "primtest", "regex", "rotate", "rsa", "scm2java", "sq", "takl", "widen")
  import Config._
  programs.flatMap(p =>
    Set(MachineConfig(p, machine = Machine.AAM),
      MachineConfig(p, machine = Machine.AAMGlobalStore)))
}, (config => config.machine match {
  case Config.Machine.AAM => "AAM"
  case Config.Machine.AAMGlobalStore => "AAM+GS"
}))

object AAMAACP4FBenchmarks extends Benchmarks("test", {
  val programs = List("ack", "blur", "church", "collatz", "count", "cpstak", "dderiv", "divrec", "eta", "fact", "fib", "gcipd", "grid", "inc", "kcfa2", "kcfa3", "loop2", "mceval", "mut-rec", "mj09", "nqueens", "primtest", "regex", "rotate", "rsa", "scm2java", "sq", "takl", "widen")
  import Config._
  programs.flatMap(p =>
    Set(MachineConfig(p, machine = Machine.AAMGlobalStore),
      MachineConfig(p, machine = Machine.AAC),
      MachineConfig(p, machine = Machine.Free)))
}, (config => config.machine match {
  case Config.Machine.AAMGlobalStore => "AAM"
  case Config.Machine.AAC => "AAC"
  case Config.Machine.Free => "P4F"
}))
