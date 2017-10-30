import scalaz.Scalaz._
import scalaz._

class ActorsAAM[Exp : Expression, Abs : IsASchemeLattice, Addr : Address, Time : ActorTimestamp, PID : ThreadIdentifier](val M: MboxImpl[PID, Abs])
    extends AbstractMachine[Exp, Abs, Addr, Time] {
  def name = "ActorsAAM"

  type Annot = (PID, Option[ActorEffect])
  type G = Graph[State, Annot, Unit]
  implicit val annot = new GraphAnnotation[Annot, Unit] {
    override def labelXml(annot: Annot) = annot match {
      case (p, None) => List(scala.xml.Text(p.toString))
      case (p, Some(eff)) => List(scala.xml.Text(p.toString), <font color="red">{eff.toString}</font>)
    }
  }
  implicit val graphNode = new GraphNode[State, Unit] {
    override def labelXml(n: State) = n.toXml
    override def color(n: State) = if (n.halted) { Colors.Yellow } else if (n.hasError) { Colors.Red } else { Colors.White }
  }

  object G {
    def apply(): G = Graph.empty[State, Annot, Unit]
    def apply(s: State): G = Graph.node[State, Annot, Unit](s)
  }

  trait KontAddr
  case class NormalKontAddress(pid: PID, exp: Exp, time: Time) extends KontAddr
  case object HaltKontAddress extends KontAddr
  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }

  object ActionHelpers extends ActorActionHelpers[Exp, Abs, Addr, Time, PID]
  import ActionHelpers._

  trait ActorInstance
  case class ActorInstanceActor(actd: Exp, env: Environment[Addr]) extends ActorInstance
  case object ActorInstanceMain extends ActorInstance

  trait Control
  case class ControlEval(e: Exp, env: Environment[Addr]) extends Control {
    override def toString = s"ev($e)"
  }
  case class ControlKont(v: Abs) extends Control {
    override def toString = s"ko($v)"
  }
  case class ControlError(err: SemanticError) extends Control {
    override def toString = s"err($err)"
  }
  case object ControlWait extends Control {
    override def toString = "wait"
  }

  case class Context(control: Control, kont: KontAddr, inst: ActorInstance, mbox: M.T, t: Time) {
    def toXml: List[scala.xml.Node] = (control match {
      case ControlEval(e, _) => List(<font color="forestgreen">{e.toString.take(40)}</font>)
      case ControlKont(v) => List(<font color="rosybrown1">{v.toString.take(40)}</font>)
      case ControlError(err) => List(<font color="black">{err.toString}</font>)
      case ControlWait => List(<font color="skyblue">wait</font>)
    }) ++ List(scala.xml.Text(mbox.toString.take(40)))
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => inst == ActorInstanceMain && kont == HaltKontAddress
      case ControlError(_) => true
      case ControlWait => mbox.isEmpty
    }
    def hasError: Boolean = control match {
      case ControlError(_) => true
      case _ => false
    }
  }
  object Context {
    def create(p: PID, actd: Exp, env: Environment[Addr]): Context =
      Context(ControlWait, HaltKontAddress, ActorInstanceActor(actd, env), M.empty, Timestamp[Time].initial(p.toString))
    def createMain(e: Exp, env: Environment[Addr]): Context =
      Context(ControlEval(e, env), HaltKontAddress, ActorInstanceMain, M.empty, Timestamp[Time].initial("main"))
  }

  case class Procs(content: CountingMap[PID, Context]) {
    def toXml: List[scala.xml.Node] = content.keys.toList.map(p => {
      val pid: scala.xml.Node = scala.xml.Text(s"$p: ")
      val entries: List[List[scala.xml.Node]] = content.lookup(p).toList.map(_.toXml)
      pid :: entries.reduceLeft({ (acc, l) => acc ++ (scala.xml.Text(", ") :: l) })
    }).reduceLeft({ (acc, l) => acc ++ (<br/> :: l) })
    def get(p: PID): Set[Context] = content.lookup(p)
    def update(v: (PID, Context)): Procs =
      Procs(content = content.update(v._1, v._2))
    def extend(v: (PID, Context)): Procs =
      Procs(content = content.extend(v._1, v._2))
    def terminate(p: PID): Procs =
      ???
      // Procs(content = content.remove(p))
    def pids: Set[PID] = content.keys
    def exists(p: (PID, Context) => Boolean): Boolean = content.exists(p)
    def forall(p: (PID, Context) => Boolean): Boolean = content.forall(p)
  }
  object Procs {
    def empty: Procs = Procs(CountingMap.empty[PID, Context])
  }

  trait ActorEffect {
    def macrostepStopper: Boolean
  }
  case class ActorEffectSend(target: PID, name: String) extends ActorEffect {
    def macrostepStopper = true
    override def toString = s"$target ! $name"
  }
  case class ActorEffectSendSelf(target: PID, name: String) extends ActorEffect {
    def macrostepStopper = true
    override def toString = s"self ! $name"
  }
  case class ActorEffectTerminate(p: PID) extends ActorEffect {
    def macrostepStopper = false
    override def toString = "x"
  }
  case class ActorEffectCreate(p: PID, name: String) extends ActorEffect {
    def macrostepStopper = false
    override def toString = s"create $name"
  }
  case class ActorEffectBecome(p: PID, name: String) extends ActorEffect {
    def macrostepStopper = false
    override def toString = s"become $name"
  }

  case class State(procs: Procs, store: Store[Addr, Abs], kstore: KontStore[KontAddr]) {
    def toXml = procs.toXml
    def halted: Boolean = procs.forall((p, ctx) => ctx.halted)
    def hasError: Boolean = procs.exists((pid, ctx) => ctx.hasError)
    def stepAll(sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, PID, Option[ActorEffect])] = stepPids(procs.pids, sem)
    def stepPids(pids: Set[PID], sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, PID, Option[ActorEffect])] = pids.flatMap(p => stepPid(p, sem))
    def stepAllExceptPid(p: PID, sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, PID, Option[ActorEffect])] = stepPids(procs.pids - p, sem)
    def stepAny(sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, PID, Option[ActorEffect])] = {
      val init: Option[Set[(State, PID, Option[ActorEffect])]] = Option.empty
      procs.pids.foldLeft(init)((acc, p) => acc match {
        case None =>
          val stepped = stepPid(p, sem)
          if (stepped.isEmpty) { Option.empty } else { Option(stepped) }
        case Some(_) => acc
      }).getOrElse(Set.empty)
    }

    def integrate(p: PID, ctx: Context, act: Act): Set[(State, PID, Option[ActorEffect])] = act match {
      case ActionReachedValue(v, store2, effs) =>
        Set((this.copy(procs = procs.update(p, ctx.copy(control = ControlKont(v), t = Timestamp[Time].tick(ctx.t))),
          store = store2), p, None))
      case ActionPush(frame, e, env, store2, effs) =>
        val next = NormalKontAddress(p, e, ctx.t)
        Set((this.copy(procs = procs.update(p, ctx.copy(control = ControlEval(e, env), kont = next, t = Timestamp[Time].tick(ctx.t))),
          kstore = kstore.extend(next, Kont(frame, ctx.kont)), store = store2), p, None))
      case ActionEval(e, env, store2, effs) =>
        Set((this.copy(procs = procs.update(p, ctx.copy(control = ControlEval(e, env), t = Timestamp[Time].tick(ctx.t))),
          store = store2), p, None))
      case ActionStepIn(fexp, _, e, env, store2, _, effs) =>
        Set((this.copy(procs = procs.update(p, ctx.copy(control = ControlEval(e, env), t = Timestamp[Time].tick(ctx.t, fexp))),
          store = store2), p, None))
      case ActionError(err) =>
        Set((this.copy(procs = procs.update(p, ctx.copy(control = ControlError(err)))), p, None))
      case ActorActionSend(ptarget : PID @unchecked, name, msg, vres, effs) if ptarget != p =>
        procs.get(ptarget).map(ctxtarget =>
          (this.copy(procs = procs
            .update(p -> ctx.copy(control = ControlKont(vres), t = Timestamp[Time].tick(ctx.t)))
            .update(ptarget -> ctxtarget.copy(mbox = ctxtarget.mbox.push((p, name, msg))))),
            p, Some(ActorEffectSend(ptarget, name))))
      case ActorActionSend(ptarget, name, msg, vres, effs) if ptarget == p =>
        /* TODO: special care need to be taken if p maps to more than a single actor */
        Set((this.copy(procs = procs
          .update(p -> ctx.copy(control = ControlKont(vres), mbox = ctx.mbox.push((p, name, msg)), t = Timestamp[Time].tick(ctx.t)))),
          p, Some(ActorEffectSendSelf(p, name))))
      case ActorActionCreate(name, actd, exp, env2, store2, fres : (PID => Abs), effs) =>
        val p2 = ThreadIdentifier[PID].thread(exp, ctx.t)
        Set((this.copy(procs = procs
          .update(p -> ctx.copy(control = ControlKont(fres(p2)), t = Timestamp[Time].tick(ctx.t)))
          .extend(p2 -> Context.create(p2, actd, env2)),
          store = store2), p, Some(ActorEffectCreate(p, name))))
      case ActorActionBecome(name, actd, env2, store2, vres, effs) =>
        Set((this.copy(procs = procs
          .update(p -> Context.create(p, actd, env2).copy(t = Timestamp[Time].tick(ctx.t), mbox = ctx.mbox)),
          store = store2), p, Some(ActorEffectBecome(p, name))))
      case ActorActionTerminate(_) =>
        Set((this.copy(procs = procs.terminate(p)), p, Some(ActorEffectTerminate(p))))
    }

    def stepPid(p: PID, sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, PID, Option[ActorEffect])] = procs.get(p).flatMap(ctx => ctx.control match {
      case ControlEval(e, env) => /* call semantics */
        sem.stepEval(e, env, store, ctx.t).flatMap(action => integrate(p, ctx, action))
      case ControlKont(v) if ctx.kont != HaltKontAddress => /* apply continuation */
        kstore.lookup(ctx.kont).flatMap({
          case Kont(frame, next) => sem.stepKont(v, frame, store, ctx.t).flatMap(action => integrate(p, ctx.copy(kont = next), action))
        })
      case ControlKont(v) if ctx.kont == HaltKontAddress && ctx.inst != ActorInstanceMain => /* go to wait */
        Set[(State, PID, Option[ActorEffect])]((this.copy(procs = procs.update(p -> ctx.copy(control = ControlWait, t = Timestamp[Time].tick(ctx.t)))), p, None))
      case ControlKont(v) if ctx.kont == HaltKontAddress && ctx.inst == ActorInstanceMain =>
        Set[(State, PID, Option[ActorEffect])]() /* main is stuck at this point */
      case ControlError(_) => Set[(State, PID, Option[ActorEffect])]() /* no successor */
      case ControlWait => /* receive a message */
        ctx.inst match {
          case ActorInstanceActor(actd, env) =>
            ctx.mbox.pop.flatMap({ case ((sender, name, values), mbox2) =>
              sem.stepReceive(p, name, values, actd, env, store, ctx.t).flatMap(action => integrate(p, ctx.copy(mbox = mbox2), action))
            })
          case ActorInstanceMain => Set[(State, PID, Option[ActorEffect])]() /* main cannot receive messages */
        }
    })
    /**
     * Performs a macrostep for a given PID, restricted only to macrosteps that
     * produce linear traces. If the state is stuck, returns None. Otherwise,
     * returns the final state of the macrostep, as well as the trace explored to
     * reach that state (not including the final state).
     */
    def macrostepPidTrace(p: PID, sem: Semantics[Exp, Abs, Addr, Time]): Option[(State, List[State])] = {
      val succs = stepPid(p, sem)
      succs.length match {
        case 0 => /* No successor, stuck state */ None
        case 1 => /* Single successor, what we want */
          val (succ, _, eff) = succs.head
          if (eff.map(_.macrostepStopper).getOrElse(false)) {
            /* there was an effect that stops the macrostep */
            Some((succ, List()))
          } else {
            /* otherwise, continue */
            succ.macrostepPidTrace(p, sem) match {
              case None => Some((succ, List())) /* next state is stuck, macrostep ends here */
              case Some((last, trace)) => Some((last, succ :: trace))
            }
          }
        case n => /* More than one successor, can't handle that here */
          throw new Exception(s"more than one successor when macrostepping thread $p (got $n successors)")
      }
    }
    def macrostepTraceAny(sem: Semantics[Exp, Abs, Addr, Time]): Option[(State, PID, List[State])] = {
      val init: Option[(State, PID, List[State])] = Option.empty
      procs.pids.foldLeft(init)((acc, p) => acc match {
        case None => macrostepPidTrace(p, sem).map({ case (s, trace) => (s, p, trace) })
        case Some(_) => acc
      })
    }
    def macrostepTraceAll(sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, PID, List[State])] =
      procs.pids.flatMap(p => macrostepPidTrace(p, sem).map({ case (s, trace) => (s, p, trace) }))
    /**
     * Performs a macrostep for a given PID. If the state is stuck, returns
     * None. Otherwise, returns the graph explored for this macrostep, as well
     * as every final state and the effect that stopped the macrostep for that
     * state. The final states *are* in the graph (unlike macrostepTrace),
     * because we need to keep track of the edge. */
    /* TODO: computing the graph can be disabled when it is not required by the main loop. */
    def macrostepPid(p: PID, sem: Semantics[Exp, Abs, Addr, Time]): Option[(G, Set[(State, Option[ActorEffect])])] = {
      println(s"Macrostepping thread $p")
      def loop(todo: Set[State], visited: Set[State], finals: Set[(State, Option[ActorEffect])], graph: G): (G, Set[(State, Option[ActorEffect])]) = {
        todo.headOption match {
          case Some(s) =>
            if (visited.contains(s)) {
              //println("visited")
              /* Already explored this state, skip it */
              loop(todo.tail, visited, finals, graph)
            } else if (s.halted) {
              //println("halted")
              /* The state is halted. It's therefore part of the final states (although it
               * doesn't produce any effect) */
              loop(todo.tail, visited, finals + ((s, None)), graph)
            } else {
              /* Otherwise, step this state */
              //println("visiting")
              val succs = s.stepPid(p, sem)
              //println(succs.size)
              if (succs.isEmpty) {
                /* No state produced, this state is stuck for this actor, treat it as a final state */
                loop(todo.tail, visited + s, finals + ((s, None)), graph)
              } else {
                /* add the successors to the graph */
                val newGraph = graph.addEdges(succs.map({ case (s2, _, eff) => (s, (p, eff), s2) }))
                /* schedule the successors that did not produce effects for exploration, and add
                 * the ones that did produce effects to the finals set */
                val (succsEff, succsNoEff) = succs.partition({ case (_, _, eff) => eff.map(_.macrostepStopper).getOrElse(false) })
                loop(todo.tail ++ succsNoEff.map(_._1), visited + s, finals ++ succsEff.map({ case (s2, _, eff) => (s2, eff) }), newGraph)
              }
            }
          case None =>
            /* Nothing more to explore */
            (graph, finals)
        }
      }
      val succs = stepPid(p, sem)
      println(s"Successors ${succs.size}")
      if (succs.isEmpty) {
        None
      } else {
        val graph = G(this)
        /* TODO: this is copy paste, solve that. */
        val newGraph = graph.addEdges(succs.map({ case (s2, _, eff) => (this, (p, eff), s2) }))
        val (succsEff, succsNoEff) = succs.partition({ case (_, _, eff) => eff.map(_.macrostepStopper).getOrElse(false) })
        Some(loop(succsNoEff.map(_._1), Set(this), succsEff.map({ case (s2, _, eff) => (s2, eff) }), newGraph))
      }
    }
    def macrostepAll(sem: Semantics[Exp, Abs, Addr, Time]): Set[(Set[(State, Option[ActorEffect])], PID, G)] =
      procs.pids.flatMap(p => macrostepPid(p, sem).map({ case (graph, states) => (states, p, graph) }))
  }

  object State {
    def inject(exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]) =
      State(
        Procs.empty.extend(ThreadIdentifier[PID].initial -> Context.createMain(exp, Environment.initial[Addr](env))),
        Store.initial[Addr, Abs](store),
        KontStore.empty[KontAddr])
  }

  case class ActorsAAMOutput(halted: Set[State], numberOfStates: Int, time: Double, graph: Option[G], timedOut: Boolean)
      extends Output {
    def finalValues: Set[Abs] = halted.flatMap(st => st.procs.get(ThreadIdentifier[PID].initial).flatMap(ctx => ctx.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    }))
    def toFile(path: String)(output: GraphOutput) = graph match {
      case Some(g) => output.toFile(g, ())(path)
      case None => println("Not generating graph because no graph was computed")
    }
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output = {
    val startingTime = System.nanoTime
    @scala.annotation.tailrec
    def loopAllInterleavings(todo: Set[State], visited: Set[State], halted: Set[State], graph: Option[G]): ActorsAAMOutput = {
      if (timeout.reached) {
        ActorsAAMOutput(halted, visited.size, timeout.time, graph, true)
      } else {
        todo.headOption match {
          case Some(s) =>
            // graph.foreach(g => println(s"State ${g.nodeId(s)}"))
            if (visited.contains(s)) {
              loopAllInterleavings(todo.tail, visited, halted, graph)
            } else if (s.halted) {
              loopAllInterleavings(todo.tail, visited + s, halted + s, graph)
            } else {
              val succs: Set[(State, PID, Option[ActorEffect])] = s.stepAll(sem)
              val newGraph = graph.map(_.addEdges(succs.map({ case (s2, pid, eff) => (s, (pid, eff), s2) })))
              loopAllInterleavings(todo.tail ++ succs.map(_._1), visited + s, halted, newGraph)
            }
          case None =>
            ActorsAAMOutput(halted, visited.size, timeout.time, graph, false)
        }
      }
    }
    @scala.annotation.tailrec
    def loopSingleInterleaving(todo: Set[State], visited: Set[State], halted: Set[State], graph: Option[G]): ActorsAAMOutput = {
      if (timeout.reached) {
        ActorsAAMOutput(halted, visited.size, timeout.time, graph, true)
      } else {
        todo.headOption match {
          case Some(s) =>
            if (visited.contains(s)) {
              loopSingleInterleaving(todo.tail, visited, halted, graph)
            } else if (s.halted) {
              loopSingleInterleaving(todo.tail, visited + s, halted + s, graph)
            } else {
              val succs = s.stepAny(sem)
              val newGraph = graph.map(_.addEdges(succs.map({ case (s2, pid, eff) => (s, (pid, eff), s2) })))
              loopSingleInterleaving(todo.tail ++ succs.map(_._1), visited + s, halted, newGraph)
            }
          case None =>
            ActorsAAMOutput(halted, visited.size, timeout.time, graph, false)
        }
      }
    }
    val initialState = State.inject(exp, sem.initialEnv, sem.initialStore)
    val g = if (graph) { Some(G()) } else { None }
    // loopAllInterleavings(Set(initialState), Set(), Set(), g)
     loopSingleInterleaving(Set(initialState), Set(), Set(), g)
  }
}
