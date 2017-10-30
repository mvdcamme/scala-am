import scalaz.Scalaz._
import scalaz._

trait ExplorationType
case object AllInterleavings extends ExplorationType
case object OneInterleaving extends ExplorationType
case object RandomInterleaving extends ExplorationType
case object DPOR extends ExplorationType
case object Joined extends ExplorationType
object ExplorationTypeParser extends scala.util.parsing.combinator.RegexParsers {
  val all = "AllInterleavings".r ^^ (_ => AllInterleavings)
  val one = "OneInterleaving".r ^^ (_ => OneInterleaving)
  val random = "RandomInterleaving".r ^^ (_ => RandomInterleaving)
  val dpor = "DPOR".r ^^ (_ => DPOR)
  def joined = "Joined".r ^^ (_ => Joined)
  def expl: Parser[ExplorationType] = all | one | random | dpor
  def parse(s: String): ExplorationType = parseAll(expl, s) match {
    case Success(res, _) => res
    case Failure(msg, _) => throw new Exception(s"cannot parse exploration type: $msg")
    case Error(msg, _) => throw new Exception(s"cannot parse exploration type: $msg")
  }
}

class ConcurrentAAM[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp, TID : ThreadIdentifier](exploration: ExplorationType)
    extends AbstractMachine[Exp, Abs, Addr, Time] {
  def abs = implicitly[JoinLattice[Abs]]
  def addr = implicitly[Address[Addr]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[Time]]
  def thread = implicitly[ThreadIdentifier[TID]]

  def name = "ConcurrentAAM"
  val aam = new AAM[Exp, Abs, Addr, Time]
  import aam._

  type KontAddr = aam.KontAddr

  private def effectsToXml(effects: Set[Effect[Addr]]): List[scala.xml.Node] = effects.toList.map(eff => eff.kind match {
    case ReadEffect => <font color="forestgreen">{eff.toString}</font>
    case WriteEffect => <font color="red2">{eff.toString}</font>
  })

  type Effects = Set[Effect[Addr]]
  val noEffect: Effects = Set[Effect[Addr]]()
  def effectsToStr(effs: Effects): String = effs.map(_.toString).mkString(", ")

  case class Context(control: Control, kstore: KontStore[KontAddr], a: KontAddr, t: Time) {
    def integrate1(tid: TID, a: KontAddr, action: Action[Exp, Abs, Addr])(threads: ThreadMap, oldstore: Store[Addr, Abs], results: ThreadResults):
        Set[(ThreadMap, ThreadResults, Store[Addr, Abs], Effects)] = action match {
      case ActionReachedValue(v, store, effs) => Set((threads.update(tid, Context(ControlKont(v), kstore, a, time.tick(t))), results, store, effs))
      case ActionPush(frame, e, ρ, store, effs) => {
        val next = NormalKontAddress(e, t)
        Set((threads.update(tid, Context(ControlEval(e, ρ), kstore.extend(next, Kont(frame, a)), next, time.tick(t))), results, store, effs))
      }
      case ActionEval(e, ρ, store, effs) => Set((threads.update(tid, Context(ControlEval(e, ρ), kstore, a, time.tick(t))), results, store, effs))
      case ActionStepIn(fexp, _, e, ρ, store, _, effs) => Set((threads.update(tid, Context(ControlEval(e, ρ), kstore, a, time.tick(t, fexp))), results, store, effs))
      case ActionError(err) => Set((threads.update(tid, Context(ControlError(err), kstore, a, time.tick(t))), results, oldstore, noEffect))
      case ActionSpawn(tid2: TID @unchecked, e, ρ, store, act, effs) => {
        assert(effs.isEmpty) /* TODO */
        integrate1(tid, a, act)(threads.add(tid2, Context(ControlEval(e, ρ), KontStore.empty[KontAddr], HaltKontAddress, time.initial(tid2.toString))), store, results)
      }
      case ActionJoin(tid2: TID @unchecked, store, effs) =>
        if (results.isDone(tid2)) {
          Set((threads.update(tid, Context(ControlKont(results.get(tid2)), kstore, a, time.tick(t))), results, store, effs))
        } else {
          Set[(ThreadMap, ThreadResults, Store[Addr, Abs], Effects)]()
        }
    }

    def integrate(tid: TID, a: KontAddr, actions: Set[Action[Exp, Abs, Addr]], threads: ThreadMap, store: Store[Addr, Abs], results: ThreadResults):
        (Set[(ThreadMap, ThreadResults, Store[Addr, Abs], Effects)]) =
      actions.flatMap(action => integrate1(tid, a, action)(threads, store, results))

    def step(sem: Semantics[Exp, Abs, Addr, Time], tid: TID, store: Store[Addr, Abs], threads: ThreadMap, results: ThreadResults):
        (Set[(ThreadMap, ThreadResults, Store[Addr, Abs], Effects)]) = control match {
      case ControlEval(e, ρ) => integrate(tid, a, sem.stepEval(e, ρ, store, t), threads, store, results)
      case ControlKont(v) if halted && tid != thread.initial =>
        /* TODO: we could avoid distinguishing the initial thread, and just get the
         * final results at its location in results */
        Set((threads.remove(tid), results.add(tid, v), store, noEffect))
      case ControlKont(v) => kstore.lookup(a).flatMap({
        case Kont(frame, next) => integrate(tid, next, sem.stepKont(v, frame, store, t), threads, store, results)
      })
      case ControlError(_) => Set()
    }

    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => a == HaltKontAddress
      case ControlError(_) => true
    }
  }

  case class ThreadMap(content: Map[TID, Set[Context]]) {
    def get(tid: TID): Set[Context] = content(tid)
    def tids: Set[TID] = content.keys.toSet
    def update(tid: TID, context: Context): ThreadMap = {
      val existing = get(tid)
      if (existing.size == 1) {
        ThreadMap(content + (tid -> Set(context))) /* strong update */
      } else {
        ThreadMap(content + (tid -> (existing + context))) /* weak update */
      }
    }
    def add(tid: TID, context: Context): ThreadMap =
      ThreadMap(content + (tid -> (get(tid) + context)))
    def remove(tid: TID): ThreadMap =
      ThreadMap(content - tid)
    def join(that: ThreadMap): ThreadMap = ThreadMap(this.content |+| that.content) /* TODO: does this correctly joins sets? */
    def forall(f: ((TID, Set[Context])) => Boolean): Boolean = content.forall(f)
  }
  object ThreadMap {
    def apply(ctx: Context): ThreadMap =
      ThreadMap(Map[TID, Set[Context]](thread.initial -> Set(ctx)).withDefaultValue(Set[Context]()))
  }

  case class ThreadResults(content: Map[TID, Abs]) {
    /* TODO: what if two threads share tid, one is done but not the other? -> use thread counting to know more*/
    def isDone(tid: TID): Boolean = content.contains(tid)
    def get(tid: TID): Abs = content.getOrElse(tid, abs.bottom)
    def add(tid: TID, v: Abs): ThreadResults = ThreadResults(content + (tid -> abs.join(get(tid), v)))
    def join(that: ThreadResults): ThreadResults = ThreadResults(this.content |+| that.content)
  }

  case class State(threads: ThreadMap, results: ThreadResults, store: Store[Addr, Abs]) {
    def join(that: State): State =
      State(threads.join(that.threads), results.join(that.results), store.join(that.store))
    def step(sem: Semantics[Exp, Abs, Addr, Time], tid: TID): Set[(Effects, State)] =
      threads.get(tid).flatMap(ctx => ctx.step(sem, tid, store, threads, results).map({
        case (threads, results, store, effects) => (effects, State(threads, results, store))
      }))
    def stepTids(sem: Semantics[Exp, Abs, Addr, Time], tids: Set[TID]): Set[(TID, Effects, State)] =
      tids.foldLeft(Set[(TID, Effects, State)]())((acc, tid) => step(sem, tid).foldLeft(acc)((acc, stepped) => acc + ((tid, stepped._1, stepped._2))))
    def stepTid(sem: Semantics[Exp, Abs, Addr, Time], tid: TID): Set[(Effects, State)] =
      stepTids(sem, Set(tid)).map(x => (x._2, x._3))
    def stepAll(sem: Semantics[Exp, Abs, Addr, Time]): Set[(TID, Effects, State)] =
      stepTids(sem, threads.tids)
    def stepAny(sem: Semantics[Exp, Abs, Addr, Time]): Option[(TID, Set[(Effects, State)])] = {
      val init: Option[(TID, Set[(Effects, State)])] = None
      threads.tids.foldLeft(init)((acc, tid) => acc match {
        case None =>
          val stepped = step(sem, tid)
          if (stepped.isEmpty) {
            None
          } else {
            Some((tid, stepped))
          }
        case Some(_) => acc
      })
    }
    def stepAnyRandom(sem: Semantics[Exp, Abs, Addr, Time]): Option[(TID, Set[(Effects, State)])] = {
      val init: Option[(TID, Set[(Effects, State)])] = None
      scala.util.Random.shuffle(threads.tids.toList).foldLeft(init)((acc, tid) => acc match {
        case None =>
          val stepped = step(sem, tid)
          if (stepped.isEmpty) {
            None
          } else {
            Some((tid, stepped))
          }
        case Some(_) => acc
      })
    }

    def halted: Boolean = threads.tids == Set(thread.initial) && threads.get(thread.initial).forall(_.halted)

    override def toString = threads.tids.map(tid =>
      s"$tid: " + threads.get(tid).map(ctx => ctx.control.toString().take(40)).mkString(", ")
    ).mkString("\n")
  }

  object State {
    def inject(exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]) = {
      State(ThreadMap(Context(ControlEval(exp, Environment.initial[Addr](env)), KontStore.empty[KontAddr], HaltKontAddress, time.initial(""))),
        ThreadResults(Map[TID, Abs]()), Store.initial[Addr, Abs](store))
    }
  }

  type Annot = (TID, Effects)
  type G = Graph[State, Annot, Unit]
  implicit val annot = new GraphAnnotation[Annot, Unit] {
    override def labelXml(annot: (TID, Effects)) = annot match { case (tid, eff) => (scala.xml.Text(tid.toString) :: effectsToXml(eff)) }
  }
  implicit val graphNode = new GraphNode[State, Unit] {
    override def label(n: State) = n.toString
    override def color(n: State) = if (n.halted) {
      Colors.Yellow
    } else if (n.threads.content.values.exists(xs => xs.exists(x => x.control.isInstanceOf[ControlError] ))) {
      Colors.Red
    } else {
      Colors.White
    }
  }

  case class ConcurrentAAMOutput(halted: Set[State], numberOfStates: Int, time: Double, graph: Option[G], timedOut: Boolean)
      extends Output {
    def finalValues = halted.flatMap(st => st.threads.get(thread.initial).flatMap(ctx => ctx.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    }))
    def toFile(path: String)(output: GraphOutput) = graph match {
      case Some(g) => output.toFile(g, ())(path)
      case None => println("Not generating graph because no graph was computed")
    }
  }

  type Exploration = (State, Set[State]) => Set[(TID, Effects, State)]

  @scala.annotation.tailrec
  private def loop(todo: Set[State], visited: Set[State],
    halted: Set[State], timeout: Timeout, graph: Option[G])
    (step: Exploration): ConcurrentAAMOutput =
    if (timeout.reached) {
      ConcurrentAAMOutput(halted, visited.size, timeout.time, graph, true)
    } else {
      todo.headOption match {
        case Some(s) =>
          if (visited.contains(s)) {
            loop(todo.tail, visited, halted, timeout, graph)(step)
          } else if (s.halted) {
            loop(todo.tail, visited + s, halted + s, timeout, graph)(step)
          } else {
            val succs = step(s, todo)
            val newGraph = graph.map(_.addEdges(succs.map({ case (tid, eff, s2) => (s, (tid, eff), s2) })))
            loop(todo.tail ++ succs.map(_._3), visited + s, halted, timeout, newGraph)(step)
          }
        case None => ConcurrentAAMOutput(halted, visited.size,
          timeout.time, graph, false)
      }
    }

  private def allInterleavings(sem: Semantics[Exp, Abs, Addr, Time]): Exploration = (s, _) => s.stepAll(sem)
  private def oneInterleaving(sem: Semantics[Exp, Abs, Addr, Time]): Exploration = (s, _) => s.stepAny(sem) match {
    case Some((tid, succs)) => succs.map(s2 => (tid, s2._1, s2._2))
    case None => Set()
  }
  private def randomInterleaving(sem: Semantics[Exp, Abs, Addr, Time]): Exploration = (s, _) => s.stepAnyRandom(sem) match {
    case Some((tid, succs)) => succs.map(s2 => (tid, s2._1, s2._2))
    case None => Set()
  }

  @scala.annotation.tailrec
  private def loopJoined(state: State, sem: Semantics[Exp, Abs, Addr, Time], timeout: Timeout): ConcurrentAAMOutput =
    if (timeout.reached) {
      ConcurrentAAMOutput(Set.empty, 0, timeout.time, None, true)
    } else {
      println(state)
      val state2 = state.stepAll(sem).foldLeft(state)((acc, succ) => acc.join(succ._3))
      if (state2 == state) {
        ConcurrentAAMOutput(Set.empty, 0, timeout.time, None, false)
      } else {
        loopJoined(state2, sem, timeout)
      }
    }

  private def effectsOf(transitions: Set[(Effects, State)]): Effects =
    transitions.flatMap(_._1)
  private def dependent(eff1: Effect[Addr], eff2: Effect[Addr]): Boolean =
      (eff1.target == eff2.target && (eff1.kind |+| eff2.kind) == WriteEffect)
  private def dependent(effs1: Effects, effs2: Effects): Boolean =
    (effs1.foldLeft(false)((acc, eff1) => effs2.foldLeft(acc)((acc, eff2) => acc || dependent(eff1, eff2))))

  case object CannotHandle extends Exception
  private def dporExplore(s0: State, start: Long, timeout: Timeout, graph: Option[G], sem: Semantics[Exp, Abs, Addr, Time]) = {
    import scala.util.control.Breaks._
    type Transition = (State, TID, Effects)
    type Stack = (scala.collection.immutable.Vector[Transition], State)
    def last(stack: Stack): State = stack._2
    def dom(stack: Stack): List[Int] = (0 to (stack._1.size-1)).toList
    def proc(stack: Stack, i: Int): TID = stack._1(i)._2
    def pre(stack: Stack, i: Int): State = stack._1(i)._1
    def size(stack: Stack): Int = stack._1.size - 1
    def enabled(state: State): Set[TID] = state.stepAll(sem).map(_._1)
    def append(stack: Stack, s: State, p: TID, effs: Effects, next: State): Stack = (stack._1 :+ (s, p, effs), next)
    val backtrack = scala.collection.mutable.Map[State, Set[TID]]().withDefaultValue(Set[TID]())
    def isDependent(tr: (Effects, State), stack: Stack, i: Int): Boolean = dependent(tr._1, stack._1(i)._3)
    def isDependentAndCoEnabled(tr: (Effects, State), stack: Stack, i: Int) = {
      isDependent(tr, stack, i) && true /* TODO: co-enabledness can be improved */
    }

    type ClockVector = Map[TID, Int]
    def emptyClockVector: ClockVector = Map[TID, Int]().withDefaultValue(0)
    type ClockVectors = Map[Either[TID, Int], ClockVector]
    def emptyClockVectors: ClockVectors = Map[Either[TID, Int], ClockVector]().withDefaultValue(emptyClockVector)
    def maxClockVector(c1: ClockVector, c2: ClockVector): ClockVector = c1.keySet.union(c2.keySet).map(k =>
      (k, scala.math.max(c1(k), c2(k)))).toMap.withDefaultValue(0)
    def happensBefore(clocks: ClockVectors, stack: Stack, i: Int, p: TID): Boolean = i <= clocks(Left(p))(proc(stack, i))
    var halted: Set[State] = Set[State]()
    var timedOut: Boolean = false
    var g: Option[G] = graph
    var visited: Set[State] = Set[State]()
    /* DPOR: Explore(S, C) { */
    def explore(stack: Stack, clocks: ClockVectors): Unit = {
      if (timeout.reached) {
        timedOut = true
      } else {
        /* DPOR: let s = last(S) */
        val s: State = last(stack)
        /* DPOR: for all precesses p { */
        for (p <- s.threads.tids) {
          /* dpor makes use of next(s, p), it might be empty so we compute next(s, p) first, giving 'succs' */
          val succs = s.stepTid(sem, p)
          succs.size match {
            case 0 => () /* do nothing (no existing i) */
            case 1 => { /* at least one successor */
              val next: (Effects, State) = succs.head /* we only handle one successor, so take it */
              /* DPOR: \exists i = max({i \in dom(S) | S_i is dependent and may be co-enabled with next(s, p) and i \not\happensBefore p}) */
              dom(stack).sortWith(_ > _).find(i => isDependentAndCoEnabled(next, stack, i) && !happensBefore(clocks, stack, i, p)) match {
                case Some(i: Int) => /* found an exesting i */
                  val preSi: State = pre(stack, i) /* computes pre(S,i) in 'preSi' */
                  val enabledTr: Set[TID] = enabled(preSi) /* computes enabled(pre(S,i)) in 'enabledTr' */
                  /* DPOR: if (p \in enabled(pre(S,i))) { */
                  if (enabledTr.contains(p)) {
                    /* DPOR: then add p to backtrack(pre(S,i)); */
                    backtrack += preSi -> (backtrack(preSi) + p)
                  } else {
                    /* DPOR: else add enabled(pre(S,i)) to backtrack(pre(S,i)) */
                    backtrack += preSi -> (backtrack(preSi) ++ enabledTr)
                  }
                case None => () /* do nothing (no existing i) */
              }
            }
            case _ => throw CannotHandle /* more than one successor */
          }
        }
        /* DPOR: if (\exists p \in enabled(s)) */
        /* a transition is enabled if it can perform a step */
        s.stepAny(sem) match {
          case None => halted += s /* no enabled transition, we reached a halted state */
          case Some((p: TID @unchecked, results)) => /* at least one transition enabled */
            if (results.size > 1) {
              throw CannotHandle /* more than one successor */
          } else {
              results.headOption match {
                case Some(_) => {
                  /* DPOR: backtrack(s) := {p}; */
                  backtrack += s -> Set(p)
                  /* DPOR: let done = \emptyset; */
                  var done: Set[TID] = Set[TID]()
                  /* DPOR: while (\exists p \in (backtrack(s) \ done)) { */
                  breakable { while(true) {
                    (backtrack(s) -- done).headOption match {
                      case Some(p: TID @unchecked) =>
                        /* DPOR: add p to done */
                        done = done + p
                        /* DPOR: let t = next(s,p); */
                        s.stepTid(sem, p).headOption match {
                          case Some(t @ (effs: Effects, next: State)) =>
                            visited += next
                            g = g.map(_.addEdge(s, (p, effs), next))
                            /* DPOR: let S' = S.t */
                            /* in our case, a transition is described by the predecessor state (s), the tid
                             * (p), and the effects (effs). 'next' is needed to
                             * compute last(S) */
                            val stack2: Stack = append(stack, s, p, effs, next)
                            /* DPOR: let cv = max{C(i) | i \in 1..|S| and S_i dependent with t}; */
                            val cv: ClockVector = (0 to size(stack)).filter(i => isDependent(t, stack, i)).map(i => clocks(Right(i))).foldLeft(emptyClockVector)((l, r) => maxClockVector(l, r))
                            /* DPOR: let cv2 = cv[p := |S'|]; */
                            val cv2: ClockVector = cv + (p -> size(stack2))
                            /* DPOR: let C' = C[p := cv2, |S'| := cv2]; */
                            val clocks2: ClockVectors = clocks + (Left(p) -> cv2) + (Right(size(stack2)) -> cv2)
                            /* DPOR: Explore(S', C') */
                            explore(stack2, clocks2)
                          case None => halted += s /* no successor */
                        }
                      case None => break /* stops while loop */
                    }
                  }}
                }
                case None => halted += s /* no successor (should not happen) */
              }
            }
        }
      }
    }
    try {
      visited += s0
      /* DPOR: Initially: Explore(\emptyset, \lambda x. \bottom) */
      explore((scala.collection.immutable.Vector[Transition](), s0), emptyClockVectors)
      ConcurrentAAMOutput(halted, visited.size, (System.nanoTime - start) / Math.pow(10, 9), g, timedOut)
    } catch {
      case CannotHandle => ConcurrentAAMOutput(Set(), 0, 0, None, true)
    }
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output = {
    val state = State.inject(exp, sem.initialEnv, sem.initialStore)
    val g = if (graph) { Some (Graph.empty) } else { None }
    exploration match {
      case AllInterleavings => loop(Set(state), Set(), Set(), timeout, g)(allInterleavings(sem))
      case OneInterleaving => loop(Set(state), Set(), Set(), timeout, g)(oneInterleaving(sem))
      case RandomInterleaving => loop(Set(state), Set(), Set(), timeout, g)(randomInterleaving(sem))
      case DPOR => dporExplore(state, System.nanoTime, timeout, g, sem)
      case Joined => loopJoined(state, sem, timeout)
    }
  }
}
