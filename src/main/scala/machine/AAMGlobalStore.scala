import scalaz._
import scalaz.Scalaz._

/**
  * AAM with a monotonically growing global store.
  */
class AAMGlobalStore[
    Exp: Expression, Abs: JoinLattice, Addr: Address, Time: Timestamp]
    extends EvalKontMachine[Exp, Abs, Addr, Time] {
  def name = "AAMMonoGlobalStore"

  trait KontAddr
  case class NormalKontAddress(exp: Exp, t: Time) extends KontAddr {
    override def toString = s"NormalKontAddress($exp)"
  }
  case object HaltKontAddress extends KontAddr {
    override def toString = "HaltKontAddress"
  }

  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }

  case class GlobalStore(val store: DeltaStore[Addr, Abs],
                         delta: Map[Addr, Abs]) {
    def includeDelta(d: Option[Map[Addr, Abs]]): GlobalStore = d match {
      case Some(d) => this.copy(delta = delta |+| d)
      case None =>
        throw new Exception(
          "AAMGlobalStore should be used with a store that supports delta!")
    }
    def isUnchanged = delta.isEmpty
    def commit = if (isUnchanged) { this } else {
      this.copy(store = store.addDelta(delta), delta = Map())
    }
  }

  case class State(control: Control, a: KontAddr, t: Time) {
    override def toString = control.toString
    def subsumes(that: State): Boolean =
      control.subsumes(that.control) && a == that.a && t == that.t

    private def integrate(a: KontAddr,
                          edges: Set[Action[Exp, Abs, Addr]],
                          store: GlobalStore,
                          kstore: KontStore[KontAddr])
      : (Set[State], GlobalStore, KontStore[KontAddr]) =
      edges.foldLeft((Set[State](), store, kstore))((acc, edge) =>
        edge match {
          case ActionReachedValue(v, store2, _) =>
            (acc._1 + State(ControlKont(v), a, time.tick(t)),
             acc._2.includeDelta(store2.delta),
             acc._3)
          case ActionPush(frame, e, env, store2, _) =>
            val next = NormalKontAddress(e, t)
            (acc._1 + State(ControlEval(e, env), next, time.tick(t)),
             acc._2.includeDelta(store2.delta),
             acc._3.extend(next, Kont(frame, a)))
          case ActionEval(e, env, store2, _) =>
            (acc._1 + State(ControlEval(e, env), a, time.tick(t)),
             acc._2.includeDelta(store2.delta),
             acc._3)
          case ActionStepIn(fexp, _, e, env, store2, _, _) =>
            (acc._1 + State(ControlEval(e, env), a, time.tick(t)),
             acc._2.includeDelta(store2.delta),
             acc._3)
          case ActionError(err) =>
            (acc._1 + State(ControlError(err), a, time.tick(t)),
             acc._2,
             acc._3)
      })

    /**
      * Computes the set of states that follow the current state, and return the new store as well.
      */
    def step(sem: Semantics[Exp, Abs, Addr, Time],
             store: GlobalStore,
             kstore: KontStore[KontAddr])
      : (Set[State], GlobalStore, KontStore[KontAddr]) = control match {
      /* In a eval state, call the semantic's evaluation method */
      case ControlEval(e, env) =>
        integrate(a, sem.stepEval(e, env, store.store, t), store, kstore)
      /* In a continuation state, call the semantic's continuation method */
      case ControlKont(v) =>
        kstore
          .lookup(a)
          .foldLeft((Set[State](), store, kstore))((acc, kont) =>
            kont match {
              case Kont(frame, next) =>
                integrate(next,
                          sem.stepKont(v, frame, store.store, t),
                          acc._2,
                          acc._3) match {
                  case (states, store2, kstore2) =>
                    (acc._1 ++ states, store2, kstore2)
                }
          })
      /* In an error state, the state is not able to make a step */
      case ControlError(_) => (Set(), store, kstore)
    }

    /**
      * Checks if the current state is a final state. It is the case if it
      * reached the end of the computation, or an error
      */
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => a == HaltKontAddress
      case ControlError(_) => true
    }
  }
  object State {
    def inject(exp: Exp,
               env: Iterable[(String, Addr)],
               store: Iterable[(Addr, Abs)])
      : (State, GlobalStore, KontStore[KontAddr]) =
      (State(ControlEval(exp, Environment.initial[Addr](env)),
             HaltKontAddress,
             time.initial("")),
       GlobalStore(DeltaStore[Addr, Abs](store.toMap, Map()), Map()),
       TimestampedKontStore[KontAddr](Map(), 0))
  }

  case class AAMOutput(halted: Set[State],
                       numberOfStates: Int,
                       time: Double,
                       graph: Option[Graph[State, Unit]],
                       timedOut: Boolean)
      extends Output[Abs] {
    def finalValues =
      halted.flatMap(st =>
        st.control match {
          case ControlKont(v) => Set[Abs](v)
          case _ => Set[Abs]()
      })
    def containsFinalValue(v: Abs) =
      finalValues.exists(v2 => abs.subsumes(v2, v))
    def toDotFile(path: String) = graph match {
      case Some(g) =>
        g.toDotFile(path,
                    node => List(scala.xml.Text(node.toString.take(40))),
                    (s) =>
                      if (halted.contains(s)) { Colors.Yellow } else {
                        s.control match {
                          case ControlEval(_, _) => Colors.Green
                          case ControlKont(_) => Colors.Pink
                          case ControlError(_) => Colors.Red
                        }
                    },
                    _ => List(),
                    None)
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  /* Explore state graph when a monotonically growing store is present */
  @scala.annotation.tailrec
  private def loop(todo: Set[State],
                   visited: Set[State],
                   store: GlobalStore,
                   kstore: KontStore[KontAddr],
                   halted: Set[State],
                   startingTime: Long,
                   timeout: Option[Long],
                   graph: Option[Graph[State, Unit]],
                   sem: Semantics[Exp, Abs, Addr, Time]): AAMOutput =
    if (todo.isEmpty || timeout
          .map(System.nanoTime - startingTime > _)
          .getOrElse(false)) {
      AAMOutput(
        halted,
        graph.map(g => g.nodes.size).getOrElse(0),
        (System.nanoTime - startingTime) / Math.pow(10, 9),
        graph,
        timeout.map(System.nanoTime - startingTime > _).getOrElse(false))
    } else {
      val (edges, store2, kstore2) =
        todo.foldLeft(Set[(State, State)](), store, kstore)((acc, state) =>
          state.step(sem, acc._2, acc._3) match {
            case (next, store2, kstore2) =>
              (acc._1 ++ next.map(state2 => (state, state2)), store2, kstore2)
        })
      if (store2.isUnchanged && kstore.fastEq(kstore2)) {
        //assert(store2.commit.store == store2.store)
        loop(
          edges.map({ case (s1, s2) => s2 }).diff(visited),
          visited ++ todo,
          store2,
          kstore2,
          halted ++ todo.filter(_.halted),
          startingTime,
          timeout,
          graph.map(_.addEdges(edges.map({ case (s1, s2) => (s1, (), s2) }))),
          sem)
      } else {
        //assert(!(!store2.isUnchanged && store2.commit.store == store2.store))
        loop(
          edges.map({ case (s1, s2) => s2 }),
          Set(),
          store2.commit,
          kstore2,
          halted ++ todo.filter(_.halted),
          startingTime,
          timeout,
          graph.map(_.addEdges(edges.map({ case (s1, s2) => (s1, (), s2) }))),
          sem)
      }
    }

  def eval(exp: Exp,
           sem: Semantics[Exp, Abs, Addr, Time],
           graph: Boolean,
           timeout: Option[Long]): Output[Abs] = {
    val (state, store, kstore) =
      State.inject(exp, sem.initialEnv, sem.initialStore)
    loop(Set(state),
         Set(),
         store,
         kstore,
         Set(),
         System.nanoTime,
         timeout,
         if (graph) { Some(new Graph[State, Unit]()) } else { None },
         sem)
  }
}
