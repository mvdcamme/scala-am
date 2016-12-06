sealed trait FreeKontAddr extends KontAddr
object FreeKontAddr {
  implicit object KontAddrKontAddress extends KontAddress[FreeKontAddr]
}

/**
  * A continuation address is either for a normal continuation, and contains an
  * expression and the corresponding binding environment from the moment where
  * the continuation has been allocated
  */
sealed case class FreeNormalKontAddress[Exp: Expression, Addr: Address](
    exp: Exp,
    env: Environment[Addr])
    extends FreeKontAddr {
  override def toString = s"FreeNormalKontAddress($exp)"
}

/** Or it is the address of the halt continuation */
object FreeHaltKontAddress extends FreeKontAddr {
  override def toString = "FreeHaltKontAddress"
}

/**
  * Implementation of "Pushdown Control-Flow Analysis for Free", which is
  * basically a variant of AAC with better complexity (Gilray, Thomas, et
  * al. "Pushdown Control-Flow Analysis for Free." arXiv preprint
  * arXiv:1507.03137 (2015)).
  */
class Free[Exp: Expression, Abs: JoinLattice, Addr: Address, Time: Timestamp]
    extends EvalKontMachine[Exp, Abs, Addr, Time]
    with KickstartEvalEvalKontMachine[Exp, Abs, Addr, Time] {

  type MachineState = States
  type GraphNode = State
  override type MachineOutput = FreeOutput

  def name = "Free"

  /**
    * A state contains the control component, a store, a continuation store, and
    * the address of the current continuation. The stores aren't actually local
    * to the state, but they are injected inside it during the state space
    * exploration phase.
    */
  case class State(control: Control,
                   store: Store[Addr, Abs],
                   kstore: KontStore[FreeKontAddr],
                   k: FreeKontAddr,
                   t: Time) {
    override def toString = control.toString
    def subsumes(that: State): Boolean =
      control.subsumes(that.control) && store.subsumes(that.store) && kstore
        .subsumes(that.kstore) && k.equals(that.k)

    /** Integrate a set of action to compute the successor states */
    private def integrate(k: FreeKontAddr,
                          edges: Set[(Action[Exp, Abs, Addr], List[EdgeInformation])]): Set[(State, List[EdgeInformation])] =
      edges.map({ case (action, edgeInfo) =>
        val newState = action match {
          case ActionReachedValue(v, store, _) =>
            State(ControlKont(v), store, kstore, k, time.tick(t))
          case ActionPush(frame, e, env, store, _) =>
            val next = new FreeNormalKontAddress(e, env)
            State(ControlEval(e, env),
              store,
              kstore.extend(next, Kont(frame, k)),
              next,
              time.tick(t))
          case ActionEval(e, env, store, _) =>
            State(ControlEval(e, env), store, kstore, k, time.tick(t))
          case ActionStepIn(fexp, _, e, env, store, _, _) =>
            State(ControlEval(e, env), store, kstore, k, time.tick(t, fexp))
          case ActionError(err) =>
            State(ControlError(err), store, kstore, k, time.tick(t))
        }
        (newState, edgeInfo)})

    /** Computes the successors states of this one relying on the given semantics */
    def step(sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, List[EdgeInformation])] =
      control match {
        case ControlEval(e, env) =>
          integrate(k, sem.stepEval(e, env, store, t))
        case ControlKont(v) =>
          kstore
            .lookup(k)
            .foldLeft(Set[(State, List[EdgeInformation])]())((acc, k) =>
              k match {
                case Kont(frame, next) =>
                  acc ++ integrate(next, sem.stepKont(v, frame, store, t))
            })
        case ControlError(_) => Set()
      }

    /** Checks whether this state has finished evaluation */
    def halted = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => k.equals(FreeHaltKontAddress)
      case ControlError(_) => true
    }
  }
  object State {
    def inject(exp: Exp,
               env: Iterable[(String, Addr)],
               store: Iterable[(Addr, Abs)]) =
      State(ControlEval(exp, Environment.empty[Addr].extend(env)),
            Store.initial[Addr, Abs](store),
            KontStore.empty[FreeKontAddr],
            FreeHaltKontAddress,
            time.initial(""))
  }

  /**
    * A configuration is basically a state without the store and continuation
    * store (because these stores are global).
    */
  case class Configuration(control: Control, k: FreeKontAddr, t: Time) {
    override def toString = s"($control, $k)"
  }

  /**
    * Represents multiple states as a set of configuration that share the same
    * store and continuation store
    */
  case class States(R: Set[Configuration],
                    store: Store[Addr, Abs],
                    kstore: KontStore[FreeKontAddr]) {
    override def toString = R.toString

    /** Performs a step on all the contained states */
    def step(sem: Semantics[Exp, Abs, Addr, Time]): States = {
      val states =
        R.map(conf => State(conf.control, store, kstore, conf.k, conf.t))
      val succsEdges = states.flatMap(state => state.step(sem))
      val (store1, kstore1) = succsEdges.map(_._1).foldLeft(
        (Store.empty[Addr, Abs], KontStore.empty[FreeKontAddr]))(
        (acc, state) => (acc._1.join(state.store), acc._2.join(state.kstore)))
      States(
        succsEdges.map(stateEdge => Configuration(stateEdge._1.control, stateEdge._1.k, stateEdge._1.t)),
        store1,
        kstore1)
    }
    def isEmpty = R.isEmpty
    def toStateSet: Set[State] =
      R.map({
        case Configuration(control, k, t) =>
          State(control, store, kstore, k, t)
      })
    def size: Int = R.size
  }

  /** The output of the machine */
  case class FreeOutput(halted: Set[State],
                        numberOfStates: Int,
                        time: Double,
                        graph: Option[Graph[State, List[EdgeInformation]]],
                        timedOut: Boolean,
                        stepSwitched: Option[Int])
      extends Output[Abs] with MayHaveGraph[State] with HasFinalStores[Addr, Abs] {

    def finalValues =
      halted.flatMap(st =>
        st.control match {
          case ControlKont(v) => Set[Abs](v)
          case _ => Set[Abs]()
      })

    /**
      * Returns the set of stores of the final states
      */
    def finalStores: Set[Store[Addr, Abs]] = halted.map(st => st.store)

    def containsFinalValue(v: Abs) =
      finalValues.exists(v2 => abs.subsumes(v2, v))
    def toDotFile(path: String) = graph match {
      case Some(g) =>
        g.toDotFile(path,
                    node => List(scala.xml.Text(node.toString)),
                    (s) =>
                      if (halted.contains(s)) { Colors.Yellow } else {
                        s.control match {
                          case ControlEval(_, _) => Colors.Green
                          case ControlKont(_) => Colors.Pink
                          case ControlError(_) => Colors.Red
                        }
                    },
                    _ => List())
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }
  object States {
    def inject(exp: Exp,
               env: Iterable[(String, Addr)],
               store: Iterable[(Addr, Abs)]) =
      States(
        Set(
          Configuration(ControlEval(exp, Environment.empty[Addr].extend(env)),
                        FreeHaltKontAddress,
                        time.initial(""))),
        Store.initial[Addr, Abs](store),
        KontStore.empty[FreeKontAddr])
  }

  /**
    * Performs state space exploration and builds the state graph at the same
    * time. We lose the "for free" part of this approach by constructing the
    * graph, since we have to take every possible combination of configurations
    * and draw edges between them.
    */
  @scala.annotation.tailrec
  private def loopWithLocalGraph(
      s: States,
      visited: Set[States],
      halted: Set[State],
      startingTime: Long,
      timeout: Option[Long],
      graph: Graph[State, List[EdgeInformation]],
      sem: Semantics[Exp, Abs, Addr, Time]): Output[Abs] = {
    val s2Edge = s.step(sem)
    val h = halted ++ s.toStateSet.filter(_.halted)
    if (s2Edge.isEmpty || visited.contains(s2Edge) || timeout
          .map(System.nanoTime - startingTime > _)
          .getOrElse(false)) {
      FreeOutput(
        h,
        visited.foldLeft(0)((acc, s) => acc + s.size),
        (System.nanoTime - startingTime) / Math.pow(10, 9),
        Some(graph),
        timeout.map(System.nanoTime - startingTime > _).getOrElse(false),
        None)
    } else {
      loopWithLocalGraph(s2Edge,
                         visited + s,
                         h,
                         startingTime,
                         timeout,
                         graph.addEdges(s.toStateSet.flatMap(state1 =>
                           s2Edge.toStateSet.map(state2 => (state1, List(TODOEdgeInformation), state2)))),
                         sem)
    }
  }

  def kickstartEval(s: States,
                    sem: Semantics[Exp, Abs, Addr, Time],
                    stopEval: Option[States => Boolean],
                    timeout: Option[Long],
                    stepSwitched: Option[Int]): FreeOutput = {

    val startingTime = System.nanoTime
    val initStateSet = s.toStateSet
    assert(initStateSet.size == 1)

    /**
      * Performs state space exploration and builds the state graph at the same
      * time. We lose the "for free" part of this approach by constructing the
      * graph, since we have to take every possible combination of configurations
      * and draw edges between them.
      */
    @scala.annotation.tailrec
    def loop(s: States,
             visited: Set[States],
             halted: Set[State],
             graph: Graph[State, List[EdgeInformation]]): FreeOutput = {
      val s2 = s.step(sem)
      val h = halted ++ s.toStateSet.filter(_.halted)
      if (s2.isEmpty || visited.contains(s2) || timeout
            .map(System.nanoTime - startingTime > _)
            .getOrElse(false)) {
        FreeOutput(h,
                   visited.foldLeft(0)((acc, s) => acc + s.size),
                   (System.nanoTime - startingTime) / Math.pow(10, 9),
                   Some(graph),
                   timeout
                     .map(System.nanoTime - startingTime > _)
                     .getOrElse(false) || stopEval.map(_(s2)).getOrElse(false),
                   stepSwitched)
      } else {
        loop(s2,
             visited + s,
             h,
             graph.addEdges(s.toStateSet.flatMap(state1 =>
               s2.toStateSet.map(state2 => (state1, List(TODOEdgeInformation), state2)))))
      }
    }

    loop(s, Set(), Set(), new Graph[State, List[EdgeInformation]]().addNode(initStateSet.head))
  }

  def eval(exp: Exp,
           sem: Semantics[Exp, Abs, Addr, Time],
           graph: Boolean,
           timeout: Option[Long]): Output[Abs] =
    if (graph) {
      loopWithLocalGraph(States.inject(exp, sem.initialEnv, sem.initialStore),
                         Set(),
                         Set(),
                         System.nanoTime,
                         timeout,
                         new Graph[State, List[EdgeInformation]](),
                         sem)
    } else {
      kickstartEval(States.inject(exp, sem.initialEnv, sem.initialStore),
                    sem,
                    None,
                    timeout,
                    None)
    }
}
