import scalaz._
import scalaz.Scalaz._

/**
  * Implementation of a CESK machine following the AAM approach (Van Horn, David,
  * and Matthew Might. "Abstracting abstract machines." ACM Sigplan
  * Notices. Vol. 45. No. 9. ACM, 2010).
  *
  * A difference with the paper is that we separate the continuation store
  * (KontStore) from the value store (Store). That simplifies the implementation
  * of both stores, and the only change it induces is that we are not able to
  * support first-class continuation as easily (we don't support them at all, but
  * they could be added).
  *
  * Also, in the paper, a CESK state is made of 4 components: Control,
  * Environment, Store, and Kontinuation. Here, we include the environment in the
  * control component, and we distinguish "eval" states from "continuation"
  * states. An eval state has an attached environment, as an expression needs to
  * be evaluated within this environment, whereas a continuation state only
  * contains the value reached.
  */
class KickstartAAMGlobalStore[Exp: Expression, Abs: IsSchemeLattice, Addr: Address, Time: Timestamp]
    extends EvalKontMachine[Exp, Abs, Addr, Time]
    with ProducesStateGraph[Exp, Abs, Addr, Time] {

  val sabs: IsSchemeLattice[Abs] = implicitly[IsSchemeLattice[Abs]]

  type MachineState = State
  type InitialState = (State, GlobalStore, KontStore[KontAddr])
  override type MachineOutput = AAMOutput

  def name = "AAMGlobalStore"

  implicit val stateWithKey = new WithKey[State] {
    type K = KontAddr
    def key(st: State) = st.a
  }

  case class GlobalStore(store: DeltaStore[Addr, Abs], delta: Map[Addr, Abs]) {
    def includeDelta(d: Option[Map[Addr, Abs]]): GlobalStore = d match {
      case Some(d) => this.copy(delta = delta |+| d)
      case None => throw new Exception("AAMGlobalStore should be used with a store that supports delta!")
    }
    def isUnchanged = delta.isEmpty
    def commit = if (isUnchanged) { this } else { this.copy(store = store.addDelta(delta), delta = Map()) }
  }

  /**
    * A machine state is made of a control component, a value store, a
    * continuation store, and an address representing where the current
    * continuation lives.
    */
  case class State(control: Control, a: KontAddr, t: Time)
    extends StateTrait[Exp, Abs, Addr, Time] {
    override def toString = control.toString

    def isErrorState: Boolean = control match {
      case _: ControlError => true
      case _ => false
    }

    /**
      * Checks whether a states subsumes another, i.e., if it is "bigger". This
      * is used to perform subsumption checking when exploring the state space,
      * in order to avoid exploring states for which another state that subsumes
      * them has already been explored.
      */
    def subsumes(that: State): Boolean =
      control.subsumes(that.control) && a == that.a && t == that.t

    case class EdgeComponents(state: State, filters: FilterAnnotations[Exp, Abs, Addr], actions: List[ActionReplay[Exp, Abs, Addr]])

    /**
      * Integrates a set of actions (returned by the semantics, see
      * Semantics.scala), in order to generate a set of states that succeeds this
      * one.
      */
    private def integrate(a: KontAddr, edgeInfos: Set[EdgeInformation[Exp, Abs, Addr]], store: GlobalStore, kstore: KontStore[KontAddr]):
      (Set[EdgeComponents], GlobalStore, KontStore[KontAddr]) =
      edgeInfos.foldLeft[(Set[EdgeComponents], GlobalStore, KontStore[KontAddr])]((Set[EdgeComponents](), store, kstore))((acc, edgeInformation) => {
        val actions = edgeInformation.actions
        /* If step applied a primitive, generate a filter for it */
        val primCallFilter = actions.foldLeft[Set[MachineFilterAnnotation]](Set())( (set, actionR) => actionR match {
          case primCall: ActionPrimCallT[Exp, Abs, Addr] =>
            Set(PrimCallMark(primCall.fExp, primCall.fValue, t))
          case _ =>
            Set()
        })
        val filters = FilterAnnotations[Exp, Abs, Addr](primCallFilter, edgeInformation.semanticsFilters)
        edgeInformation.action match {
          /* When a value is reached, we go to a continuation state */
          case ActionReachedValue(v, store2, _) =>
            (acc._1 + EdgeComponents(State(ControlKont(v), a, Timestamp[Time].tick(t)), filters, actions), acc._2.includeDelta(store2.delta), acc._3)
          /* When a continuation needs to be pushed, push it in the continuation store */
          case ActionPush(frame, e, env, store2, _) => {
            val next = NormalKontAddress[Exp, Time](e, t)
            val kont = Kont(frame, a)
            (acc._1 + EdgeComponents(State(ControlEval(e, env), next, Timestamp[Time].tick(t)), filters, actions),
             acc._2.includeDelta(store2.delta),
             acc._3.extend(next, Kont(frame, a)))
          }
          /* When a value needs to be evaluated, we go to an eval state */
          case ActionEval(e, env, store2, _) =>
            (acc._1 + EdgeComponents(State(ControlEval(e, env), a, Timestamp[Time].tick(t)), filters, actions), acc._2.includeDelta(store2.delta), acc._3)
          /* When a function is stepped in, we also go to an eval state */
          case ActionStepIn(fexp, clo, e, env, store2, _, _) =>
            val closureFilter =  ClosureCallMark[Exp, Abs, Time](fexp, sabs.inject[Exp, Addr]((clo._1, clo._2), None), clo._1, t)
            (acc._1 + EdgeComponents(State(ControlEval(e, env), a, Timestamp[Time].tick(t, fexp)), filters + closureFilter, actions), acc._2.includeDelta(store2.delta), acc._3)
          /* When an error is reached, we go to an error state */
          case ActionError(err) =>
            (acc._1 + EdgeComponents(State(ControlError(err), a, Timestamp[Time].tick(t)), filters, actions), acc._2, acc._3)
        }
      })

    def addActionPopKontT(actions: List[ActionReplay[Exp, Abs, Addr]]): List[ActionReplay[Exp, Abs, Addr]] =
      if (actions.exists( (actionR) => actionR.popsKont)) {
        /*
         * One of the actionReplays in the edge already pops the topmost continuation,
         * so no need to add an ActionPopKontT.
         */
        actions
      } else if (actions.exists({
        /*
         * Otherwise, definitely add an ActionPopKontT: add it to the front of the edge if there's
         * some new continuation frame going to be pushed:
         *
         * If a frame is pushed, the pop should happen before the frame is pushed, so that you don't just
         * pop the newly pushed frame. Otherwise, the pop should happen afterwards: if the top frame is e.g.,
         * a FrameFuncallOperands for some primitive application, the frame contains important information
         * such as the values of the operands and hence should only be popped after the primitive has been
         * applied.
         */
        case _: ActionEvalPushR[Exp, Abs, Addr] => true
        case _: ActionEvalPushDataR[Exp, Abs, Addr] => assert(false, "Should not happen"); true
        case _ => false
      })) {
        ActionPopKontT[Exp, Abs, Addr]() :: actions
      } else {
        /*
         * If no new continuation frame is going to be pushed, add the ActionPopKontT to the back of the edge.
         */
        actions :+ ActionPopKontT[Exp, Abs, Addr]()
      }

    def addTimeTickT(actions: List[ActionReplay[Exp, Abs, Addr]]): List[ActionReplay[Exp, Abs, Addr]] =
      if (actions.exists(_.ticksTime)) {
        actions
      } else {
        actions :+ ActionTimeTickR()
      }

    /**
      * Computes the set of states that follow the current state
      */
    def step(sem: ConvertableSemantics[Exp, Abs, Addr, Time], store: GlobalStore, kstore: KontStore[KontAddr]): (Set[EdgeComponents], GlobalStore, KontStore[KontAddr]) =
      control match {
        /* In a eval state, call the semantic's evaluation method */
        case ControlEval(e, env) =>
          integrate(a, sem.stepEval(e, env, store.store, t), store, kstore)
        /* In a continuation state, call the semantics' continuation method */
        case ControlKont(v) =>
          kstore.lookup(a)
            .foldLeft[(Set[EdgeComponents], GlobalStore, KontStore[KontAddr])]((Set[EdgeComponents](), store, kstore))((acc, kont) => kont match {
              case Kont(frame, next) =>
                integrate(next, sem.stepKont(v, frame, store.store, t), acc._2, acc._3) match {
                  case (edgeComponents, store2, kstore2) =>
                    (acc._1 ++ edgeComponents.map({ case EdgeComponents(succState, filterAnnotations, actions) =>
                      /* If step did not generate any EdgeAnnotation, place a FrameFollowed EdgeAnnotation */
                      val replacedFilters = filterAnnotations + KontAddrPopped(a, next) + FrameFollowed[Abs](frame.asInstanceOf[ConvertableSchemeFrame[Abs, HybridAddress.A, HybridTimestamp.T]])
                      val popKontAddedAction = addActionPopKontT(actions)
                      val timeTickAddedAction = addTimeTickT(popKontAddedAction)
                      EdgeComponents(succState, replacedFilters, timeTickAddedAction)
                    }),
                     store2,
                     kstore2)
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
    def inject(exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]): InitialState =
      (State(ControlEval(exp, Environment.initial[Addr](env)), HaltKontAddress, Timestamp[Time].initial("")), GlobalStore(DeltaStore[Addr, Abs](store.toMap, Map()), Map()), TimestampedKontStore[KontAddr](Map(), 0))
    import scala.language.implicitConversions

    type Context = Unit
    implicit val graphNode = new GraphNode[State, Unit] {
      override def label(s: State) = s.toString
      override def color(s: State) = if (s.halted) { Colors.Yellow } else { s.control match {
        case _: ControlEval => Colors.Green
        case _: ControlKont => Colors.Pink
        case _: ControlError => Colors.Red
      }}
    }
  }

  type G = Option[Graph[State, Unit, Unit]]
  case class AAMOutput(halted: Set[State], finalStore: GlobalStore, numberOfStates: Int, time: Double, graph: Graph[State, EdgeAnnotation[Exp, Abs, Addr], Unit],
                       timedOut: Boolean, stepSwitched: Option[Int])
      extends Output with HasGraph[Exp, Abs, Addr, State] with HasFinalStores[Addr, Abs] {

    override def toString = s"AAMOutput($numberOfStates states, ${time}s, $stepSwitched)"

    /**
      * Returns the list of final values that can be reached
      */
    def finalValues = {
      val errorStates = halted.filter(_.control match {
        case _: ControlError => true
        case _ => false
      })
      if (errorStates.isEmpty) {
        println("DID NOT FIND ANY ERROR STATES")
      } else {
        println(s"FOUND THE FOLLOWING ERROR STATES: $errorStates")
      }
      halted.flatMap(st =>
        st.control match {
          case ControlKont(v) => Set[Abs](v)
          case _ => Set[Abs]()
        })
    }

    /**
      * Returns the set of stores of the final states
      */
    def finalStores: Set[Store[Addr, Abs]] = Set(finalStore.store)

    /**
      * Outputs the graph in a dot file
      */
    def toDotFile(path: String): Unit = AAMGraphPrinter.printGraph(graph, path)
    def toFile(path: String)(output: GraphOutput): Unit = output.toFile(graph, ())(path)
  }

  /*
   * Checks whether the given (successor) state is a eval-state. If so, adds an EvaluatingExpression
   * annotation to the list of edge annotations.
   */
  def addSuccStateFilter(s: State, filters: FilterAnnotations[Exp, Abs, Addr]): FilterAnnotations[Exp, Abs, Addr] = s.control match {
    case ControlEval(exp, _) =>
      filters + EvaluatingExpression(exp)
    case ControlKont(v) =>
      if (filters.machineExists({
        case FrameFollowed(frame) =>
          frame.meaningfullySubsumes
        case _ =>
          false
      })) {
        filters
      } else {
//        ReachedValue(v) ::
        filters
      }
    case _ =>
      filters
  }

  def kickstartEval(initialState: InitialState, sem: ConvertableSemantics[Exp, Abs, Addr, Time], stopEval: Option[State => Boolean],
                    timeout: Timeout, stepSwitched: Option[Int]): AAMOutput = {
    val startingTime = System.nanoTime
    def loop(todo: Set[State], visited: Set[State], halted: Set[State], store: GlobalStore, kstore: KontStore[KontAddr], graph: Graph[State, EdgeAnnotation[Exp, Abs, Addr], Unit]): AAMOutput = {
      if (todo.isEmpty || timeout.reached) {
        AAMOutput(halted, store, visited.size, (System.nanoTime - startingTime) / Math.pow(10, 9), graph, true, stepSwitched)
      } else {
        val (succsEdges, store2, kstore2) = todo.foldLeft(Set[(State, EdgeAnnotation[Exp, Abs, Addr], State)](), store, kstore)((acc, state) =>
          state.step(sem, acc._2, acc._3) match {
            case (succsEdges, store2Temp, kstore2Temp) =>
              val edges = succsEdges.map(s2 => {
                val filters = addSuccStateFilter(s2.state, s2.filters)
                (state, EdgeAnnotation[Exp, Abs, Addr](filters, s2.actions), s2.state)
              })
              (acc._1 ++ edges, store2Temp, kstore2Temp)
          })
        if (store2.isUnchanged && kstore.fastEq(kstore2)) {
          //assert(store2.commit.store == store2.store)
          loop(succsEdges.map({ case (_, _, s2) => s2 }).diff(visited), visited ++ todo, halted ++ todo.filter(_.halted),
               store2, kstore2, graph.addEdges(succsEdges))
        } else {
          //assert(!(!store2.isUnchanged && store2.commit.store == store2.store))
          loop(succsEdges.map({ case (_, _, s2) => s2 }), Set(), halted ++ todo.filter(_.halted),
               store2.commit, kstore2, graph.addEdges(succsEdges))
        }
      }
    }
    val (state, store, kstore) = initialState
    loop(Set(state), Set(), Set(), store, kstore, Graph.empty[State, EdgeAnnotation[Exp, Abs, Addr], Unit].addNode(state))
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output = ???

  /**
    * Performs the evaluation of an expression, possibly writing the output graph
    * in a file, and returns the set of final states reached
    */
  def eval(exp: Exp, sem: ConvertableSemantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output = {
    val initialState = State.inject(exp, sem.initialEnv, sem.initialStore)
    kickstartEval(initialState, sem, None, timeout, None)
  }

  object AAMGraphPrinter extends GraphPrinter[Graph[State, EdgeAnnotation[Exp, Abs, Addr], Unit]] {

    def printGraph(graph: Graph[State, EdgeAnnotation[Exp, Abs, Addr], Unit],
                   path: String): Unit = {
      GraphDOTOutput.toFile(graph, ())(path)
    }
  }

  object AAMStateInfoProvider extends StateInfoProvider[Exp, Abs, Addr, Time, State] {

    def evalExp(state: State) = state.control match {
      case ControlEval(exp, _) =>
        Some(exp)
      case _ =>
        None
    }

    def valueReached(state: State) = state.control match {
      case ControlKont(value) =>
        Some(value)
      case _ =>
        None
    }

    val kaConverter = new ConvertTimestampKontAddrConverter[Exp](DefaultHybridTimestampConverter)

    def halted(state: State): Boolean =
      state.halted
  }
}
