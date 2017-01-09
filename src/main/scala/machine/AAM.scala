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
class AAM[Exp: Expression, Abs: JoinLattice, Addr: Address, Time: Timestamp]
    extends EvalKontMachine[Exp, Abs, Addr, Time]
    with ProducesStateGraph[Exp, Abs, Addr, Time] {

  type MachineState = State
  type GraphNode = State
  override type MachineOutput = AAMOutput

  def name = "AAM"

  /**
    * A machine state is made of a control component, a value store, a
    * continuation store, and an address representing where the current
    * continuation lives.
    */
  case class State(control: Control,
                   store: Store[Addr, Abs],
                   kstore: KontStore[KontAddr],
                   a: KontAddr,
                   t: Time)
    extends StateTrait[Exp, Abs, Addr, Time] {
    override def toString = control.toString

    /**
      * Checks whether a states subsumes another, i.e., if it is "bigger". This
      * is used to perform subsumption checking when exploring the state space,
      * in order to avoid exploring states for which another state that subsumes
      * them has already been explored.
      */
    def subsumes(that: State): Boolean =
      control.subsumes(that.control) && store.subsumes(that.store) && a == that.a && kstore
        .subsumes(that.kstore) && t == that.t

    type EdgeInformation = (State, List[EdgeAnnotation], List[StateChangeEdge[State]])

    /**
      * Integrates a set of actions (returned by the semantics, see
      * Semantics.scala), in order to generate a set of states that succeeds this
      * one.
      */
    private def integrate(a: KontAddr,
                          actionChanges: Set[(Action[Exp, Abs, Addr], List[StoreChangeSemantics[Abs, Addr]])])
    : Set[EdgeInformation] =
      actionChanges.map( (actionChange) => {
        val convertedStoreChanges = actionChange._2.map(_.convert[State])
        actionChange._1 match {
          /* When a value is reached, we go to a continuation state */
          case ActionReachedValue(v, store, _) =>
            (State(ControlKont(v), store, kstore, a, time.tick(t)),
              Nil,
              ControlValueReached(v) :: TimeTick() :: convertedStoreChanges)
          /* When a continuation needs to be pushed, push it in the continuation store */
          case ActionPush(frame, e, env, store, _) => {
            val next = NormalKontAddress[Exp, Time](e, t)
            val kont = Kont(frame, a)
            (State(ControlEval(e, env), store, kstore.extend(next, kont), next, time.tick(t)),
              List(KontAddrPushed(next)),
              ControlExpEvaluated[Exp, Addr, State](e, env) :: KontStoreFramePush[KontAddr, State](next, kont) ::
                KontAddrChanged[KontAddr, State](next) :: TimeTick[State]() :: convertedStoreChanges)
          }
          /* When a value needs to be evaluated, we go to an eval state */
          case ActionEval(e, env, store, _) =>
            (State(ControlEval(e, env), store, kstore, a, time.tick(t)),
              Nil,
              ControlExpEvaluated(e, env) :: TimeTick() :: convertedStoreChanges)
          /* When a function is stepped in, we also go to an eval state */
          case ActionStepIn(fexp, _, e, env, store, _, _) =>
            (State(ControlEval(e, env), store, kstore, a, time.tick(t, fexp)),
              Nil,
              ControlExpEvaluated(e, env) :: TimeTickExp(fexp) :: convertedStoreChanges)
          /* When an error is reached, we go to an error state */
          case ActionError(err) =>
            (State(ControlError(err), store, kstore, a, time.tick(t)),
              Nil,
              ControlErrorReached(err) :: TimeTick() :: convertedStoreChanges)
        }
      })

    private def maybeAddKontAddrChanged(a: KontAddr, stateChanges: List[StateChangeEdge[State]])
    : List[StateChangeEdge[State]] =
      if (stateChanges.exists({
        case KontAddrChanged(_) => true
        case _ => false})) {
        stateChanges
      } else {
        KontAddrChanged[KontAddr, State](a) :: stateChanges
      }

    /**
      * Computes the set of states that follow the current state
      */
    def step(sem: Semantics[Exp, Abs, Addr, Time]): Set[EdgeInformation] =
      control match {
        /* In a eval state, call the semantic's evaluation method */
        case ControlEval(e, env) =>
          integrate(a, sem.stepEval(e, env, store, t))
        /* In a continuation state, call the semantics' continuation method */
        case ControlKont(v) =>
          kstore
            .lookup(a)
            .flatMap({
              case Kont(frame, next) =>
                val edgeInfos = integrate(next, sem.stepKont(v, frame, store, t))
                edgeInfos.map({ case (succState, edgeAnnotations, stateChanges) =>
                  /* If step did not generate any EdgeAnnotation, place a FrameFollowed EdgeAnnotation */
                  val replacedEdgeAnnot =
                    KontAddrPopped(a, next) ::
                    FrameFollowed[Abs](frame.asInstanceOf[SchemeFrame[Abs, HybridAddress.A, HybridTimestamp.T]]) ::
                    edgeAnnotations
                  (succState, replacedEdgeAnnot, maybeAddKontAddrChanged(next, stateChanges))
                })
            })
        /* In an error state, the state is not able to make a step */
        case ControlError(_) => Set()
      }

    def stepAnalysis[L](analysis: Analysis[L, Exp, Abs, Addr, Time],
                        current: L): L = control match {
      case ControlEval(e, env) => analysis.stepEval(e, env, store, t, current)
      case ControlKont(v) => {
        val konts = kstore
          .lookup(a)
          .map({
            case Kont(frame, _) =>
              analysis.stepKont(v, frame, store, t, current)
          })
        if (konts.isEmpty) { current } else {
          konts.reduceLeft((x, y) => analysis.join(x, y))
        }
      }
      case ControlError(err) => analysis.error(err, current)
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
               store: Iterable[(Addr, Abs)]) =
      State(ControlEval(exp, Environment.initial[Addr](env)),
            Store.initial[Addr, Abs](store),
            KontStore.empty[KontAddr],
            HaltKontAddress,
            time.initial(""))
  }

  class StateDescriptor(val state: State) extends Descriptor[State] {
    override def describe: String = s"<p>${state.control}</p>\n" +
      s"<p>${state.store}</p>\n" +
      s"<p>${state.kstore}</p>\n" +
      s"<p>${state.a}</p>\n" +
      s"<p>${state.t}</p>\n"
  }

  case class AAMOutput(halted: Set[State],
                       numberOfStates: Int,
                       time: Double,
                       graph: Graph[State, (List[EdgeAnnotation], List[StateChangeEdge[State]])],
                       timedOut: Boolean,
                       stepSwitched: Option[Int])
      extends Output[Abs] with HasGraph[State] with HasFinalStores[Addr, Abs] {

    /**
      * Returns the list of final values that can be reached
      */
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

    /**
      * Checks if a halted state contains a value that subsumes @param v
      */
    def containsFinalValue(v: Abs) =
      finalValues.exists(v2 => abs.subsumes(v2, v))

    /**
      * Outputs the graph in a dot file
      */
    def toDotFile(path: String) =
      graph.toDotFile(path,
                      node => List(scala.xml.Text(node.toString.take(40))),
                      (s) =>
                        if (halted.contains(s)) { Colors.Yellow } else {
                           s.control match {
                             case ControlEval(_, _) => Colors.Green
                             case ControlKont(_) => Colors.Pink
                             case ControlError(_) => Colors.Red
                           }
                        },
                      node => List(scala.xml.Text(node._1.mkString(", ").take(300))),
                      None)
  }

  /*
   * Checks whether the given (successor) state is a eval-state. If so, adds an EvaluatingExpression
   * annotation to the list of edge annotations.
   */
  def addSuccStateEdgeAnnotation(s: State, edgeInfos: List[EdgeAnnotation]): List[EdgeAnnotation] = s.control match {
    case ControlEval(exp, _) =>
      EvaluatingExpression(exp) :: edgeInfos
    case ControlKont(v) =>
      if (edgeInfos.exists({
        case FrameFollowed(frame) =>
          frame.meaningfullySubsumes
        case _ =>
          false
      })) {
        edgeInfos
      } else {
//        ReachedValue(v) ::
        edgeInfos
      }
    case _ =>
      edgeInfos
  }

  def kickstartEval(initialState: State,
                    sem: Semantics[Exp, Abs, Addr, Time],
                    stopEval: Option[State => Boolean],
                    timeout: Option[Long],
                    stepSwitched: Option[Int]): AAMOutput = {
    def loop(todo: Set[State],
             visited: Set[State],
             halted: Set[State],
             startingTime: Long,
             graph: Graph[State, (List[EdgeAnnotation], List[StateChangeEdge[State]])]): AAMOutput = {
      if (timeout.exists(System.nanoTime - startingTime > _)) {
        AAMOutput(halted,
                  visited.size,
                  (System.nanoTime - startingTime) / Math.pow(10, 9),
                  graph,
                  true,
                  stepSwitched)
      } else {
        todo.headOption match {
          case Some(s) =>
            if (visited.contains(s)) {
              /* If we already visited the state, we ignore it. */
              loop(todo.tail, visited, halted, startingTime, graph)
            } else if (visited.exists(s2 => s2.subsumes(s))) {
              /* If the state is subsumed by another already visited state,
               * we ignore it. The subsumption part reduces the number of visited
               * states but leads to non-determinism due to the non-determinism
               * of Scala's headOption (it seems so at least).
               * We do have to add an edge from the current state to the subsumed state. */
              loop(todo.tail, visited, halted, startingTime, visited.foldLeft[Graph[State, (List[EdgeAnnotation], List[StateChangeEdge[State]])]](graph)({
                case (graph, s2) =>
                  if (s2.subsumes(s))
                    graph.addEdge(s, (List(StateSubsumed), Nil), s2)
                  else
                    graph}))
            } else if (s.halted || stopEval.fold(false)(pred => pred(s))) {
              /* If the state is a final state or the stopEval predicate determines the machine can stop exploring
               * this state, add it to the list of final states and continue exploring the graph */
              loop(todo.tail, visited + s, halted + s, startingTime, graph)
            } else {
              /* Otherwise, compute the successors (and edges to these successors) of this state,
              update the graph, and push the new successors on the todo list */
              val succsEdges = s.step(sem)
              val newGraph =
                graph.addEdges(succsEdges.map(s2 => (s, (addSuccStateEdgeAnnotation(s2._1, s2._2), s2._3), s2._1)))
              loop(todo.tail ++ succsEdges.map(_._1),
                   visited + s,
                   halted,
                   startingTime,
                   newGraph)
            }
          case None =>
            AAMOutput(halted,
                      visited.size,
                      (System.nanoTime - startingTime) / Math.pow(10, 9),
                      graph,
                      false,
                      stepSwitched)
        }
      }
    }
    val startingTime = System.nanoTime
    loop(Set(initialState), Set(), Set(), startingTime, new HyperlinkedGraph().addNode
    (initialState))
  }

  def kickstartAnalysis[L](analysis: Analysis[L, Exp, Abs, Addr, Time],
                           initialState: State,
                           sem: Semantics[Exp, Abs, Addr, Time],
                           timeout: Option[Long]): Option[L] = {
    def loop(todo: Set[(State, L)],
             visited: Set[(State, L)],
             finalValue: Option[L],
             startingTime: Long): Option[L] =
      if (timeout.exists(System.nanoTime - startingTime > _)) {
        None
      } else {
        todo.headOption match {
          case Some((s, l)) =>
            if (visited.contains((s, l)) || visited
                  .exists({ case (s2, _) => s2.subsumes(s) })) {
              loop(todo.tail, visited, finalValue, startingTime)
            } else if (s.halted) {
              loop(todo.tail, visited + ((s, l)), finalValue match {
                case None => Some(s.stepAnalysis(analysis, l))
                case Some(l2) =>
                  Some(analysis.join(l2, s.stepAnalysis(analysis, l)))
              }, startingTime)
            } else {
              val succsEdges = s.step(sem)
              val l2 = s.stepAnalysis(analysis, l)
              loop(todo.tail ++ succsEdges.map(s2 => (s2._1, l2)),
                   visited + ((s, l)),
                   finalValue,
                   startingTime)
            }
          case None => finalValue
        }
      }
    val startingTime = System.nanoTime
    loop(Set((initialState, analysis.init)), Set(), None, startingTime)
  }

  /**
    * Performs the evaluation of an expression, possibly writing the output graph
    * in a file, and returns the set of final states reached
    */
  def eval(exp: Exp,
           sem: Semantics[Exp, Abs, Addr, Time],
           graph: Boolean,
           timeout: Option[Long]): Output[Abs] =
    kickstartEval(State.inject(exp, sem.initialEnv, sem.initialStore),
                  sem,
                  None,
                  timeout,
                  None)

  override def analyze[L](exp: Exp,
                          sem: Semantics[Exp, Abs, Addr, Time],
                          analysis: Analysis[L, Exp, Abs, Addr, Time],
                          timeout: Option[Long]) =
    kickstartAnalysis[L](analysis,
                         State.inject(exp, sem.initialEnv, sem.initialStore),
                         sem,
                         timeout)

  object StateChangeApplier extends StateChangeEdgeApplier[State] {
    override def applyStateChangeEdge(state: State, stateChangeEdge: StateChangeEdge[State]): State = stateChangeEdge match {
      case e: ControlErrorReached[State] =>
        state.copy(control = ControlError(e.error))
      case e: ControlExpEvaluated[Exp, Addr, _] =>
        state.copy(control = ControlEval(e.e, e.env))
      case e: ControlValueReached[Abs, State] =>
        state.copy(control = ControlKont(e.v))
      case e: KontAddrChanged[KontAddr, State] =>
        state.copy(a = e.a)
      case e: KontStoreFramePush[KontAddr, State] =>
        state.copy(kstore = state.kstore.extend(e.pushAddress, e.kont))
      case e: StoreExtend[Abs, Addr, State] =>
        state.copy(store = state.store.extend(e.a, e.v))
      case e: StoreUpdate[Abs, Addr, State] =>
        state.copy(store = state.store.update(e.a, e.v))
      case TimeTick() =>
        state.copy(t = time.tick(state.t))
      case e: TimeTickExp[Exp, State] =>
        state.copy(t = time.tick(state.t, e.e))
    }
  }
}
