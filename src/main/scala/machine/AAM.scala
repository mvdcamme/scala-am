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
    with KickstartEvalEvalKontMachine[Exp, Abs, Addr, Time] {

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
                   t: Time) {
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

    /**
      * Integrates a set of actions (returned by the semantics, see
      * Semantics.scala), in order to generate a set of states that succeeds this
      * one.
      */
    private def integrate(a: KontAddr,
                          edges: Set[(Action[Exp, Abs, Addr], List[EdgeInformation])]): Set[(State, List[EdgeInformation])] =
      edges.map({ case (action, edgeInfo) =>
        val newState = action match {
          /* When a value is reached, we go to a continuation state */
          case ActionReachedValue(v, store, _) =>
            State(ControlKont(v), store, kstore, a, time.tick(t))
          /* When a continuation needs to be pushed, push it in the continuation store */
          case ActionPush(frame, e, env, store, _) => {
            val next = NormalKontAddress[Exp, Time](e, t)
            State(ControlEval(e, env),
              store,
              kstore.extend(next, Kont(frame, a)),
              next,
              time.tick(t))
          }
          /* When a value needs to be evaluated, we go to an eval state */
          case ActionEval(e, env, store, _) =>
            State(ControlEval(e, env), store, kstore, a, time.tick(t))
          /* When a function is stepped in, we also go to an eval state */
          case ActionStepIn(fexp, _, e, env, store, _, _) =>
            State(ControlEval(e, env), store, kstore, a, time.tick(t, fexp))
          /* When an error is reached, we go to an error state */
          case ActionError(err) =>
            State(ControlError(err), store, kstore, a, time.tick(t))
        }
        (newState, edgeInfo)
      })

    /**
      * Computes the set of states that follow the current state
      */
    def step(sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, List[EdgeInformation])] =
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
                val succsEdges = integrate(next, sem.stepKont(v, frame, store, t))
                succsEdges.map({ case (succState, edgeInfo) =>
                  /* If step did not generate any EdgeInformation, place a FrameUsed EdgeInformation */
                  val replacedEdgeInfo = FrameFollowed(frame) :: edgeInfo
                  (succState, replacedEdgeInfo)
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

  case class AAMOutput(halted: Set[State],
                       numberOfStates: Int,
                       time: Double,
                       graph: Option[Graph[State, List[EdgeInformation]]],
                       timedOut: Boolean,
                       stepSwitched: Option[Int])
      extends Output[Abs] with MayHaveGraph[State] with HasFinalStores[Addr, Abs] {

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
                    node => List(scala.xml.Text(node.toString.take(40))))
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  /*
   * Checks whether the given (successor) state is a eval-state. If so, adds an EvaluatingExpression
   * annotation to the list of edge annotations.
   */
  def addEvalEdgeAnnotation(s: State, edgeInfos: List[EdgeInformation]): List[EdgeInformation] = s.control match {
    case ControlEval(exp, _) =>
      EvaluatingExpression(exp) :: edgeInfos
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
             graph: Graph[State, List[EdgeInformation]]): AAMOutput = {
      if (timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
        AAMOutput(halted,
                  visited.size,
                  (System.nanoTime - startingTime) / Math.pow(10, 9),
                  Some(graph),
                  true,
                  stepSwitched)
      } else {
        todo.headOption match {
          case Some(s) =>
            if (visited.contains(s)) {
              /* If we already visited the state, we ignore it. */
//              loop(todo.tail, visited, halted, startingTime, graph)
              loop(todo.tail, visited, halted, startingTime, graph)
            } else if (visited.exists(s2 => s2.subsumes(s))) {
              /* If the state is subsumed by another already visited state,
               * we ignore it. The subsumption part reduces the number of visited
               * states but leads to non-determinism due to the non-determinism
               * of Scala's headOption (it seems so at least).
               * We do have to add an edge from the current state to the subsumed state. */
              loop(todo.tail, visited, halted, startingTime, visited.foldLeft[Graph[State, List[EdgeInformation]]](graph)({
                case (graph, s2) => if (s2.subsumes(s)) graph.addEdge(s, List(StateSubsumed), s2) else graph}))
            } else if (s.halted || stopEval.fold(false)(pred => pred(s))) {
              /* If the state is a final state or the stopEval predicate determines the machine can stop exploring
               * this state, add it to the list of final states and continue exploring the graph */
              loop(todo.tail, visited + s, halted + s, startingTime, graph)
            } else {
              /* Otherwise, compute the successors (and edges to these successors) of this state,
              update the graph, and push the new successors on the todo list */
              val succsEdges = s.step(sem)
              val newGraph =
                graph.addEdges(succsEdges.map(s2 => (s, addEvalEdgeAnnotation(s2._1, s2._2), s2._1)))
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
                      Some(graph),
                      false,
                      stepSwitched)
        }
      }
    }
    val startingTime = System.nanoTime
    loop(Set(initialState), Set(), Set(), startingTime, new Graph[State, List[EdgeInformation]]().addNode(initialState))
  }

  def kickstartAnalysis[L](analysis: Analysis[L, Exp, Abs, Addr, Time],
                           initialState: State,
                           sem: Semantics[Exp, Abs, Addr, Time],
                           timeout: Option[Long]): Option[L] = {
    def loop(todo: Set[(State, L)],
             visited: Set[(State, L)],
             finalValue: Option[L],
             startingTime: Long): Option[L] =
      if (timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
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
           timeout: Option[Long]): Output[Abs] = {
    kickstartEval(State.inject(exp, sem.initialEnv, sem.initialStore),
                  sem,
                  None,
                  timeout,
                  None)
  }

  override def analyze[L](exp: Exp,
                          sem: Semantics[Exp, Abs, Addr, Time],
                          analysis: Analysis[L, Exp, Abs, Addr, Time],
                          timeout: Option[Long]) = {
    val startingTime = System.nanoTime
    kickstartAnalysis[L](analysis,
                         State.inject(exp, sem.initialEnv, sem.initialStore),
                         sem,
                         timeout)
  }
}
