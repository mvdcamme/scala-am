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
      control.subsumes(that.control) && store.subsumes(that.store) && a == that.a && kstore.subsumes(that.kstore) && t == that.t

    type EdgeComponents = (State, List[EdgeFilterAnnotation], List[ActionReplay[Exp, Abs, Addr]])

    /**
      * Integrates a set of actions (returned by the semantics, see
      * Semantics.scala), in order to generate a set of states that succeeds this
      * one.
      */
    private def integrate(a: KontAddr,
                          actionChanges: Set[EdgeInformation[Exp, Abs, Addr]])
    : Set[EdgeComponents] =
      actionChanges.map( (actionChange) => {
        val actionEdges = actionChange.actionEdge
        val edgeFilterAnnots = actionChange.edgeFilterAnnotations
        actionChange.action match {
          /* When a value is reached, we go to a continuation state */
          case ActionReachedValue(v, store, _) =>
            (State(ControlKont(v), store, kstore, a, time.tick(t)),
              edgeFilterAnnots,
              actionEdges)
          /* When a continuation needs to be pushed, push it in the continuation store */
          case ActionPush(frame, e, env, store, _) => {
            val next = NormalKontAddress[Exp, Time](e, t)
            val kont = Kont(frame, a)
            (State(ControlEval(e, env), store, kstore.extend(next, kont), next, time.tick(t)),
              KontAddrPushed(next) :: edgeFilterAnnots,
              actionEdges)
          }
          /* When a value needs to be evaluated, we go to an eval state */
          case ActionEval(e, env, store, _) =>
            (State(ControlEval(e, env), store, kstore, a, time.tick(t)),
              edgeFilterAnnots,
              actionEdges)
          /* When a function is stepped in, we also go to an eval state */
          case ActionStepIn(fexp, _, e, env, store, _, _) =>
            (State(ControlEval(e, env), store, kstore, a, time.tick(t, fexp)),
              edgeFilterAnnots,
              actionEdges)
          /* When an error is reached, we go to an error state */
          case ActionError(err) =>
            (State(ControlError(err), store, kstore, a, time.tick(t)),
              edgeFilterAnnots,
              actionEdges)
        }
      })

    /**
      * Computes the set of states that follow the current state
      */
    def step(sem: Semantics[Exp, Abs, Addr, Time]): Set[EdgeComponents] =
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
                edgeInfos.map({ case (succState, edgeAnnotations, actionEdges) =>
                  /* If step did not generate any EdgeAnnotation, place a FrameFollowed EdgeAnnotation */
                  val replacedEdgeAnnot =
                    KontAddrPopped(a, next) ::
                    FrameFollowed[Abs](frame.asInstanceOf[SchemeFrame[Abs, HybridAddress.A, HybridTimestamp.T]]) ::
                    edgeAnnotations
                  val actionPopKont = ActionPopKontT[Exp, Abs, Addr]
                  /*
                   * If a frame is pushed, the pop should happen before the frame is pushed, so that you don't just
                   * pop the newly pushed frame. Otherwise, the pop should happen afterwards: if the top frame is e.g.,
                   * a FrameFuncallOperands for some primitive application, the frame contains important information
                   * such as the values of the operands and hence should only be popped after the primitive has been
                   * applied.
                   */
                  val replacedActionEdges = if (actionEdges.exists({
                    case _: ActionEvalPushR[Exp, Abs, Addr] => true
                    case _: ActionEvalPushDataR[Exp, Abs, Addr] => true
                    case _ => false
                  })) { actionPopKont :: actionEdges } else { actionEdges :+ actionPopKont }
                  (succState, replacedEdgeAnnot, replacedActionEdges)
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

  class StateDescriptor extends Descriptor[State] {
    def describe[U >: State](state: U): String = state match {
      case State(control, store, kstore, a, t) =>
          putIntoCollapsableList(List(
            control.descriptor.describe(control),
            store.descriptor.describe(store),
            kstore.descriptor.describe(kstore),
            a.toString,
            t.toString), state.toString, Some("state"))
      case _ =>
        state.toString
    }
  }

  case class AAMOutput(halted: Set[State],
                       numberOfStates: Int,
                       time: Double,
                       graph: Graph[State, (List[EdgeFilterAnnotation], List[ActionReplay[Exp, Abs, Addr]])],
                       timedOut: Boolean,
                       stepSwitched: Option[Int])
      extends Output[Abs] with HasGraph[Exp, Abs, Addr, State] with HasFinalStores[Addr, Abs] {

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
                      node => List(scala.xml.Text(("[" + node._1.mkString(", ") + "] [" + node._2.mkString(", ") + "]").take(300))),
                      None)
  }

  /*
   * Checks whether the given (successor) state is a eval-state. If so, adds an EvaluatingExpression
   * annotation to the list of edge annotations.
   */
  def addSuccStateEdgeAnnotation(s: State, edgeInfos: List[EdgeFilterAnnotation]): List[EdgeFilterAnnotation] = s.control match {
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
    val checkSubsumes = true
    def loop(todo: Set[State],
             visited: Set[State],
             halted: Set[State],
             startingTime: Long,
             graph: Graph[State, (List[EdgeFilterAnnotation], List[ActionReplay[Exp, Abs, Addr]])]): AAMOutput = {
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
            } else if (checkSubsumes && visited.exists(s2 => s2.subsumes(s))) {
              /* If the state is subsumed by another already visited state,
               * we ignore it. The subsumption part reduces the number of visited
               * states but leads to non-determinism due to the non-determinism
               * of Scala's headOption (it seems so at least).
               * We do have to add an edge from the current state to the subsumed state. */
              loop(todo.tail, visited, halted, startingTime, visited.foldLeft[Graph[State, (List[EdgeFilterAnnotation], List[ActionReplay[Exp, Abs, Addr]])]](graph)({
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
    implicit val stateDescriptor = new StateDescriptor()
    loop(Set(initialState), Set(), Set(), startingTime, new HyperlinkedGraph().addNode(initialState))
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

  object ActionReplayApplier extends ActionReplayApplier[Exp, Abs, Addr, State] {

    import scala.annotation.tailrec

    val emptyEnvironment = Environment.empty[Addr] // Move creation of the environment outside of applyActionMethod

    private def frameSavedOperands(a: KontAddr, kstore: KontStore[KontAddr]): Set[List[Abs]] =
      kstore.lookup(a).map(_.frame match {
        case frame: FrameFuncallOperands[Abs, Addr, Time] =>
          frame.f :: frame.args.map(_._2)
        case frame =>
          throw new Exception(s"Retrieving operands of non-FrameFunCallOperands $frame")
      })

    private def frameSavedValues(a: KontAddr, kstore: KontStore[KontAddr]): Set[List[Abs]] =
      kstore.lookup(a).map(_.frame match {
        case frame: FrameFuncallOperands[Abs, Addr, Time] =>
          frame.args.map(_._2)
        case frame: FrameLet[Abs, Addr, Time] =>
          frame.bindings.map(_._2)
        case _ =>
          List()
      })

    protected def popKont(state: State): Set[State] = {
      val konts = state.kstore.lookup(state.a)
      val nexts = konts.map(_.next)
      nexts.map( (next: KontAddr) => state.copy(a = next))
    }

    protected def getControlKontValue(state: State): Abs = {
      assert(state.control.isInstanceOf[ControlKont])
      state.control.asInstanceOf[ControlKont].v
    }

    protected def addControlKontValue(state: State, valuesSet: Set[List[Abs]]): Set[List[Abs]] = {
      val extraValue = getControlKontValue(state)
      valuesSet.map(_ :+ extraValue)
    }

    def applyActionReplay(state: State, action: ActionReplay[Exp, Abs, Addr])
                         (implicit sabs: IsSchemeLattice[Abs]): Set[State] = action match {
      case a: ActionAllocAddressesR[Exp, Abs, Addr] =>
        val newStore = a.addresses.foldLeft(state.store)( (store, address) => store.extend(address, abs.bottom))
        Set(state.copy(store = newStore))
      case a: ActionCreateClosureT[Exp, Abs, Addr] =>
        val closure = sabs.inject[Exp, Addr]((a.λ, a.env.get))
        Set(state.copy(control = ControlKont(closure)))
      case a: ActionDefineAddressesR[Exp, Abs, Addr] =>
        val incompleteValuesSet = frameSavedValues(state.a, state.kstore)
        val valuesSet = addControlKontValue(state, incompleteValuesSet)
        valuesSet.map( (values) => {
          assert(values.length == a.addresses.length, s"Length of ${a.addresses} does not match length of $values")
          val addressValues = a.addresses.reverse.zip(values) //TODO why do we need to reverse the arguments???
          val newStore = addressValues.foldLeft(state.store)( (store, tuple) => store.extend(tuple._1, tuple._2))
          state.copy(store = newStore)
        })
      case a: ActionEvalR[Exp, Abs, Addr] =>
        Set(state.copy(control = ControlEval(a.e, a.env)))
      case ActionEvalPushR(e, env, frame) =>
        val next = NormalKontAddress[Exp, Time](e, state.t)
        val kont = Kont(frame, state.a)
        Set(State(ControlEval(e, env), state.store, state.kstore.extend(next, kont), next, time.tick(state.t)))
      case ActionEvalPushDataR(e, env, frameGenerator) =>
        val next = NormalKontAddress[Exp, Time](e, state.t)
        val currentValue = getControlKontValue(state)
        val frame = frameGenerator(currentValue)
        val kont = Kont(frame, state.a)
        Set(State(ControlEval(e, env), state.store, state.kstore.extend(next, kont), next, time.tick(state.t)))
      case a: ActionLookupAddressR[Exp, Abs, Addr] =>
        val value = state.store.lookup(a.a).get
        Set(state.copy(control = ControlKont(value)))
      case ActionPopKontT() =>
        popKont(state)
      case a: ActionPrimCallT[SchemeExp, Abs, Addr] =>
        val incompleteValuesSet = frameSavedOperands(state.a, state.kstore)
        val valuesSet = addControlKontValue(state, incompleteValuesSet)
        valuesSet.flatMap( (values) => {
          assert(values.length == a.argsExps.length + 1, s"Length of ${a.argsExps} does not match length of $values")
          val operator = values.head
          val operands = values.tail
          val primitives = sabs.getPrimitives[Addr, Abs](operator)
          primitives.flatMap((primitive) => primitive.call(a.fExp, a.argsExps.zip(operands), state.store,
            state.t)
            .collect({
              case (res, store2, effects) =>
                Set(state.copy(control = ControlKont(res), store = store2))
            },
              err =>
                Set(state.copy(control = ControlError(err)))))
        })
      case a: ActionReachedValueT[Exp, Abs, Addr] =>
        Set(state.copy(control = ControlKont(a.v)))
      case a: ActionSetAddressR[Exp, Abs, Addr] =>
        assert(state.control.isInstanceOf[ControlKont])
        val value = state.control.asInstanceOf[ControlKont].v
        Set(state.copy(store = state.store.update(a.adress, value)))
    }

    def subsumes(s1: State, s2: State): Boolean = s1.subsumes(s2)

    def halted(state: State): Boolean = state.halted

    def evaluatedFalse(state: State)(implicit sabs: IsSchemeLattice[Abs]): Boolean = state.control match {
      case ControlKont(v) =>
        sabs.isFalse(v)
      case _ =>
        false
    }

    def evaluatedTrue(state: State)(implicit sabs: IsSchemeLattice[Abs]): Boolean = state.control match {
      case ControlKont(v) =>
        sabs.isTrue(v)
      case _ =>
        false
    }
  }
}
