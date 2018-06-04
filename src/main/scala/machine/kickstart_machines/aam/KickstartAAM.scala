import org.json4s.JObject

import scalaz.Scalaz._

/**
  * A machine state is made of a control component, a value store, a
  * continuation store, and an address representing where the current
  * continuation lives.
  */
case class KickstartAAMState[Exp: Expression, Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](control: Control[Exp, Abs, Addr], store: Store[Addr, Abs], kstore: KontStore[KontAddr], a: KontAddr, t: Time)
  extends StateTrait[Exp, Abs, Addr, Time] {
  override def toString = control.toString

  val sabs: IsSchemeLattice[Abs] = IsSchemeLattice[Abs]

  def isErrorState: Boolean = control match {
    case _: ControlError[Exp, Abs, Addr] => true
    case _ => false
  }
  def isUserErrorState: Boolean = control match {
    case ControlError(UserError(_, _)) => true
    case _ => false
  }

  /**
    * Checks whether a states subsumes another, i.e., if it is "bigger". This
    * is used to perform subsumption checking when exploring the state space,
    * in order to avoid exploring states for which another state that subsumes
    * them has already been explored.
    */
  def subsumes(that: KickstartAAMState[Exp, Abs, Addr, Time]): Boolean =
    control.subsumes(that.control) && store.subsumes(that.store) && a == that.a && kstore.subsumes(that.kstore) && t == that.t

  case class EdgeComponents(state: KickstartAAMState[Exp, Abs, Addr, Time], filters: FilterAnnotations,
                            actions: List[ActionReplay[Exp, Abs, Addr]], effects: Set[Effect[Addr]])

  /**
    * Integrates a set of actions (returned by the semantics, see
    * Semantics.scala), in order to generate a set of states that succeeds this
    * one.
    */
  private def integrate(oldValue: Option[Abs], a: KontAddr, edgeInfos: Set[EdgeInformation[Exp, Abs, Addr]]): Set[EdgeComponents] =
    edgeInfos.map( (edgeInformation) => {
      val actions = edgeInformation.actions
      /* If step applied a primitive, generate a filter for it */
      val primCallFilter = actions.foldLeft[Set[MachineFilterAnnotation]](Set())( (set, actionR) => actionR match {
        case primCall: ActionPrimCallT[Exp, Abs, Addr] =>
          Set(PrimCallMark(primCall.fExp, primCall.fValue, t))
        case _ =>
          Set()
      })
      val filters = FilterAnnotations(primCallFilter, edgeInformation.semanticsFilters)
      edgeInformation.action match {
        /* When a value is reached, we go to a continuation state */
        case ActionReachedValue(v, store, effects) =>
          EdgeComponents(KickstartAAMState(ControlKont(v), store, kstore, a, Timestamp[Time].tick(t)), filters, actions, effects)
        /* When a continuation needs to be pushed, push it in the continuation store */
        case ActionPush(frame, e, env, store, effects) => {
          val next = NormalKontAddress[Exp, Time](e, t)
          val kont = Kont(frame, a)
          EdgeComponents(KickstartAAMState(ControlEval(e, env), store, kstore.extend(next, kont), next, Timestamp[Time].tick(t)),
            filters,// + KontAddrPushed(next),
            actions, effects)
        }
        /* When a value needs to be evaluated, we go to an eval state */
        case ActionEval(e, env, store, effects) =>
          EdgeComponents(KickstartAAMState(ControlEval(e, env), store, kstore, a, Timestamp[Time].tick(t)),
            filters,
            actions, effects)
        /* When a function is stepped in, we also go to an eval state */
        case ActionStepIn(fexp, clo, e, env, store, _, effects) =>
          val closureFilter =  ClosureCallMark[Exp, Abs, Time](fexp, sabs.inject[Exp, Addr]((clo._1, clo._2), None), clo._1, t)
          EdgeComponents(KickstartAAMState(ControlEval(e, env), store, kstore, a, Timestamp[Time].tick(t, fexp)),
            filters + closureFilter,
            actions, effects)
        /* When an error is reached, we go to an error state */
        case ActionError(err) =>
          EdgeComponents(KickstartAAMState(ControlError(err), store, kstore, a, Timestamp[Time].tick(t)),
            filters,
            actions, Set())
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
  def step(sem: ConvertableSemantics[Exp, Abs, Addr, Time]): Set[EdgeComponents] =
    control match {
      /* In a eval state, call the semantic's evaluation method */
      case ControlEval(e, env) =>
        integrate(Some(sabs.inject(false)), a, sem.stepEval(e, env, store, t))
      /* In a continuation state, call the semantics' continuation method */
      case ControlKont(v) =>
        kstore
          .lookup(a)
          .flatMap({
            case Kont(frame, next) =>
              val edgeComponents = integrate(Some(v), next, sem.stepKont(v, frame, store, t))
              edgeComponents.map({ case EdgeComponents(succState, filterAnnotations, actions, effects) =>
                /* If step did not generate any EdgeAnnotation, place a FrameFollowed EdgeAnnotation */
                val replacedFilters = filterAnnotations +
                  KontAddrPopped(a, next) +
                  FrameFollowed[Abs, Addr, Time](frame.asInstanceOf[ConvertableSchemeFrame[Abs, Addr, Time]])
                val popKontAddedAction = addActionPopKontT(actions)
                val timeTickAddedAction = addTimeTickT(popKontAddedAction)
                EdgeComponents(succState, replacedFilters, timeTickAddedAction, effects)
              })
          })
      /* In an error state, the state is not able to make a step */
      case ControlError(_) => Set()
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
object KickstartAAMState {
  def inject[Exp: Expression, Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]) =
    KickstartAAMState(ControlEval(exp, Environment.initial[Addr](env)),
      Store.initial[Addr, Abs](store), KontStore.empty[KontAddr], HaltKontAddress, Timestamp[Time].initial(""))
  import scala.language.implicitConversions

  type Context[Exp, Abs, Addr, Time] = Set[KickstartAAMState[Exp, Abs, Addr, Time]]
  implicit def graphNode[Exp: Expression, Abs: IsSchemeLattice, Addr: Address, Time: Timestamp]: GraphNode[KickstartAAMState[Exp, Abs, Addr, Time], Set[KickstartAAMState[Exp, Abs, Addr, Time]]] = new GraphNode[KickstartAAMState[Exp, Abs, Addr, Time], Set[KickstartAAMState[Exp, Abs, Addr, Time]]] {
    override def label(s: KickstartAAMState[Exp, Abs, Addr, Time]) = s.toString
    override def color(s: KickstartAAMState[Exp, Abs, Addr, Time]) = if (s.halted) { Colors.Yellow } else { s.control match {
      case _: ControlEval[Exp, Abs, Addr] => Colors.Green
      case _: ControlKont[Exp, Abs, Addr] => Colors.Pink
      case _: ControlError[Exp, Abs, Addr] => Colors.Red
    }}
    import org.json4s._
    import org.json4s.JsonDSL._
    import JSON._
    override def content(s: KickstartAAMState[Exp, Abs, Addr, Time]): JObject =
      ("control" -> s.control) ~ ("store" -> s.store) ~ ("kstore" -> s.kstore) ~ ("kont" -> s.a.toString) ~ ("time" -> s.t.toString)
  }
  implicit def graphAnnotation[Exp: Expression, Abs: IsSchemeLattice, Addr: Address, Time: Timestamp]: GraphAnnotation[EdgeAnnotation[Exp, Abs, Addr], Context[Exp, Abs, Addr, Time]] = new GraphAnnotation[EdgeAnnotation[Exp, Abs, Addr], Context[Exp, Abs, Addr, Time]] {
    override def label(e: EdgeAnnotation[Exp, Abs, Addr]): String = {
      if (e.filters.semanticsFilters.contains(ThenBranchFilter)) "t"
      else if (e.filters.semanticsFilters.contains(ElseBranchFilter)) "e"
      else ""
    }
  }
}

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
class KickstartAAM[Exp: Expression, Abs: IsSchemeLattice, Addr: Address, Time: Timestamp]
    extends EvalKontMachine[Exp, Abs, Addr, Time]
    with KickstartEvalEvalKontMachine[Exp, Abs, Addr, Time] {

  val sabs: IsSchemeLattice[Abs] = implicitly[IsSchemeLattice[Abs]]

  type MachineState = KickstartAAMState[Exp, Abs, Addr, Time]
  override type InitialState = MachineState

  def name = "AAM"

  implicit val stateWithKey = new WithKey[KickstartAAMState[Exp, Abs, Addr, Time]] {
    type K = KontAddr
    def key(st: KickstartAAMState[Exp, Abs, Addr, Time]) = st.a
  }

  type G = Option[Graph[KickstartAAMState[Exp, Abs, Addr, Time], EdgeAnnotation[Exp, Abs, Addr], KickstartAAMState.Context[Exp, Abs, Addr, Time]]]
  case class AAMOutput(halted: Set[KickstartAAMState[Exp, Abs, Addr, Time]], numberOfStates: Int, time: Double, errorStates: Set[KickstartAAMState[Exp, Abs, Addr, Time]],
                       graph: Graph[KickstartAAMState[Exp, Abs, Addr, Time], EdgeAnnotation[Exp, Abs, Addr], Set[KickstartAAMState[Exp, Abs, Addr, Time]]], timedOut: Boolean,
                       stepSwitched: Option[Int])
      extends Output with AnalysisOutputGraph[Exp, Abs, Addr, KickstartAAMState[Exp, Abs, Addr, Time]] {

    /**
      * Returns the list of final values that can be reached
      */
    def finalValues: Set[Abs] = {
      val errorStates = halted.filter(_.control match {
        case _: ControlError[Exp, Abs, Addr] => true
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
    def finalStores: Set[Store[Addr, Abs]] = halted.map(st => st.store)

    /**
      * Outputs the graph in a dot file
      */
    def toDotFile(path: String): Unit = AAMGraphPrinter.printGraph(graph, path)
    def toFile(path: String)(output: GraphOutput): Unit = output.toFile(graph, halted)(path)
  }

  /*
   * Checks whether the given (successor) state is a eval-state. If so, adds an EvaluatingExpression
   * annotation to the list of edge annotations.
   */
  def addSuccStateFilter(s: KickstartAAMState[Exp, Abs, Addr, Time], filters: FilterAnnotations): FilterAnnotations = s.control match {
    case ControlEval(exp, _) =>
      filters + EvaluatingExpression(exp)
    case ControlKont(v) =>
      if (filters.machineExists({
        case FrameFollowed(frame) => frame.meaningfullySubsumes
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

  def kickstartEval(initialState: KickstartAAMState[Exp, Abs, Addr, Time], sem: ConvertableSemantics[Exp, Abs, Addr, Time], stopEval: Option[KickstartAAMState[Exp, Abs, Addr, Time] => Boolean],
    timeout: Timeout, stepSwitched: Option[Int]): AAMOutput = {
    def loop(counter: Int, todo: Set[KickstartAAMState[Exp, Abs, Addr, Time]], visited: Set[KickstartAAMState[Exp, Abs, Addr, Time]], halted: Set[KickstartAAMState[Exp, Abs, Addr, Time]], startingTime: Long,
      graph: Graph[KickstartAAMState[Exp, Abs, Addr, Time], EdgeAnnotation[Exp, Abs, Addr], Set[KickstartAAMState[Exp, Abs, Addr, Time]]]): AAMOutput = {
      if (timeout.reached) {
        AAMOutput(halted, visited.size, (System.nanoTime - startingTime) / Math.pow(10, 9),
          halted.filter(_.isErrorState), graph, true, stepSwitched)
      } else {
        todo.headOption match {
          case Some(s) =>
            if (visited.contains(s)) {
              /* If we already visited the state, we ignore it. */
              loop(counter + 1, todo.tail, visited, halted, startingTime, graph)
            } else if (GlobalFlags.AAM_CHECK_SUBSUMES && visited.exists(s2 => s2.subsumes(s))) {
              /* If the state is subsumed by another already visited state,
               * we ignore it. The subsumption part reduces the number of visited
               * states but leads to non-determinism due to the non-determinism
               * of Scala's headOption (it seems so at least).
               * We do have to add an edge from the current state to the subsumed state. */
              loop(counter + 1, todo.tail, visited, halted, startingTime, visited.foldLeft[Graph[KickstartAAMState[Exp, Abs, Addr, Time], EdgeAnnotation[Exp, Abs, Addr], Set[KickstartAAMState[Exp, Abs, Addr, Time]]]]
                (graph)({
                case (graph, s2) =>
                  if (s2.subsumes(s)) {
                    val subsumptionFilter = StateSubsumed
                    graph.addEdge(s, EdgeAnnotation.subsumptionEdge, s2)
                  }
                  else
                    graph
              }))
            } else if (s.halted || stopEval.fold(false)(pred => pred(s))) {
              /* If the state is a final state or the stopEval predicate determines the machine can stop exploring
               * this state, add it to the list of final states and continue exploring the graph */
              loop(counter + 1, todo.tail, visited + s, halted + s, startingTime, graph)
            } else {
              /* Otherwise, compute the successors (and edges to these successors) of this state,
              update the graph, and push the new successors on the todo list */
              val succsEdges = s.step(sem)
              val newGraph = graph.addEdges(succsEdges.map(s2 => {
                val filters = addSuccStateFilter(s2.state, s2.filters)
                (s, EdgeAnnotation(filters, s2.actions, s2.effects), s2.state)
              }))
              loop(counter + 1, todo.tail ++ succsEdges.map(_.state), visited + s, halted, startingTime, newGraph)
            }
          case None =>
            AAMOutput(halted, visited.size, (System.nanoTime - startingTime) / Math.pow(10, 9),
                      halted.filter(_.isErrorState), graph, false, stepSwitched)
        }
      }
    }
    val startingTime = System.nanoTime
    loop(1, Set(initialState), Set(), Set(), startingTime, Graph.empty[KickstartAAMState[Exp, Abs, Addr, Time], EdgeAnnotation[Exp, Abs, Addr], Set[KickstartAAMState[Exp, Abs, Addr, Time]]].addNode(initialState))
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output = ???

  /**
    * Performs the evaluation of an expression, possibly writing the output graph
    * in a file, and returns the set of final states reached
    */
  def eval(exp: Exp, sem: ConvertableSemantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output =
    kickstartEval(KickstartAAMState.inject(exp, sem.initialEnv, sem.initialStore), sem, None, timeout, None)

  object AAMGraphPrinter extends GraphPrinter[Graph[KickstartAAMState[Exp, Abs, Addr, Time], EdgeAnnotation[Exp, Abs, Addr], Set[KickstartAAMState[Exp, Abs, Addr, Time]]]] {

    def printGraph(graph: Graph[KickstartAAMState[Exp, Abs, Addr, Time], EdgeAnnotation[Exp, Abs, Addr], Set[KickstartAAMState[Exp, Abs, Addr, Time]]],
                   path: String): Unit = {
      GraphDOTOutput.toFile(graph, Set[KickstartAAMState[Exp, Abs, Addr, Time]]())(path)
    }
  }

  object ActionReplayApplier extends ActionReplayApplier[Exp, Abs, Addr, Time, KickstartAAMState[Exp, Abs, Addr, Time]] {

    protected def noEdgeFilters(state: KickstartAAMState[Exp, Abs, Addr, Time]): (KickstartAAMState[Exp, Abs, Addr, Time], Set[MachineFilterAnnotation]) = (state, Set())
    protected def addEvaluateExp(exp: Exp): EvaluatingExpression[Exp] = EvaluatingExpression(exp)
    protected def addFrameFollowed(frame: Frame): FrameFollowed[Abs, Addr, Time] =
      FrameFollowed[Abs, Addr, Time](frame.asInstanceOf[ConvertableSchemeFrame[Abs, Addr, Time]])
    protected def addKontAddrPopped(a: KontAddr, next: KontAddr): KontAddrPopped = KontAddrPopped(a, next)

    private def frameSavedOperands(state: KickstartAAMState[Exp, Abs, Addr, Time]): Set[(List[Abs], Kont[KontAddr])] =
      state.kstore.lookup(state.a).map( (kont) => kont.frame match {
        case frame: FrameFuncallOperands[Abs, Addr, Time] =>
          val savedOperands = frame.args.map(_._2)
          val allOperands = savedOperands :+ assertedGetControlKontValue(state)
          val allValues = frame.f :: allOperands
          (allValues, kont)
        case frame =>
          throw new Exception(s"Retrieving operands of non-FrameFunCallOperands $frame")
      })

    private def frameSavedValues(a: KontAddr, kstore: KontStore[KontAddr]): Set[(List[Abs], Kont[KontAddr])] =
      kstore.lookup(a).map( (kont) => kont.frame match {
        case frame: FrameFuncallOperands[Abs, Addr, Time] =>
          (frame.args.map(_._2), kont)
        case frame: FrameLet[Abs, Addr, Time] =>
          (frame.bindings.map(_._2), kont)
        case _ =>
          (List(), kont)
      })

    protected def popKont(state: KickstartAAMState[Exp, Abs, Addr, Time]): Set[(KickstartAAMState[Exp, Abs, Addr, Time], Set[MachineFilterAnnotation], Frame)] = {
      val konts = state.kstore.lookup(state.a)
      konts.map( (kont: Kont[KontAddr]) => {
        val frame = kont.frame
        val next = kont.next
        val kontPopped = addKontAddrPopped(state.a, next)
        val frameFollowed = addFrameFollowed(frame)
        (state.copy(a = next), Set[MachineFilterAnnotation](kontPopped, frameFollowed), frame)
      })
    }

    /**
      * Checks whether the control component of the given state is a ControlKont and if so, returns the value
      * stored in this control component, wrapped as an Option. Returns None if the control component is not
      * a ControlKont.
      * @param state
      * @return
      */
    protected def getControlKontValue(state: KickstartAAMState[Exp, Abs, Addr, Time]): Option[Abs] = state.control match {
      case ControlKont(v) =>
        Some(v)
      case _ =>
        None
    }

    /**
      * Checks whether the control component of the given state is a ControlError and if so, returns the error
      * stored in this control component, wrapped in an Option. Returns None if the control component is not
      * a ControlKont.
      * @param state
      * @return
      */
    protected def getSemanticError(state: KickstartAAMState[Exp, Abs, Addr, Time]): Option[SemanticError] = state.control match {
      case ControlError(err) =>
        Some(err)
      case _ =>
        None
    }

    /**
      * Checks whether the control component of the given state is a ControlKont and if so, returns the value
      * stored in this control component. Throws an exception if the control is not a ControlKont.
      * To be used when the control component must definitely be a ControlKont.
      * @param state
      * @return
      */
    protected def assertedGetControlKontValue(state: KickstartAAMState[Exp, Abs, Addr, Time]): Abs = getControlKontValue(state) match {
      case Some(v) =>
        v
      case None =>
        throw new Exception(s"Expected control to be a ControlKont, got a ${state.control} instead")
    }

    protected def addControlKontValue(state: KickstartAAMState[Exp, Abs, Addr, Time], values: List[Abs]): List[Abs] = {
      val extraValue = assertedGetControlKontValue(state)
      extraValue :: values
    }

    protected def addControlKontValue(state: KickstartAAMState[Exp, Abs, Addr, Time], valuesSet: Set[List[Abs]]): Set[List[Abs]] = {
      val extraValue = assertedGetControlKontValue(state)
      valuesSet.map(extraValue :: _)
    }

    protected def addKontFilterAnnotations(currentAddr: KontAddr,
                                           kont: Kont[KontAddr]): Set[MachineFilterAnnotation] = {
      val frame = kont.frame
      val next = kont.next
      val kontPopped = addKontAddrPopped(currentAddr, next)
      val frameFollowed = addFrameFollowed(frame)
      Set(kontPopped, frameFollowed)
    }

    protected def defineAddresses(state: KickstartAAMState[Exp, Abs, Addr, Time], addresses: List[Addr]): Set[(KickstartAAMState[Exp, Abs, Addr, Time], Kont[KontAddr])] = {
      val incompleteValuesKontsSet = frameSavedValues(state.a, state.kstore)
      incompleteValuesKontsSet.map({ case (values, kont) =>
        /*
         * The value saved in the ControlKont only needs to be added if there are addresses to define now.
         * If there are no addresses, e.g., because a function without any formal parameters is being called,
         * the value saved in the ControlKont should not be used.
         */
        val completeValues = if (addresses.isEmpty) {
          values
        } else {
          addControlKontValue(state, values)
        }
        assert(completeValues.length == addresses.length, s"Length of $addresses does not match length of $completeValues")
        val addressValues = addresses.zip(completeValues) //TODO why do we need to reverse the arguments???
        val newStore = addressValues.foldLeft(state.store)( (store, tuple) => store.extend(tuple._1, tuple._2) )
        (state.copy(store = newStore), kont)
      })
    }

    def applyActionReplay(state: KickstartAAMState[Exp, Abs, Addr, Time], action: ActionReplay[Exp, Abs, Addr]): Set[(KickstartAAMState[Exp, Abs, Addr, Time], Set[MachineFilterAnnotation])] = action match {
      case a: ActionAllocAddressesR[Exp, Abs, Addr] =>
        val newStore = a.addresses.foldLeft(state.store)( (store, address) => store.extend(address, JoinLattice[Abs].bottom))
        Set(noEdgeFilters(state.copy(store = newStore)))
      case ActionClosureCallR(fExp, lam, env) =>
        val closure = sabs.inject((lam, env), None)
        val filter = ClosureCallMark(fExp, closure, lam, state.t)
        Set((state, Set[MachineFilterAnnotation](filter)))
      case a: ActionCreateClosureT[Exp, Abs, Addr] =>
        val closure = sabs.inject[Exp, Addr]((a.λ, a.env.get), None)
        Set(noEdgeFilters(state.copy(control = ControlKont(closure))))
      case a: ActionDefineAddressesPopR[Exp, Abs, Addr] =>
        val statesKonts = defineAddresses(state, a.addresses)
        statesKonts.map({ case (state, kont) =>
          val filterEdge = addKontFilterAnnotations(state.a, kont)
          (state.copy(a = kont.next), filterEdge)
        })
      case a: ActionDefineAddressesR[Exp, Abs, Addr] =>
        val statesKonts = defineAddresses(state, a.addresses)
        statesKonts.map( (stateKont) => noEdgeFilters(stateKont._1))
      case ActionErrorT(err) =>
        Set(noEdgeFilters(state.copy(control = ControlError(err))))
      case a: ActionEvalR[Exp, Abs, Addr] =>
        val evaluatingExp = addEvaluateExp(a.e)
        Set((state.copy(control = ControlEval(a.e, a.env)), Set[MachineFilterAnnotation](evaluatingExp)))
      case ActionEvalPushR(e, env, frame) =>
        val next = NormalKontAddress[Exp, Time](e, state.t)
        val kont = Kont(frame, state.a)
        val evaluatingExp = addEvaluateExp(e)
        val newState = state.copy(control = ControlEval(e, env), kstore = state.kstore.extend(next, kont), a = next)
        Set((newState, Set[MachineFilterAnnotation](evaluatingExp))) //, pushedAddr)))
      case ActionEvalPushDataR(e, env, frameGenerator) =>
        val kontPoppedTriples = popKont(state)
        kontPoppedTriples.flatMap({
          case (state, machineFilters, framePopped) =>
            val next = NormalKontAddress[Exp, Time](e, state.t)
            val currentValue = assertedGetControlKontValue(state)
            val frame = frameGenerator(currentValue, None, framePopped)
            val kont = Kont(frame, state.a)
            val evaluatingExp = addEvaluateExp(e)
            val newState = state.copy(control = ControlEval(e, env), kstore = state.kstore.extend(next, kont), a = next)
            Set((newState, machineFilters + evaluatingExp)) //, pushedAddr)))
        })
      case a: ActionLookupAddressR[Exp, Abs, Addr] =>
        val value = state.store.lookup(a.a) match {
          case Some(value) =>
            value
          case None =>
            throw new Exception(s"Address ${a.a} not found in store")
        }
        Set(noEdgeFilters(state.copy(control = ControlKont(value))))
      case ActionPopKontT() =>
        popKont(state).map( (triple) => (triple._1, triple._2) )
      case a: ActionPrimCallT[SchemeExp, Abs, Addr] =>
        val savedOperandsKontsSet: Set[(List[Abs], Kont[KontAddr])] = frameSavedOperands(state)
        //val valuesSet = addControlKontValue(state, savedOperandsKontsSet)
        savedOperandsKontsSet.flatMap({ case (values, kont) =>
          assert(values.length == a.argsExps.length + 1, s"Length of ${a.argsExps} does not match length of $values")
          val operator = values.head
          val operands = values.tail :+ assertedGetControlKontValue(state)
          val primitives = sabs.getPrimitives[Addr, Abs](operator)
          val filterEdge = addKontFilterAnnotations(state.a, kont)
          primitives.flatMap( (primitive) => {
            val result = primitive.call(a.fExp, a.argsExps.zip(operands.map((_, None))), state.store, state.t)._1
            result.collect({
              case (res, store2, effects) =>
                val newState = state.copy(control = ControlKont(res), store = store2, a = kont.next)
                Set[(KickstartAAMState[Exp, Abs, Addr, Time], Set[MachineFilterAnnotation])]((newState, filterEdge + PrimCallMark(a.fExp, sabs.inject(primitive), state.t)))
            },
              err => {
                val newState = state.copy(control = ControlError(err), a = kont.next)
                Set((newState, filterEdge + PrimCallMark(a.fExp, sabs.inject(primitive), state.t)))
              })
          })
        })
      case a: ActionReachedValueT[Exp, Abs, Addr] =>
        val storeChanges = a.storeChanges
        val newStore = storeChanges.foldLeft[Store[Addr, Abs]](state.store)( (newStore, storeChange) => storeChange match {
          case storeChange: StoreExtendSemantics[Abs, Addr] =>
            newStore.extend(storeChange.a, storeChange.value)
          case StoreUpdateSemantics(a, value) =>
            newStore.update(a, value)
        })
        Set(noEdgeFilters(state.copy(control = ControlKont(a.v), store = newStore)))
      case a: ActionSetAddressR[Exp, Abs, Addr] =>
        assert(state.control.isInstanceOf[ControlKont[Exp, Abs, Addr]])
        val value = state.control.asInstanceOf[ControlKont[Exp, Abs, Addr]].v
        Set(noEdgeFilters(state.copy(store = state.store.update(a.adress, value))))
      case ActionTimeTickR() =>
        Set(noEdgeFilters(state.copy(t = Timestamp[Time].tick(state.t))))
      case ActionTimeTickExpR(fexp) =>
        Set(noEdgeFilters(state.copy(t = Timestamp[Time].tick(state.t, fexp))))
    }

    def subsumes(s1: KickstartAAMState[Exp, Abs, Addr, Time], s2: KickstartAAMState[Exp, Abs, Addr, Time]): Boolean =
      s1.subsumes(s2)

    def statesEqual(s1: KickstartAAMState[Exp, Abs, Addr, Time], s2: KickstartAAMState[Exp, Abs, Addr, Time]): Boolean = {
      val control = s1.control == s2.control
      val a = s1.a == s2.a
      val t = s1.t == s2.t
      val kstore = s1.kstore == s2.kstore
      val store = s1.store.toSet == s2.store.toSet
      control && a && t && kstore && store
    }

    def evaluatedFalse(state: KickstartAAMState[Exp, Abs, Addr, Time]): Boolean = state.control match {
      case ControlKont(v) =>
        sabs.isFalse(v)
      case _ =>
        false
    }

    def evaluatedTrue(state: KickstartAAMState[Exp, Abs, Addr, Time]): Boolean = state.control match {
      case ControlKont(v) =>
        sabs.isTrue(v)
      case _ =>
        false
    }

    def getTopKonts(state: KickstartAAMState[Exp, Abs, Addr, Time]): Set[Kont[KontAddr]] =
      state.kstore.lookup(state.a)

    def getKonts(state: KickstartAAMState[Exp, Abs, Addr, Time], ka: KontAddr): Set[Kont[KontAddr]] = {
      state.kstore.lookup(ka)
    }

    def addKonts(state: KickstartAAMState[Exp, Abs, Addr, Time], ka: KontAddr, konts: Set[Kont[KontAddr]]): KickstartAAMState[Exp, Abs, Addr, Time] = {
      val newKStore = konts.foldLeft(state.kstore)( (newKStore, kont) => newKStore.extend(ka, kont) )
      state.copy(kstore = newKStore)
    }

    def removeKonts(state: KickstartAAMState[Exp, Abs, Addr, Time], ka: KontAddr): KickstartAAMState[Exp, Abs, Addr, Time] = {
      val newKStore = state.kstore.remove(ka)
      state.copy(kstore = newKStore)
    }

    def joinStates(states: Set[KickstartAAMState[Exp, Abs, Addr, Time]]): JoinedInfo = {
      val initialInfo = JoinedInfo(JoinLattice[Abs].bottom, Store.empty, KontStore.empty, Set())
      states.foldLeft(initialInfo)( (joinedInfo, state) => {
        val stateInfo = getControlKontValue(state) match {
          case Some(v) =>
            JoinedInfo(v, state.store, state.kstore, Set())
          case None =>
            getSemanticError(state) match {
              case Some(err) =>
                JoinedInfo(JoinLattice[Abs].bottom, state.store, state.kstore, Set(err))
              case None =>
                JoinedInfo(JoinLattice[Abs].bottom, state.store, state.kstore, Set())
            }
        }
        joinedInfo.join(stateInfo)
      })
    }

  }

  object AAMStateInfoProvider extends StateInfoProvider[Exp, Abs, Addr, Time, KickstartAAMState[Exp, Abs, Addr, Time]] {

    type State = KickstartAAMState[Exp, Abs, Addr, Time]

    def evalExp(state: KickstartAAMState[Exp, Abs, Addr, Time]): Option[Exp] = state.control match {
      case ControlEval(exp, _) => Some(exp)
      case _ => None
    }

    def valueReached(state: KickstartAAMState[Exp, Abs, Addr, Time]): Option[Abs] = state.control match {
      case ControlKont(value) =>
        Some(value)
      case _ =>
        None
    }

    val kaConverter = new ConvertTimestampKontAddrConverter[Exp](DefaultHybridTimestampConverter)

    def halted(state: KickstartAAMState[Exp, Abs, Addr, Time]): Boolean =
      state.halted
    def store(state: KickstartAAMState[Exp, Abs, Addr, Time]): Store[Addr, Abs] =
      state.store
    def subsumes(state1: KickstartAAMState[Exp, Abs, Addr, Time], state2: KickstartAAMState[Exp, Abs, Addr, Time]): Boolean = {
      state1.subsumes(state2)
    }

    def deltaStoreEmpty(state1: KickstartAAMState[Exp, Abs, Addr, Time], state2: KickstartAAMState[Exp, Abs, Addr, Time]): Boolean = {
      val storeDiff1 = state1.store.diff(state2.store)
      val storeDiff2 = state2.store.diff(state1.store)
      storeDiff1.keys.isEmpty && storeDiff2.keys.isEmpty
    }

    /**
      * Expects that ka1 is the address using the concrete timestamps
      * @param ka1
      * @param ka2
      * @return
      */
    private def isSameKontAddressModuloTimestamp(ka1: KontAddr, ka2: KontAddr): Boolean = (ka1, ka2) match {
      case (NormalKontAddress(e1, t1), NormalKontAddress(e2, t2)) =>
        if (e1 == e2) {
          (t1, t2) match {
            case (HybridTimestamp.ConcreteTime(_, a1), HybridTimestamp.AbstractTime(a2)) =>
              a1 == a2
            case _ =>
              false
          }
        } else {
          false
        }
      case _ =>
        /* The addresses might actually be the same here (e.g., two HaltKontAddresses), but we don't care. */
        false
    }

    /**
      * Expects that state1 is the state using the concrete timestamps.
      * @param state1
      * @param state2
      * @return
      */
    def deltaKStore(state1: KickstartAAMState[Exp, Abs, Addr, Time], state2: KickstartAAMState[Exp, Abs, Addr, Time]): Option[Iterable[(KontAddr, KontAddr)]] = {
      val kstoreDiff1 = state1.kstore.diff(state2.kstore)
      val kstoreDiff2 = state2.kstore.diff(state1.kstore)
      val castedKstoreDiff1 = kstoreDiff1.asInstanceOf[BasicKontStore[KontAddr]]
      if (state1.store == state2.store && state1.a == state2.a &&
          state1.control == state2.control && state1.t == state2.t) {
        val kstoresEquivalent = kstoreDiff1.forall({
          case (ka1, konts1) =>
            val convertedKa1 = kaConverter.convertKontAddr(ka1)
            val konts2 = kstoreDiff2.lookup(convertedKa1)
            konts1 == konts2
        })
        if (kstoresEquivalent) {
          Some(castedKstoreDiff1.content.keys.map( (ka: KontAddr) => (ka, kaConverter.convertKontAddr(ka)) ))
        } else {
          None
        }
      } else {
        None
      }
    }

  }
}