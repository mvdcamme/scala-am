import aam_global_store._

case class KickstartAAMGlobalStoreState[Exp: Expression, Abs: IsSchemeLattice, Addr: Address, Time : Timestamp](control: Control[Exp, Abs, Addr], a: KontAddr, t: Time) extends StateTrait[Exp, Abs, Addr, Time] {

  type G = aam_global_store.G[Exp, Abs, Addr, Time]
  type Tup = aam_global_store.Tup[Exp, Abs, Addr, Time]

  override def toString = control.toString
  def subsumes(that: KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]): Boolean = control.subsumes(that.control) && a == that.a && t == that.t

  def isErrorState: Boolean = control match {
    case _: ControlError[Exp, Abs, Addr] => true
    case _ => false
  }
  def isUserErrorState: Boolean = control match {
    case ControlError(UserError(_, _)) => true
    case _ => false
  }

  import scala.language.implicitConversions
  implicit private def semFiltersToFilterAnnots(filters: Set[SemanticsFilterAnnotation]): FilterAnnotations[Exp, Abs, Addr] = {
    FilterAnnotations[Exp, Abs, Addr](Set(), filters)
  }
  implicit private def semFtrsToEdgeAnnot(filters: Set[SemanticsFilterAnnotation]): EdgeAnnotation[Exp, Abs, Addr] = {
    EdgeAnnotation(filters, Nil)
  }

  private def integrate(a: KontAddr, edgeInfos: Set[EdgeInformation[Exp, Abs, Addr]], store: GlobalStore[Addr, Abs], kstore: KontStore[KontAddr]): (Set[Tup], GlobalStore[Addr, Abs], KontStore[KontAddr]) =
    edgeInfos.foldLeft((Set[Tup](), store, kstore))((acc, edgeInfo) => {
      val edgeAnnot: EdgeAnnotation[Exp, Abs, Addr] = edgeInfo.semanticsFilters
      implicit def stateToTup(state: KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]): Tup = (edgeAnnot, state)

      edgeInfo.action match {
        case ActionReachedValue(v, store2, _) => (acc._1 + KickstartAAMGlobalStoreState(ControlKont(v), a, Timestamp[Time].tick(t)), acc._2.includeDelta(store2.delta), acc._3)
        case ActionPush(frame, e, env, store2, _) =>
          val next = NormalKontAddress[Exp, Time](e, t)
          (acc._1 + KickstartAAMGlobalStoreState(ControlEval(e, env), next, Timestamp[Time].tick(t)), acc._2.includeDelta(store2.delta), acc._3.extend(next, Kont(frame, a)))
        case ActionEval(e, env, store2, _) =>
          (acc._1 + KickstartAAMGlobalStoreState(ControlEval(e, env), a, Timestamp[Time].tick(t)), acc._2.includeDelta(store2.delta), acc._3)
        case ActionStepIn(fexp, _, e, env, store2, _, _) => //TODO Shouldn't Timestamp.tick include fexp here???
          (acc._1 + KickstartAAMGlobalStoreState(ControlEval(e, env), a, Timestamp[Time].tick(t, fexp)), acc._2.includeDelta(store2.delta), acc._3)
        case ActionError(err) =>
          (acc._1 + KickstartAAMGlobalStoreState(ControlError(err), a, Timestamp[Time].tick(t)), acc._2, acc._3)
      }
    })

  /**
    * Computes the set of states that follow the current state
    */
  def step(sem: ConvertableSemantics[Exp, Abs, Addr, Time], store: GlobalStore[Addr, Abs], kstore: KontStore[KontAddr]): (Set[Tup], GlobalStore[Addr, Abs], KontStore[KontAddr]) = control match {
    /* In a eval state, call the semantic's evaluation method */
    case ControlEval(e, env) => integrate(a, sem.stepEval(e, env, store.store, t), store, kstore)
    /* In a continuation state, call the semantics' continuation method */
    case ControlKont(v) => kstore.lookup(a).foldLeft((Set[Tup](), store, kstore))((acc, kont) => kont match {
      case Kont(frame, next) => integrate(next, sem.stepKont(v, frame, store.store, t), acc._2, acc._3) match {
        case (states, store2, kstore2) => (acc._1 ++ states, store2, kstore2)
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
object KickstartAAMGlobalStoreState {
  def inject[Exp: Expression, Abs: IsSchemeLattice, Addr: Address, Time: Timestamp](exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]): FullState[Exp, Abs, Addr, Time] =
    (KickstartAAMGlobalStoreState(ControlEval[Exp, Abs, Addr](exp, Environment.initial[Addr](env)), HaltKontAddress, Timestamp[Time].initial("")),
      GlobalStore(DeltaStore[Addr, Abs](store.toMap, Map()), Map()),
      TimestampedKontStore[KontAddr](Map(), 0))

  type Context[Exp, Abs, Addr, Time] = Set[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]]
  implicit def graphNode[Exp: Expression, Abs: IsSchemeLattice, Addr: Address, Time: Timestamp] = new GraphNode[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time], Context[Exp, Abs, Addr, Time]] {
    override def label(s: KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]) = s.toString.take(40)
    override def color(s: KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]) = if (s.halted) { Colors.Yellow } else { s.control match {
      case _: ControlEval[Exp, Abs, Addr] => Colors.Green
      case _: ControlKont[Exp, Abs, Addr] => Colors.Pink
      case _: ControlError[Exp, Abs, Addr] => Colors.Red
    }}
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
  * AAM/AAC/P4F techniques combined in a single machine abstraction
  */
class KickstartAAMGlobalStore[Exp: Expression, Abs: IsSchemeLattice, Addr: Address, Time: Timestamp]
    extends EvalKontMachine[Exp, Abs, Addr, Time] with KickstartEvalEvalKontMachine[Exp, Abs, Addr, Time] {

  type MachineState = KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]
  override type InitialState = FullState[Exp, Abs, Addr, Time]

  def name = "AAMGlobalStore"
  case class AAMOutput(halted: Set[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]], finalStore: Store[Addr, Abs], numberOfStates: Int, time: Double,
                       errorStates: Set[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]], graph: G[Exp, Abs, Addr, Time], timedOut: Boolean, stepSwitched: Option[Int])
      extends Output with AnalysisOutputGraph[Exp, Abs, Addr, KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]] {

    override def toString = s"AAMOutput($numberOfStates states, ${time}s, $stepSwitched)"

    def replaceGraph(newGraph: Graph[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time], EdgeAnnotation[Exp, Abs, Addr], Set[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]]]): AnalysisOutputGraph[Exp, Abs, Addr, KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]] = this.copy(graph = newGraph)

    def finalValues = {
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

    def finalStores: Set[Store[Addr, Abs]] = Set(finalStore)
    def toFile(path: String)(output: GraphOutput): Unit = output.toFile(graph, halted)(path)
  }

  def kickstartEval(initialState: InitialState, sem: ConvertableSemantics[Exp, Abs, Addr, Time], stopEval: Option[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time] => Boolean],
                    timeout: Timeout, stepSwitched: Option[Int]): AAMOutput = {
    import scala.language.higherKinds
    @scala.annotation.tailrec
    def loop[VS[_]: VisitedSet](todo: Set[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]], visited: VS[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]], store: GlobalStore[Addr, Abs], kstore: KontStore[KontAddr],
      halted: Set[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]], graph: G[Exp, Abs, Addr, Time], reallyVisited: Set[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]]): AAMOutput = {
      if (todo.isEmpty || timeout.reached) {
        AAMOutput(halted, store.commit.store, reallyVisited.size, timeout.time, halted.filter(_.isErrorState),
                  graph, timeout.reached, stepSwitched)
      } else {
        val (edges, store2, kstore2) = todo.foldLeft(Set[(KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time], EdgeAnnotation[Exp, Abs, Addr], KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time])](), store, kstore)((acc, state) =>
          state.step(sem, acc._2, acc._3) match {
            case (next, store2, kstore2) =>
              (acc._1 ++ next.map(annotState => (state, annotState._1, annotState._2)), store2, kstore2)
          })
        if (store2.isUnchanged && kstore.fastEq(kstore2)) {
          //assert(store2.commit.store == store2.store)
          loop(edges.map({ case (s1, e, s2) => s2 }).filter(s2 => !VisitedSet[VS].contains(visited, s2)),
            VisitedSet[VS].append(visited, todo),
            store2, kstore2,
            halted ++ todo.filter(_.halted),
               graph.addEdges(edges.map({ case (s1, e, s2) => (s1, e, s2) })),
            reallyVisited ++ todo)
        } else {
          //assert(!(!store2.isUnchanged && store2.commit.store == store2.store))
          loop(edges.map({ case (s1, e, s2) => s2 }),
            VisitedSet[VS].empty[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time]],
            store2.commit, kstore2,
            halted ++ todo.filter(_.halted),
            graph.addEdges(edges.map({ case (s1, e, s2) => (s1, e, s2) })),
            reallyVisited ++ todo)
        }
      }
    }
    val (state, store, kstore) = initialState
    loop(Set(state),
      VisitedSet.MapVisitedSet.empty,
      store, kstore, Set(),
      Graph.empty[KickstartAAMGlobalStoreState[Exp, Abs, Addr, Time], EdgeAnnotation[Exp, Abs, Addr], KickstartAAMGlobalStoreState.Context[Exp, Abs, Addr, Time]].addNode(state), Set())
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output = ???

  /**
    * Performs the evaluation of an expression, possibly writing the output graph
    * in a file, and returns the set of final states reached
    */
  def eval(exp: Exp, sem: ConvertableSemantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output = {
    val initialState: InitialState = KickstartAAMGlobalStoreState.inject[Exp, Abs, Addr, Time](exp, sem.initialEnv, sem.initialStore)
    kickstartEval(initialState, sem, None, timeout, None)
  }
}
