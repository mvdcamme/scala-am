import java.io.{File, BufferedWriter, FileWriter}

class PropagateRunTimeInfo[Exp: Expression,
                           Abs: IsSchemeLattice,
                           Addr: Address,
                           Time: Timestamp,
                           State <: StateTrait[Exp, Abs, Addr, Time] : Descriptor]
                          (graphPrinter: GraphPrinter[Graph[State, EdgeAnnotation[Exp, Abs, Addr]]])
                          (implicit actionRApplier: ActionReplayApplier[Exp, Abs, Addr, Time, State],
                                    stateInfoProvider: StateInfoProvider[State],
                                    analysisFlags: AnalysisFlags) {

  val usesGraph = new UsesGraph[Exp, Abs, Addr, State]
  import usesGraph._

  val filterEdgeFilterAnnotations = new FilterEdgeFilterAnnotations[Exp, Abs, Addr, Time, State]
  val filterPropagationEdges = new FilterPropagationEdges[Exp, Abs, Addr, Time, State]

  val LogPropagation = Logger.D

  type ActionEdge = List[ActionReplay[Exp, Abs, Addr]]

  /*
   * originalState: The original state that was present in the initial abstract graph.
   * newState: the new state that was computed by following the ActionTs
   */
  case class StateCombo(originalState: State, newState: State)

  case class StepEval(nonSubsumptionStateCombos: Set[(EdgeAnnotation2, StateCombo)],
                      subsumptionStateCombos: Set[StateCombo],
                      newVisited: Set[State])

  private def isSubsumptionEdge(edge: Edge): Boolean = {
    edge._1.filters.isSubsumptionAnnotation
  }

  /*
   * Recursively follow all StateSubsumed edges.
   *
   * When encountering an edge annotation with StateSubsumed, this edge should automatically
   * be followed, as this implies that the current concrete state can match both states, and as
   * the first state (the state from which the edge originates) will be a dead end, matching of
   * the concrete state should proceed with the state that subsumes the 'dead' state.
   */
  def followStateSubsumedEdges(node: State, prunedGraph: AbstractGraph): Set[State] =
    if (prunedGraph.nodeEdges(node).isEmpty) {
      Set(node)
    } else {
      val edges = prunedGraph.nodeEdges(node)
      edges.headOption match {
        case None =>
          Set()
        case Some(edge) if edge._1.filters.isSubsumptionAnnotation =>
          /*
           * Make sure subsumption edge does not point to itself.
           */
          assert(edge._2 != node)
          val filterSubsumptionEdges = filterEdgeFilterAnnotations.findMinimallySubsumingEdges(edges)
          filterSubsumptionEdges.flatMap( (edge) => {
            followStateSubsumedEdges(edge._2, prunedGraph)
          })
        case Some(edge) =>
          Set(node)
      }
    }

  protected def stepEval(newState: State,
                         visited: Set[State],
                         mapping: Map[State, Set[State]],
                         graph: AbstractGraph,
                         prunedGraph: AbstractGraph): StepEval = {
    val originalStates = mapping(newState)
    val afterSubsumptionStates: Set[State] = originalStates.flatMap(followStateSubsumedEdges(_, prunedGraph))

    Logger.log(s"stepEval of ${originalStates.map(PrintState.stateToString[State](_, prunedGraph))}", Logger.D)

    /* TODO Only used for debugging */
    val originalStatePrunedIds = originalStates.map(prunedGraph.nodeId)
    /*
     * All outgoing edges in abstract graph
     * If originalState does not have any outgoing edges, return empty set
     */
    val edges: Set[Edge] = originalStates.flatMap(prunedGraph.edges.getOrElse(_, Set()))
    val afterSubsumptionEdges: Set[Edge] = afterSubsumptionStates.flatMap(prunedGraph.edges.getOrElse(_, Set()))
    val storeFilteredEdges = filterPropagationEdges.filterWithStore(newState, afterSubsumptionEdges)
    val kstoreFilteredEdges = filterPropagationEdges.filterWithKStore(newState, storeFilteredEdges)

    val (subsumptionEdges, nonSubsumptionEdges) = kstoreFilteredEdges.partition(isSubsumptionEdge)

    /* The combination of action-edges from all edges. */
    val mergedActionEdges: Set[ActionEdge] = nonSubsumptionEdges.map(_._1.actions)

    Logger.log(s"Using edges $edges", LogPropagation)
    Logger.log(s"Using afterSubsumptionEdges $afterSubsumptionEdges", LogPropagation)
    Logger.log(s"Using store filteredEdges $storeFilteredEdges", LogPropagation)
    Logger.log(s"Using kontstore filteredEdges $kstoreFilteredEdges", LogPropagation)
    Logger.log(s"Using mergedActionEdges $mergedActionEdges", LogPropagation)

    /*
     * Used for following subsumption edges: these edges don't contain any actions, so the newState won't change
     * by following the subsumption edge, but the newState must still be saved, as it must be used as input or the
     * actions stored in the edges following this subsumption edge.
     *
     * TODO can be removed now, after adding the rule for following subsumption edges.
     */
//    val usedSubsumptionEdges = subsumptionEdges.nonEmpty

    /*
     * Compute the set of new states via the set of merged action edges, and, while applying the action edges,
     * collect the filter edge for each of the applied action edges.
     */
    val results: Set[(State, Set[MachineFilterAnnotation])] = mergedActionEdges.flatMap((actionEdge) =>
      /*
       * For each action edge, take all actionRs a1, a2 ... ak, and apply them consecutively on newState.
       * Each actionR may produce a set of new newStates.
       */
      actionEdge.foldLeft[Set[(State, Set[MachineFilterAnnotation])]](Set((newState, Set[MachineFilterAnnotation]())))(
        (intermediaryStates: Set[(State, Set[MachineFilterAnnotation])], actionR: ActionReplay[Exp, Abs, Addr]) =>
          intermediaryStates.flatMap((intermediaryState: (State, Set[MachineFilterAnnotation])) => {
            val intermediaryFilters = intermediaryState._2
            val nextIntermediaryStepSet = actionRApplier.applyActionReplay(intermediaryState._1, actionR)
            nextIntermediaryStepSet.map({ case (nextIntermediaryState, nextIntermediaryFilters) =>
              (nextIntermediaryState, intermediaryFilters ++ nextIntermediaryFilters)
            })
          })
      )
    )

    def replaceFilters(edgeAnnotation: EdgeAnnotation2,
                       newMachineFilters: Set[MachineFilterAnnotation]): EdgeAnnotation2 = {
      edgeAnnotation.copy(filters = FilterAnnotations(newMachineFilters, edgeAnnotation.filters.semanticsFilters))
    }

    Logger.log(s"Results: $results", LogPropagation)

    /*
     * Using the filter edges gathered while applying the action edges, find the appropriate edge in the original
     * graph. This edge then provides an EdgeAnnotation (used to print text over the edge in the outputted graph)
     * and a StateCombo, consisting of the state at the end of the edge in the initial graph and the newly computed
     * state.
     */
    val nonSubsumptionStateCombos: Set[(EdgeAnnotation2, StateCombo)] = results.flatMap({
      case (newNewState, machineFilters) =>
        val currentId = graph.nodeId(newState)
        val initialGraphFilteredEdge: Set[Edge] =
          filterEdgeFilterAnnotations.filterToFilterEdge(nonSubsumptionEdges, FilterAnnotations(machineFilters, Set()))
        Logger.log(s"FilterEdge for state $newNewState (current ID $currentId) and concrete-ish " +
                   s"$machineFilters is $initialGraphFilteredEdge", LogPropagation)
        initialGraphFilteredEdge.map( (edge) => {
          /* Use the new filters, generated by applying the actionEdge. */
          val edgeAnnotation = replaceFilters(edge._1, machineFilters)
          val newOriginalState = edge._2
          (edgeAnnotation, StateCombo(newOriginalState, newNewState))
        })
    })
    Logger.log(s"nonSubsumptionStateCombos: $nonSubsumptionStateCombos", LogPropagation)

    /*
     * Calculate new StateCombos for subsumption edges.
     */
    val subsumptionStateCombos: Set[StateCombo] = subsumptionEdges.map( (edge: Edge) => StateCombo(edge._2, newState) )

    Logger.log(s"newStateCombos = ${nonSubsumptionStateCombos.map( (sc: (EdgeAnnotation2, StateCombo)) =>
                 prunedGraph.nodeId(sc._2.originalState))}", Logger.D)
    val newVisited = visited + newState // TODO remove if (usedSubsumptionEdges) visited else visited + newState
    StepEval(nonSubsumptionStateCombos, subsumptionStateCombos, newVisited)
  }

  case class TodoPair(todo: Set[State], mapping: Map[State, Set[State]]) {

    def addStates(stateCombos: Set[StateCombo]): TodoPair =
      TodoPair(todo ++ stateCombos.map(_.newState), TodoPair.extendMapping(mapping, stateCombos))

    def dropHead: TodoPair =
      TodoPair(todo.tail, mapping)

    def dropHeadAndAddStates(stateCombos: Set[StateCombo]): TodoPair =
      TodoPair(todo.tail ++ stateCombos.map(_.newState), TodoPair.extendMapping(mapping, stateCombos))

    def ++(other: TodoPair): TodoPair = {
      TodoPair(todo ++ other.todo, mapping ++ other.mapping)
    }
  }

  object TodoPair {

    private def extendMapping(initial:  Map[State, Set[State]], stateCombos: Set[StateCombo]): Map[State, Set[State]] = {
      stateCombos.foldLeft(initial)( (newMapping, sc) => {
        val oldValues = newMapping.getOrElse(sc.newState, Set())
        newMapping + (sc.newState -> (oldValues + sc.originalState))
      })
    }

    def init(stateCombos: Set[StateCombo]): TodoPair =
      TodoPair(stateCombos.map(_.newState), extendMapping(Map.empty, stateCombos))
  }

  protected def addAllEdges(initialState: State,
                            graph: AbstractGraph): AbstractGraph = {
    def loop(todo: Set[State], visited: Set[State], graph: AbstractGraph): AbstractGraph = todo.headOption match {
      case None =>
        graph
      case Some(state) if ! visited.contains(state) =>
        val edges = graph.edges(state)
        val targetStates = edges.map(_._2)
        val newGraph = graph.addEdges(edges.map( (edge) => (state, edge._1, edge._2)) )
        loop(todo.tail ++ targetStates, visited + state, newGraph)
      case _ =>
        loop(todo.tail, visited, graph)
    }
    loop(Set(initialState), Set(), graph)
  }

  /**
    * Checks whether the extra optimisation is applicable for the given newState.
    * @param newState
    * @param originalStates
    * @return
    */
  private def extraOptimisationApplicable(newState: State,
                                          originalStates: Set[State],
                                          prunedGraph: AbstractGraph): Option[Iterable[Delta]] = {
    val optionDeltas: Set[Option[Either[Iterable[(KontAddr, KontAddr)], KontStore[KontAddr]]]] =
      originalStates.map(stateInfoProvider.deltaKStore(newState, _))
    /*
     * The delta should be the same for all original states. Otherwise, the optimisation should not be applied.
     * Since optionDeltas is a set, if the delta is the same everywhere, the set should only contain one item.
     */

    if (optionDeltas.size != 1 || optionDeltas.head.isEmpty) {
      None
    } else {
      val eitherTuples: Either[Iterable[(KontAddr, KontAddr)], KontStore[KontAddr]] = optionDeltas.head.get
      val deltas: Iterable[Delta] = eitherTuples match {
        case Left(tuples) =>
          tuples.map( (tuple) => DeltaReplaceKontAddr(tuple._1, tuple._2))
        case Right(diffKontStore) =>
          Set(DeltaUseKontStore(diffKontStore, actionRApplier.getKontStore(newState)))
      }

      /* Collect all edges going outwards from all of these original states. */
      val originalEdges: Set[Edge] = originalStates.flatMap(prunedGraph.nodeEdges)
      /*
       * Check that not a SINGLE edge in originalEdges uses the delta.
       * TODO Should also work if a couple edges actually do use this delta.
       */

      if (originalEdges.forall( (edge: Edge) => ! deltas.exists( (delta: Delta) => delta.relevantForEdge(edge) ) )) {
        Some(deltas)
      } else {
        None
      }
    }
  }

  private def usesKontAddr(edge: Edge, ka: KontAddr): Boolean = {
    val filters = edge._1.filters
    filters.machineExists({
      case KontAddrPopped(oldKa, newKa) =>
        oldKa == ka || newKa == ka
      case KontAddrPushed(newKa) =>
        newKa == ka
      case _ =>
        false
    })
  }

  /*
   * States and edges that could not be improved ("unviable") should be added to the TodoPair.
   * States that were visited go into the set of visited States.
   */
  case class DeltaEdgesAdded(todoPair: TodoPair, graph: AbstractGraph, visited: Set[State])

  trait Delta {
    def applyDelta(state: State): State
    def relevantForEdge(edge: Edge): Boolean
  }

  case class DeltaReplaceKontAddr(concreteKa: KontAddr, abstractKa: KontAddr) extends Delta {

    def applyDelta(state: State): State = {
      val abstractKaKonts = actionRApplier.getKonts(state, abstractKa)
      /* Safe to remove ALL Konts at abstractKa, because there shouldn't be any Konts at abstractKa that are not */
      val removedKontsState = actionRApplier.removeKonts(state, abstractKa)
      actionRApplier.addKonts(removedKontsState, concreteKa, abstractKaKonts)
    }

    def relevantForEdge(edge: Edge): Boolean =
      usesKontAddr(edge, abstractKa)

  }

  case class DeltaUseKontStore(diffKontStore: KontStore[KontAddr], kontStore: KontStore[KontAddr]) extends Delta {

    def applyDelta(state: State): State = {
      actionRApplier.replaceKontStore(state, kontStore)
    }

    def relevantForEdge(edge: Edge): Boolean = {
      diffKontStore.forall({
        case (ka, _) =>
          ! usesKontAddr(edge, ka)
      })
    }
  }

  protected def addAllEdgesDelta(initialState: State,
                                 mapping: Map[State, Set[State]],
                                 deltas: Iterable[Delta],
                                 visited: Set[State],
                                 prunedGraph: AbstractGraph,
                                 graph: AbstractGraph): DeltaEdgesAdded = {
    def loop(todoForThisOptimisation: TodoPair,
             notContinuedTodo: TodoPair,
             visited: Set[State],
             graph: AbstractGraph): DeltaEdgesAdded = {
      Logger.log(s"In loop iteration", Logger.U)
      todoForThisOptimisation.todo.headOption match {
        case None =>
          DeltaEdgesAdded(notContinuedTodo, graph, visited)
        case Some(state) if !visited.contains(state) =>
          val originalStates = todoForThisOptimisation.mapping(state)
          val edges = originalStates.flatMap(prunedGraph.edges.getOrElse(_, Set()))
          /*
         * A tuple of the edges from the pruned (i.e., abstract graph) that can be used in this optimisation,
         * along with the edges that cannot be used in this optimisation.
         */

          /*
         * UnviableEdges: have not been added to the graph yet, must be used conventionally.
         * These should be filtered out automatically though. Initially, before calling addAllEdgesDelta,
         * the extraOptimisationApplicable must have been already called, which guarantees that there are initially
         * no unviable edges. Later on, in this loop, the extraOptimisationApplicable is called again to separate
         * the continueDeltaStates from the abortedDeltaStates. This ensures that the edges continueDeltaStates
         *
         * abortDeltaStates: have been optimised and added to the graph, but cannot be used in the next iteration.
         * must be added to the TodoPair of the main evalLoop.
         *
         * continueTodoPair: have been optimised and added to the graph, will also be used in the next iteration.
         * these states will be added to the todoForThisOptimisation.
         *
         *
         * All states that were seen in the todoForThisOptimisation must be added to the set of visited states,
         * to be used by the main evalLoop.
         */

          /* Partition according to whether or not it uses SOME delta from the list of deltas.  */
          val (unviableEdges, viableEdges) = edges.partition( (edge: Edge) => deltas.exists(_.relevantForEdge(edge)) )
          // usesKontAddr(_, delta.abstractKa))
          assert(unviableEdges.isEmpty, s"unviable edges are $unviableEdges with deltas $deltas")
          //val (viableTargetStates, unviableTargetStates) = (viableEdges.map(_._2), unviableEdges.map(_._2))
          /* A set of tuples consisting of the original edge (along with the original target state) and the "concrete" state */
          val deltasStates: Set[(Edge, State)] = viableEdges.map( (edge) => {
            val updatedState = deltas.foldLeft[State](edge._2)( (state, delta) => delta.applyDelta(state))
            (edge, updatedState)
          })
          /* filter out deltas states for which optimisation is not applicable anymore */
          val (continueDeltaStates, abortDeltaStates) = deltasStates.partition({
            /*
             * originalEdge = an edge from the pruned graph.
             * deltaState = the target state of the originalEdge, with the delta applied to it.
             */
            case (originalEdge, deltaState) =>
              val optionNewDeltas = extraOptimisationApplicable(deltaState, Set(originalEdge._2), prunedGraph)
              /*
               * Check whether there are new deltas and check whether the set of new deltas is a superset of the
               * current set of deltas.
               */
              optionNewDeltas.isDefined && optionNewDeltas.get.forall( (delta: Delta) => deltas.exists(_ == delta))
          })
          /* Add all edges from state to deltasStates. */
          val newGraph = graph.addEdges(deltasStates.map({
            case (edge, deltaState) =>
              (state, edge._1, deltaState)
          }))
          val continueStateCombos: Set[StateCombo] = continueDeltaStates.map( (deltasState) =>
            StateCombo(deltasState._1._2, deltasState._2))
          val abortStateCombos: Set[StateCombo] = abortDeltaStates.map( (deltasState) =>
            StateCombo(deltasState._1._2, deltasState._2))
          val continueTodoPair = todoForThisOptimisation.dropHeadAndAddStates(continueStateCombos)
          val newNotContinuedTodo = notContinuedTodo.addStates(abortStateCombos)
          loop(continueTodoPair, newNotContinuedTodo, visited + state, newGraph)
        case _ =>
          loop(todoForThisOptimisation.dropHead, notContinuedTodo, visited, graph)
      }
    }
    loop(TodoPair(Set(initialState), mapping), TodoPair(Set(), Map()), visited, graph)
  }

  /*
   * Keep track of the set of visited originalStates, but not of the set of newStates.
   */
  private def evalLoop(todoPair: TodoPair,
                       visited: Set[State],
                       graph: AbstractGraph,
                       stepCount: Int,
                       initialGraph: AbstractGraph,
                       prunedGraph: AbstractGraph): AbstractGraph = {
    Logger.log(s"Size of visited set ${visited.size}", Logger.D)
    Logger.log(s"Size of todo set ${todoPair.todo.size}", Logger.D)
    val checkSubsumes = true
    todoPair.todo.headOption match {
      case None =>
        graph
      case Some(newState) =>
        //        val originalStateId = prunedGraph.nodeId(originalState)
        //        Logger.log(s"Incrementally evaluating original state ${initialGraph.nodeId(originalState)} " +
        //                   s"(currentID: $originalStateId) $originalState with new state $newState", LogPropagation)
        if (actionRApplier.halted(newState)) {
          Logger.log(s"State halted", LogPropagation)
          evalLoop(todoPair.dropHead, visited + newState, graph, stepCount, initialGraph, prunedGraph)
        } else if (visited.contains(newState)) {
          Logger.log(s"State already visited", LogPropagation)
          evalLoop(todoPair.dropHead, visited, graph, stepCount, initialGraph, prunedGraph)
        } else if (checkSubsumes && visited.exists((s2) => actionRApplier.subsumes(s2, newState))) {
          Logger.log(s"State subsumed", LogPropagation)
          val updatedGraph = visited.foldLeft[AbstractGraph](graph)({
            case (graph, s2) =>
              if (actionRApplier.subsumes(s2, newState)) {
                graph.addEdge(newState, EdgeAnnotation.subsumptionEdge, s2)
              } else {
                graph
              }
          })
          evalLoop(todoPair.dropHead, visited, updatedGraph, stepCount, initialGraph, prunedGraph)
        } else {
          val applyOptimisation = analysisFlags.extraIncrementalOptimisation
          lazy val optionDeltas: Option[Iterable[Delta]] = extraOptimisationApplicable(newState, todoPair.mapping(newState), prunedGraph)
          if (applyOptimisation && optionDeltas.isDefined) {
            Logger.log(s"Extra optimisation applicable: ${optionDeltas.get}", Logger.U)
            val deltas = optionDeltas.get
            val deltaEdgesAdded = addAllEdgesDelta(newState, todoPair.mapping, deltas, visited, prunedGraph, graph)
            val newTodo = todoPair.dropHead ++ deltaEdgesAdded.todoPair
            Logger.log(s"Extra optimisation applied", Logger.U)
            evalLoop(newTodo, deltaEdgesAdded.visited, deltaEdgesAdded.graph, stepCount, initialGraph, prunedGraph)
          } else {
            val StepEval(nonSubsumptionStateCombos, subsumptionStateCombos, newVisited) =
              stepEval(newState, visited, todoPair.mapping, graph, prunedGraph)
            evalLoop(todoPair.dropHeadAndAddStates(subsumptionStateCombos ++ nonSubsumptionStateCombos.map(_._2)),
              newVisited,
              graph.addEdges(nonSubsumptionStateCombos.map({
                case (edgeAnnotation, StateCombo(_, newNewState)) =>
                  (newState, edgeAnnotation, newNewState)
              })),
              stepCount,
              initialGraph,
              prunedGraph)
          }
        }
    }
  }

  def convertGraph[Node: Descriptor, EdgeAnnotation, NewEdgeAnnotation](g: Graph[Node, EdgeAnnotation],
                                                                        f: EdgeAnnotation => NewEdgeAnnotation): Graph[Node, NewEdgeAnnotation] = {
    val newValues: Map[Node, Set[(NewEdgeAnnotation, Node)]] = g.edges.mapValues((value: Set[(EdgeAnnotation, Node)]) =>
      value.map((value: (EdgeAnnotation, Node)) => (f(value._1), value._2)))
    new HyperlinkedGraph[Node, NewEdgeAnnotation](g.ids, g.next, g.nodes, newValues)
  }

  def applyEdgeActions(convertedState: State,
                       stepCount: Int,
                       rootNodes: Set[State],
                       initialGraph: AbstractGraph,
                       prunedGraph: AbstractGraph): AbstractGraph = {
    rootNodes.foreach((node) => Logger.log(s"node id: ${initialGraph.nodeId(node)}", Logger.U))
    if (analysisFlags.incrementalOptimisation && rootNodes.size == 1 && rootNodes.head == convertedState) {
      Logger.log(s"Skipping propagation phase because convertedState equals single root state", Logger.U)
      prunedGraph
    } else {
      /*
       * Associate the (one) abstracted concrete state with all states in the CurrentNodes set, as the states in
       * this set ought to correspond with this concrete state.
       */
      val rootStateCombos = rootNodes.map( (state) => StateCombo(state, convertedState) )
      evalLoop(TodoPair.init(rootStateCombos),
               Set(),
               new HyperlinkedGraph[State, EdgeAnnotation2](convertedState),
               stepCount,
               initialGraph,
               prunedGraph)
    }
  }

}