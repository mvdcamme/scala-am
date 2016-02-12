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

class HybridMachine[Exp : Expression, Time : Timestamp](semantics : Semantics[Exp, HybridLattice.Hybrid, HybridAddress, Time])
    extends EvalKontMachine[Exp, HybridLattice.Hybrid, HybridAddress, Time](semantics) {
  
  type HybridValue = HybridLattice.Hybrid
  
  def name = "HybridMachine"

  val SWITCH_ABSTRACT = false
  val DO_TRACING = true

  val THRESHOLD = 1

  val tracerContext : TracerContext[Exp, HybridValue, HybridAddress, Time] = new TracerContext[Exp, HybridValue, HybridAddress, Time](sem)

  /** The primitives are defined in AbstractValue.scala and are available through the Primitives class */
  val primitives = new Primitives[HybridAddress, HybridValue]()

  /**
   * The store used for continuations is a KontStore (defined in
   * Kontinuation.scala). It is parameterized by continuation addresses, that
   * are element of the KontAddress typeclass.
   */
  trait KontAddr
  case class NormalKontAddress(exp: Exp, addr: HybridAddress) extends KontAddr {
    override def toString = s"NormalKontAddress($exp)"
  }
  object HaltKontAddress extends KontAddr {
    override def toString = "HaltKontAddress"
  }

  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }

  object Converter {

    val valueConverter : AbstractConcreteToAbstractType = new AbstractConcreteToAbstractType

    def convertValue(σ : Store[HybridAddress, HybridLattice.Hybrid])(value : HybridValue) : HybridValue = value match {
      case HybridLattice.Left(v) => HybridLattice.Right(valueConverter.convert[Exp](v, σ))
      case HybridLattice.Right(v) => HybridLattice.Right(v)
      case HybridLattice.Prim(p) => HybridLattice.Prim(p)
    }

    def convertEnvironment(env : Environment[HybridAddress]) : Environment[HybridAddress] =
      new Environment[HybridAddress](env.content.map { tuple => (tuple._1, HybridAddress.convertAddress(tuple._2))})

    def convertControl(control : Control, σ : Store[HybridAddress, HybridLattice.Hybrid]) : Control = control match {
      case ControlEval(exp) => ControlEval(exp)
      case ControlKont(v) => ControlKont(convertValue(σ)(v))
      case ControlError(string) => ControlError(string)
    }

    def convertKontAddress(address : KontAddr) : KontAddr = address match {
      case NormalKontAddress(exp, addr) => NormalKontAddress(exp, HybridAddress.convertAddress(addr))
      case HaltKontAddress => HaltKontAddress
    }

    def convertState(s : State) : (State, State) = s match {
      case State(control, ρ, σ, kstore, a, t, tc, v, vStack) =>
        val newControl = convertControl(control, σ)
        val newρ = convertEnvironment(ρ)
        var newσ = Store.empty[HybridAddress, HybridLattice.Hybrid]
        val newKStore = kstore.map(convertKontAddress, sem.convertFrame(HybridAddress.convertAddress, convertValue(σ)))
        val newA = convertKontAddress(a)
        val newV = convertValue(σ)(v)
        val newVStack = vStack.map(convertValue(σ))
        def addToNewStore(tuple: (HybridAddress, HybridValue)): Boolean = {
          val newAddress = HybridAddress.convertAddress(tuple._1)
          val newValue = convertValue(σ)(tuple._2)
          newσ = newσ.extend(newAddress, newValue)
          true
        }
        σ.forall(addToNewStore)
        (s, State(newControl, newρ, newσ, newKStore, newA, t, tracerContext.newTracerContext, newV, newVStack))
    }
  }

  def popStack[A](stack : List[A], n : Integer) : (List[A], List[A]) = stack.splitAt(n)

  def replaceTc(state: State, tc : tracerContext.TracerContext) =
    new State(state.control, state.ρ, state.σ, state.kstore, state.a, state.t, tc, state.v, state.vStack)

  def applyAction(a: KontAddr)(state : State, action : Action[Exp, HybridValue, HybridAddress]) : State = {

    val control = state.control
    val ρ = state.ρ
    val σ = state.σ
    val kstore = state.kstore
    val t = state.t
    val tc = state.tc
    val v = state.v
    val vStack = state.vStack

    val newTc = if (tracerContext.isTracing(tc)) {
      tracerContext.appendTrace(tc, List(action))
    } else {
      tc
    }

    def handleGuard(guard: ActionGuard[SchemeExp, HybridValue, HybridAddress, sem.RestartPoint], guardCheckFunction : HybridValue => Boolean) =
      if (guardCheckFunction(v)) {
        println(s"Guard $guard succeeded")
        replaceTc(state, newTc)
      } else {
        println(s"Guard $guard failed")
        val newTc = new tracerContext.TracerContext(tc.label, tc.traceNodes, tc.trace, tracerContext.semantics.ExecutionPhase.NI, tc.traceExecuting)
        State(ControlEval(guard.restartPoint._1), guard.restartPoint._2, σ, kstore, a, t, newTc, v, vStack)
      }

    action match {
      case ActionLiteral(v) => State(control, ρ, σ, kstore, a, t, newTc, v, vStack)
      case ActionSetVar(va) => State(control, ρ, σ.update(va, v), kstore, a, t, newTc, v, vStack)
      case ActionReachedValue(v, _, _) => State(ControlKont(v), ρ, σ, kstore, a, t, newTc, v, vStack)
      case ActionLookupVariable(varName, _, _) =>
        val newV = σ.lookup(ρ.lookup(varName).get)
        State(ControlKont(newV), ρ, σ, kstore, a, t, newTc, newV, vStack)
      case ActionExtendEnv(varName : String) =>
        val va = addr.variable(varName, t)
        val ρ1 = ρ.extend(name, va)
        val σ1 = σ.extend(va, v)
        State(control, ρ1, σ1, kstore, a, t, tc, v, vStack)
      /* When a continuation needs to be pushed, push it in the continuation store */
        /*
        Replace frame to be pushed by fnction that takes a store and returns a new frame
         */
      case ActionPush(e, frame, _, _) =>
        val next = NormalKontAddress(e, addr.variable("__kont__", t)) // Hack to get infinite number of addresses in concrete mode
        State(ControlEval(e), ρ, σ, kstore.extend(next, Kont(frame, a)), next, t, newTc, v, vStack)
      case ActionPushEnv(e, frame, ρ, σ, _, _) =>
        val next = NormalKontAddress(e, addr.variable("__kont__", t)) // Hack to get infinite number of addresses in concrete mode
        State(ControlEval(e), ρ, σ, kstore.extend(next, Kont(frame, a)), next, t, newTc, v, vStack)
      /* When a value needs to be evaluated, we go to an eval state */
      case ActionEval(e, ρ, σ, _, _) => State(ControlEval(e), ρ, σ, kstore, a, t, newTc, v, vStack)
      /* When a function is stepped in, we also go to an eval state */
      case ActionStepIn(fexp, (_, ρ1), e, args, argsv, n, _, _) =>
        val (vals, newVStack) = popStack(vStack, n)
        println(vals)
        if (args.length == n - 1) {
          sem.bindArgs(args.zip(argsv.zip(vals)), ρ1, σ, t) match {
            case (ρ2, σ2) =>
              State(ControlEval(e), ρ2, σ2, kstore, a, time.tick(t, fexp), newTc, v, newVStack)
          }
        } else { State(ControlError(s"Arity error when calling $fexp. (${args.length} arguments expected, got ${n - 1})"), ρ, σ, kstore, a, t, newTc, v, newVStack) }
      case ActionPushVal() => State(control, ρ, σ, kstore, a, t, newTc, v, v :: vStack)
      /* When an error is reached, we go to an error state */
      case ActionError(err) => State(ControlError(err), ρ, σ, kstore, a, t, newTc, v, vStack)
      case ActionPrimCall(n : Integer, fExp : SchemeExp, argsExps : List[SchemeExp]) =>
        val (vals, newVStack) = popStack(vStack, n)
        val operator : HybridValue = vals.last
        val operands : List[HybridValue] = vals.take(n - 1)
        val primitive : Option[Primitive[HybridAddress, HybridValue]] = abs.getPrimitive[HybridAddress, HybridValue](operator)
        val result = primitive match {
          case Some(primitive) => primitive.call(fExp, argsExps.zip(operands.reverse), σ, t)
          case None => throw new Exception(s"Operator $fExp not a primitive: $operator")
        }
        result match {
          case Left(error) => throw new Exception(error)
          case Right((v, newσ)) => State(ControlKont(v), ρ, σ, kstore, a, t, newTc, v, newVStack)
        }
      case v : ActionGuardFalse[SchemeExp, HybridValue, HybridAddress, sem.RestartPoint] => handleGuard(v, abs.isFalse)
      case v : ActionGuardTrue[SchemeExp, HybridValue, HybridAddress, sem.RestartPoint] => handleGuard(v, abs.isTrue)
    }
  }

  /**
   * A machine state is made of a control component, a value store, a
   * continuation store, and an address representing where the current
   * continuation lives.
   */
  case class State(control: Control, ρ : Environment[HybridAddress], σ: Store[HybridAddress, HybridValue], kstore: KontStore[KontAddr],
                   a: KontAddr, t: Time, tc: tracerContext.TracerContext, v : HybridValue, vStack : List[HybridValue]) {

    /**
     * Builds the state with the initial environment and stores
     */
    def this(exp: Exp) = this(ControlEval(exp), Environment.empty[HybridAddress]().extend(primitives.forEnv),
      Store.initial(primitives.forStore, true),
      new KontStore[KontAddr](), HaltKontAddress, time.initial, tracerContext.newTracerContext, abs.inject(false), Nil)
    override def toString() = control.toString(σ)
    /**
     * Checks whether a states subsumes another, i.e., if it is "bigger". This
     * is used to perform subsumption checking when exploring the state space,
     * in order to avoid exploring states for which another state that subsumes
     * them has already been explored.
     *
     * The tracer context is ignored in this check, because it only stores "meta" information not relevant to the actual program state.
     */
    def subsumes(that: State): Boolean = control.subsumes(that.control) && ρ.subsumes(that.ρ) && σ.subsumes(that.σ) && a == that.a && kstore.subsumes(that.kstore) && t == that.t

    type InterpreterReturn = Semantics[Exp, HybridLattice.Hybrid, HybridAddress, Time]#InterpreterReturn

    /**
     * Integrates a set of interpreterReturns (returned by the semantics, see
     * Semantics.scala), in order to generate a set of states that succeeds this
     * one.
     */
    private def integrate(a: KontAddr, interpreterReturns: Set[sem.InterpreterReturn]): Set[State] = {

      def applyTrace(state : State, trace : sem.Trace) : State = {
        trace.foldLeft(this)(applyAction(a))
      }

      def startExecutingTrace(trace : sem.Trace, label : sem.Label): State = {
        println(s"Trace with label $label already exists")
        val newState = applyTrace(this, trace)
        val tc = newState.tc
        val traceNode = tracerContext.getTrace(tc, label)
        val newTracerContext = new tracerContext.TracerContext(tc.label, tc.traceNodes, tc.trace, tracerContext.semantics.ExecutionPhase.TE, Some(traceNode))
        new State(newState.control, newState.ρ, newState.σ, newState.kstore, newState.a, newState.t, newTracerContext, newState.v, newState.vStack)
      }

      interpreterReturns.map({itpRet => itpRet match {
        case sem.InterpreterReturn(trace, sem.TracingSignalFalse()) => applyTrace(this, trace)
        case sem.InterpreterReturn(trace, sem.TracingSignalStart(label)) =>
          if (tracerContext.traceExists(tc, label)) {
            startExecutingTrace(trace, label)
          } else if (tracerContext.isTracingLabel(tc, label)) {
            val newState = applyTrace(this, trace)
            println(s"Stopped tracing $label")
            new State(newState.control, newState.ρ, newState.σ, newState.kstore, newState.a, newState.t, tracerContext.stopTracing(newState.tc, true, None), newState.v, newState.vStack)
          } else if (DO_TRACING) {
            println(s"Started tracing $label")
            val newTc = tracerContext.startTracingLabel(tc, label)
            val newState = applyTrace(this, trace)
            new State(newState.control, newState.ρ, newState.σ, newState.kstore, newState.a, newState.t, newTc, newState.v, newState.vStack)
          } else {
            applyTrace(this, trace)
          }
        case sem.InterpreterReturn(trace, sem.TracingSignalEnd(label, restartPoint)) =>
          if (tracerContext.isTracingLabel(tc, label)) {
            println(s"Stopped tracing $label")
            val newState = applyTrace(this, trace)
            new State(newState.control, newState.ρ, newState.σ, newState.kstore, newState.a, newState.t, tracerContext.stopTracing(tc, false, Some(restartPoint)), newState.v, newState.vStack)
          } else {
            val newState = applyTrace(this, trace)
            newState
          }
      }})}

    def stepTrace() : State = {
      val (traceHead, newTc) = tracerContext.stepTrace(tc)
      val newState = applyAction(a)(this, traceHead)
      new State(newState.control, newState.ρ, newState.σ, newState.kstore, newState.a, newState.t, newTc, newState.v, newState.vStack)
    }

    /**
     * Computes the set of states that follow the current state
     */
    def step() : Set[State] = {
      if (tracerContext.isExecuting(tc)) {
        Set(stepTrace())
      } else {
        control match {
          /* In a eval state, call the semantic's evaluation method */
          case ControlEval(e) => integrate(a, sem.stepEval(e, ρ, σ, t))
          /* In a continuation state, if the value reached is not an error, call the
           * semantic's continuation method */
          case ControlKont(v) if abs.isError(v) => Set()
          case ControlKont(v) => kstore.lookup(a).flatMap({
            case Kont(frame, next) => integrate(next, sem.stepKont(v, frame, σ, t))
          })
          /* In an error state, the state is not able to make a step */
          case ControlError(_) => Set()
        }
      }
    }
    /**
     * Checks if the current state is a final state. It is the case if it
     * reached the end of the computation, or an error
     */
    def halted: Boolean = control match {
      case ControlEval(_) => false
      case ControlKont(v) => a == HaltKontAddress || abs.isError(v)
      case ControlError(_) => true
    }
  }

  case class AAMOutput(halted: Set[State], count: Int, t: Double, graph: Option[Graph[State, String]])
      extends Output[HybridValue] {

    /**
     * Returns the list of final values that can be reached
     */
    def finalValues = halted.flatMap(st => st.control match {
      case ControlKont(v) => Set[HybridValue](v)
      case _ => Set[HybridValue]()
    })

    /**
     * Checks if a halted state contains a value that subsumes @param v
     */
    def containsFinalValue(v: HybridValue) = finalValues.exists(v2 => abs.subsumes(v2, v))

    /**
     * Returns the number of visited states
     */
    def numberOfStates = count

    /**
     * Returns the time taken to evaluate the expression
     */
    def time = t

    /**
     * Outputs the graph in a dot file
     */
    def toDotFile(path: String) = graph match {
      case Some(g) => g.toDotFile(path, _.toString.take(40),
        (s) => if (halted.contains(s)) { "#FFFFDD" } else { s.control match {
          case ControlEval(_) => "#DDFFDD"
          case ControlKont(_) => "#FFDDDD"
          case ControlError(_) => "#FF0000"
        }}, _.toString.take(20))
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  var loopCounter = 0
  /**
   * Explores the state graph generated by State's step function.
   * @param todo is the set of states that needs to be visited
   * @param visited is the set of states already visited, they won't be visited again
   * @param halted is the set of final states reached
   * @param graph is the graph in its current form
   * @return the final states as well as the computed graph
   */
  @scala.annotation.tailrec
  private def loop(todo: Set[State], visited: Set[State],
    halted: Set[State], startingTime: Long, graph: Option[Graph[State, String]]): AAMOutput =
    todo.headOption match {
      case Some(s) =>
//        if (visited.contains(s) || visited.exists(s2 => s2.subsumes(s))) {
//          /* If we already visited the state, or if it is subsumed by another already
//           * visited state, we ignore it. The subsumption part reduces the
//           * number of visited states but leads to non-determinism due to the
//           * non-determinism of Scala's headOption (it seems so at least). */
//          loop(todo.tail, visited, halted, startingTime, graph)
//        } else
        if (s.halted) {
          /* If the state is a final state, add it to the list of final states and
           * continue exploring the graph */
          loop(todo.tail, visited + s, halted + s, startingTime, graph)
        } else {
          /* Otherwise, compute the successors of this state, update the graph, and push
           * the new successors on the todo list */
          loopCounter += 1
          if (SWITCH_ABSTRACT && loopCounter >= THRESHOLD) {
            switchToAbstract(todo, visited, halted, startingTime, graph)
          } else {
            val succs = s.step()
            val newGraph = graph.map(_.addEdges(succs.map(s2 => (s, "", s2))))
            loop(todo.tail ++ succs, visited + s, halted, startingTime, newGraph)
          }
        }
      case None => AAMOutput(halted, visited.size,
        (System.nanoTime - startingTime) / Math.pow(10, 9), graph)
  }

  private def switchToAbstract(todo: Set[State], visited: Set[State], halted: Set[State],
                               startingTime: Long, graph: Option[Graph[State, String]]) : AAMOutput = {
    println("HybridMachine switching to abstract")
    def mapF(states : Set[(State, State)], annotation : String)(graph : Graph[State, String]) = {
      graph.addEdges(states.map(tuple => (tuple._1, annotation, tuple._2)))
    }
    HybridLattice.switchToAbstract
    HybridAddress.switchToAbstract
    val convTodo = todo.map(Converter.convertState)
    val convVisited = visited.map(Converter.convertState)
    val convHalted = halted.map(Converter.convertState)
    val newTodo = convTodo.map(_._2)
    val newVisited = convVisited.map(_._2)
    val newHalted = convHalted.map(_._2)
    val newGraph = graph.map(mapF(convTodo, "Converted todo")_ compose mapF(convVisited, "Converted visited")_ compose mapF(convHalted, "Converted halted")_)
    loopAbstract(newTodo, newVisited, newHalted, startingTime, newGraph)
  }

  @scala.annotation.tailrec
  private def loopAbstract(todo: Set[State], visited: Set[State],
                   halted: Set[State], startingTime: Long, graph: Option[Graph[State, String]]): AAMOutput = {
    todo.headOption match {
      case Some(s) =>
        if (visited.contains(s) || visited.exists(s2 => s2.subsumes(s))) {
          /* If we already visited the state, or if it is subsumed by another already
           * visited state, we ignore it. The subsumption part reduces the
           * number of visited states but leads to non-determinism due to the
           * non-determinism of Scala's headOption (it seems so at least). */
          loopAbstract(todo.tail, visited, halted, startingTime, graph)
        } else if (s.halted) {
          /* If the state is a final state, add it to the list of final states and
           * continue exploring the graph */
          loopAbstract(todo.tail, visited + s, halted + s, startingTime, graph)
        } else {
          /* Otherwise, compute the successors of this state, update the graph, and push
           * the new successors on the todo list */
          val succs = s.step()
          val newGraph = graph.map(_.addEdges(succs.map(s2 => (s, "", s2))))
          loopAbstract(todo.tail ++ succs, visited + s, halted, startingTime, newGraph)
        }
      case None => AAMOutput(halted, visited.size,
        (System.nanoTime - startingTime) / Math.pow(10, 9), graph)
    }
  }

  /**
   * Performs the evaluation of an expression, possibly writing the output graph
   * in a file, and returns the set of final states reached
   */
  def eval(exp: Exp, graph: Boolean): Output[HybridValue] = {
    loop(Set(new State(exp)), Set(), Set(), System.nanoTime,
      if (graph) { Some(new Graph[State, String]()) } else { None })
  }
}