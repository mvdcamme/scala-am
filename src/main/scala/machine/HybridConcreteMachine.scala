import scala.annotation.tailrec

import ConcreteConcreteLattice.ConcreteValue

class HybridConcreteMachine[
    PAbs: IsConvertableLattice: PointsToableLatticeInfoProvider](
    pointsToAnalysisLauncher: PointsToAnalysisLauncher[PAbs],
    tracingFlags: TracingFlags)(implicit unused1: IsSchemeLattice[ConcreteConcreteLattice.L])
    extends EvalKontMachine[SchemeExp,
                            ConcreteConcreteLattice.L,
                            HybridAddress.A,
                            HybridTimestamp.T] {

  def name = "HybridConcreteMachine"

  var stepCount: Integer = 0

  trait ConcreteMachineOutput extends Output[ConcreteConcreteLattice.L] {
    def toDotFile(path: String) =
      println("Not generating graph for ConcreteMachine")
  }

  case class ConcreteMachineOutputError(time: Double,
                                        numberOfStates: Int,
                                        err: String)
      extends ConcreteMachineOutput {
    def finalValues = {
      println(s"Execution failed: $err")
      Set()
    }
    def containsFinalValue(v: ConcreteConcreteLattice.L) = false
    def timedOut = false
  }
  case class ConcreteMachineOutputTimeout(time: Double, numberOfStates: Int)
      extends ConcreteMachineOutput {
    def finalValues = Set()
    def containsFinalValue(v: ConcreteConcreteLattice.L) = false
    def timedOut = true
  }
  case class ConcreteMachineOutputValue(time: Double,
                                        numberOfStates: Int,
                                        v: ConcreteConcreteLattice.L,
                                        graph: Graph[State, FilterAnnotations[SchemeExp, ConcreteValue, HybridAddress.A]])
      extends ConcreteMachineOutput {
    def finalValues = Set(v)
    def containsFinalValue(v2: ConcreteConcreteLattice.L) = v == v2
    def timedOut = false
    override def toDotFile(path: String) = {
      graph.toDotFile(path,
        node => List(scala.xml.Text(node.toString.take(40))),
        (s) => s.graphNodeColor,
        _ => List(),
        None)
    }
  }

  private def convertValue[AbstL: IsConvertableLattice](abstPrims: SchemePrimitives[HybridAddress.A, AbstL])
                                                       (value: ConcreteConcreteLattice.L): AbstL =
    ConcreteConcreteLattice.convert[SchemeExp, AbstL, HybridAddress.A](
      value,
      new DefaultHybridAddressConverter[SchemeExp],
      convertEnv,
      abstPrims)

  /**
    * Converts all addresses in the environment.
    * @param env The environment for which all addresses must be converted.
    * @return A new environment with all addresses converted.
    */
  private def convertEnv(env: Environment[HybridAddress.A]): Environment[HybridAddress.A] = {
    val addressConverter = new DefaultHybridAddressConverter[SchemeExp]()
    env.map { (address) =>
      addressConverter.convertAddress(address)
    }
  }

  case class State(control: Control,
                   store: Store[HybridAddress.A, ConcreteConcreteLattice.L],
                   kstore: KontStore[KontAddr],
                   a: KontAddr,
                   t: HybridTimestamp.T)
      extends ConcreteTracingProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T]
      with StateTrait[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T] {

    def halted = control match {
      case ControlError(_) => true
      case ControlKont(_) => a == HaltKontAddress
      case _ => false
    }

    def graphNodeColor = control match {
      case ControlEval(_, _) => Colors.Green
      case ControlKont(_) => Colors.Pink
      case ControlError(_) => Colors.Red
    }

    /**
      * Converts all addresses in the store.
      * @param σ The store for which all addresses must be converted.
      * @return A new store with all addresses converted.
      */
    /* Currently, we don't actually convert addresses in the environment or store, so at the moment,
     * this function is just the identity function. */
    private def convertSto[AbstL: IsConvertableLattice](
        σ: Store[HybridAddress.A, AbstL]): Store[HybridAddress.A, AbstL] = σ

    private def convertKStoreToFrames[KAddr <: KontAddr: KontAddress](
        concSem: ConvertableSemantics[SchemeExp,
                                      ConcreteValue,
                                      HybridAddress.A,
                                      HybridTimestamp.T],
        abstSem: BaseSchemeSemantics[ConcreteValue,
                                     HybridAddress.A,
                                     HybridTimestamp.T],
        initialKontAddress: KAddr,
        mapKontAddress: (KontAddr, Environment[HybridAddress.A]) => KAddr,
        kontStore: KontStore[KontAddr],
        ρ: Environment[HybridAddress.A],
        a: KontAddr,
        vStack: List[Storable[ConcreteValue, HybridAddress.A]])
      : (KAddr, KontStore[KAddr]) = {

      @tailrec
      def loop(a: KontAddr,
               vStack: List[Storable[ConcreteValue, HybridAddress.A]],
               ρ: Environment[HybridAddress.A],
               stack: List[
                 (KontAddr,
                  Option[Frame],
                  List[Storable[ConcreteValue, HybridAddress.A]],
                  Environment[HybridAddress.A])]): (KAddr, KontStore[KAddr]) =
        a match {
          case HaltKontAddress =>
            stack.foldLeft[(KAddr, KontStore[KAddr])](
              (initialKontAddress, KontStore.empty[KAddr]))({
              case ((actualNext, extendedKontStore),
                    (a, someNewSemFrame, newVStack, newρ)) =>
                someNewSemFrame match {
                  case Some(newSemFrame) =>
                    val convertedA = mapKontAddress(a, newρ)
                    (convertedA,
                     extendedKontStore.extend(convertedA,
                                              Kont(newSemFrame, actualNext)))
                  case None =>
                    (actualNext, extendedKontStore)
                }

            })
          case _ =>
            val Kont(frame, next) = kontStore.lookup(a).head
            val (someNewSemFrame, newVStack, newρ) =
              concSem.convertToAbsSemanticsFrame(frame, ρ, vStack, abstSem)
            loop(next,
                 newVStack,
                 newρ,
                 (a, someNewSemFrame, newVStack, newρ) :: stack)
        }
      loop(a, vStack, ρ, Nil)
    }

    private def convertStore[AbstL: IsConvertableLattice](
        store: Store[HybridAddress.A, ConcreteValue],
        convertValue: ConcreteValue => AbstL)
      : Store[HybridAddress.A, AbstL] = {
      var valuesConvertedStore = Store.empty[HybridAddress.A, AbstL]
      def addToNewStore(tuple: (HybridAddress.A, ConcreteValue)): Boolean = {
        val convertedAddress =
          new DefaultHybridAddressConverter[SchemeExp]().convertAddress(tuple._1)
        val convertedValue = convertValue(tuple._2)
        valuesConvertedStore =
          valuesConvertedStore.extend(convertedAddress, convertedValue)
        true
      }
      store.forall(addToNewStore)
      valuesConvertedStore
    }

    private def convertKStore[AbstL: IsConvertableLattice,
                              KAddr <: KontAddr: KontAddress](
        mapKontAddress: (KontAddr,
                         Option[Environment[HybridAddress.A]]) => KAddr,
        kontStore: KontStore[KontAddr],
        convertValue: ConcreteValue => AbstL,
        concBaseSem: ConvertableSemantics[SchemeExp,
                                          ConcreteValue,
                                          HybridAddress.A,
                                          HybridTimestamp.T],
        abstSem: BaseSchemeSemantics[AbstL,
                                     HybridAddress.A,
                                     HybridTimestamp.T]): KontStore[KAddr] = {
      kontStore.map[KAddr](
        (ka) => convertKontAddr(ka, None, mapKontAddress),
        (frame: Frame) =>
          concBaseSem.convertAbsInFrame[AbstL](
            frame.asInstanceOf[SchemeFrame[ConcreteValue,
                                           HybridAddress.A,
                                           HybridTimestamp.T]],
            convertValue,
            convertEnv,
            abstSem))
    }

    var reached = Set[HybridAddress.A]()

    private def reachesValue(concBaseSem: BaseSchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                             sto: Store[HybridAddress.A, ConcreteValue])
                            (value: ConcreteValue): Set[HybridAddress.A] =
      ConcreteConcreteLattice.latticeInfoProvider.reaches[HybridAddress.A](value,
                                                                           reachesEnvironment(concBaseSem, sto),
                                                                           reachesAddress(concBaseSem, sto))

    private def reachesAddress(concBaseSem: BaseSchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                               sto: Store[HybridAddress.A, ConcreteValue])(
                               address: HybridAddress.A): Set[HybridAddress.A] = {
      if (! reached.contains(address)) {
        reached = reached + address
        Set(address) ++ sto.lookup(address).foldLeft[Set[HybridAddress.A]](Set())((_, value) =>
          reachesValue(concBaseSem, sto)(value))
      } else {
        Set()
      }
    }

    private def reachesEnvironment(
                                    concBaseSem: BaseSchemeSemantics[ConcreteValue,
                                      HybridAddress.A,
                                      HybridTimestamp.T],
                                    sto: Store[HybridAddress.A, ConcreteValue])(
                                    env: Environment[HybridAddress.A]): Set[HybridAddress.A] = {
      var reached: Set[HybridAddress.A] = Set()
      env.forall((tuple) => {
        reached = (reached + tuple._2) ++ reachesAddress(concBaseSem, sto)(
          tuple._2)
        true
      })
      reached
    }

    private def reachesKontAddr[KAddr <: KontAddr](
                                                    concBaseSem: BaseSchemeSemantics[ConcreteValue,
                                                      HybridAddress.A,
                                                      HybridTimestamp.T],
                                                    sto: Store[HybridAddress.A, ConcreteValue],
                                                    kstore: KontStore[KAddr])(
                                                    ka: KAddr): Set[HybridAddress.A] = {
      kstore
        .lookup(ka)
        .foldLeft[Set[HybridAddress.A]](Set())(
        (acc, kont) =>
          acc ++ concBaseSem.frameReaches(
            kont.frame.asInstanceOf[SchemeFrame[ConcreteValue,
              HybridAddress.A,
              HybridTimestamp.T]],
            reachesValue(concBaseSem, sto),
            reachesEnvironment(concBaseSem, sto),
            reachesAddress(concBaseSem, sto)) ++ reachesKontAddr(concBaseSem, sto, kstore)(kont.next))
    }

    private def reachesControl[KAddr <: KontAddr](concBaseSem: BaseSchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                                  sto: Store[HybridAddress.A, ConcreteValue],
                                                  kstore: KontStore[KAddr])(
                                                  control: Control)
    : Set[HybridAddress.A] =
      control match {
        case ControlEval(_, env) =>
          reachesEnvironment(concBaseSem, sto)(env)
        case ControlKont(value) =>
          reachesValue(concBaseSem, sto)(value)
        case ControlError(_) => Set()
      }

    private def reachesStoreAddresses[KAddr <: KontAddr](concBaseSem: BaseSchemeSemantics[ConcreteValue,
                                                            HybridAddress.A,
                                                            HybridTimestamp.T],
                                                         sto: Store[HybridAddress.A, ConcreteValue])(
                                                         control: Control,
                                                         kstore: KontStore[KAddr],
                                                         ka: KAddr): Set[HybridAddress.A] = {
      reachesControl[KAddr](concBaseSem, sto, kstore)(control) ++ reachesKontAddr[KAddr](concBaseSem, sto, kstore)(ka)
    }

    private def garbageCollectStore[KAddr <: KontAddr]
                                   (concBaseSem: BaseSchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                    store: Store[HybridAddress.A, ConcreteValue],
                                    control: Control,
                                    kstore: KontStore[KAddr],
                                    ka: KAddr): Store[HybridAddress.A, ConcreteValue] = {
      reached = Set()
      val storeAddressReachable = reachesStoreAddresses[KAddr](concBaseSem, store)(control, kstore, ka)
      store.gc(storeAddressReachable)
    }

    private def convertKontAddr[KAddr <: KontAddr](ka: KontAddr,
                                                   env: Option[Environment[HybridAddress.A]],
                                                   mapKontAddress: (KontAddr, Option[Environment[HybridAddress.A]]) => KAddr): KAddr = {
      val kontAddrConverter = new DefaultKontAddrConverter[SchemeExp]
      mapKontAddress(kontAddrConverter.convertKontAddr(ka), env)
    }

    def convertState[AbstL: IsConvertableLattice](
        concSem: ConvertableSemantics[SchemeExp,
                                      ConcreteValue,
                                      HybridAddress.A,
                                      HybridTimestamp.T],
        abstSem: BaseSchemeSemantics[AbstL,
                                     HybridAddress.A,
                                     HybridTimestamp.T],
        initialKontAddress: KontAddr,
        mapKontAddress: (KontAddr,
                         Option[Environment[HybridAddress.A]]) => KontAddr)
      : (ConvertedControl[SchemeExp, AbstL, HybridAddress.A],
         Store[HybridAddress.A, AbstL],
         KontStore[KontAddr],
         KontAddr,
         HybridTimestamp.T) = {

      val concBaseSem =
        new BaseSchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T](
          new SchemePrimitives[HybridAddress.A, ConcreteValue])

      val convertValueFun =
        convertValue[AbstL](abstSem.primitives) _

      val convertedControl: ConvertedControl[SchemeExp, AbstL, HybridAddress.A] =
        control match {
          case ControlEval(exp, env) =>
            ConvertedControlEval[SchemeExp, AbstL, HybridAddress.A](
              exp,
              convertEnv(env))
          case ControlKont(v) =>
            ConvertedControlKont[SchemeExp, AbstL, HybridAddress.A](
              convertValueFun(v))
        }

      val GCedStore = garbageCollectStore(concBaseSem, store, control, kstore, a)
      val convertedStore = convertStore(GCedStore, convertValueFun)
      val convertedKStore = convertKStore[AbstL, KontAddr](mapKontAddress,
                                                        kstore,
                                                        convertValueFun,
                                                        concBaseSem,
                                                        abstSem)

      val convertedA = convertKontAddr(a, None, mapKontAddress)
      val newT = DefaultHybridTimestampConverter.convertTimestamp(t)
      (convertedControl, convertedStore, convertedKStore, convertedA, newT)
    }
  }

  /**
    * Performs the evaluation of an expression, possibly writing the output graph
    * in a file, and returns the set of final states reached
    */
  def eval(exp: SchemeExp,
           sem: Semantics[SchemeExp,
                          ConcreteConcreteLattice.L,
                          HybridAddress.A,
                          HybridTimestamp.T],
           graph: Boolean,
           timeout: Option[Long]): Output[ConcreteConcreteLattice.L] = {
    def loop(state: State,
             start: Long,
             count: Int,
             graph: Graph[State, FilterAnnotations[SchemeExp, ConcreteValue, HybridAddress.A]]):
    ConcreteMachineOutput = {

      tracingFlags.RUNTIME_ANALYSIS_INTERVAL match {
        case NoRunTimeAnalysis =>
        case RunTimeAnalysisEvery(analysis_interval) =>
          Logger.log(s"stepCount: $stepCount", Logger.U)
          if (stepCount % analysis_interval == 0) {
            pointsToAnalysisLauncher.filterReachable(stepCount)
            pointsToAnalysisLauncher.applyEdgeActions(state, stepCount)
          }
      }

      stepCount += 1

      if (timeout.exists(System.nanoTime - start > _)) {
        ConcreteMachineOutputTimeout(
          (System.nanoTime - start) / Math.pow(10, 9),
          count)
      } else {
        val control = state.control
        val store = state.store
        val kstore = state.kstore
        val a = state.a
        val t = state.t

        case class StepSucceeded(state: State,
                                 filters: FilterAnnotations[SchemeExp, ConcreteValue, HybridAddress.A],
                                 actionTs: List[ActionReplay[SchemeExp, ConcreteValue, HybridAddress.A]])

        def step(control: Control): Either[ConcreteMachineOutput, StepSucceeded] =
          control
        match {
          case ControlEval(e, env) =>
            val edges = sem.stepEval(e, env, store, t)
            if (edges.size == 1) {
              edges.head match {
                case EdgeInformation(ActionReachedValue(v, store2, _), actions, semanticsFilters) =>
                  val machineFilters = Set[MachineFilterAnnotation]()
                  Right(StepSucceeded(State(ControlKont(v), store2, kstore, a, time.tick(t)),
                                      FilterAnnotations(machineFilters, semanticsFilters),
                                      actions))
                case EdgeInformation(ActionPush(frame, e, env, store2, _), actions, semanticsFilters) =>
                  val next = NormalKontAddress[SchemeExp, HybridTimestamp.T](e, t)
                  val kont = Kont(frame, a)
                  val machineFilters = Set[MachineFilterAnnotation](KontAddrPushed(next),
                                                                    EvaluatingExpression(e))
                  Right(StepSucceeded(State(ControlEval(e, env), store2, kstore.extend(next, kont), next, time.tick(t)),
                                      FilterAnnotations(machineFilters, semanticsFilters),
                                      actions))
                case EdgeInformation(ActionEval(e, env, store2, _), actions, semanticsFilters) =>
                  val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
                  Right(StepSucceeded(State(ControlEval(e, env), store2, kstore, a, time.tick(t)),
                                      FilterAnnotations(machineFilters, semanticsFilters),
                                      actions))
                case EdgeInformation(ActionStepIn(fexp, _, e, env, store2, _, _), actions, semanticsFilters) =>
                  val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
                  Right(StepSucceeded(State(ControlEval(e, env), store2, kstore, a, time.tick(t, fexp)),
                                      FilterAnnotations(machineFilters, semanticsFilters),
                                      actions))
                case EdgeInformation(ActionError(err), actions, semanticsFilters) =>
                  Left(ConcreteMachineOutputError(
                    (System.nanoTime - start) / Math.pow(10, 9),
                    count,
                    err.toString))
              }
            } else {
              Left(ConcreteMachineOutputError(
                (System.nanoTime - start) / Math.pow(10, 9),
                count,
                s"execution was not concrete (got ${edges.size} actions instead of 1)"))
            }

          case ControlKont(v) =>
            /* pop a continuation */
            if (a == HaltKontAddress) {
              Left(ConcreteMachineOutputValue(
                (System.nanoTime - start) / Math.pow(10, 9),
                count,
                v, graph))
            } else {
              val frames = kstore.lookup(a)
              if (frames.size == 1) {
                val frame = frames.head.frame
                val originFrameCast = frame.asInstanceOf[SchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]]
                val oldA = state.a
                val a = frames.head.next
                val edges = sem.stepKont(v, frame, store, t)
                if (edges.size == 1) {
                  edges.head match {
                    case EdgeInformation(ActionReachedValue(v, store2, _), actions, semanticsFilters) =>
                      val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                                                                        FrameFollowed[ConcreteValue](originFrameCast))
                      Right(StepSucceeded(State(ControlKont(v), store2, kstore, a, time.tick(t)),
                                          FilterAnnotations(machineFilters, semanticsFilters),
                                          actions))
                    case EdgeInformation(ActionPush(frame, e, env, store2, _), actions, semanticsFilters) =>
                      val next = NormalKontAddress[SchemeExp, HybridTimestamp.T](e, t)
                      val machineFilters = Set[MachineFilterAnnotation](KontAddrPushed(next),
                                                                        KontAddrPopped(oldA, a),
                                                                        EvaluatingExpression(e),
                                                                        FrameFollowed(originFrameCast))
                      Right(StepSucceeded(State(ControlEval(e, env), store2, kstore.extend(next, Kont(frame, a)), next, time.tick(t)),
                                          FilterAnnotations(machineFilters, semanticsFilters),
                                          actions))
                    case EdgeInformation(ActionEval(e, env, store2, _), actions, semanticsFilters) =>
                      val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                                                                        EvaluatingExpression(e),
                                                                        FrameFollowed[ConcreteValue](originFrameCast))
                      Right(StepSucceeded(State(ControlEval(e, env), store2, kstore, a, time.tick(t)),
                                          FilterAnnotations(machineFilters, semanticsFilters),
                                          actions))
                    case EdgeInformation(ActionStepIn(fexp, _, e, env, store2, _, _), actions, semanticsFilters) =>
                      ClosuresCalledMetric.incConcreteClosuresCalled()
                      val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                                                                        EvaluatingExpression(e),
                                                                        FrameFollowed[ConcreteValue](originFrameCast))
                      Right(StepSucceeded(State(ControlEval(e, env), store2, kstore, a, time.tick(t, fexp)),
                                          FilterAnnotations(machineFilters, semanticsFilters),
                                          actions))
                    case EdgeInformation(ActionError(err), actions, semanticsFilters) =>
                      Left(ConcreteMachineOutputError(
                        (System.nanoTime - start) / Math.pow(10, 9),
                        count,
                        err.toString))
                  }
                } else {
                  Left(ConcreteMachineOutputError(
                    (System.nanoTime - start) / Math.pow(10, 9),
                    count,
                    s"execution was not concrete (got ${edges.size} actions instead of 1)"))
                }
              } else {
                Left(ConcreteMachineOutputError(
                  (System.nanoTime - start) / Math.pow(10, 9),
                  count,
                  s"execution was not concrete (got ${frames.size} frames instead of 1)"))
              }
            }

          case ControlError(err) =>
            Left(ConcreteMachineOutputError(
              (System.nanoTime - start) / Math.pow(10, 9),
              count,
              err.toString))
        }

        val stepped = step(control)
        stepped match {
          case Left(output) =>
            output.toDotFile("concrete.dot")
            pointsToAnalysisLauncher.end
            output
          case Right(StepSucceeded(succState, filters, actions)) =>
            def convertFrameFun(concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                abstSem: BaseSchemeSemantics[PAbs, HybridAddress.A, HybridTimestamp.T],
                                convertValueFun: ConcreteValue => PAbs):
            SchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T] =>
            SchemeFrame[PAbs, HybridAddress.A, HybridTimestamp.T] = {
              (frame: SchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]) =>
                concBaseSem.convertAbsInFrame[PAbs](
                  frame,
                  convertValueFun,
                  convertEnv,
                  abstSem)
            }

            pointsToAnalysisLauncher.doConcreteStep(convertValue[PAbs], convertFrameFun, filters, stepCount)
            loop(succState, start, count + 1, graph.addEdge(state, filters, succState))
        }
      }

    }

    def inject(exp: SchemeExp,
               env: Environment[HybridAddress.A],
               sto: Store[HybridAddress.A, ConcreteValue]): State =
      State(ControlEval(exp, env),
            sto,
            KontStore.empty[KontAddr],
            HaltKontAddress,
            time.initial(""))

    val initialState = inject(
      exp,
      Environment.initial[HybridAddress.A](sem.initialEnv),
      Store.initial[HybridAddress.A, ConcreteValue](
        sem.initialStore))
    pointsToAnalysisLauncher.runInitialStaticAnalysis(initialState)

    ClosuresCalledMetric.resetConcreteClosuresCalled()
    loop(initialState,
         System.nanoTime,
         0,
         new Graph[State, FilterAnnotations[SchemeExp, ConcreteValue, HybridAddress.A]]())
  }
}
