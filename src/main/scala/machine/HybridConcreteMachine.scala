import scala.annotation.tailrec

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
                                        graph: Graph[State, List[EdgeAnnotation]])
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
      extends ConcreteTracingProgramState[SchemeExp,
                                          HybridAddress.A,
                                          HybridTimestamp.T] {

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
          new DefaultHybridAddressConverter[SchemeExp]()
            .convertAddress(tuple._1)
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
      val kontAddrConverter = new DefaultKontAddrConverter[SchemeExp, KontAddr]
      kontStore.map[KAddr](
        (ka) => mapKontAddress(kontAddrConverter.convertKontAddr(ka), None),
        (frame: Frame) =>
          concBaseSem.convertAbsInFrame[AbstL](
            frame.asInstanceOf[SchemeFrame[ConcreteValue,
                                           HybridAddress.A,
                                           HybridTimestamp.T]],
            convertValue,
            convertEnv,
            abstSem))
    }

    def convertState[AbstL: IsConvertableLattice,
                     KAddr <: KontAddr: KontAddress](
        concSem: ConvertableSemantics[SchemeExp,
                                      ConcreteValue,
                                      HybridAddress.A,
                                      HybridTimestamp.T],
        abstSem: BaseSchemeSemantics[AbstL,
                                     HybridAddress.A,
                                     HybridTimestamp.T],
        initialKontAddress: KAddr,
        mapKontAddress: (KontAddr,
                         Option[Environment[HybridAddress.A]]) => KAddr)
      : (ConvertedControl[SchemeExp, AbstL, HybridAddress.A],
         Store[HybridAddress.A, AbstL],
         KontStore[KAddr],
         KAddr,
         HybridTimestamp.T) = {

      val concBaseSem =
        new SchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T](
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

      val convertedStore = convertStore(store, convertValueFun)

      val convertedKStore = convertKStore[AbstL, KAddr](mapKontAddress,
                                                        kstore,
                                                        convertValueFun,
                                                        concBaseSem,
                                                        abstSem)

      val convertedA = mapKontAddress(a, None)
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
    def loop(state: State, start: Long, count: Int, graph: Graph[State, List[EdgeAnnotation]]): ConcreteMachineOutput = {

      tracingFlags.RUNTIME_ANALYSIS_INTERVAL match {
        case NoRunTimeAnalysis =>
        case RunTimeAnalysisEvery(analysis_interval) =>
          if (stepCount % analysis_interval == 0) {
            Logger.log(s"stepCount: $stepCount", Logger.U)
            pointsToAnalysisLauncher.filterReachable(stepCount)
//            pointsToAnalysisLauncher.runStaticAnalysis(state, Some(stepCount))
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

        def maybeAddReachedConcreteValue(v: ConcreteConcreteLattice.L,
                                         edgeInfos: List[EdgeAnnotation]): List[EdgeAnnotation] = {
          if (edgeInfos.exists({
            case FrameFollowed(frame) =>
              frame.meaningfullySubsumes
            case _ =>
              false
          })) {
            edgeInfos
          }
          else {
//            ReachedConcreteValue(v) ::
            edgeInfos
          }
        }

        def step(control: Control): Either[ConcreteMachineOutput, (State, List[EdgeAnnotation])] = control match {
          case ControlEval(e, env) =>
            val edges = sem.stepEval(e, env, store, t)
            if (edges.size == 1) {
              edges.head match {
                case ActionReachedValue(v, store2, _) =>
                  Right(State(ControlKont(v), store2, kstore, a, time.tick(t)),
                        maybeAddReachedConcreteValue(v, Nil))
                case ActionPush(frame, e, env, store2, _) =>
                  val frameCast = frame.asInstanceOf[SchemeFrame[ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]]
                  val next = NormalKontAddress[SchemeExp, HybridTimestamp.T](e, t)
                  Right(State(ControlEval(e, env), store2, kstore.extend(next, Kont(frame, a)), next, time.tick(t)),
                        KontAddrPushed(next) :: EvaluatingExpression(e) :: Nil)
                case ActionEval(e, env, store2, _) =>
                  Right(State(ControlEval(e, env), store2, kstore, a, time.tick(t)),
                        EvaluatingExpression(e) :: Nil)
                case ActionStepIn(fexp, _, e, env, store2, _, _) =>
                  Right(State(ControlEval(e, env), store2, kstore, a, time.tick(t, fexp)),
                        EvaluatingExpression(e) :: Nil)
                case ActionError(err) =>
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
                val originFrameCast = frame.asInstanceOf[SchemeFrame[ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]]
                val oldA = state.a
                val a = frames.head.next
                val edges = sem.stepKont(v, frame, store, t)
                if (edges.size == 1) {
                  edges.head match {
                    case ActionReachedValue(v, store2, _) =>
                      Right((State(ControlKont(v), store2, kstore, a, time.tick(t)),
                            KontAddrPopped(oldA, a) :: maybeAddReachedConcreteValue(v,
                              FrameFollowed[ConcreteConcreteLattice.L](originFrameCast) :: Nil)))
                    case ActionPush(frame, e, env, store2, _) =>
                      val destinationFrameCast = frame.asInstanceOf[SchemeFrame[ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]]
                      val next = NormalKontAddress[SchemeExp, HybridTimestamp.T](e, t)
                      Right((State(ControlEval(e, env), store2, kstore.extend(next, Kont(frame, a)), next, time.tick(t)),
                            KontAddrPushed(next) ::
                            KontAddrPopped(oldA, a) ::
                            EvaluatingExpression(e) ::
                            FrameFollowed(originFrameCast) ::
                            Nil))
                    case ActionEval(e, env, store2, _) =>
                      Right(State(ControlEval(e, env),
                        store2,
                        kstore,
                        a,
                        time.tick(t)),
                            KontAddrPopped(oldA, a) ::
                            EvaluatingExpression(e) ::
                            FrameFollowed[ConcreteConcreteLattice.L](originFrameCast) ::
                            Nil)
                    case ActionStepIn(fexp, _, e, env, store2, _, _) =>
                      Right(State(ControlEval(e, env),
                        store2,
                        kstore,
                        a,
                        time.tick(t, fexp)),
                            KontAddrPopped(oldA, a) ::
                            EvaluatingExpression(e) ::
                            FrameFollowed[ConcreteConcreteLattice.L](originFrameCast) ::
                            Nil)
                    case ActionError(err) =>
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
          case Right((succState, edgeInfo)) =>
            def convertFrameFun(concBaseSem: ConvertableSemantics[SchemeExp,
                                                                  ConcreteConcreteLattice.L,
                                                                  HybridAddress.A,
                                                                  HybridTimestamp.T],
                                abstSem: BaseSchemeSemantics[PAbs, HybridAddress.A, HybridTimestamp.T],
                                convertValueFun: ConcreteConcreteLattice.L => PAbs):
            SchemeFrame[ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T] =>
            SchemeFrame[PAbs, HybridAddress.A, HybridTimestamp.T] = {
              (frame: SchemeFrame[ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T]) =>
                concBaseSem.convertAbsInFrame[PAbs](
                  frame,
                  convertValueFun,
                  convertEnv,
                  abstSem)
            }

            pointsToAnalysisLauncher.doConcreteStep(convertValue[PAbs], convertFrameFun, edgeInfo, stepCount)
            loop(succState, start, count + 1, graph.addEdge(state, edgeInfo, succState))
        }
      }

    }

    def inject(exp: SchemeExp,
               env: Environment[HybridAddress.A],
               sto: Store[HybridAddress.A, ConcreteConcreteLattice.L]): State =
      State(ControlEval(exp, env),
            sto,
            KontStore.empty[KontAddr],
            HaltKontAddress,
            time.initial(""))

    val initialState = inject(
      exp,
      Environment.initial[HybridAddress.A](sem.initialEnv),
      Store.initial[HybridAddress.A, ConcreteConcreteLattice.L](
        sem.initialStore))
    pointsToAnalysisLauncher.runInitialStaticAnalysis(initialState)

    loop(initialState, System.nanoTime, 0, new Graph[State, List[EdgeAnnotation]]())
  }
}
