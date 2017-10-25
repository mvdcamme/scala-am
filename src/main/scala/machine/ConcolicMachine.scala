import ConcreteConcreteLattice.{ConcreteValue, lattice}

class ConcolicMachine[PAbs: IsConvertableLattice: PointsToableLatticeInfoProvider](
    pointsToAnalysisLauncher: PointsToAnalysisLauncher[PAbs],
    analysisFlags: AnalysisFlags)(
    implicit unused1: IsSchemeLattice[ConcreteConcreteLattice.L])
    extends EvalKontMachine[SchemeExp,
                            ConcreteConcreteLattice.L,
                            HybridAddress.A,
                            HybridTimestamp.T] {

  def name = "ConcolicMachine"

  var stepCount: Integer = 0

  val errorPathDetector = new ErrorPathDetector[SchemeExp, PAbs, HybridAddress.A, HybridTimestamp.T](pointsToAnalysisLauncher.aam)

  trait ConcolicMachineOutput extends Output[ConcreteConcreteLattice.L] {
    def toDotFile(path: String) =
      println("Not generating graph for ConcreteMachine")
  }

  case object ConcolicMachineOutputUnnecessary
    extends ConcolicMachineOutput {
    def finalValues = Set()
    def containsFinalValue(v: ConcreteConcreteLattice.L) = false
    def timedOut: Boolean = false
    def numberOfStates: Int = 0
    def time: Double = 0
  }

  case class ConcolicMachineOutputError(time: Double,
                                        numberOfStates: Int,
                                        err: String)
      extends ConcolicMachineOutput {
    def finalValues = {
      println(s"Execution failed: $err")
      Set()
    }
    def containsFinalValue(v: ConcreteConcreteLattice.L) = false
    def timedOut = false
  }
  case class ConcolicMachineOutputTimeout(time: Double, numberOfStates: Int)
      extends ConcolicMachineOutput {
    def finalValues = Set()
    def containsFinalValue(v: ConcreteConcreteLattice.L) = false
    def timedOut = true
  }
  case class ConcolicMachineOutputValue(time: Double,
                                        numberOfStates: Int,
                                        v: ConcreteConcreteLattice.L,
                                        graph: Graph[State, FilterAnnotations[SchemeExp, ConcreteValue, HybridAddress.A]])
      extends ConcolicMachineOutput {
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

  trait ConcolicControl extends Control {
    override def subsumes(that: Control): Boolean = false
  }

  case class ConcolicControlEval(exp: SchemeExp, env: Environment[HybridAddress.A]) extends ConcolicControl
  case class ConcolicControlKont(v: ConcreteValue, concolicValue: Option[ConcolicExpression]) extends ConcolicControl
  case class ConcolicControlError(error: SemanticError) extends ConcolicControl

  case class State(control: ConcolicControl,
                   store: Store[HybridAddress.A, ConcreteConcreteLattice.L],
                   kstore: KontStore[KontAddr],
                   a: KontAddr,
                   t: HybridTimestamp.T)
      extends ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T]
      with StateTrait[SchemeExp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T] {

    def halted = control match {
      case ConcolicControlError(_) => true
      case ConcolicControlKont(_, _) => a == HaltKontAddress
      case _ => false
    }

    def graphNodeColor = control match {
      case ConcolicControlEval(_, _) => Colors.Green
      case ConcolicControlKont(_, _) => Colors.Pink
      case ConcolicControlError(_) => Colors.Red
    }

    private def convertStore[AbstL: IsConvertableLattice](
        store: Store[HybridAddress.A, ConcreteValue],
        convertValue: ConcreteValue => AbstL): Store[HybridAddress.A, AbstL] = {
      var valuesConvertedStore = Store.empty[HybridAddress.A, AbstL]
      def addToNewStore(tuple: (HybridAddress.A, ConcreteValue)): Boolean = {
        val convertedAddress = new DefaultHybridAddressConverter[SchemeExp]().convertAddress(tuple._1)
        val convertedValue = convertValue(tuple._2)
        valuesConvertedStore = valuesConvertedStore.extend(convertedAddress, convertedValue)
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
                                                  control: ConcolicControl)
    : Set[HybridAddress.A] =
      control match {
        case ConcolicControlEval(_, env) =>
          reachesEnvironment(concBaseSem, sto)(env)
        case ConcolicControlKont(value, _) =>
          reachesValue(concBaseSem, sto)(value)
        case ConcolicControlError(_) => Set()
      }

    private def reachesStoreAddresses[KAddr <: KontAddr](concBaseSem: BaseSchemeSemantics[ConcreteValue,
                                                            HybridAddress.A,
                                                            HybridTimestamp.T],
                                                         sto: Store[HybridAddress.A, ConcreteValue])(
                                                         control: ConcolicControl,
                                                         kstore: KontStore[KAddr],
                                                         ka: KAddr): Set[HybridAddress.A] = {
      reachesControl[KAddr](concBaseSem, sto, kstore)(control) ++ reachesKontAddr[KAddr](concBaseSem, sto, kstore)(ka)
    }

    private def garbageCollectStore[KAddr <: KontAddr]
                                   (concBaseSem: BaseSchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                    store: Store[HybridAddress.A, ConcreteValue],
                                    control: ConcolicControl,
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
          case ConcolicControlEval(exp, env) =>
            ConvertedControlEval[SchemeExp, AbstL, HybridAddress.A](
              exp,
              convertEnv(env))
          case ConcolicControlKont(v, _) =>
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

  override def eval(
    programName: String,
    exp: SchemeExp,
    sem: Semantics[SchemeExp, lattice.LSet, HybridAddress.A, HybridTimestamp.T],
    graph: Boolean,
    timeout: Option[Long]
  ): Output[lattice.LSet] = ???

  /**
    * Performs the evaluation of an expression, possibly writing the output graph
    * in a file, and returns the set of final states reached
    */
  def eval(programName: String,
           exp: SchemeExp,
           sem: ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T],
           graph: Boolean,
           timeout: Option[Long]): Output[ConcreteConcreteLattice.L] = {
    def loop(state: State,
             start: Long,
             count: Int,
             graph: Graph[State, FilterAnnotations[SchemeExp, ConcreteValue, HybridAddress.A]]): ConcolicMachineOutput = {

      Logger.log(s"stepCount: $stepCount", Logger.V)
      val currentAddresses: Set[HybridAddress.A] = state.store.toSet.map(_._1)
      analysisFlags.runTimeAnalysisInterval match {
        case NoRunTimeAnalysis =>
        case RunTimeAnalysisEvery(analysisInterval) =>
          if (stepCount % analysisInterval == 0) {
            startRunTimeAnalysis(programName, state)
          }
      }
      analysisFlags.incrementalAnalysisInterval match {
        case NoIncrementalAnalysis =>
        case IncrementalAnalysisEvery(analysisInterval) =>
          if (stepCount % analysisInterval == 0) {
            val addressConverter = new DefaultHybridAddressConverter[SchemeExp]
            val convertedCurrentAddresses = currentAddresses.map(addressConverter.convertAddress)
            pointsToAnalysisLauncher.incrementalAnalysis(state, stepCount, programName, convertedCurrentAddresses)
          }
      }

      stepCount += 1

      if (timeout.exists(System.nanoTime - start > _)) {
        ConcolicMachineOutputTimeout(
          (System.nanoTime - start) / Math.pow(10, 9),
          count)
      } else {
        val control = state.control
        val store = state.store
        val kstore = state.kstore
        val a = state.a
        val t = state.t

        def handleFunctionCalled(edgeInfo: EdgeInformation[SchemeExp, ConcreteValue, HybridAddress.A]): Unit = {
          if (edgeInfo.actions.exists(_.marksFunctionCall)) {
            FunctionsCalledMetric.incConcreteFunctionsCalled()
          }
        }

        case class StepSucceeded(state: State,
                                 filters: FilterAnnotations[SchemeExp, ConcreteValue, HybridAddress.A],
                                 actionTs: List[ActionReplay[SchemeExp, ConcreteValue, HybridAddress.A]])

        def step(control: ConcolicControl): Either[ConcolicMachineOutput, StepSucceeded] = control match {
          case ConcolicControlEval(e, env) =>
            val edgeInfo = sem.stepConcolicEval(e, env, store, t)
            startRunTimeAnalysisIfIfEncountered(programName, state)
            handleFunctionCalled(edgeInfo)
            edgeInfo match {
              case EdgeInformation(ActionReachedValue(v, optionConcolicValue, store2, _), actions, semanticsFilters) =>
                val machineFilters = Set[MachineFilterAnnotation]()
                Right(StepSucceeded(State(ConcolicControlKont(v, optionConcolicValue), store2, kstore, a, time.tick(t)),
                  FilterAnnotations(machineFilters, semanticsFilters),
                  actions))
                case EdgeInformation(ActionPush(frame, e, env, store2, _), actions, semanticsFilters) =>
                  val next = NormalKontAddress[SchemeExp, HybridTimestamp.T](e, t)
                  val kont = Kont(frame, a)
                  val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
                  Right(StepSucceeded(State(ConcolicControlEval(e, env), store2, kstore.extend(next, kont), next, time.tick(t)),
                                      FilterAnnotations(machineFilters, semanticsFilters),
                                      actions))
                case EdgeInformation(ActionEval(e, env, store2, _), actions, semanticsFilters) =>
                  val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
                  Right(StepSucceeded(State(ConcolicControlEval(e, env), store2, kstore, a, time.tick(t)),
                                      FilterAnnotations(machineFilters, semanticsFilters),
                                      actions))
                case EdgeInformation(ActionStepIn(fexp, _, e, env, store2, _, _), actions, semanticsFilters) =>
                  Reporter.pushEnvironment()
                  val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
                  Right(StepSucceeded(State(ConcolicControlEval(e, env), store2, kstore, a, time.tick(t, fexp)),
                                      FilterAnnotations(machineFilters, semanticsFilters),
                                      actions))
                case EdgeInformation(ActionError(err), actions, semanticsFilters) =>
                  Left(ConcolicMachineOutputError(
                    (System.nanoTime - start) / Math.pow(10, 9),
                    count,
                    err.toString))
              }

          case ConcolicControlKont(v, symbolicValue) =>
            /* pop a continuation */
            if (a == HaltKontAddress) {
              Left(ConcolicMachineOutputValue(
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
                val edgeInfo = sem.stepConcolicKont(v, symbolicValue, frame, store, t)
                startRunTimeAnalysisIfIfEncountered(programName, state)
                handleFunctionCalled(edgeInfo)
                edgeInfo match {
                  case EdgeInformation(ActionReachedValue(v, optionConcolicValue, store2, _), actions, semanticsFilters) =>
                    val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                      FrameFollowed[ConcreteValue](originFrameCast))
                    Right(StepSucceeded(State(ConcolicControlKont(v, optionConcolicValue), store2, kstore, a, time.tick(t)),
                      FilterAnnotations(machineFilters, semanticsFilters),
                      actions))
                  case EdgeInformation(ActionPush(frame, e, env, store2, _), actions, semanticsFilters) =>
                    val next = NormalKontAddress[SchemeExp, HybridTimestamp.T](e, t)
                    val machineFilters = Set[MachineFilterAnnotation](//KontAddrPushed(next),
                      KontAddrPopped(oldA, a),
                      EvaluatingExpression(e),
                      FrameFollowed(originFrameCast))
                    Right(StepSucceeded(State(ConcolicControlEval(e, env), store2, kstore.extend(next, Kont(frame, a)), next, time.tick(t)),
                      FilterAnnotations(machineFilters, semanticsFilters),
                      actions))
                  case EdgeInformation(ActionEval(e, env, store2, _), actions, semanticsFilters) =>
                    val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                      EvaluatingExpression(e),
                      FrameFollowed[ConcreteValue](originFrameCast))
                    Right(StepSucceeded(State(ConcolicControlEval(e, env), store2, kstore, a, time.tick(t)),
                      FilterAnnotations(machineFilters, semanticsFilters),
                      actions))
                  case EdgeInformation(ActionStepIn(fexp, _, e, env, store2, _, _), actions, semanticsFilters) =>
                    Reporter.pushEnvironment()
                    val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                      EvaluatingExpression(e),
                      FrameFollowed[ConcreteValue](originFrameCast))
                    Right(StepSucceeded(State(ConcolicControlEval(e, env), store2, kstore, a, time.tick(t, fexp)),
                      FilterAnnotations(machineFilters, semanticsFilters),
                      actions))
                  case EdgeInformation(ActionError(err), actions, semanticsFilters) =>
                    Left(ConcolicMachineOutputError(
                      (System.nanoTime - start) / Math.pow(10, 9),
                      count,
                      err.toString))
                }
              } else {
                Left(ConcolicMachineOutputError(
                  (System.nanoTime - start) / Math.pow(10, 9),
                  count,
                  s"execution was not concrete (got ${frames.size} frames instead of 1)"))
              }
            }

          case ConcolicControlError(err) =>
            Left(ConcolicMachineOutputError(
              (System.nanoTime - start) / Math.pow(10, 9),
              count,
              err.toString))
        }

        val stepped = step(control)
        stepped match {
          case Left(output) =>
            output.toDotFile("concrete.dot")
            pointsToAnalysisLauncher.end()
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
               sto: Store[HybridAddress.A, ConcreteValue]): State = {
      val instrumentedExp = ConcolicInstrumenter.instrument(exp)
      State(ConcolicControlEval(instrumentedExp, env),
            sto,
            KontStore.empty[KontAddr],
            HaltKontAddress,
            time.initial(""))
    }

    @scala.annotation.tailrec
    def loopConcolic(initialState: State, nrOfRuns: Int): ConcolicMachineOutput = {
      Reporter.clear(nrOfRuns < 2)
      Logger.log(s"\n\nCONCOLIC ITERATION ${ConcolicSolver.getInputs}", Logger.U)
      FunctionsCalledMetric.resetConcreteFunctionsCalled()
      val result = loop(initialState,
                        System.nanoTime,
                        0,
                        new Graph[State, FilterAnnotations[SchemeExp, ConcreteValue, HybridAddress.A]]())
      Reporter.printTree()
      Reporter.printReports()
      val shouldContinue = ConcolicSolver.solve
      if (nrOfRuns < ConcolicRunTimeFlags.MAX_CONCOLIC_ITERATIONS && shouldContinue) {
        loopConcolic(initialState, nrOfRuns + 1)
      } else {
        result
      }
    }

    val initialState = inject(exp, Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial[HybridAddress.A, ConcreteValue]
                             (sem.initialStore))
    Reporter.disableConcolic()
    val analysisResult = pointsToAnalysisLauncher.runInitialStaticAnalysis(initialState, programName)
    Reporter.enableConcolic()

    // Use initial static analysis to detect paths to errors
    if (ConcolicRunTimeFlags.checkAnalysis) {
      val errorPaths = ConcolicSolver.handleAnalysisResult[PAbs](errorPathDetector)(analysisResult, Reporter.getRoot, true)
      if (errorPaths.isEmpty) {
        Logger.log("Initial static analysis detected no possible errors: aborting concolic testing", Logger.U)
        ConcolicMachineOutputUnnecessary
      } else {
        loopConcolic(initialState, 1)
      }
    } else {
      loopConcolic(initialState, 1)
    }
  }

  /**
    * If an if-expression has just been encountered (and a corresponding branch node has been made), launch a
    * rum-time static analysis and use the results to further prune the symbolic tree.
    * @param programName
    * @param state
    */
  private def startRunTimeAnalysisIfIfEncountered(programName: String, state: State): Unit = {
    if (ConcolicRunTimeFlags.wasIfEncountered && ConcolicRunTimeFlags.checkAnalysis && ConcolicRunTimeFlags.checkRunTimeAnalysis) {
      val optCurrentNode = Reporter.getCurrentNode
      val optBranchFollowedCurrentNode = optCurrentNode
      assert(state.control.isInstanceOf[ControlKont])
      val analysisResult = startRunTimeAnalysis(programName, state)
      ConcolicSolver.handleAnalysisResult[PAbs](errorPathDetector)(analysisResult, optBranchFollowedCurrentNode, false)
    }
  }

  private def startRunTimeAnalysis(programName: String, state: State): StaticAnalysisResult = {
    Reporter.disableConcolic()
    val currentAddresses: Set[HybridAddress.A] = state.store.toSet.map(_._1)
    val addressConverter = new DefaultHybridAddressConverter[SchemeExp]
    val convertedCurrentAddresses = currentAddresses.map(addressConverter.convertAddress)
    val result = pointsToAnalysisLauncher.runStaticAnalysis(state, Some(stepCount), programName, convertedCurrentAddresses)
    Reporter.enableConcolic()
    result
  }
}
