import java.io.{BufferedWriter, File, FileWriter}

import backend._
import backend.expression._

import ConcreteConcreteLattice.{ L => ConcreteValue }

class ConcolicMachine[PAbs: IsConvertableLattice: PointsToLatticeInfoProvider](
  analysisLauncher: AnalysisLauncher[PAbs], analysisFlags: AnalysisFlags)(
  implicit unused1: IsSchemeLattice[ConcreteValue]) extends EvalKontMachine[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T] {

  def name = "ConcolicMachine"

  var stepCount: Int = 0

  val errorPathDetector = new ErrorPathDetector[SchemeExp, PAbs, HybridAddress.A, HybridTimestamp.T](analysisLauncher.aam)

  trait ConcolicMachineOutput extends Output {
    def toFile(path: String)(output: GraphOutput): Unit = println("Not generating graph for ConcreteMachine")
  }

  case object ConcolicMachineOutputUnnecessary extends ConcolicMachineOutput {
    def finalValues = Set()
    override def containsFinalValue(v: ConcreteValue) = false
    def timedOut: Boolean = false
    def numberOfStates: Int = 0
    def time: Double = 0
  }

  case class ConcolicMachineOutputTimeout(time: Double, numberOfStates: Int) extends ConcolicMachineOutput {
    def finalValues = Set()
    override def containsFinalValue(v: ConcreteValue) = false
    def timedOut = true
  }

  private def convertValue[AbstL: IsConvertableLattice](abstPrims: SchemePrimitives[HybridAddress.A, AbstL])
                                                       (value: ConcreteValue): AbstL =
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
                   store: Store[HybridAddress.A, ConcreteValue],
                   kstore: KontStore[KontAddr],
                   a: KontAddr,
                   t: HybridTimestamp.T)
      extends ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T]
      with StateTrait[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T] {

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
        abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T]): KontStore[KAddr] = {
      kontStore.map[KAddr](
        (ka) => convertKontAddr(ka, None, mapKontAddress),
        (frame: Frame) =>
          concBaseSem.convertAbsInFrame[AbstL](
            frame.asInstanceOf[ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]],
            convertValue,
            convertEnv,
            abstSem))
    }

    var reached = Set[HybridAddress.A]()

    private def reachesValue(concBaseSem: ConvertableBaseSchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                             sto: Store[HybridAddress.A, ConcreteValue])
                            (value: ConcreteValue): Set[HybridAddress.A] =
      ConcreteConcreteLattice.latticeInfoProvider.reaches[HybridAddress.A](value, reachesEnvironment(concBaseSem, sto), reachesAddress(concBaseSem, sto))

    private def reachesAddress(concBaseSem: ConvertableBaseSchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T],
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

    private def reachesEnvironment(concBaseSem: ConvertableBaseSchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                   sto: Store[HybridAddress.A, ConcreteValue])
                                  (env: Environment[HybridAddress.A]): Set[HybridAddress.A] = {
      var reached: Set[HybridAddress.A] = Set()
      env.forall((tuple) => {
        reached = (reached + tuple._2) ++ reachesAddress(concBaseSem, sto)(
          tuple._2)
        true
      })
      reached
    }

    private def reachesKontAddr[KAddr <: KontAddr](concBaseSem: ConvertableBaseSchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                                   sto: Store[HybridAddress.A, ConcreteValue], kstore: KontStore[KAddr])
                                                  (ka: KAddr): Set[HybridAddress.A] = {
      kstore
        .lookup(ka)
        .foldLeft[Set[HybridAddress.A]](Set())(
        (acc, kont) =>
          acc ++ concBaseSem.frameReaches(
            kont.frame.asInstanceOf[ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]],
            reachesValue(concBaseSem, sto),
            reachesEnvironment(concBaseSem, sto),
            reachesAddress(concBaseSem, sto)) ++ reachesKontAddr(concBaseSem, sto, kstore)(kont.next))
    }

    private def reachesControl[KAddr <: KontAddr](concBaseSem: ConvertableBaseSchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T],
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

    private def reachesStoreAddresses[KAddr <: KontAddr](concBaseSem: ConvertableBaseSchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                                         sto: Store[HybridAddress.A, ConcreteValue])(
                                                         control: ConcolicControl,
                                                         kstore: KontStore[KAddr],
                                                         ka: KAddr): Set[HybridAddress.A] = {
      reachesControl[KAddr](concBaseSem, sto, kstore)(control) ++ reachesKontAddr[KAddr](concBaseSem, sto, kstore)(ka)
    }

    private def garbageCollectStore[KAddr <: KontAddr]
                                   (concBaseSem: ConvertableBaseSchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                    store: Store[HybridAddress.A, ConcreteValue],
                                    control: ConcolicControl,
                                    kstore: KontStore[KAddr],
                                    ka: KAddr): Store[HybridAddress.A, ConcreteValue] = {
      reached = Set()
      val storeAddressReachable = reachesStoreAddresses[KAddr](concBaseSem, store)(control, kstore, ka)
      store.gc(storeAddressReachable)
    }

    private def convertKontAddr[KAddr <: KontAddr](ka: KontAddr, env: Option[Environment[HybridAddress.A]], mapKontAddress: (KontAddr, Option[Environment[HybridAddress.A]]) => KAddr): KAddr = {
      val kontAddrConverter = new DefaultKontAddrConverter[SchemeExp]
      mapKontAddress(kontAddrConverter.convertKontAddr(ka), env)
    }

    def convertState[AbstL: IsConvertableLattice](
        concSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
        abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
        initialKontAddress: KontAddr,
        mapKontAddress: (KontAddr,
                         Option[Environment[HybridAddress.A]]) => KontAddr)
      : (ConvertedControl[SchemeExp, AbstL, HybridAddress.A],
         Store[HybridAddress.A, AbstL],
         KontStore[KontAddr],
         KontAddr,
         HybridTimestamp.T) = {

      val concBaseSem = new ConvertableBaseSchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T](new SchemePrimitives[HybridAddress.A, ConcreteValue])

      val convertValueFun = convertValue[AbstL](abstSem.primitives) _

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
      val convertedKStore = convertKStore[AbstL, KontAddr](mapKontAddress, kstore, convertValueFun, concBaseSem, abstSem)

      val convertedA = convertKontAddr(a, None, mapKontAddress)
      val newT = DefaultHybridTimestampConverter.convertTimestamp(t)
      (convertedControl, convertedStore, convertedKStore, convertedA, newT)
    }
  }

  def eval(exp: SchemeExp, sem: Semantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
           graph: Boolean, timeout:Timeout): Output = ???

  /**
    * Performs the evaluation of an expression, possibly writing the output graph
    * in a file, and returns the set of final states reached
    */
  def concolicEval(programName: String, exp: SchemeExp, sem: ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T],
           graph: Boolean, timeout: Timeout): Output = {
    def loop(state: State, start: Long, count: Int): ConcolicMachineOutput = {

      Logger.log(s"stepCount: $stepCount", Logger.V)
      stepCount += 1

      if (timeout.reached) {
        ConcolicMachineOutputTimeout((System.nanoTime - start) / Math.pow(10, 9), count)
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
            handleFunctionCalled(edgeInfo)
            edgeInfo match {
              case EdgeInformation(ActionConcolicReachedValue(v, optionConcolicValue, store2, _), actions, semanticsFilters) =>
                val machineFilters = Set[MachineFilterAnnotation]()
                Right(StepSucceeded(State(ConcolicControlKont(v, optionConcolicValue), store2, kstore, a, Timestamp[HybridTimestamp.T].tick(t)),
                  FilterAnnotations(machineFilters, semanticsFilters),
                  actions))
                case EdgeInformation(ActionPush(frame, e, env, store2, _), actions, semanticsFilters) =>
                  val next = NormalKontAddress[SchemeExp, HybridTimestamp.T](e, t)
                  val kont = Kont(frame, a)
                  val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
                  Right(StepSucceeded(State(ConcolicControlEval(e, env), store2, kstore.extend(next, kont), next, Timestamp[HybridTimestamp.T].tick(t)),
                                      FilterAnnotations(machineFilters, semanticsFilters),
                                      actions))
                case EdgeInformation(ActionEval(e, env, store2, _), actions, semanticsFilters) =>
                  val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
                  Right(StepSucceeded(State(ConcolicControlEval(e, env), store2, kstore, a, Timestamp[HybridTimestamp.T].tick(t)),
                                      FilterAnnotations(machineFilters, semanticsFilters),
                                      actions))
                case EdgeInformation(ActionStepIn(fexp, _, e, env, store2, _, _), actions, semanticsFilters) =>
                  GlobalSymbolicEnvironment.pushEnvironment()
                  val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
                  Right(StepSucceeded(State(ConcolicControlEval(e, env), store2, kstore, a, Timestamp[HybridTimestamp.T].tick(t, fexp)),
                                      FilterAnnotations(machineFilters, semanticsFilters),
                                      actions))
                case EdgeInformation(ActionError(err), actions, semanticsFilters) =>
                  Left(ConcolicMachineOutputUnnecessary)
              }

          case ConcolicControlKont(v, symbolicValue) =>
            /* pop a continuation */
            if (a == HaltKontAddress) {
              Left(ConcolicMachineOutputUnnecessary)
            } else {
              val frames = kstore.lookup(a)
              if (frames.size == 1) {
                val frame = frames.head.frame
                val originFrameCast = frame.asInstanceOf[ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]]
                val oldA = state.a
                val a = frames.head.next
                val edgeInfo = sem.stepConcolicKont(v, symbolicValue, frame, store, t)
                handleFunctionCalled(edgeInfo)
                edgeInfo match {
                  case EdgeInformation(ActionConcolicReachedValue(v, optionConcolicValue, store2, _), actions, semanticsFilters) =>
                    val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                      FrameFollowed[ConcreteValue](originFrameCast))
                    Right(StepSucceeded(State(ConcolicControlKont(v, optionConcolicValue), store2, kstore, a, Timestamp[HybridTimestamp.T].tick(t)),
                      FilterAnnotations(machineFilters, semanticsFilters),
                      actions))
                  case EdgeInformation(ActionPush(frame, e, env, store2, _), actions, semanticsFilters) =>
                    val next = NormalKontAddress[SchemeExp, HybridTimestamp.T](e, t)
                    val machineFilters = Set[MachineFilterAnnotation](//KontAddrPushed(next),
                      KontAddrPopped(oldA, a),
                      EvaluatingExpression(e),
                      FrameFollowed(originFrameCast))
                    Right(StepSucceeded(State(ConcolicControlEval(e, env), store2, kstore.extend(next, Kont(frame, a)), next, Timestamp[HybridTimestamp.T].tick(t)),
                      FilterAnnotations(machineFilters, semanticsFilters),
                      actions))
                  case EdgeInformation(ActionEval(e, env, store2, _), actions, semanticsFilters) =>
                    val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                      EvaluatingExpression(e),
                      FrameFollowed[ConcreteValue](originFrameCast))
                    Right(StepSucceeded(State(ConcolicControlEval(e, env), store2, kstore, a, Timestamp[HybridTimestamp.T].tick(t)),
                      FilterAnnotations(machineFilters, semanticsFilters),
                      actions))
                  case EdgeInformation(ActionStepIn(fexp, _, e, env, store2, _, _), actions, semanticsFilters) =>
                    GlobalSymbolicEnvironment.pushEnvironment()
                    val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                      EvaluatingExpression(e),
                      FrameFollowed[ConcreteValue](originFrameCast))
                    Right(StepSucceeded(State(ConcolicControlEval(e, env), store2, kstore, a, Timestamp[HybridTimestamp.T].tick(t, fexp)),
                      FilterAnnotations(machineFilters, semanticsFilters),
                      actions))
                  case EdgeInformation(ActionError(err), actions, semanticsFilters) =>
                    Left(ConcolicMachineOutputUnnecessary)
                }
              } else {
                Left(ConcolicMachineOutputUnnecessary)
              }
            }

          case ConcolicControlError(err) =>
            Left(ConcolicMachineOutputUnnecessary)
        }

        val stepped = step(control)
        potentiallyStartRunTimeAnalysis(programName, state)

        stepped match {
          case Left(output) =>
//            analysisLauncher.end
            output
          case Right(StepSucceeded(succState, filters, actions)) =>
            def convertFrameFun(concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                abstSem: ConvertableBaseSchemeSemantics[PAbs, HybridAddress.A, HybridTimestamp.T],
                                convertValueFun: ConcreteValue => PAbs):
            ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T] =>
              ConvertableSchemeFrame[PAbs, HybridAddress.A, HybridTimestamp.T] = {
              (frame: ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]) =>
                concBaseSem.convertAbsInFrame[PAbs](
                  frame,
                  convertValueFun,
                  convertEnv,
                  abstSem)
            }

//            analysisLauncher.doConcreteStep(convertValue[PAbs], convertFrameFun, filters, stepCount)
            loop(succState, start, count + 1)
        }
      }

    }

    def inject(exp: SchemeExp, env: Environment[HybridAddress.A], sto: Store[HybridAddress.A, ConcreteValue]): State = {
      val instrumentedExp = ConcolicInstrumenter.instrument(exp)
      State(ConcolicControlEval(instrumentedExp, env), sto, KontStore.empty[KontAddr], HaltKontAddress, Timestamp[HybridTimestamp.T].initial(""))
    }

    def loopConcolic(initialState: State, nrOfRuns: Int): Unit = {
      ScalaAMReporter.enableConcolic()
      Logger.log(s"\n\nSTART CONCOLIC ITERATION $nrOfRuns ${ScalaAMConcolicSolver.getInputs}", Logger.U)
      ScalaAMReporter.clear(nrOfRuns < 2)
      FunctionsCalledMetric.resetConcreteFunctionsCalled()
      try {
        loop(initialState, System.nanoTime, 0)
      } catch {
        case AbortConcolicRunException =>
      } finally {
        Logger.log(s"END CONCOLIC ITERATION $nrOfRuns", Logger.U)
        Reporter.printTree()
        ScalaAMReporter.printReports()
        val shouldContinue = ScalaAMConcolicSolver.solve
        if (nrOfRuns < ConcolicRunTimeFlags.MAX_CONCOLIC_ITERATIONS && shouldContinue) {
          loopConcolic(initialState, nrOfRuns + 1)
        }
      }
    }

    val initialState = inject(exp, Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial[HybridAddress.A, ConcreteValue]
                             (sem.initialStore))

    // Use initial static analysis to detect paths to errors
    if (ConcolicRunTimeFlags.checkAnalysis) {
      ScalaAMReporter.disableConcolic()
      val analysisResult = analysisLauncher.runInitialStaticAnalysis(initialState, programName)
      val errorPaths = ScalaAMConcolicSolver.handleInitialAnalysisResult[PAbs](errorPathDetector)(analysisResult)
      if (errorPaths.isEmpty) {
        Logger.log("Initial static analysis detected no possible errors: aborting concolic testing", Logger.U)
      } else {
        loopConcolic(initialState, 1)
      }
    } else {
      loopConcolic(initialState, 1)
    }
    ConcolicMachineOutputUnnecessary
  }

  /**
    * If an if-expression has just been encountered (and a corresponding branch node has been made), launch a
    * rum-time static analysis and use the results to further prune the symbolic tree.
    * @param programName
    * @param state
    */
  private def potentiallyStartRunTimeAnalysis(programName: String, state: State): Unit = {
    if (ConcolicRunTimeFlags.shouldStartRunTimeAnalysis && ConcolicRunTimeFlags.checkAnalysis && ConcolicRunTimeFlags.checkRunTimeAnalysis) {
      Logger.log("Starting run-time analysis because divergence in error paths has been detected", Logger.U)
      val analysisResult = startRunTimeAnalysis(programName, state)
      val prefixErrorPath = ScalaAMReporter.getCurrentPath
      ScalaAMConcolicSolver.handleRunTimeAnalysisResult[PAbs](errorPathDetector)(analysisResult, prefixErrorPath)
    }
  }

  private def startRunTimeAnalysis(programName: String, state: State): StaticAnalysisResult = {
    ScalaAMReporter.disableConcolic()
    val currentAddresses: Set[HybridAddress.A] = state.store.toSet.map(_._1)
    val addressConverter = new DefaultHybridAddressConverter[SchemeExp]
    val convertedCurrentAddresses = currentAddresses.map(addressConverter.convertAddress)
    val result = analysisLauncher.runStaticAnalysis(state, Some(stepCount), programName, convertedCurrentAddresses)
    ScalaAMReporter.enableConcolic()
    result
  }

  def printExecutionTimes[Abs: JoinLattice](benchmarks_results_file: String): Unit = {
    val file = new File(benchmarks_results_file)
    val bw = new BufferedWriter(new FileWriter(file, true))
    bw.write(s"${GlobalFlags.CURRENT_PROGRAM}: ${Stopwatch.time}\n")
    bw.close()
  }
}
