import java.io.{BufferedWriter, File, FileWriter}

import backend.expression._
import ConcreteConcreteLattice.{L => ConcreteValue}
import backend._
import concolic.SymbolicEnvironment

case class Reached[Addr : Address](addressesReachable: Set[Addr], preciseAddresses: Set[Addr]) {
  def ++(other: Reached[Addr]): Reached[Addr] = {
    Reached(this.addressesReachable ++ other.addressesReachable, this.preciseAddresses ++ other.preciseAddresses)
  }
}
object Reached {
  def empty[Addr : Address] = Reached[Addr](Set(), Set())
}

class ConcolicMachine[PAbs: IsConvertableLattice: LatticeInfoProvider, PCElementUsed, NodeExtraInfo](
  val analysisLauncher: AnalysisLauncher[PAbs],
  val analysisFlags: AnalysisFlags,
  val reporter: ScalaAMReporter[PCElementUsed, NodeExtraInfo],
  val concolicFlags: ConcolicRunTimeFlags)
 (implicit unused1: IsSchemeLattice[ConcreteValue])
  extends EvalKontMachine[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T] with RTAnalysisStarter[NodeExtraInfo] {

  def name = "ConcolicMachine"

  private val rtAnalysis = new LaunchAnalyses[PAbs, PCElementUsed](analysisLauncher, reporter)
  private val semanticsConcolicHelper = new SemanticsConcolicHelper[PCElementUsed, NodeExtraInfo](this, reporter)

  private var currentState: Option[State] = None

  var stepCount: Int = 0
  var currentConcolicRun: Int = 0

  trait ConcolicMachineOutput extends Output {
    def toFile(path: String)(output: GraphOutput): Unit = println("Not generating graph for ConcolicMachine")
    def allInputs: List[List[(ConcolicInput, Int)]]
    def allPathConstraints: List[PathConstraintWith[PCElementUsed]]
  }
  case class ConcolicMachineOutputFinished(allInputs: List[List[(ConcolicInput, Int)]], allPathConstraints: List[PathConstraintWith[PCElementUsed]]) extends ConcolicMachineOutput {
    def finalValues = Set()
    override def containsFinalValue(v: ConcreteValue) = false
    def timedOut: Boolean = false
    def numberOfStates: Int = 0
    def time: Double = 0
  }
  case object ConcolicMachineTestingNotStarted extends ConcolicMachineOutput {
    def allInputs: List[List[(ConcolicInput, Int)]] = Nil
    def allPathConstraints: List[PathConstraintWith[PCElementUsed]] = Nil
    def finalValues = Set()
    def numberOfStates: Int = 0
    def time: Double = 0
    def timedOut = false
  }

  sealed trait ConcolicIterationOutput
  case object ConcolicIterationNoErrors extends ConcolicIterationOutput
  case object ConcolicIterationTimedOut extends ConcolicIterationOutput
  case class ConcolicIterationErrorEncountered(err: SemanticError) extends ConcolicIterationOutput

  private def convertValue[AbstL: IsConvertableLattice](abstPrims: SchemePrimitives[HybridAddress.A, AbstL])
                                                       (value: ConcreteValue, makeValuePrecise: Boolean): AbstL = {
    val addrConverter = new DefaultHybridAddressConverter[SchemeExp]
    ConcreteConcreteLattice.convert[SchemeExp, AbstL, HybridAddress.A](value, addrConverter, convertEnv, abstPrims, makeValuePrecise)
  }

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

  trait ConcolicControl extends Control[SchemeExp, ConcreteValue, HybridAddress.A] {
    override def subsumes(that: Control[SchemeExp, ConcreteValue, HybridAddress.A]): Boolean = false
  }

  case class ConcolicControlEval(exp: SchemeExp, env: Environment[HybridAddress.A], symEnv: SymbolicEnvironment) extends ConcolicControl
  case class ConcolicControlKont(v: ConcreteValue, concolicValue: Option[ConcolicExpression]) extends ConcolicControl
  case class ConcolicControlError(error: SemanticError) extends ConcolicControl

  case class State(control: ConcolicControl, store: Store[HybridAddress.A, ConcreteValue],
                   kstore: KontStore[KontAddr], a: KontAddr, t: HybridTimestamp.T)
      extends ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T]
      with StateTrait[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T] {

    def isErrorState: Boolean = control match {
      case _: ConcolicControlError => true
      case _ => false
    }
    def isUserErrorState: Boolean = control match {
      case ConcolicControlError(UserError(_, _)) => true
      case _ => false
    }

    def addressesReachable: Set[HybridAddress.A] = {
      store.toSet.map(_._1)
    }

    def halted: Boolean = control match {
      case ConcolicControlError(_) => true
      case ConcolicControlKont(_, _) => a == HaltKontAddress
      case _ => false
    }

    def graphNodeColor: Color = control match {
      case ConcolicControlEval(_, _, _) => Colors.Green
      case ConcolicControlKont(_, _) => Colors.Pink
      case ConcolicControlError(_) => Colors.Red
    }

    private def convertStoreWithExactSymVariables[AbstL: IsConvertableLattice](
      store: Store[HybridAddress.A, ConcreteValue],
      convertValue: (ConcreteValue, Boolean) => AbstL,
      preciseVariablesAddresses: Set[HybridAddress.A]): Store[HybridAddress.A, AbstL] = {

      var valuesConvertedStore = Store.empty[HybridAddress.A, AbstL]
      def addToNewStore(tuple: (HybridAddress.A, ConcreteValue)): Boolean = {
        val (address: HybridAddress.A, value: ConcreteValue) = tuple
        val convertedAddress = new DefaultHybridAddressConverter[SchemeExp]().convertAddress(address)
        val convertedValue = convertValue(value, preciseVariablesAddresses.contains(address))
        valuesConvertedStore = valuesConvertedStore.extend(convertedAddress, convertedValue)
        true
      }
      store.forall(addToNewStore)
      valuesConvertedStore
    }

    private def convertKStore[AbstL: IsConvertableLattice](
        kontStore: KontStore[KontAddr], initialAddress: KontAddr, convertValue: ConcreteValue => AbstL,
        abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T]): KontStore[KontAddr] = {
      @scala.annotation.tailrec
      def loop(address: KontAddr, convertedKStore: KontStore[KontAddr]): KontStore[KontAddr] = {
        if (address == HaltKontAddress) {
          convertedKStore
        } else {
          val convertedAddress = convertKontAddr(address)
          val konts = kontStore.lookup(address)
          assert(konts.size == 1) // Concolic machine uses concrete execution, so there should only be one Kont
          val next = konts.head.next
          val convertedNext = convertKontAddr(next)
          val frame = konts.head.frame
          val castedFrame = frame.asInstanceOf[ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]]
          val convertedFrame = castedFrame.convert[AbstL](convertValue, convertEnv, abstSem)
          val newConvertedKStore = convertedKStore.extend(convertedAddress, Kont(convertedFrame, convertedNext))
          loop(next, newConvertedKStore)
        }
      }
      loop(initialAddress, TimestampedKontStore(Map(), 0))
    }

    private var reached = Set[HybridAddress.A]()

    private def reachesValue(sto: Store[HybridAddress.A, ConcreteValue], pathConstraint: PathConstraint)
                            (value: ConcreteValue): Reached[HybridAddress.A] =
      ConcreteConcreteLattice.latticeInfoProvider.reaches[HybridAddress.A](value, reachesEnvironment(sto, pathConstraint), reachesAddress(sto, pathConstraint))

    private def reachesAddress(sto: Store[HybridAddress.A, ConcreteValue], pathConstraint: PathConstraint)
                              (address: HybridAddress.A): Reached[HybridAddress.A] = {
      if (! reached.contains(address)) {
        reached = reached + address
        val lookedUp = sto.lookup(address).map(reachesValue(sto, pathConstraint)).getOrElse(Reached.empty[HybridAddress.A])
        Reached(Set(address) ++ lookedUp.addressesReachable, lookedUp.preciseAddresses)
      } else {
        Reached.empty[HybridAddress.A]
      }
    }

    private def reachesEnvironment(sto: Store[HybridAddress.A, ConcreteValue], pathConstraint: PathConstraint)
                                  (env: Environment[HybridAddress.A], symEnv: SymbolicEnvironment): Reached[HybridAddress.A] = {

      val exactSymVariables = ExactSymbolicVariablesFinder.findExactSymbolicVariables(env, symEnv, pathConstraint)
      Logger.log(s"exactSymVariables are $exactSymVariables", Logger.I)
      def shouldMakeValPrecise(variable: String): Boolean = {
        /*
         * If a variable is not part of the symbolic environment, the variable's value was not computed via
         * operations supported by the concolic tester (e.g., because the value is a string).
         * We therefore *assume* [TODO This is not actually correct: counterexample:
         * (let ((a 0) (i (random))) (if (< i 0) (set! a 1) (set! a 2)))
         *    -> a is exact but should not be made precise? Although, given the path constraint, a is of course constant]
         * for the moment that this value is independent of the value of the input variables,
         * and can therefore be precisely abstracted.
         *
         *
         */
        exactSymVariables.contains(variable) || ! concolic.containsVariable(variable, symEnv)
      }

      var preciseAddresses = exactSymVariables.map(env.lookup(_).get)

      var reached: Set[HybridAddress.A] = Set()
      env.forall((tuple) => {
        val reachedResult = reachesAddress(sto, pathConstraint)(tuple._2)
        reached = (reached + tuple._2) ++ reachedResult.addressesReachable
        preciseAddresses ++= reachedResult.preciseAddresses
        true
      })
      Reached(reached, preciseAddresses)
    }

    private def reachesKontAddr[KAddr <: KontAddr](sto: Store[HybridAddress.A, ConcreteValue], kstore: KontStore[KAddr],
                                                   ka: KAddr, pathConstraint: PathConstraint): Reached[HybridAddress.A] = {
      kstore.lookup(ka).foldLeft[Reached[HybridAddress.A]](Reached.empty[HybridAddress.A])((acc, kont) => {
        val castedFrame = kont.frame.asInstanceOf[ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]]
        acc ++ castedFrame.reaches(reachesValue(sto, pathConstraint), reachesEnvironment(sto, pathConstraint), reachesAddress(sto, pathConstraint)) ++ reachesKontAddr(sto, kstore, kont.next, pathConstraint)
      })
    }

    private def reachesControl[KAddr <: KontAddr](sto: Store[HybridAddress.A, ConcreteValue],
                                                  kstore: KontStore[KAddr], control: ConcolicControl,
                                                  pathConstraint: PathConstraint): Reached[HybridAddress.A] =
      control match {
        case ConcolicControlEval(_, env, symEnv) =>
          reachesEnvironment(sto, pathConstraint)(env, symEnv)
        case ConcolicControlKont(value, _) =>
          reachesValue(sto, pathConstraint)(value)
        case ConcolicControlError(_) => Reached.empty[HybridAddress.A]
      }

    private def reachesStoreAddresses[KAddr <: KontAddr](sto: Store[HybridAddress.A, ConcreteValue],
                                                         control: ConcolicControl, kstore: KontStore[KAddr],
                                                         ka: KAddr, pathConstraint: PathConstraint): Reached[HybridAddress.A] = {
      reachesControl[KAddr](sto, kstore, control, pathConstraint) ++ reachesKontAddr[KAddr](sto, kstore, ka, pathConstraint)
    }

    private def garbageCollectStore[KAddr <: KontAddr]
                                   (store: Store[HybridAddress.A, ConcreteValue], control: ConcolicControl, kstore: KontStore[KAddr],
                                    ka: KAddr, pathConstraint: PathConstraint): (Store[HybridAddress.A, ConcreteValue], Set[HybridAddress.A]) = {
      reached = Set()
      val result: Reached[HybridAddress.A] = reachesStoreAddresses[KAddr](store, control, kstore, ka, pathConstraint)
      val gcedStore = store.gc(result.addressesReachable)
      (gcedStore, result.preciseAddresses)
    }

    private def convertKontAddr(ka: KontAddr): KontAddr = {
      val kontAddrConverter = new DefaultKontAddrConverter[SchemeExp]
      kontAddrConverter.convertKontAddr(ka)
    }

    private def converstateWithExactSymVariables[AbstL: IsConvertableLattice](
      abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
      pathConstraint: PathConstraint): Conversion[AbstL] = {

      val convertValueFun = convertValue[AbstL](abstSem.primitives) _

      val convertedControl: ConvertedControl[SchemeExp, AbstL, HybridAddress.A] = control match {
        case ConcolicControlEval(exp, env, _) =>
          ConvertedControlEval[SchemeExp, AbstL, HybridAddress.A](exp, convertEnv(env))
        case ConcolicControlKont(v, _) =>
          ConvertedControlKont[SchemeExp, AbstL, HybridAddress.A](convertValueFun(v, false))
      }

      val (gcedStore, preciseVariablesAddresses) = garbageCollectStore(store, control, kstore, a, pathConstraint)
      val convertedStore = convertStoreWithExactSymVariables(gcedStore, convertValueFun, preciseVariablesAddresses)

      val convertedKStore = convertKStore[AbstL](kstore, a, convertValueFun(_, false), abstSem)
      val convertedA = convertKontAddr(a)
      val newT = DefaultHybridTimestampConverter.convertTimestamp(t)
      (convertedControl, convertedStore, convertedKStore, convertedA, newT)

    }

    def convertState[AbstL: IsConvertableLattice](
        abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
        pathConstraint: PathConstraint): Conversion[AbstL] = {
      converstateWithExactSymVariables(abstSem, pathConstraint)
    }
  }

  def inject(exp: SchemeExp, env: Environment[HybridAddress.A], sto: Store[HybridAddress.A, ConcreteValue]): State = {
    State(ConcolicControlEval(exp, env, concolic.initialSymEnv), sto, TimestampedKontStore[KontAddr](Map(), 0), HaltKontAddress, Timestamp[HybridTimestamp.T].initial(""))
  }

  case class StepSucceeded(state: State, filters: FilterAnnotations[SchemeExp, ConcreteValue, HybridAddress.A],
                           actionTs: List[ActionReplay[SchemeExp, ConcreteValue, HybridAddress.A]])

  private def handleFunctionCalled(edgeInfo: EdgeInformation[SchemeExp, ConcreteValue, HybridAddress.A]): Unit = {
    if (edgeInfo.actions.exists(_.marksFunctionCall)) {
      FunctionsCalledMetric.incConcreteFunctionsCalled()
    }
  }

  def eval(exp: SchemeExp, sem: Semantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
           graph: Boolean, timeout:Timeout): Output = ???

  def step(state: State, sem: ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T, PCElementUsed]): Either[ConcolicIterationOutput, StepSucceeded] = state.control match {
    case ConcolicControlEval(e, env, symEnv) =>
      val edgeInfo = sem.stepConcolicEval(e, env, symEnv, state.store, state.t)
      handleFunctionCalled(edgeInfo)
      edgeInfo match {
        case EdgeInformation(ActionConcolicReachedValue(ActionReachedValue(v, store2, _), optionConcolicValue), actions, semanticsFilters) =>
          val machineFilters = Set[MachineFilterAnnotation]()
          Right(StepSucceeded(State(ConcolicControlKont(v, optionConcolicValue), store2, state.kstore, state.a, Timestamp[HybridTimestamp.T].tick(state.t)),
            FilterAnnotations(machineFilters, semanticsFilters),
            actions))
        case EdgeInformation(ActionConcolicPush(ActionPush(frame, e, env, store2, _), _, symEnv), actions, semanticsFilters) =>
          val next = NormalKontAddress[SchemeExp, HybridTimestamp.T](e, state.t)
          val kont = Kont(frame, state.a)
          val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
          Right(StepSucceeded(State(ConcolicControlEval(e, env, symEnv), store2, state.kstore.extend(next, kont), next, Timestamp[HybridTimestamp.T].tick(state.t)),
            FilterAnnotations(machineFilters, semanticsFilters),
            actions))
        case EdgeInformation(ActionConcolicEval(ActionEval(e, env, store2, _), symEnv), actions, semanticsFilters) =>
          val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
          Right(StepSucceeded(State(ConcolicControlEval(e, env, symEnv), store2, state.kstore, state.a, Timestamp[HybridTimestamp.T].tick(state.t)),
            FilterAnnotations(machineFilters, semanticsFilters),
            actions))
        case EdgeInformation(ActionConcolicStepIn(ActionStepIn(fexp, _, e, env, store2, _, _), symEnv), actions, semanticsFilters) =>
          val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
          Right(StepSucceeded(State(ConcolicControlEval(e, env, symEnv), store2, state.kstore, state.a, Timestamp[HybridTimestamp.T].tick(state.t, fexp)),
            FilterAnnotations(machineFilters, semanticsFilters),
            actions))
        case EdgeInformation(ActionConcolicError(ActionError(err)), actions, semanticsFilters) =>
          Left(ConcolicIterationErrorEncountered(err))
      }

    case ConcolicControlKont(v, symbolicValue) =>
      /* pop a continuation */
      if (state.a == HaltKontAddress) {
        Left(ConcolicIterationNoErrors)
      } else {
        val frames = state.kstore.lookup(state.a)
        if (frames.size == 1) {
          val frame = frames.head.frame.asInstanceOf[SchemeConcolicFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]]
          val originFrameCast = frame.asInstanceOf[ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]]
          val oldA = state.a
          val a = frames.head.next
          val edgeInfo = sem.stepConcolicKont(v, symbolicValue, frame, state.store, state.t, semanticsConcolicHelper)
          handleFunctionCalled(edgeInfo)
          edgeInfo match {
            case EdgeInformation(ActionConcolicReachedValue(ActionReachedValue(v, store2, _), optionConcolicValue), actions, semanticsFilters) =>
              val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                FrameFollowed[ConcreteValue](originFrameCast))
              Right(StepSucceeded(State(ConcolicControlKont(v, optionConcolicValue), store2, state.kstore, a, Timestamp[HybridTimestamp.T].tick(state.t)),
                FilterAnnotations(machineFilters, semanticsFilters),
                actions))
            case EdgeInformation(ActionConcolicPush(ActionPush(frame, e, env, store2, _), _, symEnv), actions, semanticsFilters) =>
              val next = NormalKontAddress[SchemeExp, HybridTimestamp.T](e, state.t)
              val machineFilters = Set[MachineFilterAnnotation](
                KontAddrPopped(oldA, a),
                EvaluatingExpression(e),
                FrameFollowed(originFrameCast))
              Right(StepSucceeded(State(ConcolicControlEval(e, env, symEnv), store2, state.kstore.extend(next, Kont(frame, a)), next, Timestamp[HybridTimestamp.T].tick(state.t)),
                FilterAnnotations(machineFilters, semanticsFilters),
                actions))
            case EdgeInformation(ActionConcolicEval(ActionEval(e, env, store2, _), symEnv), actions, semanticsFilters) =>
              val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                EvaluatingExpression(e),
                FrameFollowed[ConcreteValue](originFrameCast))
              Right(StepSucceeded(State(ConcolicControlEval(e, env, symEnv), store2, state.kstore, a, Timestamp[HybridTimestamp.T].tick(state.t)),
                FilterAnnotations(machineFilters, semanticsFilters),
                actions))
            case EdgeInformation(ActionConcolicStepIn(ActionStepIn(fexp, _, e, env, store2, _, _), symEnv), actions, semanticsFilters) =>
              val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                EvaluatingExpression(e),
                FrameFollowed[ConcreteValue](originFrameCast))
              Right(StepSucceeded(State(ConcolicControlEval(e, env, symEnv), store2, state.kstore, a, Timestamp[HybridTimestamp.T].tick(state.t, fexp)),
                FilterAnnotations(machineFilters, semanticsFilters),
                actions))
            case EdgeInformation(ActionConcolicError(ActionError(err)), actions, semanticsFilters) =>
              Left(ConcolicIterationErrorEncountered(err))
          }
        } else {
          Left(ConcolicIterationNoErrors)
        }
      }

    case ConcolicControlError(err) => Left(ConcolicIterationErrorEncountered(err))
  }

  /**
    * Performs the evaluation of an expression, possibly writing the output graph
    * in a file, and returns the set of final states reached
    */
  def concolicEval(programName: String, exp: SchemeExp, sem: ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T, PCElementUsed], graph: Boolean): ConcolicMachineOutput = {
    @scala.annotation.tailrec
    def loopOneIteration(state: State, start: Long, count: Int, nrOfRuns: Int): ConcolicIterationOutput = {
      currentState = Some(state)
      Logger.log(s"stepCount: $stepCount", Logger.V)
      stepCount += 1

      if (concolicFlags.endCondition.shouldStop(nrOfRuns)) {
        ConcolicIterationTimedOut
      } else {
        val stepped = step(state, sem)
        stepped match {
          case Left(output) => output
          case Right(StepSucceeded(succState, filters, actions)) => loopOneIteration(succState, start, count + 1, nrOfRuns)
        }
      }
    }

    @scala.annotation.tailrec
    def loopConcolic(initialState: State, nrOfRuns: Int, allInputsUntilNow: List[List[(ConcolicInput, Int)]], allPathConstraints: List[PathConstraintWith[PCElementUsed]]): ConcolicMachineOutput = {
      currentConcolicRun = nrOfRuns
      def finishUpLoop: Boolean = {
        Logger.log(s"END CONCOLIC ITERATION $nrOfRuns", Logger.U)
        reporter.printReports()
        val shouldContinue = reporter.solver.solve(reporter.pathStorage.getCurrentReport)
        shouldContinue
      }
      def initLoop(): Unit = {
        reporter.inputVariableStore.enableConcolic()
        Logger.log(s"\n\nSTART CONCOLIC ITERATION $nrOfRuns ${reporter.solver.getInputs}", Logger.U)
        stepCount = 0
        reporter.clear(reporter.solver.getInputs)
        FunctionsCalledMetric.resetConcreteFunctionsCalled()
      }

      if (concolicFlags.endCondition.shouldStop(nrOfRuns)) {
        ConcolicMachineOutputFinished(allInputsUntilNow, allPathConstraints)
      } else {
        initLoop()
        val eitherOutputOrShouldContinue: Either[ConcolicMachineOutput, Boolean] = try {
          val concolicIterationResult = loopOneIteration(initialState, System.nanoTime, 0, nrOfRuns)
          concolicIterationResult match {
            case ConcolicIterationTimedOut => Left(ConcolicMachineOutputFinished(allInputsUntilNow, allPathConstraints))
            case ConcolicIterationErrorEncountered(err) =>
              Logger.log(s"ERROR DETECTED DURING CONCOLIC TESTING: $err", Logger.E)
              Right(finishUpLoop)
            case ConcolicIterationNoErrors => Right(finishUpLoop)
          }
        } catch {
          case AbortConcolicIterationException => Right(finishUpLoop)
        }
        eitherOutputOrShouldContinue match {
          case Left(output) => output
          case Right(shouldContinue) =>
            if (! concolicFlags.endCondition.shouldStop(nrOfRuns) && shouldContinue) {
              loopConcolic(initialState, nrOfRuns + 1, allInputsUntilNow :+ reporter.solver.getInputs,
                           allPathConstraints :+ reporter.pathStorage.getCurrentReport)
            } else {
              ConcolicMachineOutputFinished(allInputsUntilNow, allPathConstraints)
            }
        }
      }
    }

    val initialState = inject(exp, Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial[HybridAddress.A, ConcreteValue](sem.initialStore))
    val output: ConcolicMachineOutput = if (concolicFlags.checkAnalysis) {
      // Use initial static analysis to detect paths to errors
      val initialAnalysisResult = rtAnalysis.startInitialAnalysis(initialState, programName)
      if (initialAnalysisResult.shouldContinueTesting) {
        loopConcolic(initialState, 1, Nil, Nil)
      } else {
        Logger.log("Concolic testing not started because initial analysis did not report any suitable error states", Logger.E)
        ConcolicMachineTestingNotStarted
      }
    } else {
      loopConcolic(initialState, 1, Nil, Nil)
    }
    reporter.writeSymbolicTree("tree.dot")
    output
  }

  def printExecutionTimes[Abs: JoinLattice](benchmarks_results_file: String): Unit = {
    val file = new File(benchmarks_results_file)
    val bw = new BufferedWriter(new FileWriter(file, true))
    bw.write(s"${GlobalFlags.CURRENT_PROGRAM}: ${Stopwatch.time}\n")
    bw.close()
  }

  def abstractCurrentState(pathConstraint: PathConstraint): NodeExtraInfo = {
    analysisLauncher.stateConverter.convertStateAAM(currentState.get, pathConstraint).asInstanceOf[NodeExtraInfo]
  }

  def startAnalysisFromCurrentState(thenBranchTaken: Boolean, pathConstraint: PathConstraint): AnalysisResult = {
    assert(currentState.isDefined)
    rtAnalysis.startRunTimeAnalysis(currentState.get, thenBranchTaken, stepCount, pathConstraint, currentConcolicRun)
  }
}
