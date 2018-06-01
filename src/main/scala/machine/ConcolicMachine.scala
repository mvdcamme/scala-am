import java.io.{BufferedWriter, File, FileWriter}

import backend.expression._
import backend._

import ConcreteConcreteLattice.{L => ConcreteValue}

case class Reached[Addr : Address](addressesReachable: Set[Addr], preciseAddresses: Set[Addr]) {
  def ++(other: Reached[Addr]): Reached[Addr] = {
    Reached(this.addressesReachable ++ other.addressesReachable, this.preciseAddresses ++ other.preciseAddresses)
  }
}
object Reached {
  def empty[Addr : Address]: Reached[Addr] = Reached[Addr](Set(), Set())
}

class ConcolicMachine[PAbs: IsConvertableLattice: LatticeInfoProvider, PCElementUsed, NodeExtraInfo](
  val analysisLauncher: AnalysisLauncher[PAbs],
  val analysisFlags: AnalysisFlags,
  val reporter: ScalaAMReporter[PCElementUsed, NodeExtraInfo],
  val concolicFlags: ConcolicRunTimeFlags)
 (implicit unused1: IsSchemeLattice[ConcreteValue])
    extends EvalKontMachine[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T]
    with RTAnalysisStarter[AnalysisLauncher[PAbs]#SpecInitState, AnalysisLauncher[PAbs]#SpecState]
    with AbstractGraphTracker[AnalysisLauncher[PAbs]#SpecState] {

  def name = "ConcolicMachine"

  private val rtAnalysis = new LaunchAnalyses[PAbs, PCElementUsed](analysisLauncher, reporter)
  private val semanticsConcolicHelper: SemanticsConcolicHelper = if (concolicFlags.tryMergePaths) {
    new MergingSemanticsConcolicHelper[PCElementUsed, analysisLauncher.SpecInitState, analysisLauncher.SpecState](this, this, reporter, concolicFlags)
  } else {
    new NoMergingSemanticsConcolicHelper[PCElementUsed, analysisLauncher.SpecInitState, analysisLauncher.SpecState](this, this, reporter, concolicFlags)
  }

  private var currentState: Option[ConcolicMachineState] = None
  def getCurrentState: ConcolicMachineState = currentState.get

  /* AbstractStateTracker */
  private var initialStateGraph: Option[Graph[AnalysisLauncher[PAbs]#SpecState, EdgeAnnotation[_, _, HybridAddress.A], _]] = None
  private var trackAbstractNodes: Option[TrackAbstractNodes[SchemeExp, PAbs, HybridAddress.A, HybridTimestamp.T, analysisLauncher.SpecState]] = None
  def getInitialStateGraph: Option[Graph[AnalysisLauncher[PAbs]#SpecState, EdgeAnnotation[_, _, HybridAddress.A], _]] = initialStateGraph
  def getCurrentAbstractStateNode: Option[Set[AnalysisLauncher[PAbs]#SpecState]] = trackAbstractNodes.map(_.getCurrentNodes)

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

  case class StepSucceeded(state: ConcolicMachineState, filters: FilterAnnotations,
                           actionTs: List[ActionReplay[SchemeExp, ConcreteValue, HybridAddress.A]])

  private def handleFunctionCalled(edgeInfo: EdgeInformation[SchemeExp, ConcreteValue, HybridAddress.A]): Unit = {
    if (edgeInfo.actions.exists(_.marksFunctionCall)) {
      FunctionsCalledMetric.incConcreteFunctionsCalled()
    }
  }

  def eval(exp: SchemeExp, sem: Semantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
           graph: Boolean, timeout:Timeout): Output = ???

  def step(state: ConcolicMachineState, sem: ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T, PCElementUsed]): Either[ConcolicIterationOutput, StepSucceeded] = state.control match {
    case ConcolicControlEval(e, env, symEnv) =>
      val edgeInfo = sem.stepConcolicEval(e, env, symEnv, state.store, state.t, semanticsConcolicHelper)
      handleFunctionCalled(edgeInfo)
      edgeInfo match {
        case EdgeInformation(ActionConcolicReachedValue(ActionReachedValue(v, store2, _), optionConcolicValue), actions, semanticsFilters) =>
          val machineFilters = Set[MachineFilterAnnotation]()
          Right(StepSucceeded(ConcolicMachineState(ConcolicControlKont(v, optionConcolicValue), store2, state.kstore, state.a, Timestamp[HybridTimestamp.T].tick(state.t)),
            FilterAnnotations(machineFilters, semanticsFilters),
            actions))
        case EdgeInformation(ActionConcolicPush(ActionPush(frame, e, env, store2, _), _, symEnv), actions, semanticsFilters) =>
          val next = NormalKontAddress[SchemeExp, HybridTimestamp.T](e, state.t)
          val kont = Kont(frame, state.a)
          val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
          Right(StepSucceeded(ConcolicMachineState(ConcolicControlEval(e, env, symEnv), store2, state.kstore.extend(next, kont), next, Timestamp[HybridTimestamp.T].tick(state.t)),
            FilterAnnotations(machineFilters, semanticsFilters),
            actions))
        case EdgeInformation(ActionConcolicEval(ActionEval(e, env, store2, _), symEnv), actions, semanticsFilters) =>
          val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
          Right(StepSucceeded(ConcolicMachineState(ConcolicControlEval(e, env, symEnv), store2, state.kstore, state.a, Timestamp[HybridTimestamp.T].tick(state.t)),
            FilterAnnotations(machineFilters, semanticsFilters),
            actions))
        case EdgeInformation(ActionConcolicStepIn(ActionStepIn(fexp, _, e, env, store2, _, _), symEnv), actions, semanticsFilters) =>
          val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
          Right(StepSucceeded(ConcolicMachineState(ConcolicControlEval(e, env, symEnv), store2, state.kstore, state.a, Timestamp[HybridTimestamp.T].tick(state.t, fexp)),
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
                FrameFollowed[ConcreteValue, HybridAddress.A, HybridTimestamp.T](originFrameCast))
              Right(StepSucceeded(ConcolicMachineState(ConcolicControlKont(v, optionConcolicValue), store2, state.kstore, a, Timestamp[HybridTimestamp.T].tick(state.t)),
                FilterAnnotations(machineFilters, semanticsFilters),
                actions))
            case EdgeInformation(ActionConcolicPush(ActionPush(frame, e, env, store2, _), _, symEnv), actions, semanticsFilters) =>
              val next = NormalKontAddress[SchemeExp, HybridTimestamp.T](e, state.t)
              val machineFilters = Set[MachineFilterAnnotation](
                KontAddrPopped(oldA, a),
                EvaluatingExpression(e),
                FrameFollowed(originFrameCast))
              Right(StepSucceeded(ConcolicMachineState(ConcolicControlEval(e, env, symEnv), store2, state.kstore.extend(next, Kont(frame, a)), next, Timestamp[HybridTimestamp.T].tick(state.t)),
                FilterAnnotations(machineFilters, semanticsFilters),
                actions))
            case EdgeInformation(ActionConcolicEval(ActionEval(e, env, store2, _), symEnv), actions, semanticsFilters) =>
              val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                EvaluatingExpression(e),
                FrameFollowed[ConcreteValue, HybridAddress.A, HybridTimestamp.T](originFrameCast))
              Right(StepSucceeded(ConcolicMachineState(ConcolicControlEval(e, env, symEnv), store2, state.kstore, a, Timestamp[HybridTimestamp.T].tick(state.t)),
                FilterAnnotations(machineFilters, semanticsFilters),
                actions))
            case EdgeInformation(ActionConcolicStepIn(ActionStepIn(fexp, _, e, env, store2, _, _), symEnv), actions, semanticsFilters) =>
              val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                EvaluatingExpression(e),
                FrameFollowed[ConcreteValue, HybridAddress.A, HybridTimestamp.T](originFrameCast))
              Right(StepSucceeded(ConcolicMachineState(ConcolicControlEval(e, env, symEnv), store2, state.kstore, a, Timestamp[HybridTimestamp.T].tick(state.t, fexp)),
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
    def loopOneIteration(state: ConcolicMachineState, start: Long, count: Int, nrOfRuns: Int): ConcolicIterationOutput = {
      currentState = Some(state)
      Logger.V(s"stepCount: $stepCount")
      stepCount += 1

      if (concolicFlags.endCondition.shouldStop(nrOfRuns)) {
        ConcolicIterationTimedOut
      } else {
        val stepped = step(state, sem)
        stepped match {
          case Left(output) => output
          case Right(StepSucceeded(succState, filters, actions)) =>
            trackAbstractNodes.foreach(_.computeSuccNodes(ConcolicMachineState.convertFrame[PAbs](_, analysisLauncher.abstSem), filters, nrOfRuns))
            loopOneIteration(succState, start, count + 1, nrOfRuns)
        }
      }
    }

    @scala.annotation.tailrec
    def loopConcolic(initialState: ConcolicMachineState, nrOfRuns: Int,
                     allInputsUntilNow: List[List[(ConcolicInput, Int)]],
                     allPathConstraints: List[PathConstraintWith[PCElementUsed]]): ConcolicMachineOutput = {
      currentConcolicRun = nrOfRuns
      def finishUpLoop(shouldMerge: Boolean): Boolean = {
        Logger.E(s"END CONCOLIC ITERATION $nrOfRuns")
        reporter.printReports()
        val shouldContinue = reporter.solver.solve(reporter.pathStorage.getCurrentReport, shouldMerge)
        shouldContinue
      }
      def initLoop(): Unit = {
        reporter.inputVariableStore.enableConcolic()
        Logger.E(s"\n\nSTART CONCOLIC ITERATION $nrOfRuns ${reporter.solver.getInputs}")
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
              Logger.E(s"ERROR DETECTED DURING CONCOLIC TESTING: $err")
              Right(finishUpLoop(false))
            case ConcolicIterationNoErrors => Right(finishUpLoop(false))
          }
        } catch {
          case AbortConcolicIterationException => Right(finishUpLoop(false))
          case MergeExecutionPath =>
            Logger.E("ABORTING CONCOLIC TESTING: Path should be merged")
            Right(finishUpLoop(true))
        } finally {
          trackAbstractNodes.foreach(_.resetCurrentNodes())
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

    val initialState = ConcolicMachineState.inject(exp, Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial[HybridAddress.A, ConcreteValue](sem.initialStore))
    val output: ConcolicMachineOutput = if (concolicFlags.checkAnalysis) {
      // Use initial static analysis to detect paths to errors
      val initialAnalysisResult = rtAnalysis.startInitialAnalysis(initialState, programName)
      initialStateGraph = Some(initialAnalysisResult.graph)
      trackAbstractNodes = Some(new TrackAbstractNodes[SchemeExp, PAbs, HybridAddress.A, HybridTimestamp.T, analysisLauncher.SpecState](initialAnalysisResult.graph.asInstanceOf[Graph[analysisLauncher.SpecState, EdgeAnnotation[SchemeExp, PAbs, HybridAddress.A], Set[analysisLauncher.SpecState]]]))
      if (initialAnalysisResult.shouldContinueTesting) {
        loopConcolic(initialState, 1, Nil, Nil)
      } else {
        Logger.E("Concolic testing not started because initial analysis did not report any suitable error states")
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

  def abstractCurrentState(pathConstraint: PathConstraint): analysisLauncher.SpecInitState = {
    analysisLauncher.convertStateAAM(currentState.get, pathConstraint)
  }

  def startAnalysisFromCurrentState(thenBranchTaken: Boolean, pathConstraint: PathConstraint): AnalysisResult[analysisLauncher.SpecState] = {
    assert(currentState.isDefined)
    rtAnalysis.startRunTimeAnalysis(currentState.get, thenBranchTaken, stepCount, pathConstraint, currentConcolicRun)
  }
}
