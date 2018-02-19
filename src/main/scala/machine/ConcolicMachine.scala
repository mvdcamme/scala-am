import java.io.{BufferedWriter, File, FileWriter}

import backend.expression._
import ConcreteConcreteLattice.{L => ConcreteValue}
import backend.PathConstraint
import concolic.SymbolicEnvironment

case class Reached[Addr : Address](addressesReachable: Set[Addr], preciseAddresses: Set[Addr]) {
  def ++(other: Reached[Addr]): Reached[Addr] = {
    Reached(this.addressesReachable ++ other.addressesReachable, this.preciseAddresses ++ other.preciseAddresses)
  }
}
object Reached {
  def empty[Addr : Address] = Reached[Addr](Set(), Set())
}

class ConcolicMachine[PAbs: IsConvertableLattice: LatticeInfoProvider](analysisLauncher: AnalysisLauncher[PAbs], analysisFlags: AnalysisFlags)
                                                                              (implicit unused1: IsSchemeLattice[ConcreteValue])
  extends EvalKontMachine[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T] with RTAnalysisStarter {

  def name = "ConcolicMachine"

  val rtAnalysis = new LaunchAnalyses[PAbs](analysisLauncher)

  private var currentState: Option[State] = None

  var stepCount: Int = 0

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

  case class ConcolicMachineOutputInputs(allInputs: List[List[(ConcolicInput, Int)]]) extends ConcolicMachineOutput {
    def finalValues = Set()
    override def containsFinalValue(v: ConcreteValue) = false
    def timedOut: Boolean = false
    def numberOfStates: Int = 0
    def time: Double = 0
  }

  case class ConcolicMachineErrorEncountered(err: SemanticError) extends ConcolicMachineOutput {
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

  trait ConcolicControl extends Control {
    override def subsumes(that: Control): Boolean = false
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

    def halted = control match {
      case ConcolicControlError(_) => true
      case ConcolicControlKont(_, _) => a == HaltKontAddress
      case _ => false
    }

    def graphNodeColor = control match {
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
        concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
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
          val convertedFrame = concBaseSem.convertAbsInFrame[AbstL](
            frame.asInstanceOf[ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]],
            convertValue, convertEnv, abstSem)
          val newConvertedKStore = convertedKStore.extend(convertedAddress, Kont(convertedFrame, convertedNext))
          loop(next, newConvertedKStore)
        }
      }
      loop(initialAddress, TimestampedKontStore(Map(), 0))
    }

    private var reached = Set[HybridAddress.A]()

    private def reachesValue(concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                             sto: Store[HybridAddress.A, ConcreteValue], pathConstraint: PathConstraint)
                            (value: ConcreteValue): Reached[HybridAddress.A] =
      ConcreteConcreteLattice.latticeInfoProvider.reaches[HybridAddress.A](value, reachesEnvironment(concBaseSem, sto, pathConstraint), reachesAddress(concBaseSem, sto, pathConstraint))

    private def reachesAddress(concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                               sto: Store[HybridAddress.A, ConcreteValue], pathConstraint: PathConstraint)
                              (address: HybridAddress.A): Reached[HybridAddress.A] = {
      if (! reached.contains(address)) {
        reached = reached + address
        val lookedUp = sto.lookup(address).map(reachesValue(concBaseSem, sto, pathConstraint)).getOrElse(Reached.empty[HybridAddress.A])
        Reached(Set(address) ++ lookedUp.addressesReachable, lookedUp.preciseAddresses)
      } else {
        Reached.empty[HybridAddress.A]
      }
    }

    private def reachesEnvironment(concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                   sto: Store[HybridAddress.A, ConcreteValue], pathConstraint: PathConstraint)
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
        exactSymVariables.contains(variable) || !symEnv.contains(variable)
      }

      var preciseAddresses = exactSymVariables.map(env.lookup(_).get)

      var reached: Set[HybridAddress.A] = Set()
      env.forall((tuple) => {
        val reachedResult = reachesAddress(concBaseSem, sto, pathConstraint)(tuple._2)
        reached = (reached + tuple._2) ++ reachedResult.addressesReachable
        preciseAddresses ++= reachedResult.preciseAddresses
        true
      })
      Reached(reached, preciseAddresses)
    }

    private def reachesKontAddr[KAddr <: KontAddr](concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                                   sto: Store[HybridAddress.A, ConcreteValue], kstore: KontStore[KAddr],
                                                   ka: KAddr, pathConstraint: PathConstraint): Reached[HybridAddress.A] = {
      kstore.lookup(ka).foldLeft[Reached[HybridAddress.A]](Reached.empty[HybridAddress.A])((acc, kont) =>
          acc ++ concBaseSem.frameReaches(
            kont.frame.asInstanceOf[ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]],
            reachesValue(concBaseSem, sto, pathConstraint),
            reachesEnvironment(concBaseSem, sto, pathConstraint),
            reachesAddress(concBaseSem, sto, pathConstraint)) ++ reachesKontAddr(concBaseSem, sto, kstore, kont.next, pathConstraint))
    }

    private def reachesControl[KAddr <: KontAddr](concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                                  sto: Store[HybridAddress.A, ConcreteValue],
                                                  kstore: KontStore[KAddr], control: ConcolicControl,
                                                  pathConstraint: PathConstraint): Reached[HybridAddress.A] =
      control match {
        case ConcolicControlEval(_, env, symEnv) =>
          reachesEnvironment(concBaseSem, sto, pathConstraint)(env, symEnv)
        case ConcolicControlKont(value, _) =>
          reachesValue(concBaseSem, sto, pathConstraint)(value)
        case ConcolicControlError(_) => Reached.empty[HybridAddress.A]
      }

    private def reachesStoreAddresses[KAddr <: KontAddr](concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                                         sto: Store[HybridAddress.A, ConcreteValue],
                                                         control: ConcolicControl, kstore: KontStore[KAddr],
                                                         ka: KAddr, pathConstraint: PathConstraint): Reached[HybridAddress.A] = {
      reachesControl[KAddr](concBaseSem, sto, kstore, control, pathConstraint) ++ reachesKontAddr[KAddr](concBaseSem, sto, kstore, ka, pathConstraint)
    }

    private def garbageCollectStore[KAddr <: KontAddr]
                                   (concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                    store: Store[HybridAddress.A, ConcreteValue], control: ConcolicControl, kstore: KontStore[KAddr],
                                    ka: KAddr, pathConstraint: PathConstraint): (Store[HybridAddress.A, ConcreteValue], Set[HybridAddress.A]) = {
      reached = Set()
      val result: Reached[HybridAddress.A] = reachesStoreAddresses[KAddr](concBaseSem, store, control, kstore, ka, pathConstraint)
      val gcedStore = store.gc(result.addressesReachable)
      (gcedStore, result.preciseAddresses)
    }

    private def convertKontAddr(ka: KontAddr): KontAddr = {
      val kontAddrConverter = new DefaultKontAddrConverter[SchemeExp]
      kontAddrConverter.convertKontAddr(ka)
    }

    private def converstateWithExactSymVariables[AbstL: IsConvertableLattice](
      concSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
      abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
      initialKontAddress: KontAddr, pathConstraint: PathConstraint): Conversion[AbstL] = {

      val convertValueFun = convertValue[AbstL](abstSem.primitives) _

      val convertedControl: ConvertedControl[SchemeExp, AbstL, HybridAddress.A] = control match {
        case ConcolicControlEval(exp, env, _) =>
          ConvertedControlEval[SchemeExp, AbstL, HybridAddress.A](exp, convertEnv(env))
        case ConcolicControlKont(v, _) =>
          ConvertedControlKont[SchemeExp, AbstL, HybridAddress.A](convertValueFun(v, false))
      }

      val (gcedStore, preciseVariablesAddresses) = garbageCollectStore(concSem, store, control, kstore, a, pathConstraint)
      val convertedStore = convertStoreWithExactSymVariables(gcedStore, convertValueFun, preciseVariablesAddresses)

      val convertedKStore = convertKStore[AbstL](kstore, a, convertValueFun(_, false), concSem, abstSem)
      val convertedA = convertKontAddr(a)
      val newT = DefaultHybridTimestampConverter.convertTimestamp(t)
      (convertedControl, convertedStore, convertedKStore, convertedA, newT)

    }

    def convertState[AbstL: IsConvertableLattice](
        concSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
        abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
        initialKontAddress: KontAddr, pathConstraint: PathConstraint): Conversion[AbstL] = {
      converstateWithExactSymVariables(concSem, abstSem, initialKontAddress, pathConstraint)
    }
  }

  def eval(exp: SchemeExp, sem: Semantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
           graph: Boolean, timeout:Timeout): Output = ???

  /**
    * Performs the evaluation of an expression, possibly writing the output graph
    * in a file, and returns the set of final states reached
    */
  def concolicEval(programName: String, exp: SchemeExp, sem: ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T],
           graph: Boolean, timeout: Timeout): ConcolicMachineOutput = {
    def loop(state: State, start: Long, count: Int): ConcolicMachineOutput = {

      currentState = Some(state)

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
          case ConcolicControlEval(e, env, symEnv) =>
            val edgeInfo = sem.stepConcolicEval(e, env, symEnv, store, t)
            handleFunctionCalled(edgeInfo)
            edgeInfo match {
              case EdgeInformation(ActionConcolicReachedValue(ActionReachedValue(v, store2, _), optionConcolicValue), actions, semanticsFilters) =>
                val machineFilters = Set[MachineFilterAnnotation]()
                Right(StepSucceeded(State(ConcolicControlKont(v, optionConcolicValue), store2, kstore, a, Timestamp[HybridTimestamp.T].tick(t)),
                  FilterAnnotations(machineFilters, semanticsFilters),
                  actions))
                case EdgeInformation(ActionConcolicPush(ActionPush(frame, e, env, store2, _), _, symEnv), actions, semanticsFilters) =>
                  val next = NormalKontAddress[SchemeExp, HybridTimestamp.T](e, t)
                  val kont = Kont(frame, a)
                  val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
                  Right(StepSucceeded(State(ConcolicControlEval(e, env, symEnv), store2, kstore.extend(next, kont), next, Timestamp[HybridTimestamp.T].tick(t)),
                                      FilterAnnotations(machineFilters, semanticsFilters),
                                      actions))
                case EdgeInformation(ActionConcolicEval(ActionEval(e, env, store2, _), symEnv), actions, semanticsFilters) =>
                  val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
                  Right(StepSucceeded(State(ConcolicControlEval(e, env, symEnv), store2, kstore, a, Timestamp[HybridTimestamp.T].tick(t)),
                                      FilterAnnotations(machineFilters, semanticsFilters),
                                      actions))
                case EdgeInformation(ActionConcolicStepIn(ActionStepIn(fexp, _, e, env, store2, _, _), symEnv), actions, semanticsFilters) =>
                  val machineFilters = Set[MachineFilterAnnotation](EvaluatingExpression(e))
                  Right(StepSucceeded(State(ConcolicControlEval(e, env, symEnv), store2, kstore, a, Timestamp[HybridTimestamp.T].tick(t, fexp)),
                                      FilterAnnotations(machineFilters, semanticsFilters),
                                      actions))
                case EdgeInformation(ActionConcolicError(ActionError(err)), actions, semanticsFilters) =>
                  Left(ConcolicMachineErrorEncountered(err))
              }

          case ConcolicControlKont(v, symbolicValue) =>
            /* pop a continuation */
            if (a == HaltKontAddress) {
              Left(ConcolicMachineOutputUnnecessary)
            } else {
              val frames = kstore.lookup(a)
              if (frames.size == 1) {
                val frame = frames.head.frame.asInstanceOf[SchemeConcolicFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]]
                val originFrameCast = frame.asInstanceOf[ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]]
                val oldA = state.a
                val a = frames.head.next
                val edgeInfo = sem.stepConcolicKont(v, symbolicValue, frame, store, t)
                handleFunctionCalled(edgeInfo)
                edgeInfo match {
                  case EdgeInformation(ActionConcolicReachedValue(ActionReachedValue(v, store2, _), optionConcolicValue), actions, semanticsFilters) =>
                    val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                      FrameFollowed[ConcreteValue](originFrameCast))
                    Right(StepSucceeded(State(ConcolicControlKont(v, optionConcolicValue), store2, kstore, a, Timestamp[HybridTimestamp.T].tick(t)),
                      FilterAnnotations(machineFilters, semanticsFilters),
                      actions))
                  case EdgeInformation(ActionConcolicPush(ActionPush(frame, e, env, store2, _), _, symEnv), actions, semanticsFilters) =>
                    val next = NormalKontAddress[SchemeExp, HybridTimestamp.T](e, t)
                    val machineFilters = Set[MachineFilterAnnotation](//KontAddrPushed(next),
                      KontAddrPopped(oldA, a),
                      EvaluatingExpression(e),
                      FrameFollowed(originFrameCast))
                    Right(StepSucceeded(State(ConcolicControlEval(e, env, symEnv), store2, kstore.extend(next, Kont(frame, a)), next, Timestamp[HybridTimestamp.T].tick(t)),
                      FilterAnnotations(machineFilters, semanticsFilters),
                      actions))
                  case EdgeInformation(ActionConcolicEval(ActionEval(e, env, store2, _), symEnv), actions, semanticsFilters) =>
                    val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                      EvaluatingExpression(e),
                      FrameFollowed[ConcreteValue](originFrameCast))
                    Right(StepSucceeded(State(ConcolicControlEval(e, env, symEnv), store2, kstore, a, Timestamp[HybridTimestamp.T].tick(t)),
                      FilterAnnotations(machineFilters, semanticsFilters),
                      actions))
                  case EdgeInformation(ActionConcolicStepIn(ActionStepIn(fexp, _, e, env, store2, _, _), symEnv), actions, semanticsFilters) =>
//                    GlobalSymbolicEnvironment.pushEnvironment()
                    val machineFilters = Set[MachineFilterAnnotation](KontAddrPopped(oldA, a),
                      EvaluatingExpression(e),
                      FrameFollowed[ConcreteValue](originFrameCast))
                    Right(StepSucceeded(State(ConcolicControlEval(e, env, symEnv), store2, kstore, a, Timestamp[HybridTimestamp.T].tick(t, fexp)),
                      FilterAnnotations(machineFilters, semanticsFilters),
                      actions))
                  case EdgeInformation(ActionConcolicError(ActionError(err)), actions, semanticsFilters) =>
                    Left(ConcolicMachineErrorEncountered(err))
                }
              } else {
                Left(ConcolicMachineOutputUnnecessary)
              }
            }

          case ConcolicControlError(err) =>
            Left(ConcolicMachineErrorEncountered(err))
        }
        val stepped = step(control)
        stepped match {
          case Left(output) => output
          case Right(StepSucceeded(succState, filters, actions)) => loop(succState, start, count + 1)
        }
      }

    }

    def inject(exp: SchemeExp, env: Environment[HybridAddress.A], sto: Store[HybridAddress.A, ConcreteValue]): State = {
      State(ConcolicControlEval(exp, env, concolic.initialSymEnv), sto, TimestampedKontStore[KontAddr](Map(), 0), HaltKontAddress, Timestamp[HybridTimestamp.T].initial(""))
    }

    @scala.annotation.tailrec
    def loopConcolic(initialState: State, nrOfRuns: Int, allInputsUntilNow: List[List[(ConcolicInput, Int)]]): List[List[(ConcolicInput, Int)]] = {

      def finishUpLoop: Boolean = {
        Logger.log(s"END CONCOLIC ITERATION $nrOfRuns", Logger.U)
        ScalaAMReporter.printReports()
        val shouldContinue = ScalaAMConcolicSolver.solve()
        shouldContinue
      }

      ScalaAMReporter.enableConcolic()
      Logger.log(s"\n\nSTART CONCOLIC ITERATION $nrOfRuns ${ScalaAMConcolicSolver.getInputs}", Logger.U)
      stepCount = 0
      ScalaAMReporter.clear()
      FunctionsCalledMetric.resetConcreteFunctionsCalled()
      val shouldContinue = try {
        val concolicIterationResult = loop(initialState, System.nanoTime, 0)
        concolicIterationResult match {
          case ConcolicMachineErrorEncountered(err) =>
            Logger.log(s"ERROR DETECTED DURING CONCOLIC TESTING: $err", Logger.E)
          case _ =>
        }
        finishUpLoop
      } catch {
        case AbortConcolicIterationException => finishUpLoop
      }
      if (nrOfRuns < ConcolicRunTimeFlags.MAX_CONCOLIC_ITERATIONS && shouldContinue) {
        loopConcolic(initialState, nrOfRuns + 1, allInputsUntilNow :+ ScalaAMConcolicSolver.getInputs)
      } else {
        allInputsUntilNow
      }
    }

    val initialState = inject(exp, Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial[HybridAddress.A, ConcreteValue](sem.initialStore))

    val output: ConcolicMachineOutput = if (ConcolicRunTimeFlags.checkAnalysis) {
      // Use initial static analysis to detect paths to errors
      val initialAnalysisResult = rtAnalysis.startInitialAnalysis(initialState, programName)
      if (initialAnalysisResult.shouldContinueTesting) {
        val allInputs = loopConcolic(initialState, 1, Nil)
        ConcolicMachineOutputInputs(allInputs)
      } else {
        Logger.log("Concolic testing not started because initial analysis did not report any suitable error states", Logger.E)
        ConcolicMachineOutputUnnecessary
      }
    } else {
      val allInputs = loopConcolic(initialState, 1, Nil)
      ConcolicMachineOutputInputs(allInputs)
    }
    ScalaAMReporter.writeSymbolicTree("tree.dot")
    output
  }

  def printExecutionTimes[Abs: JoinLattice](benchmarks_results_file: String): Unit = {
    val file = new File(benchmarks_results_file)
    val bw = new BufferedWriter(new FileWriter(file, true))
    bw.write(s"${GlobalFlags.CURRENT_PROGRAM}: ${Stopwatch.time}\n")
    bw.close()
  }

  def startAnalysisFromCurrentState(thenBranchTaken: Boolean, pathConstraint: PathConstraint): AnalysisResult = {
    assert(currentState.isDefined)
    val analysisResult = rtAnalysis.startRunTimeAnalysis(currentState.get, thenBranchTaken, stepCount, pathConstraint)
    analysisResult
  }
}
