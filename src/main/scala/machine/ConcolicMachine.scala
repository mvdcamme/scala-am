import java.io.{BufferedWriter, File, FileWriter}

import backend.tree.Constraint
import backend.expression._
import ConcreteConcreteLattice.{L => ConcreteValue}
import backend.path_filtering.PartialRegexMatcher
import concolic.SymbolicEnvironment

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

  case class State(control: ConcolicControl,
                   store: Store[HybridAddress.A, ConcreteValue],
                   kstore: KontStore[KontAddr],
                   a: KontAddr,
                   t: HybridTimestamp.T)
      extends ConvertableProgramState[SchemeExp, HybridAddress.A, HybridTimestamp.T]
      with StateTrait[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T] {

    def addressesReachable: Set[HybridAddress.A] = {
      store.toSet.map(_._1)
    }

    def optEnvs: Option[(Environment[HybridAddress.A], SymbolicEnvironment)] = control match {
      case ConcolicControlEval(_, env, symEnv) => Some((env, symEnv))
      case ConcolicControlKont(_, _) => if (halted) {
        None
      } else {
        val topKonts = kstore.lookup(a)
        assert(topKonts.size == 1)
        val topFrame = topKonts.head.frame.asInstanceOf[SchemeConcolicFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]]
        Some((topFrame.env, topFrame.symEnv))
      }
      case _ => None
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

    private def convertStoreNoExactSymVariables[AbstL: IsConvertableLattice](
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

    private def reachesValue(concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                             sto: Store[HybridAddress.A, ConcreteValue])
                            (value: ConcreteValue): Set[HybridAddress.A] =
      ConcreteConcreteLattice.latticeInfoProvider.reaches[HybridAddress.A](value, reachesEnvironment(concBaseSem, sto), reachesAddress(concBaseSem, sto))

    private def reachesAddress(concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                               sto: Store[HybridAddress.A, ConcreteValue])
                              (address: HybridAddress.A): Set[HybridAddress.A] = {
      if (! reached.contains(address)) {
        reached = reached + address
        Set(address) ++ sto.lookup(address).foldLeft[Set[HybridAddress.A]](Set())((_, value) =>
          reachesValue(concBaseSem, sto)(value))
      } else {
        Set()
      }
    }

    private def reachesEnvironment(concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                   sto: Store[HybridAddress.A, ConcreteValue])
                                  (env: Environment[HybridAddress.A]): Set[HybridAddress.A] = {
      var reached: Set[HybridAddress.A] = Set()
      env.forall((tuple) => {
        reached = (reached + tuple._2) ++ reachesAddress(concBaseSem, sto)(tuple._2)
        true
      })
      reached
    }

    private def reachesKontAddr[KAddr <: KontAddr](concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                                   sto: Store[HybridAddress.A, ConcreteValue], kstore: KontStore[KAddr],
                                                   ka: KAddr): Set[HybridAddress.A] = {
      kstore.lookup(ka).foldLeft[Set[HybridAddress.A]](Set())(
        (acc, kont) =>
          acc ++ concBaseSem.frameReaches(
            kont.frame.asInstanceOf[ConvertableSchemeFrame[ConcreteValue, HybridAddress.A, HybridTimestamp.T]],
            reachesValue(concBaseSem, sto),
            reachesEnvironment(concBaseSem, sto),
            reachesAddress(concBaseSem, sto)) ++ reachesKontAddr(concBaseSem, sto, kstore, kont.next))
    }

    private def reachesControl[KAddr <: KontAddr](concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                                  sto: Store[HybridAddress.A, ConcreteValue],
                                                  kstore: KontStore[KAddr], control: ConcolicControl): Set[HybridAddress.A] =
      control match {
        case ConcolicControlEval(_, env, _) =>
          reachesEnvironment(concBaseSem, sto)(env)
        case ConcolicControlKont(value, _) =>
          reachesValue(concBaseSem, sto)(value)
        case ConcolicControlError(_) => Set()
      }

    private def reachesStoreAddresses[KAddr <: KontAddr](concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                                         sto: Store[HybridAddress.A, ConcreteValue],
                                                         control: ConcolicControl, kstore: KontStore[KAddr],
                                                         ka: KAddr): Set[HybridAddress.A] = {
      reachesControl[KAddr](concBaseSem, sto, kstore, control) ++ reachesKontAddr[KAddr](concBaseSem, sto, kstore, ka)
    }

    private def garbageCollectStore[KAddr <: KontAddr]
                                   (concBaseSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
                                    store: Store[HybridAddress.A, ConcreteValue],
                                    control: ConcolicControl, kstore: KontStore[KAddr],
                                    ka: KAddr): Store[HybridAddress.A, ConcreteValue] = {
      reached = Set()
      val storeAddressReachable = reachesStoreAddresses[KAddr](concBaseSem, store, control, kstore, ka)
      store.gc(storeAddressReachable)
    }

    private def convertKontAddr[KAddr <: KontAddr](ka: KontAddr, env: Option[Environment[HybridAddress.A]], mapKontAddress: (KontAddr, Option[Environment[HybridAddress.A]]) => KAddr): KAddr = {
      val kontAddrConverter = new DefaultKontAddrConverter[SchemeExp]
      mapKontAddress(kontAddrConverter.convertKontAddr(ka), env)
    }

    private def converstateNoExactSymVariables[AbstL: IsConvertableLattice](
      concSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
      abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
      initialKontAddress: KontAddr,
      mapKontAddress: (KontAddr, Option[Environment[HybridAddress.A]]) => KontAddr): Conversion[AbstL] = {

      val convertValueFun = (value: ConcreteValue) => convertValue[AbstL](abstSem.primitives) (value, false)

      val convertedControl: ConvertedControl[SchemeExp, AbstL, HybridAddress.A] = control match {
        case ConcolicControlEval(exp, env, _) =>
          ConvertedControlEval[SchemeExp, AbstL, HybridAddress.A](exp, convertEnv(env))
        case ConcolicControlKont(v, _) =>
          ConvertedControlKont[SchemeExp, AbstL, HybridAddress.A](convertValueFun(v))
      }

      val GCedStore = garbageCollectStore(concSem, store, control, kstore, a)
      val convertedStore = convertStoreNoExactSymVariables(GCedStore, convertValueFun)
      val convertedKStore = convertKStore[AbstL, KontAddr](mapKontAddress, kstore, convertValueFun, concSem, abstSem)

      val convertedA = convertKontAddr(a, None, mapKontAddress)
      val newT = DefaultHybridTimestampConverter.convertTimestamp(t)
      (convertedControl, convertedStore, convertedKStore, convertedA, newT)

    }

    private def converstateWithExactSymVariables[AbstL: IsConvertableLattice](
      concSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
      abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
      initialKontAddress: KontAddr, mapKontAddress: (KontAddr, Option[Environment[HybridAddress.A]]) => KontAddr,
      env: Environment[HybridAddress.A], symEnv: SymbolicEnvironment,
      pathConstraint: List[(Constraint, Boolean)]): Conversion[AbstL] = {

      val exactSymVariables = ExactSymbolicVariablesFinder.findExactSymbolicVariables(env, symEnv, pathConstraint)
      Logger.log(s"exactSymVariables are $exactSymVariables", Logger.E)
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

      val preciseVariables = env.keys.filter(shouldMakeValPrecise)
      val preciseVariablesAddresses = preciseVariables.map(env.lookup(_).get)
      val convertValueFun = convertValue[AbstL](abstSem.primitives) _

      val convertedControl: ConvertedControl[SchemeExp, AbstL, HybridAddress.A] = control match {
        case ConcolicControlEval(exp, env, _) =>
          ConvertedControlEval[SchemeExp, AbstL, HybridAddress.A](exp, convertEnv(env))
        case ConcolicControlKont(v, _) =>
          ConvertedControlKont[SchemeExp, AbstL, HybridAddress.A](convertValueFun(v, false))
      }

      val GCedStore = garbageCollectStore(concSem, store, control, kstore, a)
      val convertedStore = convertStoreWithExactSymVariables(GCedStore, convertValueFun, preciseVariablesAddresses.toSet)
      val convertedKStore = convertKStore[AbstL, KontAddr](mapKontAddress, kstore, convertValueFun(_, false), concSem, abstSem)

      val convertedA = convertKontAddr(a, None, mapKontAddress)
      val newT = DefaultHybridTimestampConverter.convertTimestamp(t)
      (convertedControl, convertedStore, convertedKStore, convertedA, newT)

    }

    def convertState[AbstL: IsConvertableLattice](
        concSem: ConvertableSemantics[SchemeExp, ConcreteValue, HybridAddress.A, HybridTimestamp.T],
        abstSem: ConvertableBaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T],
        initialKontAddress: KontAddr, mapKontAddress: (KontAddr, Option[Environment[HybridAddress.A]]) => KontAddr,
        pathConstraint: List[(Constraint, Boolean)]): Conversion[AbstL] = {

      optEnvs match {
        case Some((env, symEnv)) =>
          converstateWithExactSymVariables(concSem, abstSem, initialKontAddress, mapKontAddress, env, symEnv, pathConstraint)
        case None => converstateNoExactSymVariables(concSem, abstSem, initialKontAddress, mapKontAddress)
      }
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
      State(ConcolicControlEval(exp, env, concolic.initialSymEnv), sto, TimestampedKontStore[KontAddr](Map(), 0), HaltKontAddress, Timestamp[HybridTimestamp.T].initial(""))
    }

    @scala.annotation.tailrec
    def loopConcolic(initialState: State, nrOfRuns: Int): Unit = {

      def finishUpLoop: Boolean = {
        Logger.log(s"END CONCOLIC ITERATION $nrOfRuns", Logger.U)
        ScalaAMReporter.printReports()
        val shouldContinue = ScalaAMConcolicSolver.solve()
        shouldContinue
      }

      ScalaAMReporter.enableConcolic()
      Logger.log(s"\n\nSTART CONCOLIC ITERATION $nrOfRuns ${ScalaAMConcolicSolver.getInputs}", Logger.U)
      ScalaAMReporter.clear(nrOfRuns < 2)
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
        loopConcolic(initialState, nrOfRuns + 1)
      }
    }

    val initialState = inject(exp, Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial[HybridAddress.A, ConcreteValue](sem.initialStore))

    if (ConcolicRunTimeFlags.checkAnalysis) {
      // Use initial static analysis to detect paths to errors
      rtAnalysis.startInitialAnalysis(initialState, programName)
    }

    loopConcolic(initialState, 1)

    ConcolicMachineOutputUnnecessary //TODO Don't care about the actual return-value
  }

  def printExecutionTimes[Abs: JoinLattice](benchmarks_results_file: String): Unit = {
    val file = new File(benchmarks_results_file)
    val bw = new BufferedWriter(new FileWriter(file, true))
    bw.write(s"${GlobalFlags.CURRENT_PROGRAM}: ${Stopwatch.time}\n")
    bw.close()
  }

  def startAnalysisFromCurrentState(thenBranchTaken: Boolean, pathConstraint: List[(Constraint, Boolean)]): AnalysisResult = {
    assert(currentState.isDefined)
    val analysisResult = rtAnalysis.startRunTimeAnalysis(currentState.get, thenBranchTaken, stepCount, pathConstraint)
    analysisResult
  }
}
