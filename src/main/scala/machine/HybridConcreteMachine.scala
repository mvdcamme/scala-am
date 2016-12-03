import scala.annotation.tailrec

class HybridConcreteMachine[Exp](implicit unused1: IsSchemeLattice[ConcreteConcreteLattice.L],
                                          unused2: Expression[Exp])
    extends EvalKontMachine[Exp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T] {
  def name = "ConcreteMachine"

//  implicit val isSchemeLattice: IsSchemeLattice[ConcreteConcreteLattice.L] = ConcreteConcreteLattice.isSchemeLattice
//  implicit val isAddress: Address[HybridAddress.A] = HybridAddress.isAddress
//  implicit val isTimestamp: Timestamp[HybridTimestamp.T] = HybridTimestamp.isTimestamp

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
                                        v: ConcreteConcreteLattice.L)
      extends ConcreteMachineOutput {
    def finalValues = Set(v)
    def containsFinalValue(v2: ConcreteConcreteLattice.L) = v == v2
    def timedOut = false
  }

  case class State(control: Control,
                   store: Store[HybridAddress.A, ConcreteConcreteLattice.L],
                   kstore: KontStore[KontAddr],
                   a: KontAddr,
                   t: HybridTimestamp.T)
      extends ConcreteTracingProgramState[Exp, HybridAddress.A, HybridTimestamp.T] {

    def halted = control match {
      case ControlError(_) => true
      case ControlKont(_) => a == HaltKontAddress
      case _ => false
    }

    private def convertValue[AbstL: IsConvertableLattice](
                                                           concPrims: Primitives[HybridAddress.A, ConcreteValue],
                                                           abstPrims: SchemePrimitives[HybridAddress.A, AbstL]
                                                         )(value: ConcreteValue): AbstL =
      ConcreteConcreteLattice.convert[Exp, AbstL, HybridAddress.A](
        value,
        new DefaultHybridAddressConverter[Exp],
        convertEnv,
        concPrims,
        abstPrims)

    /**
      * Converts all addresses in the environment.
      * @param env The environment for which all addresses must be converted.
      * @return A new environment with all addresses converted.
      */
    private def convertEnv(
                            env: Environment[HybridAddress.A]): Environment[HybridAddress.A] = {
      val addressConverter = new DefaultHybridAddressConverter()
      env.map { (address) => addressConverter.convertAddress(address) }
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

    private def convertKStoreToFrames[KAddr <: KontAddr : KontAddress](
                                                                        concSem: ConvertableSemantics[Exp,
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
               stack: List[(KontAddr,
                 Option[Frame],
                 List[Storable[ConcreteValue, HybridAddress.A]],
                 Environment[HybridAddress.A])])
      : (KAddr, KontStore[KAddr]) = a match {
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

    private def convertKStoreAbsValuesInFrames[AbstL: IsConvertableLattice, KAddr <: KontAddr : KontAddress](
                                                                                                kontStore: KontStore[KAddr],
                                                                                                convertValue: ConcreteValue => AbstL,
                                                                                                concBaseSem: BaseSchemeSemantics[ConcreteValue,
                                                                                                  HybridAddress.A,
                                                                                                  HybridTimestamp.T],
                                                                                                abstSem: BaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T])
    : KontStore[KAddr] = {
      val kontAddrConverter = new DefaultKontAddrConverter[Exp]
      kontStore.map(
        kontAddrConverter.convertKontAddr, //TODO used the identity function, should use the DefaultKontAddrConverter
        (frame: Frame) =>
          concBaseSem.convertAbsInFrame[AbstL](
            frame.asInstanceOf[SchemeFrame[ConcreteValue,
              HybridAddress.A,
              HybridTimestamp.T]],
            convertValue,
            convertEnv,
            abstSem))
    }

    private def convertStore[AbstL: IsConvertableLattice](
                                                           store: Store[HybridAddress.A, ConcreteValue],
                                                           convertValue: ConcreteValue => AbstL): Store[HybridAddress.A, AbstL] = {
      var valuesConvertedStore = Store.empty[HybridAddress.A, AbstL]
      def addToNewStore(tuple: (HybridAddress.A, ConcreteValue)): Boolean = {
        val convertedAddress =
          new DefaultHybridAddressConverter().convertAddress(tuple._1)
        val convertedValue = convertValue(tuple._2)
        valuesConvertedStore =
          valuesConvertedStore.extend(convertedAddress, convertedValue)
        true
      }
      store.forall(addToNewStore)
      valuesConvertedStore
    }

    private def convertKStore[AbstL: IsConvertableLattice, KAddr <: KontAddr : KontAddress](
                                                                               mapKontAddress: (KontAddr,
                                                                                 Option[Environment[HybridAddress.A]])
                                                                                 => KAddr,
                                                                               kontStore: KontStore[KontAddr],
                                                                               convertValue: ConcreteValue => AbstL,
                                                                               concBaseSem: BaseSchemeSemantics[ConcreteValue,
                                                                                 HybridAddress.A,
                                                                                 HybridTimestamp.T],
                                                                               abstSem: BaseSchemeSemantics[AbstL, HybridAddress.A, HybridTimestamp.T])
    : KontStore[KAddr] = {
      val kontAddrConverter = new DefaultKontAddrConverter[Exp]
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
        concSem: ConvertableSemantics[Exp,
                                      ConcreteValue,
                                      HybridAddress.A,
                                      HybridTimestamp.T],
        abstSem: BaseSchemeSemantics[AbstL,
                                     HybridAddress.A,
                                     HybridTimestamp.T],
        initialKontAddress: KAddr,
        mapKontAddress: (KontAddr, Option[Environment[HybridAddress.A]]) => KAddr)
      : (ConvertedControl[Exp, AbstL, HybridAddress.A],
         Store[HybridAddress.A, AbstL],
         KontStore[KAddr],
         KAddr,
         HybridTimestamp.T) = {

      val concBaseSem =
        new SchemeSemantics[ConcreteValue, HybridAddress.A, HybridTimestamp.T](
          new SchemePrimitives[HybridAddress.A, ConcreteValue])

      val convertValueFun =
        convertValue[AbstL](concSem.primitives, abstSem.primitives) _

      val convertedControl: ConvertedControl[Exp, AbstL, HybridAddress.A] =
        control match {
          case ControlEval(exp, env) =>
            ConvertedControlEval[Exp, AbstL, HybridAddress.A](exp, convertEnv(env))
          case ControlKont(v) =>
            ConvertedControlKont[Exp, AbstL, HybridAddress.A](convertValueFun(v))
        }

      val convertedStore = convertStore(store, convertValueFun)

      val convertedKStore = convertKStore[AbstL, KAddr](mapKontAddress, kstore, convertValueFun, concBaseSem, abstSem)

      val convertedA = mapKontAddress(a, None)
      val newT = DefaultHybridTimestampConverter.convertTimestamp(t)
      (convertedControl,
       convertedStore,
       convertedKStore,
       convertedA,
       newT)
    }
  }

  /**
    * Performs the evaluation of an expression, possibly writing the output graph
    * in a file, and returns the set of final states reached
    */
  def eval(exp: Exp,
           sem: Semantics[Exp, ConcreteConcreteLattice.L, HybridAddress.A, HybridTimestamp.T],
           graph: Boolean,
           timeout: Option[Long]): Output[ConcreteConcreteLattice.L] = {
    def loop(state: State, start: Long, count: Int): ConcreteMachineOutput = {
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
        control match {
          case ControlEval(e, env) =>
            val edges = sem.stepEval(e, env, store, t)
            if (edges.size == 1) {
              edges.head._1 match {
                case ActionReachedValue(v, store2, _) =>
                  loop(State(ControlKont(v), store2, kstore, a, time.tick(t)),
                       start,
                       count + 1)
                case ActionPush(frame, e, env, store2, _) =>
                  val next = NormalKontAddress[Exp, HybridTimestamp.T](e, t)
                  loop(State(ControlEval(e, env),
                             store2,
                             kstore.extend(next, Kont(frame, a)),
                             next,
                             time.tick(t)),
                       start,
                       count + 1)
                case ActionEval(e, env, store2, _) =>
                  loop(State(ControlEval(e, env),
                             store2,
                             kstore,
                             a,
                             time.tick(t)),
                       start,
                       count + 1)
                case ActionStepIn(fexp, _, e, env, store2, _, _) =>
                  loop(State(ControlEval(e, env),
                             store2,
                             kstore,
                             a,
                             time.tick(t, fexp)),
                       start,
                       count + 1)
                case ActionError(err) =>
                  ConcreteMachineOutputError(
                    (System.nanoTime - start) / Math.pow(10, 9),
                    count,
                    err.toString)
              }
            } else {
              ConcreteMachineOutputError(
                (System.nanoTime - start) / Math.pow(10, 9),
                count,
                s"execution was not concrete (got ${edges.size} actions instead of 1)")
            }
          case ControlKont(v) =>
            /* pop a continuation */
            if (a == HaltKontAddress) {
              ConcreteMachineOutputValue(
                (System.nanoTime - start) / Math.pow(10, 9),
                count,
                v)
            } else {
              val frames = kstore.lookup(a)
              if (frames.size == 1) {
                val frame = frames.head.frame
                val a = frames.head.next
                val edges = sem.stepKont(v, frame, store, t)
                if (edges.size == 1) {
                  edges.head._1 match {
                    case ActionReachedValue(v, store2, _) =>
                      loop(
                        State(ControlKont(v), store2, kstore, a, time.tick(t)),
                        start,
                        count + 1)
                    case ActionPush(frame, e, env, store2, _) =>
                      val next = NormalKontAddress[Exp, HybridTimestamp.T](e, t)
                      loop(State(ControlEval(e, env),
                                 store2,
                                 kstore.extend(next, Kont(frame, a)),
                                 next,
                                 time.tick(t)),
                           start,
                           count + 1)
                    case ActionEval(e, env, store2, _) =>
                      loop(State(ControlEval(e, env),
                                 store2,
                                 kstore,
                                 a,
                                 time.tick(t)),
                           start,
                           count + 1)
                    case ActionStepIn(fexp, _, e, env, store2, _, _) =>
                      loop(State(ControlEval(e, env),
                                 store2,
                                 kstore,
                                 a,
                                 time.tick(t, fexp)),
                           start,
                           count + 1)
                    case ActionError(err) =>
                      ConcreteMachineOutputError(
                        (System.nanoTime - start) / Math.pow(10, 9),
                        count,
                        err.toString)
                  }
                } else {
                  ConcreteMachineOutputError(
                    (System.nanoTime - start) / Math.pow(10, 9),
                    count,
                    s"execution was not concrete (got ${edges.size} actions instead of 1)")
                }
              } else {
                ConcreteMachineOutputError(
                  (System.nanoTime - start) / Math.pow(10, 9),
                  count,
                  s"execution was not concrete (got ${frames.size} frames instead of 1)")
              }
            }
          case ControlError(err) =>
            ConcreteMachineOutputError(
              (System.nanoTime - start) / Math.pow(10, 9),
              count,
              err.toString)
        }
      }
    }

    def inject(exp: Exp,
               env: Environment[HybridAddress.A],
               sto: Store[HybridAddress.A, ConcreteConcreteLattice.L]): State =
      State(ControlEval(exp, env),
            sto,
            KontStore.empty[KontAddr],
            HaltKontAddress,
            time.initial(""))

    loop(inject(exp,
                Environment.initial[HybridAddress.A](sem.initialEnv),
                Store.initial[HybridAddress.A, ConcreteConcreteLattice.L](sem.initialStore)),
         System.nanoTime,
         0)
  }
}
