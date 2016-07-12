import scala.collection.mutable.Map
import scala.collection.mutable.Map
import scala.collection.mutable.Stack

/**
  * Created by mvdcamme on 08/03/16.
  */
class VariableAnalysis[Exp : Expression, Addr : Address, Time : Timestamp]
  (val sem: SemanticsTraced[Exp, HybridLattice.L, Addr, Time]) {

  type HybridValue = HybridLattice.L

  type TraceInstructionStates = Tracer[Exp, HybridValue, Addr, Time]#TraceInstructionInfo
  type TraceInstruction = Tracer[Exp, HybridValue, Addr, Time]#TraceInstruction
  type Trace = Tracer[Exp, HybridValue, Addr, Time]#TraceWithInfos

  /**
    * Computes the set of bound addresses in the given trace.
    * @param initialBoundAddresses The list of addresses, if any, that are initially bound in the trace: e.g.,
    *                              the parameters of the function being traced.
    * @param traceFull The trace of which the bound addresses must be computed.
    * @return The set of bound addresses in the trace.
    */
  def analyzeBoundAddresses(initialBoundAddresses: Set[Addr],
                            traceFull: TraceFull[Exp, HybridValue, Addr, Time])
                           :Set[Addr] = {

    /*
     * Compute, for each action in the trace, what the set of bound addresses are at this position in the trace.
     * This set is computed by starting from the inital set of bound addresses, and adding a new bound address
     * whenever we encounter a new action that allocates a address.
     *
     * However, we have to take saving/restoring of the environment into account: when we save an environment,
     * bind an address and then restore the environment, this address is no longer bound at the point in the
     * trace after the restoration of the environment.
     * Concretely, if we e.g., allocate a new address and afterwards restore some previous environment, the address
     * is no longer part of that environment and must therefore be removed from the set of bound addresses from
     * that point in the trace onwards.
     * We therefore simulate saving/restoring the environment via the framesStack.
     *
     * Lastly, if we encounter an assignment to some address at any point in the trace, this address becomes bound
     * AT ALL POINTS in the trace. Note that this overapproximates the set of bound addresses.
     */

    var currentEnv: Environment[Addr] = traceFull.info.startState.ρ
    var vStack: List[Storable[HybridValue, Addr]] = traceFull.info.startState.vStack

    /*
     * The set of addresses that are assigned, not defined, to inside of the trace.
     * I.e., the set of addresses involved in an ActionSetVarTraced.
     */
    var assignedAddresses: Set[Addr] = Set()

    /*
     * Simulates the environment stack: saving the environment triggers a push of a new, empty, list of vars on this
     * stack, restoring the environment triggers a pop.
     *
     */
    val framesStack: Stack[List[Addr]] = Stack(List())

    /**
      * Adds a new bound addresses, i.e., because an action is encountered that allocates this address in the environment.
      * If the address was not already placed in the boundAddresses-set, it is inserted there.
      * @param address The address that is bound.
      * @param boundAddresses The set of previously encountered bound addresses
      * @return The updated set of bound addresses.
      */
    def addAddress(address: Addr, boundAddresses: Set[Addr]): Set[Addr] = {
      if (framesStack.isEmpty) {
        framesStack.push(List())
      }
      framesStack.push(framesStack.pop :+ address)
      boundAddresses + address
    }

    /**
      * Adds a list of newly bound addresses. Similar to [[addAddress(String, Set[String]].
      * @param addresses The names of the list of bound addresses.
      * @param boundAddresses The set of previously encountered bound addresses
      * @return The updated set of bound addresses.
      */
    def addAddresses(addresses: List[Addr], boundAddresses: Set[Addr]): Set[Addr]  = {
      addresses.foldLeft(boundAddresses)({ (boundAddresses, varName) =>
          addAddress(varName, boundAddresses)})
    }

    def handleRestoreEnvironment(boundAddresses: Set[Addr]): Set[Addr] = {
      if (framesStack.isEmpty) {
        boundAddresses
      } else {
        val boundAddressesFrame = framesStack.pop()
        boundAddressesFrame.foldLeft(boundAddresses)({ (boundAddresses, varName) =>
          boundAddresses - varName })
      }
    }

    def handleSaveEnvironment(boundAddresses: Set[Addr]): Set[Addr] = {
      framesStack.push(List())
      boundAddresses
    }

    def handleSetVar(address: Addr, boundAddresses: Set[Addr]): Set[Addr] = {
      if (! boundAddresses.contains(address)) {
        assignedAddresses += address
        boundAddresses + address
      } else {
        boundAddresses
      }
    }

    def handleSetVars(addresses: List[Addr], boundAddresses: Set[Addr]): Set[Addr] = {
      addresses.foldLeft(boundAddresses)({case (boundAddresses, address) => handleSetVar(address, boundAddresses) })
    }

    type TraceInstructionInfo = Tracer[Exp, HybridValue, Addr, Time]#TraceInstructionInfo

    /*
     * We are interested in: actions that allocate or reassign addresses, actions that save the environment on the
     * stack and actions that restore the environment from the stack.
     */
    def handleInfos(instructionInfo: TraceInstructionInfo, boundAddresses: Set[Addr]) = instructionInfo match {
      case (ActionRestoreEnvT(), _) =>
        handleRestoreEnvironment(boundAddresses)
      case (ActionSaveEnvT(), _) =>
        handleSaveEnvironment(boundAddresses)
      case _ =>
        val result = instructionInfo._2.find[Set[Addr]]({
          case AddressesAllocated(_) => true
          case AddressesReassigned(_) => true
          case _ => false
        }, {
          case AddressesAllocated(addresses) =>
            addAddresses(addresses.asInstanceOf[List[Addr]], boundAddresses)
          case AddressesReassigned(addresses) =>
            handleSetVars(addresses.asInstanceOf[List[Addr]], boundAddresses)
        })
        result match {
          case None => boundAddresses
          case Some(r) => r
        }
    }

//    match {
//      case ActionAllocVarsT(addresses) =>
//        addAddresses(addresses, boundAddresses)
//      case ActionExtendEnvT(address) =>
//        addAddress(address, boundAddresses)
//      case ActionSaveEnvT() =>
//        handleSaveEnvironment(boundAddresses)
//      case ActionSetVarT(address) =>
//        handleSetVar(address, boundAddresses)
//      case ActionStepInT(_, _, args, _, _, _, _, _) =>
//        addAddresses(args, handleSaveEnvironment(boundAddresses))
//      case ActionRestoreEnvT() =>
//        handleRestoreEnvironment(boundAddresses)
//      case _ =>
//        boundAddresses
//     }

    val traceBoundAddresses = traceFull.trace.scanLeft(initialBoundAddresses)({ (boundAddresses, actionState) => handleInfos(actionState, boundAddresses)})
    traceBoundAddresses.map({ (boundAddresses) =>
      boundAddresses ++ assignedAddresses
    }).last
  }

  def analyzeDeadVariables(trace: Trace): Set[String] = {

    def addVariable(variableName: String, liveVariables: Set[String], deadVariables: Set[String]): (Set[String], Set[String]) = {
      if (liveVariables.contains(variableName)) {
        (liveVariables, deadVariables)
      } else {
        (liveVariables, deadVariables + variableName)
      }
    }

    def addVariables(varNames: List[String], liveVariables: Set[String], deadVariables: Set[String]) = {
      varNames.foldLeft((liveVariables, deadVariables))({ (liveDeadVariables, variableName) =>
        addVariable(variableName, liveDeadVariables._1, liveDeadVariables._2)})
    }

    val initialLiveDeadVariables: (Set[String], Set[String]) = (Set(), Set())
    val liveDeadVariables = trace.foldLeft(initialLiveDeadVariables)({ (liveDeadVariables, action) => {
      val (liveVariables, deadVariables) = liveDeadVariables
      action._1 match {
        case ActionLookupVariableT(variableName, _, _) =>
          /* The variable is used somewhere, because it is being looked up */
          (liveVariables + variableName, deadVariables - variableName)
          /* Whenever we allocate a new variable, we initially assign it to the set of dead variables,
           * unless a variable with that name already exists, to avoid confusing two variables with the same name. */
        case ActionAllocVarsT(varNames) =>
          addVariables(varNames, liveVariables, deadVariables)
        case ActionExtendEnvT(variableName) =>
          addVariable(variableName, liveVariables, deadVariables)
        case ActionSetVarT(variableName) =>
          addVariable(variableName, liveVariables, deadVariables)
        case _ => liveDeadVariables
    }
    }})

    /* Only return the variables found to be dead, we're not interested in the live variables. */
    liveDeadVariables._2
  }

}
