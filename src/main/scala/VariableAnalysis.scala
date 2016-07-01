import scala.collection.mutable.Map
import scala.collection.mutable.Map
import scala.collection.mutable.Stack

/**
  * Created by mvdcamme on 08/03/16.
  */
class VariableAnalysis[Exp : Expression, Addr : Address, Time : Timestamp]
  (val sem: SemanticsTraced[Exp, HybridLattice.L, Addr, Time]) {

  type TraceInstructionStates = Tracer[Exp, Time]#TraceInstructionInfo
  type TraceInstruction = Tracer[Exp, Time]#TraceInstruction
  type Trace = Tracer[Exp, Time]#TraceWithInfos

  type HybridValue = HybridLattice.L

  /**
    * Computes the set of bound variables in the given trace.
    * @param initialBoundVariables The list of variables, if any, that are initially bound in the trace: e.g.,
    *                              the parameters of the function being traced.
    * @param traceFull The trace of which the bound variables must be computed.
    * @return The set of bound variables in the trace.
    */
  def analyzeBoundVariables(initialBoundVariables: Set[String], traceFull: TraceFull[Exp, Time]): Set[String] = {

    /*
     * Compute, for each action in the trace, what the set of bound variables are at this position in the trace.
     * This set is computed by starting from the inital set of bound variables, and adding a new bound variable
     * whenever we encounter a new action that binds a variable.
     *
     * However, we have to take saving/restoring of the environment into account: when we save an environment,
     * bind a variable and then restore the environment, this variable is no longer bound at the point in the
     * trace after the restoration of the environment.
     * Concretely, if we e.g., define a new variable and afterwards restore some previous environment, the variable
     * is no longer part of that environment and must therefore be removed from the set of bound variables from
     * that point in the trace onwards.
     * We therefore simulate saving/restoring the environment via the framesStack.
     *
     * Lastly, if we encounter an assignment to some variable at any point in the trace, this variable becomes bound
     * AT ALL POINTS in the trace. Note that this overapproximates the set of bound variables.
     */

    val initialState: ProgramState[Exp, Time] = traceFull.info.startState match {
      case s: ProgramState[Exp, Time] => s
      case _ => throw new Exception(s"Variable folding optimization expected state of type ProgramState[Exp, Time], got state ${traceFull.info.startState} instead")
    }

    var currentEnv: Environment[HybridAddress.A] = initialState.ρ
    var vStack: List[Storable[HybridValue, HybridAddress.A]] = initialState.vStack

    /*
     * The set of variables that are assigned, not defined, to inside of the trace.
     * I.e., the set of variables involved in an ActionSetVarTraced.
     */
    var assignedVariables: Set[String] = Set()

    /*
     * Simulates the environment stack: saving the environment triggers a push of a new, empty, list of vars on this
     * stack, restoring the environment triggers a pop.
     *
     */
    val framesStack: Stack[List[String]] = Stack(List())

    /**
      * Adds a new bound variable, i.e., because an action is encountered that defines this variable in the environment.
      * If the variable was not already placed in the boundVariables-set, it is inserted there.
      * @param varName The name of the bound variable.
      * @param boundVariables The set of previously encountered bound variables
      * @return The updated set of bound variables.
      */
    def addVariable(varName: String, boundVariables: Set[String]): Set[String] = {
      if (framesStack.isEmpty) {
        framesStack.push(List())
      }
      framesStack.top + varName
      boundVariables + varName
    }

    /**
      * Adds a list of newly bound variables. Similar to [[addVariable(String, Set[String]].
      * @param varNames The names of the list of bound variables.
      * @param boundVariables The set of previously encountered bound variables
      * @return The updated set of bound variables.
      */
    def addVariables(varNames: List[String], boundVariables: Set[String]): Set[String]  = {
      varNames.foldLeft(boundVariables)({ (boundVariables, varName) =>
          addVariable(varName, boundVariables)})
    }

    def handleRestoreEnvironment(boundVariables: Set[String]): Set[String] = {
      if (framesStack.isEmpty) {
        boundVariables
      } else {
        val boundVariablesFrame = framesStack.pop()
        boundVariablesFrame.foldLeft(boundVariables)({ (boundVariables, varName) =>
          boundVariables - varName })
      }
    }

    def handleSaveEnvironment(boundVariables: Set[String]): Set[String] = {
      framesStack.push(List())
      boundVariables
    }

    def handleSetVar(varName: String, boundVariables: Set[String]): Set[String] = {
      if (! boundVariables.contains(varName)) {
        assignedVariables += varName
        boundVariables + varName
      } else {
        boundVariables
      }
    }

    def handleAction(action: ActionT[Exp, HybridValue, HybridAddress.A], boundVariables: Set[String]) = action match {
      case ActionAllocVarsT(varNames) =>
        addVariables(varNames, boundVariables)
      case ActionExtendEnvT(varName) =>
        addVariable(varName, boundVariables)
      case ActionSaveEnvT() =>
        handleSaveEnvironment(boundVariables)
      case ActionSetVarT(varName) =>
        handleSetVar(varName, boundVariables)
      case ActionStepInT(_, _, args, _, _, _, _, _) =>
        addVariables(args, handleSaveEnvironment(boundVariables))
      case ActionRestoreEnvT() =>
        handleRestoreEnvironment(boundVariables)
      case _ =>
        boundVariables
     }

    val traceBoundVariables = traceFull.trace.scanLeft(initialBoundVariables)({ (boundVariables, actionState) => handleAction(actionState._1, boundVariables)})
    traceBoundVariables.map({ (boundVariables) =>
      boundVariables ++ assignedVariables
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
