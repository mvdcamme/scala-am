import scala.collection.mutable.Map
import scala.collection.mutable.Stack

/**
  * Created by mvdcamme on 08/03/16.
  */
class VariableAnalysis[Exp : Expression, Abs, Addr, Time : Timestamp](val sem: SemanticsTraced[Exp, Abs, Addr, Time], val hybridMachine : HybridMachine[Exp, Time]) {

  type ProgramState = HybridMachine[Exp, Time]#ProgramState
  type TraceInstructionStates = HybridMachine[Exp, Time]#TraceInstructionInfo
  type TraceInstruction = HybridMachine[Exp, Time]#TraceInstruction
  type Trace = HybridMachine[Exp, Time]#TraceWithInfos

  type HybridValue = HybridLattice.Hybrid

  def analyzeBoundVariables(initialBoundVariables : Set[String], trace : Trace) : List[Set[String]] = {

    var assignedVariables : Set[String] = Set()

    val framesStack : Stack[List[String]] = Stack(List())

    def addVariable(varName : String, boundVariables : Set[String]) : Set[String] = {
      framesStack.top + varName
      boundVariables + varName
    }

    def addVariables(varNames : List[String], boundVariables : Set[String]) : Set[String]  = {
      varNames.foldLeft(boundVariables)({ (boundVariables, varName) =>
          addVariable(varName, boundVariables)})
    }

    def handleRestoreEnvironment(boundVariables : Set[String]) : Set[String] = {
      if (framesStack.isEmpty) {
        boundVariables
      } else {
        val boundVariablesFrame = framesStack.pop()
        boundVariablesFrame.foldLeft(boundVariables)({ (boundVariables, varName) =>
          boundVariables - varName })
      }
    }

    def handleSaveEnvironment(boundVariables : Set[String]) : Set[String] = {
      framesStack.push(List())
      boundVariables
    }

    def handleSetVar(varName : String, boundVariables : Set[String]) : Set[String] = {
      if (! boundVariables.contains(varName)) {
        assignedVariables += varName
        boundVariables + varName
      } else {
        boundVariables
      }
    }

    def handleAction(action : Action[Exp, HybridValue, HybridAddress], boundVariables : Set[String]) = action match {
      case ActionAllocVarsTraced(varNames) =>
        addVariables(varNames, boundVariables)
      case ActionDefineVarsTraced(varNames) =>
        addVariables(varNames, boundVariables)
      case ActionExtendEnvTraced(varName) =>
        addVariable(varName, boundVariables)
      case ActionSaveEnvTraced() =>
        handleSaveEnvironment(boundVariables)
      case ActionSetVarTraced(varName) =>
        handleSetVar(varName, boundVariables)
      case ActionStepInTraced(_, _, args, _, _, _, _, _) =>
        addVariables(args, handleSaveEnvironment(boundVariables))
      case ActionRestoreEnvTraced() =>
        handleRestoreEnvironment(boundVariables)
      case _ =>
        boundVariables
     }

    val traceBoundVariables = trace.scanLeft(initialBoundVariables)({ (boundVariables, actionState) => handleAction(actionState._1, boundVariables)})
    traceBoundVariables.map({ (boundVariables) =>
      boundVariables ++ assignedVariables
    }).tail

    traceBoundVariables
  }

  def analyzeDeadVariables(trace : Trace) : Set[String] = {

    def addVariable(variableName : String, liveVariables : Set[String], deadVariables : Set[String]) : (Set[String], Set[String]) = {
      if (liveVariables.contains(variableName)) {
        (liveVariables, deadVariables)
      } else {
        (liveVariables, deadVariables + variableName)
      }
    }

    def addVariables(varNames : List[String], liveVariables : Set[String], deadVariables : Set[String]) = {
      varNames.foldLeft((liveVariables, deadVariables))({ (liveDeadVariables, variableName) =>
        addVariable(variableName, liveDeadVariables._1, liveDeadVariables._2)})
    }

    val initialLiveDeadVariables : (Set[String], Set[String]) = (Set(), Set())
    val liveDeadVariables = trace.foldLeft(initialLiveDeadVariables)({ (liveDeadVariables, action) => {
      val (liveVariables, deadVariables) = liveDeadVariables
      action._1 match {
        case ActionLookupVariableTraced(variableName, _, _) =>
          /* The variable is used somewhere, because it is being looked up */
          (liveVariables + variableName, deadVariables - variableName)
          /* Whenever we allocate a new variable, we initially assign it to the set of dead variables,
           * unless a variable with that name already exists, to avoid confusing two variables with the same name. */
        case ActionAllocVarsTraced(varNames) =>
          addVariables(varNames, liveVariables, deadVariables)
        case ActionDefineVarsTraced(varNames) =>
          addVariables(varNames, liveVariables, deadVariables)
        case ActionExtendEnvTraced(variableName) =>
          addVariable(variableName, liveVariables, deadVariables)
        case ActionSetVarTraced(variableName) =>
          addVariable(variableName, liveVariables, deadVariables)
        case _ => liveDeadVariables
    }
    }})

    /* Only return the variables found to be dead, we're not interested in the live variables. */
    liveDeadVariables._2
  }

}
