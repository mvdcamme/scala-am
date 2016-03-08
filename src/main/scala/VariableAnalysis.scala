import scala.collection.mutable.Map
import scala.collection.mutable.Stack

/**
  * Created by mvdcamme on 08/03/16.
  */
class VariableAnalysis[Exp : Expression, Abs, Addr, Time : Timestamp](val sem: SemanticsTraced[Exp, Abs, Addr, Time], val hybridMachine : HybridMachine[Exp, Time]) {

  type ProgramState = HybridMachine[Exp, Time]#ProgramState
  type TraceInstructionStates = HybridMachine[Exp, Time]#TraceInstructionStates
  type TraceInstruction = HybridMachine[Exp, Time]#TraceInstruction
  type Trace = HybridMachine[Exp, Time]#TraceWithStates

  type HybridValue = HybridLattice.Hybrid

  case class VariableInfo(variable : String, isFree : Boolean, isLive : Boolean)

  def analyze(trace : Trace) : List[Set[String]] = {

    var boundVariables : Set[String] = Set()
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
      val boundVariablesFrame = framesStack.pop()
      boundVariablesFrame.foldLeft(boundVariables)({ (boundVariables, varName) =>
        boundVariables - varName })
    }

    def handleSaveEnvironment() = {
      framesStack.push(List())
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
        handleSaveEnvironment()
        boundVariables
      case ActionSetVarTraced(varName) =>
        handleSetVar(varName, boundVariables)
      case ActionStepInTraced(_, _, args, _, _, _, _, _) =>
        addVariables(args, boundVariables)
      case ActionRestoreEnvTraced() =>
        handleRestoreEnvironment(boundVariables)
      case _ =>
        boundVariables
     }

    val traceBoundVariables = trace.scanLeft(Set[String]())({ (boundVariables, actionState) => handleAction(actionState._1, boundVariables)})
    traceBoundVariables.map({ (boundVariables) =>
      boundVariables ++ assignedVariables
    }).tail

    traceBoundVariables
  }

}
