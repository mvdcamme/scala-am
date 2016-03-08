import scala.collection.mutable.Stack
import scala.collection.mutable.Set

/**
  * Created by mvdcamme on 08/03/16.
  */
class VariableAnalysis[Exp : Expression, Abs, Addr, Time : Timestamp](val sem: SemanticsTraced[Exp, Abs, Addr, Time], val hybridMachine : HybridMachine[Exp, Time]) {

  type ProgramState = hybridMachine.ProgramState
  type TraceInstructionStates = hybridMachine.TraceInstructionStates
  type TraceInstruction = hybridMachine.TraceInstruction
  type Trace = hybridMachine.TraceWithStates

  type HybridValue = HybridLattice.Hybrid

  case class VariableInfo(variable : String, isFree : Boolean, isLive : Boolean)

  def analyze(trace : Trace) : Map[String, VariableInfo] = {

    val boundVariables : Map[String, VariableInfo] = Map()

    val framesStack : Stack[List[String]] = Stack(List())

    val assignedVars : Set[String] = Set()

    def addVariable(varName : String) = {
      val varInfo = VariableInfo(varName, false, true)
      boundVariables + (varName -> varInfo)
      framesStack.top + varName
    }

    def addVariables(varNames : List[String]) = {
      for (varName <- varNames) {
        addVariable(varName)
      }
    }

    def handleRestoreEnvironment() {
      val boundVariablesFrame = framesStack.pop()
      for (varName <- boundVariablesFrame) {
        boundVariables - varName
      }
    }

    def handleSaveEnvironment() = {
      framesStack.push(List())
    }

    def handleSetVar(varName : String) = {
      if (! boundVariables.contains(varName)) {
        val varInfo = VariableInfo(varName, false, true)
        assignedVars + varName
        boundVariables + (varName -> varInfo)
      }
    }

    def handleAction(action : Action[Exp, HybridValue, HybridAddress]) = action match {
      case ActionAllocVarsTraced(varNames) =>
        addVariables(varNames)
      case ActionDefineVarsTraced(varNames) =>
        addVariables(varNames)
      case ActionExtendEnvTraced(varName) =>
        addVariable(varName)
      case ActionSaveEnvTraced() =>
        handleSaveEnvironment()
      case ActionSetVarTraced(varName) =>
        handleSetVar(varName)
      case ActionStepInTraced(_, _, args, _, _, _, _, _) =>
        addVariables(args)
      case ActionRestoreEnvTraced() =>
        handleRestoreEnvironment()
     }

    trace.foreach({ (actionState) => handleAction(actionState._1)})
    boundVariables

  }

}
