import scala.collection.mutable.Stack

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

  def analyze(trace : Trace) : List[Map[String, VariableInfo]] = {

    val boundVariables : Map[String, VariableInfo] = Map()

    val framesStack : Stack[List[String]] = Stack(List())

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

    def restoreEnvironment() {
      val boundVariablesFrame = framesStack.pop()
      for (varName <- boundVariablesFrame) {
        boundVariables - varName
      }
    }

    def saveEnvironment() = {
      framesStack.push(List())
    }

    def handleAction(action : Action[Exp, HybridValue, HybridAddress]) = action match {
      case ActionAllocVarsTraced(varNames) =>
        addVariables(varNames)
      case ActionDefineVarsTraced(varNames) =>
        addVariables(varNames)
      case ActionExtendEnvTraced(varName) =>
        addVariable(varName)
      case ActionSaveEnvTraced() =>
        saveEnvironment()
      case ActionStepInTraced(_, _, args, _, _, _, _, _) =>
        addVariables(args)
      case ActionRestoreEnvTraced() =>
        restoreEnvironment()
     }

    def loop(trace : Trace) : List[Map[String, VariableInfo]] = {
      trace.map({ (actionState) => handleAction(actionState._1); boundVariables})
    }

    loop(trace)
  }

}
