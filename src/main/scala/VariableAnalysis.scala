import scala.collection.mutable.Map
import scala.collection.mutable.Map
import scala.collection.mutable.Stack

/**
  * Created by mvdcamme on 08/03/16.
  */
class VariableAnalysis[Exp: Expression, Addr: Address, Time: Timestamp](
    val sem: SemanticsTraced[Exp, ConcreteConcreteLattice.L, Addr, Time]) {

  type TraceInstructionStates =
    Tracer[Exp, ConcreteConcreteLattice.L, Addr, Time]#TraceInstructionInfo
  type TraceInstruction =
    Tracer[Exp, ConcreteConcreteLattice.L, Addr, Time]#TraceInstruction
  type Trace =
    Tracer[Exp, ConcreteConcreteLattice.L, Addr, Time]#TraceWithInfos

  def analyzeDeadVariables(trace: Trace): Set[String] = {

    def addVariable(variableName: String,
                    liveVariables: Set[String],
                    deadVariables: Set[String]): (Set[String], Set[String]) = {
      if (liveVariables.contains(variableName)) {
        (liveVariables, deadVariables)
      } else {
        (liveVariables, deadVariables + variableName)
      }
    }

    def addVariables(varNames: List[String],
                     liveVariables: Set[String],
                     deadVariables: Set[String]) = {
      varNames.foldLeft((liveVariables, deadVariables))({
        (liveDeadVariables, variableName) =>
          addVariable(variableName, liveDeadVariables._1, liveDeadVariables._2)
      })
    }

    val initialLiveDeadVariables: (Set[String], Set[String]) = (Set(), Set())
    val liveDeadVariables = trace.foldLeft(initialLiveDeadVariables)({
      (liveDeadVariables, action) =>
        {
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
        }
    })

    /* Only return the variables found to be dead, we're not interested in the live variables. */
    liveDeadVariables._2
  }

}
