import backend.expression._
import backend.tree._

trait SemanticsConcolicHelper {
  def handleIf(optConcolicExpression: Option[ConcolicExpression], thenBranchTaken: Boolean): Unit
  def tryMerge(exp: SchemeExp): Unit
}

abstract class BaseSemanticsConcolicHelper[PCElementUsed, RTInitialState, RTNormalState](
  val rtAnalysisStarter: RTAnalysisStarter[RTInitialState, _],
  val abstractGraphTracker: AbstractGraphTracker[RTNormalState],
  val reporter: ScalaAMReporter[PCElementUsed, _],
  val concolicFlags: ConcolicRunTimeFlags) extends SemanticsConcolicHelper with DoMergingHelper[PCElementUsed, RTInitialState] {

  val concolicMachineStateComparerOuter = new ConcolicMachineStateComparerWithAnalysis[RTNormalState](DefaultHybridAddressConverter)

  protected val concreteStatesStore: ConcreteStatesStore = new SingleConcreteStatesStore
  protected def makeStateComparer: ConcolicMachineStateComparer = {
    if (concolicFlags.checkAnalysis) {
      val currentAbstractStateNodes = abstractGraphTracker.getCurrentAbstractStateNode.get
      val initialStateGraph: Graph[RTNormalState, EdgeAnnotation[_, _, HybridAddress.A], _] = abstractGraphTracker.getInitialStateGraph.get
      new concolicMachineStateComparerOuter.DoIt(initialStateGraph, currentAbstractStateNodes)
    } else {
      ConcolicMachineStateComparerBasic
    }
  }

  def handleIf(optConcolicExpression: Option[ConcolicExpression], thenBranchTaken: Boolean): Unit = {
    reporter.pathStorage.updateCurrentPath(thenBranchTaken)
    optConcolicExpression match {
      case Some(b: BooleanConcolicExpression) =>
        val baseConstraint = BranchConstraint(b)
        reporter.addBranchConstraint(baseConstraint, thenBranchTaken, rtAnalysisStarter)
      case Some(_) =>
        reporter.addUnusableConstraint(thenBranchTaken)
        Logger.E(s"Using a non-BooleanConcolicExpression in a branch constraint: $optConcolicExpression")
      case None => reporter.addUnusableConstraint(thenBranchTaken)
    }
  }
}

trait DoMergingHelper[PCElementUsed, RTInitialState] {
  protected def makeStateComparer: ConcolicMachineStateComparer
  def rtAnalysisStarter: RTAnalysisStarter[RTInitialState, _]
  def reporter: ScalaAMReporter[PCElementUsed, _]
  protected def concreteStatesStore: ConcreteStatesStore
}

trait DoMerging { this: DoMergingHelper[_, _] =>

  def tryMerge(exp: SchemeExp): Unit = {
    val stateComparer: ConcolicMachineStateComparer = makeStateComparer
    val currentState = rtAnalysisStarter.getCurrentState
    val gcedState = currentState.garbageCollectState
    val currentStateStoreKeys = currentState.store.toSet.filterNot(t => HybridAddress.isAddress.isPrimitive(t._1))
    val gcedStoreKeys = gcedState.store.toSet.filterNot(t => HybridAddress.isAddress.isPrimitive(t._1))
    val path = reporter.pathStorage.getNonTriviallyResettablePath
    if (concreteStatesStore.containsState(exp, gcedState, path, stateComparer)) {
      Logger.E("FOUND AN EQUIVALENT STATE")
      throw MergeExecutionPath
    } else {
      concreteStatesStore.addState(exp, gcedState, path)
    }
  }
}

trait NoMerging {
  def tryMerge(exp: SchemeExp): Unit = {}
}

class MergingSemanticsConcolicHelper[PCElementUsed, RTInitialState, RTNormalState](
  rtAnalysisStarter: RTAnalysisStarter[RTInitialState, _],
  abstractGraphTracker: AbstractGraphTracker[RTNormalState],
  reporter: ScalaAMReporter[PCElementUsed, _],
  concolicFlags: ConcolicRunTimeFlags)
  extends BaseSemanticsConcolicHelper(rtAnalysisStarter, abstractGraphTracker, reporter, concolicFlags) with DoMerging {
}

class NoMergingSemanticsConcolicHelper[PCElementUsed, RTInitialState, RTNormalState](
  rtAnalysisStarter: RTAnalysisStarter[RTInitialState, _],
  abstractGraphTracker: AbstractGraphTracker[RTNormalState],
  reporter: ScalaAMReporter[PCElementUsed, _],
  concolicFlags: ConcolicRunTimeFlags) extends BaseSemanticsConcolicHelper(rtAnalysisStarter, abstractGraphTracker, reporter, concolicFlags) with NoMerging {
}
