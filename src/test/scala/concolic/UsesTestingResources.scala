import backend.RegularPCElement
import ConcreteConcreteLattice.{L => ConcreteValue}
import abstract_state_heuristic.AbstractStatePCElement

trait UsesTestingResources {

  val testingReourcesFolder: String = "./testing resources/"

  val PP1Folder: String = testingReourcesFolder + "PP1-15-16-Fase1-1"
  val PP1Program: String = PP1Folder + "PP1-15-16-Fase1-1.rkt"

  val connect4Folder: String = testingReourcesFolder + "4-op-een-rij/"
  val connect4Program: String = connect4Folder + "4-op-een-rij.rkt"

}

trait UsesPointsToLattice {

  implicit val sabsCCLattice: IsConvertableLattice[ConcreteValue] = ConcreteConcreteLattice.isSchemeLattice

  val pointsToLattice = new PointsToLattice(false)
  implicit val pointsToConvLattice: IsConvertableLattice[pointsToLattice.L] = pointsToLattice.isSchemeLattice
  implicit val pointsToLatInfoProv: LatticeInfoProvider[pointsToLattice.L] = pointsToLattice.latticeInfoProvider
  implicit val CCLatInfoProv: LatticeInfoProvider[ConcreteValue] = ConcreteConcreteLattice.latticeInfoProvider

  def makeConcolicMachineAndSemantics(flags: ConcolicRunTimeFlags,
    abstSem: ConvertableSchemeSemantics[pointsToLattice.L, HybridAddress.A, HybridTimestamp.T] = new ConvertableSchemeSemantics[pointsToLattice.L, HybridAddress.A, HybridTimestamp.T](new SchemePrimitives(None))):
  (ConcolicMachine[pointsToLattice.L, RegularPCElement, _], ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T, RegularPCElement]) = {
    val inputVariableStore = new InputVariableStore
    val sem = new ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T, RegularPCElement](new SchemePrimitives[HybridAddress.A, ConcreteConcreteLattice.L](Some(inputVariableStore)))
    val pointsToAnalysisLauncher = new PointsToAnalysisLauncher[pointsToLattice.L](abstSem)(pointsToConvLattice, pointsToLatInfoProv, AnalysisFlags())
    val reporter: ScalaAMReporter[RegularPCElement, pointsToAnalysisLauncher.stateConverter.aam.InitialState] = ??? // TODO
    val machine = new ConcolicMachine[pointsToLattice.L, RegularPCElement, pointsToAnalysisLauncher.stateConverter.aam.InitialState](pointsToAnalysisLauncher, AnalysisFlags(), reporter, flags)
    (machine, sem)
  }

}

trait TestCommon extends UsesTestingResources with UsesPointsToLattice