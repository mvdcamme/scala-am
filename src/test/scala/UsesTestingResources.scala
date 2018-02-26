import ConcreteConcreteLattice.{L => ConcreteValue}

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
    abstSem: ConvertableSchemeSemantics[pointsToLattice.L, HybridAddress.A, HybridTimestamp.T] = new ConvertableSchemeSemantics[pointsToLattice.L, HybridAddress.A, HybridTimestamp.T](new SchemePrimitives)):
  (ConcolicMachine[pointsToLattice.L], ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T]) = {
    val reporter = new ScalaAMReporter(flags, new BackendSolver)
    val sem = new ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T](new SchemePrimitives[HybridAddress.A, ConcreteConcreteLattice.L](reporter))
    val pointsToAnalysisLauncher = new PointsToAnalysisLauncher[pointsToLattice.L](sem, abstSem)(pointsToConvLattice, pointsToLatInfoProv, AnalysisFlags())
    val machine = new ConcolicMachine[pointsToLattice.L](pointsToAnalysisLauncher, AnalysisFlags(), reporter, flags)
    sem.rTAnalysisStarter = machine
    (machine, sem)
  }

}

trait TestCommon extends UsesTestingResources with UsesPointsToLattice