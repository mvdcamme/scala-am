import org.scalatest.FunSuite
import Util.runOnFile
import backend.PathConstraint
import backend.expression.ConcolicInput
import ConcreteConcreteLattice.{L => ConcreteValue}
import backend.path_filtering.PartialRegexMatcher

class PartialMatchersTest extends FunSuite {

  implicit val sabsCCLattice = ConcreteConcreteLattice.isSchemeLattice

  val pointsToLattice = new PointsToLattice(false)
  implicit val pointsToConvLattice: IsConvertableLattice[pointsToLattice.L] = pointsToLattice.isSchemeLattice
  implicit val pointsToLatInfoProv = pointsToLattice.latticeInfoProvider
  implicit val CCLatInfoProv = ConcreteConcreteLattice.latticeInfoProvider

  private val connect4Folder = "./testing resources/4-op-een-rij/"
  private val connect4Program = connect4Folder + "4-op-een-rij.rkt"

  private def generateRandomPaths(nrOfPaths: Int, maxPathSize: Int): List[String] = {
    1.to(nrOfPaths).map(_ => {
      val randomSize: Int = scala.util.Random.nextInt(maxPathSize) + 1
      1.to(randomSize).map(_ => if (scala.util.Random.nextInt(2) == 0) "t" else "e").mkString
    }).toList
  }

  private def writeRandomPaths(path: String, nrOfPaths: Int, maxPathSize: Int): List[String] = {
    val randomPaths = generateRandomPaths(nrOfPaths, maxPathSize)
    Util.withFileWriter(path)(writer => writer.write(randomPaths.mkString("\n")))
    randomPaths
  }

  private def makeConcolicMachineAndSemantics(flags: ConcolicRunTimeFlags): (ConcolicMachine[pointsToLattice.L], ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T]) = {
    val reporter = new ScalaAMReporter(flags, new BackendSolver)
    val sem = new ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T](new SchemePrimitives[HybridAddress.A, ConcreteConcreteLattice.L](reporter))
    val pointsToAnalysisLauncher = new PointsToAnalysisLauncher[pointsToLattice.L](sem)(pointsToConvLattice, pointsToLatInfoProv, AnalysisFlags())
    val machine = new ConcolicMachine[pointsToLattice.L](pointsToAnalysisLauncher, AnalysisFlags(), reporter, flags)
    sem.rTAnalysisStarter = machine
    (machine, sem)
  }

  private def performInitialAnalysis(program: String, flags: ConcolicRunTimeFlags): PartialRegexMatcher = {
    val (machine, sem) = makeConcolicMachineAndSemantics(flags)
    val launchAnalyses = new LaunchAnalyses[pointsToLattice.L](machine.analysisLauncher, machine.reporter)
    val initialState: machine.State = machine.inject(sem.parse(program), Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial[HybridAddress.A, ConcreteValue](sem.initialStore))
    val analysisResult = launchAnalyses.startInitialAnalysis(initialState, connect4Program)
    analysisResult.partialMatcher
  }

  test("Empirically tests whether identical partial matchers are generated from the initial static analysis of Connect-4") {
    val randomPaths = generateRandomPaths(1000, 1000)
    val flags = ConcolicRunTimeFlags(100, true, true)
    Util.runOnFile(connect4Program, program => {
      var pm1 = performInitialAnalysis(program, flags)
      var pm2 = performInitialAnalysis(program, flags)
      randomPaths.foreach(path => {
        println(path)
        val (bool1, updatedPm1) = pm1.incrementalMatch(path)
        val (bool2, updatedPm2) = pm2.incrementalMatch(path)
        pm1 = updatedPm1
        pm2 = updatedPm2
        assert(bool1 == bool2)
      })
    })
  }

  test("Empirically tests whether partial matchers generated from the initial static analysis of Connect-4 matches the same strings as was written to file") {
    val maybePreviousPaths = Util.fileContent(connect4Folder + "random_paths")
    val maybePreviousResults = Util.fileContent(connect4Folder + "random_paths_matched")
    assert(maybePreviousPaths.isDefined)
    assert(maybePreviousResults.isDefined)
    val previousPaths = maybePreviousPaths.get.split("\n").toList
    val previousResults = maybePreviousResults.get.split("").toList
    val flags = ConcolicRunTimeFlags(100, true, true)
    Util.runOnFile(connect4Program, program => {
      val pm = performInitialAnalysis(program, flags)
      previousPaths.zip(previousResults).foreach(tuple => {
        val (previousPath, previousResultString) = tuple
        val previousResult = if (previousResultString == "T") true else false
        println(s"Previous path was: $previousPath")
        println(s"With result $previousResult")
        val (currentResult, _) = pm.incrementalMatch(previousPath)
        assert(currentResult == previousResult)
      })
    })
  }

}
