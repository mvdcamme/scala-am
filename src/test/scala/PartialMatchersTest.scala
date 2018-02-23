import org.scalatest.{BeforeAndAfterEach, FunSuite, PrivateMethodTester}
import scala.util.Random

import ConcreteConcreteLattice.{L => ConcreteValue}
import backend.path_filtering.PartialRegexMatcher

class PartialMatchersTest extends FunSuite with PrivateMethodTester with BeforeAndAfterEach with UsesTestingResources with UsesPointsToLattice {

//  private var random = resetRandom
//
//  override def beforeEach(): Unit = {
//    super.beforeEach()
//    println("Random reset")
//    random = resetRandom
//  }

  private def resetRandom: scala.util.Random = {
    new scala.util.Random(421)
  }

  private def generateRandomPaths(nrOfPaths: Int, maxPathSize: Int, random: Random): List[String] = {
    1.to(nrOfPaths).map(_ => {
      val randomSize: Int = random.nextInt(maxPathSize) + 1
      1.to(maxPathSize).map(_ => if (random.nextInt(2) == 0) "t" else "e").mkString
    }).toList
  }

  private def regenerateRandomPaths(): Unit = {
    val randomPaths = generateRandomPaths(2000, 2000, resetRandom)
    Util.withFileWriter(connect4Folder + "random_paths")(writer => writer.write(randomPaths.mkString("\n")))
    Util.withFileWriter(connect4Folder + "random_paths_matched")(writer => {
      Util.runOnFile(connect4Program, program => {
        val pm = performInitialAnalysis(program)
        randomPaths.foreach(path => {
          val (result, _) = pm.incrementalMatch(path)
          if (result) writer.append('T') else writer.append('F')
        })
      })
    })
  }

  private def performInitialAnalysis(program: String): PartialRegexMatcher = {
    val (machine, sem) = makeConcolicMachineAndSemantics(ConcolicRunTimeFlags())
    val launchAnalyses = new LaunchAnalyses[pointsToLattice.L](machine.analysisLauncher, machine.reporter)
    val initialState: machine.State = machine.inject(sem.parse(program), Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial[HybridAddress.A, ConcreteValue](sem.initialStore))
    val analysisResult = launchAnalyses.startInitialAnalysis(initialState, connect4Program)
    analysisResult.partialMatcher
  }

  private def checkPartialMatchersEqual(pm1: PartialRegexMatcher, pm2: PartialRegexMatcher): Unit = {
    val randomPaths = generateRandomPaths(2000, 2000, resetRandom)
    randomPaths.foreach(path => {
      val (result1, _) = pm1.incrementalMatch(path)
      val (result2, _) = pm2.incrementalMatch(path)
      assert(result1 == result2)
    })
  }

  test("Sanity check: randomly generate strings, feed them twice to the same matcher and check if results are the same") {
    Util.runOnFile(connect4Program, (program) => {
      val pm = performInitialAnalysis(program)
      checkPartialMatchersEqual(pm, pm)
    })
  }

  test("1") { // ("Tests whether the same analysis output also results in identical partial matchers") {
    Util.runOnFile(connect4Program, program => {
      val (machine, sem) = makeConcolicMachineAndSemantics(ConcolicRunTimeFlags())
      val launchAnalyses = new LaunchAnalyses[pointsToLattice.L](machine.analysisLauncher, machine.reporter)
      val initialState: machine.State = machine.inject(sem.parse(program), Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial[HybridAddress.A, ConcreteValue](sem.initialStore))
      val analysisResult = machine.analysisLauncher.runInitialStaticAnalysis(initialState, connect4Program)
      val handleInitialAnalysisResultMethod = PrivateMethod[AnalysisResult]('handleInitialAnalysisResult)
      val result1 = launchAnalyses.invokePrivate(handleInitialAnalysisResultMethod(analysisResult))
      val result2 = launchAnalyses.invokePrivate(handleInitialAnalysisResultMethod(analysisResult))
      checkPartialMatchersEqual(result1.partialMatcher, result2.partialMatcher)
    })
  }

  test("2") {
    Util.runOnFile(connect4Program, program => {
      val (machine, sem) = makeConcolicMachineAndSemantics(ConcolicRunTimeFlags())
      val launchAnalyses = new LaunchAnalyses[pointsToLattice.L](machine.analysisLauncher, machine.reporter)
      val initialState: machine.State = machine.inject(sem.parse(program), Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial[HybridAddress.A, ConcreteValue](sem.initialStore))
      val analysisResult = machine.analysisLauncher.runInitialStaticAnalysis(initialState, connect4Program)
      val handleAnalysisResultMethod = PrivateMethod[AnalysisResult]('handleAnalysisResult)
      val result1 = launchAnalyses.invokePrivate(handleAnalysisResultMethod(analysisResult, -1))
      val result2 = launchAnalyses.invokePrivate(handleAnalysisResultMethod(analysisResult, -1))
      checkPartialMatchersEqual(result1.partialMatcher, result2.partialMatcher)
    })
  }

  test("3") { // ("More low-level test to verify that the same analysis output also results in identical partial matchers") {
    Util.runOnFile(connect4Program, program => {
      val (machine, sem) = makeConcolicMachineAndSemantics(ConcolicRunTimeFlags())
      val initialState: machine.State = machine.inject(sem.parse(program), Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial[HybridAddress.A, ConcreteValue](sem.initialStore))
      val analysisResult = machine.analysisLauncher.runInitialStaticAnalysis(initialState, connect4Program)
      val errorPathDetector = new ErrorPathDetector[SchemeExp, pointsToLattice.L, HybridAddress.A, HybridTimestamp.T, machine.analysisLauncher.aam.State](machine.analysisLauncher.aam)
      val result1 = errorPathDetector.detectErrors(analysisResult.graph, analysisResult.stepSwitched, -1)
      val result2 = errorPathDetector.detectErrors(analysisResult.graph, analysisResult.stepSwitched, -1)
      checkPartialMatchersEqual(result1.get, result2.get)
    })
  }

  test("Empirically tests whether identical partial matchers are generated from the initial static analysis of Connect-4") {
    Util.runOnFile(connect4Program, program => {
      val pm1 = performInitialAnalysis(program)
      val pm2 = performInitialAnalysis(program)
      checkPartialMatchersEqual(pm1, pm2)
    })
  }

  test("Empirically tests whether partial matchers generated from the initial static analysis of Connect-4 matches the same strings as was written to file") {
    val maybePreviousPaths = Util.fileContent(connect4Folder + "random_paths")
    val maybePreviousResults = Util.fileContent(connect4Folder + "random_paths_matched")
    assert(maybePreviousPaths.isDefined)
    assert(maybePreviousResults.isDefined)
    val previousPaths = maybePreviousPaths.get.split("\n").toList
    val previousResults = maybePreviousResults.get.split("").toList
    Util.runOnFile(connect4Program, program => {
      val pm = performInitialAnalysis(program)
      previousPaths.zip(previousResults).foreach(tuple => {
        val (previousPath, previousResultString) = tuple
        val previousResult = if (previousResultString == "T") true else false
        val (currentResult, _) = pm.incrementalMatch(previousPath)
        assert(currentResult == previousResult)
      })
    })
  }
}
