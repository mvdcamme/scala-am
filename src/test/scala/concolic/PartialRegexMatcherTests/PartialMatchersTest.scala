import backend.path_filtering.PartialRegexMatcher
import org.scalatest.{BeforeAndAfterEach, FunSuite, PrivateMethodTester}
import ConcreteConcreteLattice.{L => ConcreteValue}
import backend.{PathConstraint, RegularPCElement}

class PartialMatchersTest extends FunSuite with PrivateMethodTester with BeforeAndAfterEach with TestCommon with TestsPartialMatchers {

  val (machine: ConcolicMachine[pointsToLattice.L, RegularPCElement, _], sem: ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T, RegularPCElement]) = makeConcolicMachineAndSemantics(ConcolicRunTimeFlags())

  val (launchAnalyses: LaunchAnalyses[pointsToLattice.L, RegularPCElement],
       initialState: machine.State,
       analysisResult: AnalysisOutputGraph[SchemeExp, pointsToLattice.L, HybridAddress.A, machine.analysisLauncher.aam.State]) = Util.runOnFile(connect4Program, (program) => {
    val launchAnalyses = new LaunchAnalyses[pointsToLattice.L, RegularPCElement](machine.analysisLauncher, machine.reporter)
    val initialState: machine.State = machine.inject(sem.parse(program), Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial[HybridAddress.A, ConcreteValue](sem.initialStore))
    val analysisResult = machine.analysisLauncher.runInitialStaticAnalysis(initialState, connect4Program)
    (launchAnalyses, initialState, analysisResult)
  })

  private def regenerateRandomPaths(): Unit = {
    val randomPaths = generateRandomPaths(2000, 2000)
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
    val launchAnalyses = new LaunchAnalyses[pointsToLattice.L, RegularPCElement](machine.analysisLauncher, machine.reporter)
    val initialState: machine.State = machine.inject(sem.parse(program), Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial[HybridAddress.A, ConcreteValue](sem.initialStore))
    val analysisResult = launchAnalyses.startInitialAnalysis(initialState, connect4Program)
    analysisResult.partialMatcher
  }

  test("Sanity check: randomly generate strings, feed them twice to the same matcher and check if results are the same") {
    Util.runOnFile(connect4Program, (program) => {
      val pm = performInitialAnalysis(program)
      checkPartialMatchersEqual(pm, pm)
    })
  }

  test("1 Check same PM from same analysis result") {
    val handleInitialAnalysisResultMethod = PrivateMethod[AnalysisResult]('handleInitialAnalysisResult)
    val result1 = launchAnalyses.invokePrivate(handleInitialAnalysisResultMethod(analysisResult))
    val result2 = launchAnalyses.invokePrivate(handleInitialAnalysisResultMethod(analysisResult))
    checkPartialMatchersEqual(result1.partialMatcher, result2.partialMatcher)
    checkPartialMatcherGraphsEqual(result1.partialMatcher, result2.partialMatcher)
  }

  test("2 Check same PM from same analysis result") {
    val handleAnalysisResultMethod = PrivateMethod[AnalysisResult]('handleAnalysisResult)
    val result1 = launchAnalyses.invokePrivate(handleAnalysisResultMethod(analysisResult, -1))
    val result2 = launchAnalyses.invokePrivate(handleAnalysisResultMethod(analysisResult, -1))
    checkPartialMatchersEqual(result1.partialMatcher, result2.partialMatcher)
    checkPartialMatcherGraphsEqual(result1.partialMatcher, result2.partialMatcher)
  }

  test("3 Check same PM from same analysis result") {
    val errorPathDetector = new ErrorPathDetector[SchemeExp, pointsToLattice.L, HybridAddress.A, HybridTimestamp.T, machine.analysisLauncher.aam.State](machine.analysisLauncher.aam)
    val result1 = errorPathDetector.detectErrors(analysisResult.graph, analysisResult.stepSwitched, -1)
    val result2 = errorPathDetector.detectErrors(analysisResult.graph, analysisResult.stepSwitched, -1)
    checkPartialMatchersEqual(result1.get, result2.get)
    checkPartialMatcherGraphsEqual(result1.get, result2.get)
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
        val previousResult = previousResultString == "T"
        val (currentResult, _) = pm.incrementalMatch(previousPath)
        assert(currentResult == previousResult)
      })
    })
  }

  private def loopUntilIfConditionEvaluated(state: machine.State, stepCount: Int): (machine.State, Int, Boolean) = machine.step(state, sem) match {
    case Left(_) =>
      assert(false, "Should not happen")
      ???
    case Right(succeeded) if succeeded.filters.semanticsFilters.contains(ElseBranchFilter) =>
      (succeeded.state, stepCount, false)
    case Right(succeeded) if succeeded.filters.semanticsFilters.contains(ThenBranchFilter) =>
      (succeeded.state, stepCount, true)
    case Right(succeeded) =>
      loopUntilIfConditionEvaluated(succeeded.state, stepCount + 1)
  }

  test("Do 100 concrete steps, start two RT analyses from the same concrete state and check if their graphs are equal") {
    Util.runOnFile(connect4Program, program => {
      val initialState = machine.inject(sem.parse(program), Environment.initial(sem.initialEnv), Store.initial(sem.initialStore))
      val state100 = 1.to(100).foldLeft(initialState)((state, _) => {
        machine.step(state, sem) match {
          case Left(_) =>
            assert(false, "Should not happen")
            ???
          case Right(succeeded) =>
            succeeded.state
        }
      })
      /* Now start stepping from state100 until a state is reached where an if-condition has been evaluated. */
      val (concreteState, stepCount, thenBranchTaken) = loopUntilIfConditionEvaluated(state100, 100)
      val launchAnalysis1 = new LaunchAnalyses[pointsToLattice.L, RegularPCElement](machine.analysisLauncher, machine.reporter)
      val output1 = launchAnalysis1.startRunTimeAnalysis(concreteState, thenBranchTaken, stepCount, machine.reporter.pathStorage.getCurrentReport, 1)
      val launchAnalysis2 = new LaunchAnalyses[pointsToLattice.L, RegularPCElement](machine.analysisLauncher, machine.reporter)
      val output2 = launchAnalysis2.startRunTimeAnalysis(concreteState, thenBranchTaken, stepCount, machine.reporter.pathStorage.getCurrentReport, 1)
      checkPartialMatchersEqual(output1.partialMatcher, output2.partialMatcher)
    })
  }
}
