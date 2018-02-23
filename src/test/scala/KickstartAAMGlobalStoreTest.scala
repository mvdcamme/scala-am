import org.scalatest.{BeforeAndAfterEach, FunSuite, PrivateMethodTester}

class KickstartAAMGlobalStoreTest extends FunSuite with PrivateMethodTester with BeforeAndAfterEach
                                  with UsesTestingResources with UsesPointsToLattice {

  override def beforeEach(): Unit = {
    super.beforeEach()
    HybridTimestamp.switchToAbstract()
  }

  test("Test whether the abstracted initial ConcolicMachine's state equals an abstract initial state") {
    Util.runOnFile(connect4Program, (program) => {

      HybridTimestamp.switchToAbstract()
      val schemePrimitives = new SchemePrimitives[HybridAddress.A, pointsToLattice.L]
      val abstSem = new ConvertableSchemeSemantics[pointsToLattice.L, HybridAddress.A, HybridTimestamp.T](schemePrimitives)
      val abstMachine = new KickstartAAMGlobalStore[SchemeExp, pointsToLattice.L, HybridAddress.A, HybridTimestamp.T]
      val parsedProgram = abstSem.parse(program)

      val abstInitState = abstMachine.State.inject(parsedProgram, abstSem.initialEnv, abstSem.initialStore)
      HybridTimestamp.switchToConcrete()

      val (concMachine, concSem) = makeConcolicMachineAndSemantics(ConcolicRunTimeFlags(), abstSem)
      val concInitState = concMachine.inject(parsedProgram, Environment.initial(concSem.initialEnv), Store.initial(concSem.initialStore))
      val abstractedConcInitState = concMachine.analysisLauncher.convertStateAAM(abstMachine, concSem, abstSem, concInitState, Nil)

      assert(abstInitState == abstractedConcInitState)
    })
  }

  test("") {
    val maybeContent = Util.fileContent(connect4Program)
    assert(maybeContent.isDefined)
    val content = maybeContent.get

    val sem = new ConvertableSchemeSemantics[pointsToLattice.L, HybridAddress.A, HybridTimestamp.T](new SchemePrimitives)
    val machine = new KickstartAAMGlobalStore[SchemeExp, pointsToLattice.L, HybridAddress.A, HybridTimestamp.T]
    println("before")
    val parsedProgram = sem.parse(content)
    val output1 = machine.eval(parsedProgram, sem, true, Timeout.none).asInstanceOf[machine.AAMOutput]
    println("mid")
    val output2 = machine.eval(parsedProgram, sem, true, Timeout.none).asInstanceOf[machine.AAMOutput]
    println("after")

    assert(output1.graph.nodes == output2.graph.nodes)
  }

  test("Sanity check: test whether the graphs produced by two initial static analyses of the same program have the same nodes") {
    Util.runOnFile(connect4Program, program => {
      val (machine, sem) = makeConcolicMachineAndSemantics(ConcolicRunTimeFlags())
      val parsedProgram = sem.parse(program)
      val output1 = machine.analysisLauncher.runInitialStaticAnalysis(machine.inject(parsedProgram, Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial(sem.initialStore)), connect4Program)
      val output2 = machine.analysisLauncher.runInitialStaticAnalysis(machine.inject(parsedProgram, Environment.initial[HybridAddress.A](sem.initialEnv), Store.initial(sem.initialStore)), connect4Program)
      assert(output1.graph.nodes == output2.graph.nodes)
    })
  }

}
