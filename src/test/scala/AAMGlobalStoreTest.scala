import org.scalatest.FunSuite

class AAMGlobalStoreTest extends FunSuite with UsesPointsToLattice with UsesTestingResources {

  test("Sanity check") {
    val sem = new SchemeSemantics[pointsToLattice.L, ClassicalAddress.A, ZeroCFA.T](new SchemePrimitives)
    val machine = new AAMAACP4F[SchemeExp, pointsToLattice.L, ClassicalAddress.A, ZeroCFA.T](AAMKAlloc)
    Util.runOnFile(connect4Program, (program) => {
      val parsedProgram = sem.parse(program)
      val output1 = machine.eval(parsedProgram, sem, true, Timeout.none)
      val output2 = machine.eval(parsedProgram, sem, true, Timeout.none)
      assert(output1.finalValues == output2.finalValues)
      assert(output1.numberOfStates == output2.numberOfStates)
      val aamOutput1 = output1.asInstanceOf[machine.AAMAACP4FOutput]
      val aamOutput2 = output2.asInstanceOf[machine.AAMAACP4FOutput]
      assert(aamOutput1.store.toSet == aamOutput2.store.toSet)
      assert(aamOutput1.graph.get.nodes == aamOutput2.graph.get.nodes)
      assert(aamOutput1.graph.get.edges == aamOutput2.graph.get.edges)
    })
  }

}
