import Util.runOnFile
import backend.Reporter
import backend.expression.ConcolicInput
import org.scalatest.FunSuite

class ConcolicMachineTest extends FunSuite {

  implicit val sabsCCLattice = ConcreteConcreteLattice.isSchemeLattice

  val pointsToLattice = new PointsToLattice(false)
  implicit val pointsToConvLattice: IsConvertableLattice[pointsToLattice.L] = pointsToLattice.isSchemeLattice
  implicit val pointsToLatInfoProv = pointsToLattice.latticeInfoProvider
  implicit val CCLatInfoProv = ConcreteConcreteLattice.latticeInfoProvider


  private val PP1Program = "../scala-am misc/students_code/PP1-15-16-Fase1-1.rkt"
  private val connect4Program = "../scala-am misc/students_code/4-op-een-rij.rkt"

  private def makeConcolicMachineAndSemantics: (ConcolicMachine[pointsToLattice.L], ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T]) = {
    val reporter = new ScalaAMReporter
    val sem = new ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T](new SchemePrimitives[HybridAddress.A, ConcreteConcreteLattice.L](reporter))
    val pointsToAnalysisLauncher = new PointsToAnalysisLauncher[pointsToLattice.L](sem)(pointsToConvLattice, pointsToLatInfoProv, AnalysisFlags())
    val machine = new ConcolicMachine[pointsToLattice.L](pointsToAnalysisLauncher, AnalysisFlags(), reporter)
    sem.rTAnalysisStarter = machine
    (machine, sem)
  }

  private def runProgram(program: String): List[List[(ConcolicInput, Int)]] = {
    val (machine, sem) = makeConcolicMachineAndSemantics
    val output: machine.ConcolicMachineOutput = runOnFile(program, program => machine.concolicEval(program, sem.parse(program), sem, false, Timeout.none))
    assert(output.isInstanceOf[machine.ConcolicMachineOutputInputs])
    output.asInstanceOf[machine.ConcolicMachineOutputInputs].allInputs
  }

  test("Make sure Connect-4 test explores the same inputs, in the same order. Assumes both initial and run-time analyses to have been disabled") {
//    assert(! ConcolicRunTimeFlags.checkAnalysis)
//    assert(! ConcolicRunTimeFlags.checkRunTimeAnalysis)
    val output1 = runProgram(connect4Program)
    Reporter.deleteSymbolicTree()
    val output2 = runProgram(connect4Program)
    assert(output1 == output2)
  }

}
