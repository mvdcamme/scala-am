import java.io.FileInputStream

import Util.runOnFile
import backend.{PathConstraint, Reporter}
import backend.expression.ConcolicInput
import org.scalatest.{BeforeAndAfterEach, FunSuite}

class ConcolicMachineTest extends FunSuite with BeforeAndAfterEach {

  implicit val sabsCCLattice = ConcreteConcreteLattice.isSchemeLattice

  val pointsToLattice = new PointsToLattice(false)
  implicit val pointsToConvLattice: IsConvertableLattice[pointsToLattice.L] = pointsToLattice.isSchemeLattice
  implicit val pointsToLatInfoProv = pointsToLattice.latticeInfoProvider
  implicit val CCLatInfoProv = ConcreteConcreteLattice.latticeInfoProvider


  private val PP1Program = "../scala-am misc/students_code/PP1-15-16-Fase1-1.rkt"
  private val connect4Folder = "./testing resources/4-op-een-rij/"
  private val connect4Program = connect4Folder + "4-op-een-rij.rkt"

  private def makeConcolicMachineAndSemantics(flags: ConcolicRunTimeFlags): (ConcolicMachine[pointsToLattice.L], ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T]) = {
    val reporter = new ScalaAMReporter(flags)
    val sem = new ConcolicBaseSchemeSemantics[HybridAddress.A, HybridTimestamp.T](new SchemePrimitives[HybridAddress.A, ConcreteConcreteLattice.L](reporter))
    val pointsToAnalysisLauncher = new PointsToAnalysisLauncher[pointsToLattice.L](sem)(pointsToConvLattice, pointsToLatInfoProv, AnalysisFlags())
    val machine = new ConcolicMachine[pointsToLattice.L](pointsToAnalysisLauncher, AnalysisFlags(), reporter, flags)
    sem.rTAnalysisStarter = machine
    (machine, sem)
  }

  private def readFile(path: String): String = {
    val br = new java.io.BufferedReader(new java.io.FileReader(path))
    val sb = new StringBuilder
    var line: String = ""
    do {
      sb.append(line)
      line = br.readLine
    }
    while (line != null)
    br.close()
    sb.mkString
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    Reporter.deleteSymbolicTree()
  }

  private def runProgram(program: String, flags: ConcolicRunTimeFlags): (List[List[(ConcolicInput, Int)]], List[PathConstraint]) = {
    val (machine, sem) = makeConcolicMachineAndSemantics(flags)
    val output: machine.ConcolicMachineOutput = runOnFile(program, program => machine.concolicEval(program, sem.parse(program), sem, false, Timeout.none))
    assert(output.isInstanceOf[machine.ConcolicMachineOutputNormal])
    val castedOutput = output.asInstanceOf[machine.ConcolicMachineOutputNormal]
    (castedOutput.allInputs, castedOutput.allPathConstraints)
  }

  test("Make sure Connect-4 test explores the same inputs, in the same order, and generates the same path constraints," +
       " in the same order. Assumes both initial and run-time analyses to have been disabled") {
    val (allInputs1, allPathConstraints1) = runProgram(connect4Program, ConcolicRunTimeFlags(100, false, false))
    Reporter.deleteSymbolicTree()
    val (allInputs2, allPathConstraints2) = runProgram(connect4Program, ConcolicRunTimeFlags(100, false, false))
    assert(allInputs1 == allInputs2)
    assert(allPathConstraints1 == allPathConstraints2)
  }

  test("Test whether path constraints are ") {
    val (_, pathConstraints1) = runProgram(connect4Program, ConcolicRunTimeFlags(100, false, false))
    val previousPathConstraints = readFile(connect4Folder + "path_constraints")
    assert(pathConstraints1.toString == previousPathConstraints)
  }
}
