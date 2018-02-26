import org.scalatest.{BeforeAndAfterEach, FunSuite}

import Util.runOnFile
import backend.{PathConstraint, Reporter}
import backend.expression.ConcolicInput
import backend.solvers.{ConcolicSolver, ConcolicSolverResult, NewInput, SymbolicTreeFullyExplored}

class ConcolicMachineTest extends FunSuite with BeforeAndAfterEach with TestCommon {

  private def pathConstraintsToString(allPathConstraints: List[PathConstraint]): String = {
    allPathConstraints.map(_.map(tuple => if (tuple._2) "t" else "e").mkString).mkString("\n")
  }

  private def regeneratePaths(): Unit = {
    val (_, pathConstraints) = runProgram(connect4Program, ConcolicRunTimeFlags(100, true, true))
    val allPaths = pathConstraintsToString(pathConstraints)
    writeFile(connect4Folder + "all_paths", allPaths)
  }

  private def writeFile(path: String, content: String): Unit = {
    val fw = new java.io.FileWriter(path)
    fw.write(content)
    fw.close()
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

  class MockupSolver extends SolverInterface {
    private var mockupResults: List[NewInput] = List(
      /* IT 2 */  NewInput(Map(ConcolicInput(0) -> 1)),
      /* IT 3 */  NewInput(Map(ConcolicInput(0) -> 0, ConcolicInput(1) -> 1)),
      /* IT 4 */  NewInput(Map(ConcolicInput(0) -> 2)))
//      /* IT 5 */  NewInput(Map(ConcolicInput(0) -> 0, ConcolicInput(1) -> 0, ConcolicInput(2) -> 1)),
//      /* IT 6 */  NewInput(Map(ConcolicInput(0) -> 0, ConcolicInput(1) -> 2)))
//      /* IT 7 */  NewInput(Map(ConcolicInput(0) -> 1, ConcolicInput(1) -> 1)),
//      /* IT 8 */  NewInput(Map(ConcolicInput(0) -> 3)))
//      /* IT 9 */  NewInput(Map(ConcolicInput(0) -> 0, ConcolicInput(1) -> 0, ConcolicInput(2) -> 1, ConcolicInput(3) -> 1)),
//      /* IT 10 */ NewInput(Map(ConcolicInput(0) -> 0, ConcolicInput(1) -> 0, ConcolicInput(2) -> 2)))
//      /* IT 11 */  NewInput(Map(ConcolicInput(0) -> 0, ConcolicInput(1) -> 1, ConcolicInput(2) -> 1)))
    def solve: ConcolicSolverResult = {
      ConcolicSolver.solve
      mockupResults.headOption match {
        case Some(result) =>
          mockupResults = mockupResults.tail
          result
        case None => SymbolicTreeFullyExplored
      }
    }
  }

  private def runProgram(program: String, flags: ConcolicRunTimeFlags): (List[List[(ConcolicInput, Int)]], List[PathConstraint]) = {
    val (machine, sem) = makeConcolicMachineAndSemantics(flags)
    val output: machine.ConcolicMachineOutput = runOnFile(program, program => machine.concolicEval(program, sem.parse(program), sem, false, Timeout.none))
    assert(output.isInstanceOf[machine.ConcolicMachineOutputNormal])
    val castedOutput = output.asInstanceOf[machine.ConcolicMachineOutputNormal]
    (castedOutput.allInputs, castedOutput.allPathConstraints)
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    Reporter.deleteSymbolicTree()
  }

  test("Make sure Connect-4 test explores the same inputs, in the same order, and generates the same path constraints," +
       " in the same order. Assumes both initial and run-time analyses to have been disabled.") {
    val (allInputs1, allPathConstraints1) = runProgram(connect4Program, ConcolicRunTimeFlags(100, false, false))
    Reporter.deleteSymbolicTree()
    val (allInputs2, allPathConstraints2) = runProgram(connect4Program, ConcolicRunTimeFlags(100, false, false))
    assert(allInputs1 == allInputs2)
    assert(allPathConstraints1 == allPathConstraints2)
  }

  test("Test whether path constraints reported by the concolic tester for Connect-4 are identical to a previous run " +
       "that was saved to a file and will serve as a test oracle.") {
    val (_, pathConstraints1) = runProgram(connect4Program, ConcolicRunTimeFlags(100, true, true))
    val string1 = pathConstraintsToString(pathConstraints1)
    val previousPathConstraintsString = readFile(connect4Folder + "path_constraints")
    assert(string1 == previousPathConstraintsString)
  }
}
