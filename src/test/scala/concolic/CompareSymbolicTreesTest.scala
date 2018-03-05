import backend.expression.ConcolicBool
import org.scalatest.FunSuite
import backend.tree._

class CompareSymbolicTreesTest extends FunSuite {

  val dummyConstraint = BranchConstraint(ConcolicBool(true))

  test("Illegalized root vs tree of height 2") {
    val illegalizedRoot = IllegalizedNode(EmptyNode)
    val rootEB = BranchSymbolicNode(dummyConstraint, true, true, EmptyNode, EmptyNode)
    val rootTB = BranchSymbolicNode(dummyConstraint, true, true, EmptyNode, EmptyNode)
    val otherRoot = BranchSymbolicNode(dummyConstraint, true, true, rootTB, rootEB)
    assert(CompareSymbolicTrees.compareTrees(illegalizedRoot, otherRoot) == 4)
  }

  private def constructTree(height: Int): SymbolicNode = {
    if (height == 0) {
      EmptyNode
    } else {
      BranchSymbolicNode(dummyConstraint, true, true, constructTree(height - 1), constructTree(height - 1))
    }
  }

  test("Illegalized root vs trees of height N, with N <= 10") {
    val illegalizedNode = IllegalizedNode(EmptyNode)
    1.to(10).foreach(n => {
      val otherRoot = constructTree(n)
      assert(CompareSymbolicTrees.compareTrees(illegalizedNode, otherRoot) == Math.pow(2, n))
    })
  }

  test("Illegalize an intermediate node in the tree") {
    val root1 = constructTree(5).asInstanceOf[BranchSymbolicNode]
    /* Illegalize a grandchild-node of root1 */
    val updatedRoot1 = root1.copy(thenBranch = root1.thenBranch.asInstanceOf[BranchSymbolicNode].copy(thenBranch = IllegalizedNode(EmptyNode)))
    val root2 = constructTree(5)
    assert(CompareSymbolicTrees.compareTrees(updatedRoot1, root2) == 8)
  }

}
