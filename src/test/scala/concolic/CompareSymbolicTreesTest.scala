import backend.expression.ConcolicBool
import org.scalatest.FunSuite
import backend.tree._

class CompareSymbolicTreesTest extends FunSuite {

  val dummyConstraint = BranchConstraint(ConcolicBool(true))

  private def constructTree(height: Int): SymbolicNode = {
    if (height == 0) {
      EmptyNode
    } else {
      BranchSymbolicNode(dummyConstraint, true, true, constructTree(height - 1), constructTree(height - 1))
    }
  }

  test("Empty tree vs. tree of height N, with N <= 10") {
    val empty = EmptyNode
    1.to(10).foreach(N => {
      val otherRoot = constructTree(N)
      assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(empty, otherRoot) == 0)
      assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(empty, otherRoot) == Math.pow(2, N))
      /* The first tree does not contain any paths/nodes that do not also appear in the first tree. */
      assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(otherRoot, empty) == 0)
      assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(otherRoot, empty) == 0)
    })
  }

  test("Illegalized root vs. tree of height 2") {
    val illegalizedRoot = IllegalizedNode(EmptyNode)
    val rootEB = BranchSymbolicNode(dummyConstraint, true, true, EmptyNode, EmptyNode)
    val rootTB = BranchSymbolicNode(dummyConstraint, true, true, EmptyNode, EmptyNode)
    val otherRoot = BranchSymbolicNode(dummyConstraint, true, true, rootTB, rootEB)
    assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(illegalizedRoot, otherRoot) == 4)
    assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(illegalizedRoot, otherRoot) == 0)
    /* The first tree does not contain any paths/nodes that do not also appear in the first tree. */
    assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(otherRoot, illegalizedRoot) == 0)
    assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(otherRoot, illegalizedRoot) == 0)
  }

  test("Illegalized root vs. trees of height N, with N <= 10") {
    val illegalizedNode = IllegalizedNode(EmptyNode)
    1.to(10).foreach(N => {
      val otherRoot = constructTree(N)
      assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(illegalizedNode, otherRoot) == Math.pow(2, N))
      assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(illegalizedNode, otherRoot) == 0)
      /* The first tree does not contain any paths/nodes that do not also appear in the first tree. */
      assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(otherRoot, illegalizedNode) == 0)
      assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(otherRoot, illegalizedNode) == 0)
    })
  }

  test("Illegalize an intermediate node in the tree") {
    val root1 = constructTree(5).asInstanceOf[BranchSymbolicNode]
    /* Illegalize a grandchild-node of root1 */
    val updatedRoot1 = root1.copy(thenBranch = root1.thenBranch.asInstanceOf[BranchSymbolicNode].copy(thenBranch = IllegalizedNode(EmptyNode)))
    val root2 = constructTree(5)
    assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(updatedRoot1, root2) == 8)
    assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(updatedRoot1, root2) == 0)
    /* The first tree does not contain any paths/nodes that do not also appear in the first tree. */
    assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(root2, updatedRoot1) == 0)
    assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(root2, updatedRoot1) == 0)
  }

  test("Compare an incomplete tree vs. a complete tree of height N, with 2 <= N <= 10") {
    2.to(10).foreach(N => {
      val root1 = constructTree(N).asInstanceOf[BranchSymbolicNode]
      /* Remove a grandchild-node of root1 */
      val updatedRoot1 = root1.copy(thenBranch = root1.thenBranch.asInstanceOf[BranchSymbolicNode].copy(thenBranch = EmptyNode))
      val root2 = constructTree(N)
      /* The grandchild-node the root of the first tree was removed: the subtree belonging to this node has height N - 2 */
      assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(updatedRoot1, root2) == 0)
      /* If N == 2, the grandchild-node of root1 was already an EmptyNode. Hence, no actual paths were removed */
      val value = if (N == 2) 0 else Math.pow(2, N - 2)
      assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(updatedRoot1, root2) == value)
      /* The first tree does not contain any paths/nodes that do not also appear in the first tree. */
      assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(root2, updatedRoot1) == 0)
      assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(root2, updatedRoot1) == 0)
    })
  }

  test("Forall N with 3 <= N <= 10, compare an incomplete tree (one node at height N - 2 removed) vs. another incomplete tree (one node at height N - 3 removed)") {
    3.to(10).foreach(N => {
      val root1 = constructTree(N).asInstanceOf[BranchSymbolicNode]
      /* Remove a grandchild-node of root1 */
      val updatedRoot1 = root1.copy(thenBranch = root1.thenBranch.asInstanceOf[BranchSymbolicNode].copy(thenBranch = EmptyNode))
      val root2 = constructTree(N).asInstanceOf[BranchSymbolicNode]
      val updatedRoot2 = root2.copy(thenBranch = root2.thenBranch.asInstanceOf[BranchSymbolicNode].copy(elseBranch = root2.thenBranch.asInstanceOf[BranchSymbolicNode].elseBranch.asInstanceOf[BranchSymbolicNode].copy(elseBranch = EmptyNode)))
      /* The grandchild-node of the root of the first tree was removed: the subtree belonging to this node has height N - 2 */
      assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(updatedRoot1, updatedRoot2) == 0)
      assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(updatedRoot1, updatedRoot2) == Math.pow(2, N - 2))
      /* The grand-grandchild node of the root of the second tree was removed: the subtree belonging to this node has height N - 3 */
      assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(updatedRoot2, updatedRoot1) == 0)
      /* If N == 3, the grand-grandchild-node of root2 was already an EmptyNode. Hence, no actual paths were removed */
      val value = if (N == 3) 0 else Math.pow(2, N - 3)
      assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(updatedRoot2, updatedRoot1) == value)
    })
  }

  test("Forall N with 3 <= N <= 10, compare an incomplete tree (two nodes at height N - 2 removed) vs. another incomplete tree (two nodes at height N - 3 removed)") {
    3.to(10).foreach(N => {
      val root1 = constructTree(N).asInstanceOf[BranchSymbolicNode]
      /* Remove a grandchild-node of root1 */
      val tempRoot1 = root1.copy(thenBranch = root1.thenBranch.asInstanceOf[BranchSymbolicNode].copy(thenBranch = EmptyNode))
      val updatedRoot1 = tempRoot1.copy(elseBranch = tempRoot1.elseBranch.asInstanceOf[BranchSymbolicNode].copy(thenBranch = EmptyNode))
      val root2 = constructTree(N).asInstanceOf[BranchSymbolicNode]
      val tempRoot2 = root2.copy(thenBranch = root2.thenBranch.asInstanceOf[BranchSymbolicNode].copy(elseBranch = root2.thenBranch.asInstanceOf[BranchSymbolicNode].elseBranch.asInstanceOf[BranchSymbolicNode].copy(elseBranch = EmptyNode)))
      val updatedRoot2 = tempRoot2.copy(thenBranch = tempRoot2.thenBranch.asInstanceOf[BranchSymbolicNode].copy(elseBranch = tempRoot2.thenBranch.asInstanceOf[BranchSymbolicNode].elseBranch.asInstanceOf[BranchSymbolicNode].copy(thenBranch = EmptyNode)))
      /* Two grandchild-nodes of the root of the first tree were removed: the subtrees belonging to these nodes each have height N - 2 */
      assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(updatedRoot1, updatedRoot2) == 0)
      assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(updatedRoot1, updatedRoot2) == 2 * Math.pow(2, N - 2))
      /* Two grand-grandchild nodes of the root of the second tree were removed: the subtrees belonging to these nodes each have height N - 3 */
      assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(updatedRoot2, updatedRoot1) == 0)
      /* If N == 3, the grand-grandchild-node of root2 was already an EmptyNode. Hence, no actual paths were removed */
      val value = if (N == 3) 0 else 2 * Math.pow(2, N - 3)
      assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(updatedRoot2, updatedRoot1) == value)
    })
  }

  test("Forall N with 3 <= N <= 10, compare an incomplete tree (two nodes at height N - 2 removed) vs. another incomplete tree (one nodes at height N - 3, and one node at height N - 2 removed)") {
    3.to(10).foreach(N => {
      val root1 = constructTree(N).asInstanceOf[BranchSymbolicNode]
      /* Remove a grandchild-node of root1 */
      val tempRoot1 = root1.copy(thenBranch = root1.thenBranch.asInstanceOf[BranchSymbolicNode].copy(thenBranch = EmptyNode))
      val updatedRoot1 = tempRoot1.copy(elseBranch = tempRoot1.elseBranch.asInstanceOf[BranchSymbolicNode].copy(thenBranch = EmptyNode))
      val root2 = constructTree(N).asInstanceOf[BranchSymbolicNode]
      val tempRoot2 = root2.copy(thenBranch = root2.thenBranch.asInstanceOf[BranchSymbolicNode].copy(elseBranch = root2.thenBranch.asInstanceOf[BranchSymbolicNode].elseBranch.asInstanceOf[BranchSymbolicNode].copy(elseBranch = EmptyNode)))
      val updatedRoot2 = tempRoot2.copy(elseBranch = tempRoot2.elseBranch.asInstanceOf[BranchSymbolicNode].copy(elseBranch = EmptyNode))
      /* Two grandchild-nodes of the root of the first tree were removed: the subtrees belonging to these nodes each have height N - 2 */
      assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(updatedRoot1, updatedRoot2) == 0)
      assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(updatedRoot1, updatedRoot2) == 2 * Math.pow(2, N - 2))
      /* Two grand-grandchild nodes of the root of the second tree were removed: the subtrees belonging to these nodes each have height N - 3 */
      assert(CompareSymbolicTrees.countUniqueIllegalizedPaths(updatedRoot2, updatedRoot1) == 0)
      /* If N == 3, the grand-grandchild-node of root2 was already an EmptyNode. Hence, no actual paths were removed */
      val value = if (N == 3) Math.pow(2, N - 2) else Math.pow(2, N - 3) + Math.pow(2, N - 2)
      assert(CompareSymbolicTrees.countUniqueNonIllegalizedPaths(updatedRoot2, updatedRoot1) == value)
    })
  }

}
