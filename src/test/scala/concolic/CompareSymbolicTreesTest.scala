import backend.expression.ConcolicBool
import org.scalatest.FunSuite
import backend.tree._

class CompareSymbolicTreesTest extends FunSuite {

  val dummyConstraint = BranchConstraint(ConcolicBool(true))

  private def constructTree[NodeExtraInfo](height: Int, extraInfo: NodeExtraInfo): SymbolicNode[NodeExtraInfo] = {
    if (height == 0) {
      RegularLeafNode()
    } else {
      BranchSymbolicNode(dummyConstraint, constructTree(height - 1, extraInfo), constructTree(height - 1, extraInfo), extraInfo)
    }
  }
  private def constructTree(height: Int): SymbolicNode[Unit] = constructTree[Unit](height, ())

  private def constructOneLegalPath[NodeExtraInfo](height: Int, extraInfo: NodeExtraInfo): SymbolicNode[NodeExtraInfo] = {
    if (height == 0) {
      RegularLeafNode()
    } else {
      BranchSymbolicNode(dummyConstraint, constructOneLegalPath(height - 1, extraInfo), SafeNode(UnexploredNode()), extraInfo)
    }
  }
  private def constructOneLegalPath(height: Int): SymbolicNode[Unit] = constructOneLegalPath[Unit](height, ())

  test("Empty tree vs. tree of height N, with N <= 10") {
    val empty = RegularLeafNode[Unit]()
    1.to(10).foreach(N => {
      val otherRoot = constructTree[Unit](N, ())
      assert(CompareSymbolicTrees.countUniqueSafePaths(empty, otherRoot) == 0)
      assert(CompareSymbolicTrees.countUniqueNonSafePaths(empty, otherRoot) == Math.pow(2, N))
      /* The first tree does not contain any paths/nodes that do not also appear in the second tree. */
      assert(CompareSymbolicTrees.countUniqueSafePaths(otherRoot, empty) == 0)
      assert(CompareSymbolicTrees.countUniqueNonSafePaths(otherRoot, empty) == 0)
    })
  }

  test("Illegalized root vs. tree of height 2") {
    val illegalizedRoot = SafeNode[Unit](UnexploredNode())
    val rootEB = BranchSymbolicNode[Unit](dummyConstraint, RegularLeafNode(), RegularLeafNode(), ())
    val rootTB = BranchSymbolicNode[Unit](dummyConstraint, RegularLeafNode(), RegularLeafNode(), ())
    val otherRoot = BranchSymbolicNode[Unit](dummyConstraint, rootTB, rootEB, ())
    assert(CompareSymbolicTrees.countUniqueSafePaths(illegalizedRoot, otherRoot) == 4)
    assert(CompareSymbolicTrees.countUniqueNonSafePaths(illegalizedRoot, otherRoot) == 0)
    /* The first tree does not contain any paths/nodes that do not also appear in the second tree. */
    assert(CompareSymbolicTrees.countUniqueSafePaths(otherRoot, illegalizedRoot) == 0)
    assert(CompareSymbolicTrees.countUniqueNonSafePaths(otherRoot, illegalizedRoot) == 0)
  }

  test("Illegalized root vs. trees of height N, with N <= 10") {
    val illegalizedNode = SafeNode[Unit](UnexploredNode())
    1.to(10).foreach(N => {
      val otherRoot = constructTree(N)
      assert(CompareSymbolicTrees.countUniqueSafePaths(illegalizedNode, otherRoot) == Math.pow(2, N))
      assert(CompareSymbolicTrees.countUniqueNonSafePaths(illegalizedNode, otherRoot) == 0)
      /* The first tree does not contain any paths/nodes that do not also appear in the second tree. */
      assert(CompareSymbolicTrees.countUniqueSafePaths(otherRoot, illegalizedNode) == 0)
      assert(CompareSymbolicTrees.countUniqueNonSafePaths(otherRoot, illegalizedNode) == 0)
    })
  }

  test("Illegalize an intermediate node in the tree") {
    val root1 = constructTree(5).asInstanceOf[BranchSymbolicNode[Unit]]
    /* Illegalize a grandchild-node of root1 */
    val updatedRoot1 = root1.copy(thenBranch = root1.thenBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(thenBranch = SafeNode[Unit](UnexploredNode())))
    val root2 = constructTree(5)
    assert(CompareSymbolicTrees.countUniqueSafePaths(updatedRoot1, root2) == 8)
    assert(CompareSymbolicTrees.countUniqueNonSafePaths(updatedRoot1, root2) == 0)
    /* The first tree does not contain any paths/nodes that do not also appear in the second tree. */
    assert(CompareSymbolicTrees.countUniqueSafePaths(root2, updatedRoot1) == 0)
    assert(CompareSymbolicTrees.countUniqueNonSafePaths(root2, updatedRoot1) == 0)
  }

  test("Compare an incomplete tree vs. a complete tree of height N, with 2 <= N <= 10") {
    2.to(10).foreach(N => {
      val root1 = constructTree(N).asInstanceOf[BranchSymbolicNode[Unit]]
      /* Remove a grandchild-node of root1 */
      val updatedRoot1 = root1.copy(thenBranch = root1.thenBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(thenBranch = UnexploredNode[Unit]()))
      val root2 = constructTree(N)
      /* The grandchild-node the root of the first tree was removed: the subtree belonging to this node has height N - 2 */
      assert(CompareSymbolicTrees.countUniqueSafePaths(updatedRoot1, root2) == 0)
      assert(CompareSymbolicTrees.countUniqueNonSafePaths(updatedRoot1, root2) == Math.pow(2, N - 2))
      /* The first tree does not contain any paths/nodes that do not also appear in the first tree. */
      assert(CompareSymbolicTrees.countUniqueSafePaths(root2, updatedRoot1) == 0)
      assert(CompareSymbolicTrees.countUniqueNonSafePaths(root2, updatedRoot1) == 0)
    })
  }

  test("Forall N with 3 <= N <= 10, compare an incomplete tree (one node at height N - 2 removed) vs. another incomplete tree (one node at height N - 3 removed)") {
    3.to(10).foreach(N => {
      val root1 = constructTree(N).asInstanceOf[BranchSymbolicNode[Unit]]
      /* Remove a grandchild-node of root1 */
      val updatedRoot1 = root1.copy(thenBranch = root1.thenBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(thenBranch = UnexploredNode[Unit]()))
      val root2 = constructTree(N).asInstanceOf[BranchSymbolicNode[Unit]]
      val updatedRoot2 = root2.copy(thenBranch = root2.thenBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(elseBranch = root2.thenBranch.asInstanceOf[BranchSymbolicNode[Unit]].elseBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(elseBranch = UnexploredNode[Unit]())))
      /* The grandchild-node of the root of the first tree was removed: the subtree belonging to this node has height N - 2 */
      assert(CompareSymbolicTrees.countUniqueSafePaths(updatedRoot1, updatedRoot2) == 0)
      assert(CompareSymbolicTrees.countUniqueNonSafePaths(updatedRoot1, updatedRoot2) == Math.pow(2, N - 2))
      /* The grand-grandchild node of the root of the second tree was removed: the subtree belonging to this node has height N - 3 */
      assert(CompareSymbolicTrees.countUniqueSafePaths(updatedRoot2, updatedRoot1) == 0)
      val value = if (N == 3) 0 else Math.pow(2, N - 3)
      assert(CompareSymbolicTrees.countUniqueNonSafePaths(updatedRoot2, updatedRoot1) == Math.pow(2, N - 3))
    })
  }

  test("Forall N with 3 <= N <= 10, compare an incomplete tree (two nodes at height N - 2 removed) vs. another incomplete tree (two nodes at height N - 3 removed)") {
    3.to(10).foreach(N => {
      val root1 = constructTree(N).asInstanceOf[BranchSymbolicNode[Unit]]
      /* Remove a grandchild-node of root1 */
      val tempRoot1 = root1.copy(thenBranch = root1.thenBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(thenBranch = UnexploredNode[Unit]()))
      val updatedRoot1 = tempRoot1.copy(elseBranch = tempRoot1.elseBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(thenBranch = UnexploredNode[Unit]()))
      val root2 = constructTree(N).asInstanceOf[BranchSymbolicNode[Unit]]
      val tempRoot2 = root2.copy(thenBranch = root2.thenBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(elseBranch = root2.thenBranch.asInstanceOf[BranchSymbolicNode[Unit]].elseBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(elseBranch = UnexploredNode[Unit]())))
      val updatedRoot2 = tempRoot2.copy(thenBranch = tempRoot2.thenBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(elseBranch = tempRoot2.thenBranch.asInstanceOf[BranchSymbolicNode[Unit]].elseBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(thenBranch = UnexploredNode[Unit]())))
      /* Two grandchild-nodes of the root of the first tree were removed: the subtrees belonging to these nodes each have height N - 2 */
      assert(CompareSymbolicTrees.countUniqueSafePaths(updatedRoot1, updatedRoot2) == 0)
      assert(CompareSymbolicTrees.countUniqueNonSafePaths(updatedRoot1, updatedRoot2) == 2 * Math.pow(2, N - 2))
      /* Two grand-grandchild nodes of the root of the second tree were removed: the subtrees belonging to these nodes each have height N - 3 */
      assert(CompareSymbolicTrees.countUniqueSafePaths(updatedRoot2, updatedRoot1) == 0)
      assert(CompareSymbolicTrees.countUniqueNonSafePaths(updatedRoot2, updatedRoot1) == 2 * Math.pow(2, N - 3))
    })
  }

  test("Forall N with 3 <= N <= 10, compare an incomplete tree (two nodes at height N - 2 removed) vs. another incomplete tree (one nodes at height N - 3, and one node at height N - 2 removed)") {
    3.to(10).foreach(N => {
      val root1 = constructTree(N).asInstanceOf[BranchSymbolicNode[Unit]]
      /* Remove a grandchild-node of root1 */
      val tempRoot1 = root1.copy(thenBranch = root1.thenBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(thenBranch = UnexploredNode[Unit]()))
      val updatedRoot1 = tempRoot1.copy(elseBranch = tempRoot1.elseBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(thenBranch = UnexploredNode[Unit]()))
      val root2 = constructTree(N).asInstanceOf[BranchSymbolicNode[Unit]]
      val tempRoot2 = root2.copy(thenBranch = root2.thenBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(elseBranch = root2.thenBranch.asInstanceOf[BranchSymbolicNode[Unit]].elseBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(elseBranch = UnexploredNode[Unit]())))
      val updatedRoot2 = tempRoot2.copy(elseBranch = tempRoot2.elseBranch.asInstanceOf[BranchSymbolicNode[Unit]].copy(elseBranch = UnexploredNode[Unit]()))
      /* Two grandchild-nodes of the root of the first tree were removed: the subtrees belonging to these nodes each have height N - 2 */
      assert(CompareSymbolicTrees.countUniqueSafePaths(updatedRoot1, updatedRoot2) == 0)
      assert(CompareSymbolicTrees.countUniqueNonSafePaths(updatedRoot1, updatedRoot2) == 2 * Math.pow(2, N - 2))
      /* Two grand-grandchild nodes of the root of the second tree were removed: the subtrees belonging to these nodes each have height N - 3 */
      assert(CompareSymbolicTrees.countUniqueSafePaths(updatedRoot2, updatedRoot1) == 0)
      assert(CompareSymbolicTrees.countUniqueNonSafePaths(updatedRoot2, updatedRoot1) == Math.pow(2, N - 3) + Math.pow(2, N - 2))
    })
  }

  test("Only one legal (= only one visited) path vs. full tree") {
    1.to(10).foreach(N => {
      val root1 = constructOneLegalPath(N)
      val root2 = constructTree(N)
      /* Only one path is legal: total illegal paths = total paths - 1 */
      assert(CompareSymbolicTrees.countUniqueSafePaths(root1, root2) == Math.pow(2, N) - 1)
      assert(CompareSymbolicTrees.countUniqueNonSafePaths(root1, root2) == 0)

      assert(CompareSymbolicTrees.countUniqueSafePaths(root2, root1) == 0)
      assert(CompareSymbolicTrees.countUniqueNonSafePaths(root2, root1) == 0)
    })
  }

}
