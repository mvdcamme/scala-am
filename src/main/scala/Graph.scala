case class Graph[Node, Annotation](ids: Map[Node, Int], next: Int, nodes: Set[Node], edges: Map[Node, Set[(Annotation, Node)]]) {
  def this() = this(Map[Node, Int](), 0, Set[Node](), Map[Node, Set[(Annotation, Node)]]())
  def this(node: Node) = this(Map[Node, Int]() + (node -> 0), 1, Set[Node](node), Map[Node, Set[(Annotation, Node)]]())
  def addNode(node: Node): Graph[Node, Annotation] =
    if (nodes.contains(node)) { this } else {
      Graph(ids + (node -> next), next + 1, nodes + node, edges)
    }
  def addEdge(node1: Node, annot: Annotation, node2: Node): Graph[Node, Annotation] =
    addNode(node1).addNode(node2).addEdgeNoCheck(node1, annot, node2)
  def addEdges(l: Traversable[(Node, Annotation, Node)]): Graph[Node, Annotation] =
    l.foldLeft(this)({ case (g, (n1, annot, n2)) => g.addEdge(n1, annot, n2) })
  def addEdgeNoCheck(node1: Node, annot: Annotation, node2: Node): Graph[Node, Annotation] =
    if (edges.contains(node1) && edges(node1).contains((annot, node2))) { this } else {
      val existing: Set[(Annotation, Node)] = edges.getOrElse(node1, Set[(Annotation, Node)]())
      Graph(ids, next, nodes, edges + (node1 -> (existing ++ Set((annot, node2)))))
    }
  def size: Integer = nodes.size
  def foldNodes[B](init: B)(f: (B, Node) => B) = nodes.foldLeft(init)(f)
  def getNode(id: Int): Option[Node] = ids.find({ case (_, v) => id == v }).map(_._1)
  def toDot(label: Node => String, color: Node => String, annotLabel: Annotation => String): String = {
      val sb = new StringBuilder("digraph G {\nsize=\"8,10.5\"\n")
      nodes.foreach((n) =>
        sb.append("node_" + ids(n) + "[label=\"" /* + ids(n).toString + " " */ + label(n).replaceAll("\"", "\\\\\"") + "\", fillcolor=\"" + color(n) + "\" style=\"filled\"];\n")
      )
      edges.foreach({ case (n1, ns) => ns.foreach({ case (annot, n2) => sb.append("node_" + ids(n1) + " -> node_" + ids(n2) + " [label=\"" + annotLabel(annot).replaceAll("\"", "\\\\\"") + "\"]")})})
      sb.append("}")
      return sb.toString
    }
  def toDotFile(path: String, label: Node => String, color: Node => String, annotLabel: Annotation => String): Unit = {
    val f = new java.io.File(path)
    val bw = new java.io.BufferedWriter(new java.io.FileWriter(f))
    bw.write(toDot(label, color, annotLabel))
    bw.close()
  }
}
