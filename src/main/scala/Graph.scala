object Colors {
  val Yellow = "#FFFFDD"
  val Green = "#DDFFDD"
  val Pink = "#FFDDDD"
  val Red = "#FF0000"
  val Black = "#000000"
  val White = "#FFFFFF"
}

case class Graph[Node, Annotation](ids: Map[Node, Int],
                                   next: Int,
                                   nodes: Set[Node],
                                   edges: Map[Node, Set[(Annotation, Node)]]) {
  def this() =
    this(Map[Node, Int](),
         0,
         Set[Node](),
         Map[Node, Set[(Annotation, Node)]]())
  def this(node: Node) =
    this(Map[Node, Int]() + (node -> 0),
         1,
         Set[Node](node),
         Map[Node, Set[(Annotation, Node)]]())
  def addNode(node: Node): Graph[Node, Annotation] =
    if (nodes.contains(node)) { this } else {
      Graph(ids + (node -> next), next + 1, nodes + node, edges)
    }
  def addEdge(node1: Node,
              annot: Annotation,
              node2: Node): Graph[Node, Annotation] =
    addNode(node1).addNode(node2).addEdgeNoCheck(node1, annot, node2)
  def addEdges(
      l: Traversable[(Node, Annotation, Node)]): Graph[Node, Annotation] =
    l.foldLeft(this)({ case (g, (n1, annot, n2)) => g.addEdge(n1, annot, n2) })
  def addEdgeNoCheck(node1: Node,
                     annot: Annotation,
                     node2: Node): Graph[Node, Annotation] =
    if (edges.contains(node1) && edges(node1).contains((annot, node2))) {
      this
    } else {
      val existing: Set[(Annotation, Node)] =
        edges.getOrElse(node1, Set[(Annotation, Node)]())
      Graph(ids,
            next,
            nodes,
            edges + (node1 -> (existing ++ Set((annot, node2)))))
    }
  def size: Int = nodes.size
  def transitions: Int = edges.size
  def foldNodes[B](init: B)(f: (B, Node) => B) = nodes.foldLeft(init)(f)
  def getNode(id: Int): Option[Node] =
    ids.find({ case (_, v) => id == v }).map(_._1)
  def nodeId(node: Node): Int =
    ids.getOrElse(node, -1)
  def nodeExists(node: Node): Boolean = nodeId(node) != -1

  def toDot(label: Node => List[scala.xml.Node],
            colorNode: Node => String,
            addHyperlink: Boolean,
            annotLabel: Annotation => List[scala.xml.Node],
            colorEdge: Option[((Node, Annotation, Node)) => String]): String = {
    val sb = new StringBuilder("digraph G {\nsize=\"8,10.5\"\n")
    nodes.foreach((n) => {
      val labelstr = label(n).mkString(" ")
      val target = "_blank"
      sb.append(
        s"node_${ids(n)}[label=<${ids(n)}: $labelstr>, fillcolor=<${colorNode(n)}> style=<filled>" +
          s"${if (addHyperlink) {"URL=" + "\"" + "file:///Users/mvdcamme/PhD/Projects/scala-am/index.html#" +
            s"${ids(n)}" + "\""} else ""} ];\n")
    })
    edges.foreach({
      case (n1, ns) =>
        ns.foreach({
          case (annots, n2) =>
            val annotstr = annotLabel(annots).mkString(" ")
            sb.append(
              s"node_${ids(n1)} -> node_${ids(n2)} [label=<$annotstr>" +
              s"${colorEdge.fold("")( (f) => " color=<" + f((n1, annots, n2)) + ">")}]\n")
        })
    })
    sb.append("}")
    return sb.toString
  }

  private def openBufferedWriter(path: String, append: Boolean = false): java.io.BufferedWriter = {
    val f = new java.io.File(path)
    new java.io.BufferedWriter(new java.io.FileWriter(f, append))
  }

  private def writeNodesDump(describeNode: Node => String, outputPath: String): Unit = {
    val bw = openBufferedWriter(outputPath)
    bw.write("<!DOCTYPE html>\n<html>\n<body>")
    ids.foreach({ case (node, id) => bw.write(s"<a name=$id></a>\n${describeNode(node)}\n") })
    bw.write("</body>\n</html>")
    bw.close()
  }

  private def handleNodesDump(describeNode: Option[Node => String]): Unit = {
    val outputPath = "/Users/mvdcamme/PhD/Projects/scala-am/index.html"
    if (describeNode.isDefined) writeNodesDump(describeNode.get, outputPath)
  }

  def toDotFile(path: String,
                label: Node => List[scala.xml.Node],
                color: Node => String,
                describeNode: Option[Node => String],
                annotLabel: Annotation => List[scala.xml.Node],
                colorEdge: Option[((Node, Annotation, Node)) => String]): Unit = {
    handleNodesDump(describeNode)
    val bw = openBufferedWriter(path)
    bw.write(toDot(label, color, describeNode.isDefined, annotLabel, colorEdge))
    bw.close()
  }


  def nodeEdges(node: Node): Set[(Annotation, Node)] =
    if (edges.contains(node)) {
      edges(node)
    } else {
      Set()
    }
}
