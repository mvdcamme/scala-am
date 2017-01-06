class HyperlinkedGraph[Node, Annotation](implicit nodeDescriptor: Descriptor[Node]) extends Graph[Node, Annotation] {

  override def toDot(label: Node => List[scala.xml.Node],
                     colorNode: Node => String,
                     annotLabel: Annotation => List[scala.xml.Node],
                     colorEdge: Option[((Node, Annotation, Node)) => String]): String = {
    val sb = new StringBuilder("digraph G {\nsize=\"8,10.5\"\n")
    nodes.foreach((n) => {
      val labelstr = label(n).mkString(" ")
      val target = "_blank"
      sb.append(
        s"node_${ids(n)}[label=<${ids(n)}: $labelstr>, fillcolor=<${colorNode(n)}> style=<filled>" +
          s"${"URL=" + "\"" + "file:///Users/mvdcamme/PhD/Projects/scala-am/index.html#" +
            s"${ids(n)}" + "\""} ];\n")
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
    sb.toString
  }

  private def writeNodesDump(outputPath: String): Unit = {
    val bw = openBufferedWriter(outputPath)
    val descriptor = implicitly[Descriptor[Node]]
    bw.write("<!DOCTYPE html>\n<html>\n<body>")
    ids.foreach({ case (node, id) => bw.write(s"<a name=$id></a>\n${descriptor.describe}\n") })
    bw.write("</body>\n</html>")
    bw.close()
  }

  override def toDotFile(path: String,
                         label: Node => List[scala.xml.Node],
                         color: Node => String,
                         annotLabel: Annotation => List[scala.xml.Node],
                         colorEdge: Option[((Node, Annotation, Node)) => String]): Unit = {
    val outputPath = "/Users/mvdcamme/PhD/Projects/scala-am/index.html"
    writeNodesDump(outputPath)
    val bw = openBufferedWriter(path)
    bw.write(toDot(label, color, annotLabel, colorEdge))
    bw.close()
  }

}
