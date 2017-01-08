class HyperlinkedGraph[Node, Annotation](ids: Map[Node, Int],
                                         next: Int,
                                         nodes: Set[Node],
                                         edges: Map[Node, Set[(Annotation, Node)]])
                                        (implicit nodeDescriptor: Descriptor[Node]) extends Graph[Node, Annotation](ids, next, nodes, edges) {

  def this()(implicit nodeDescriptor: Descriptor[Node]) = {
    this(Map[Node, Int](),
      0,
      Set[Node](),
      Map[Node, Set[(Annotation, Node)]]())
  }


  def this(node: Node)(implicit nodeDescriptor: Descriptor[Node]) =
    this(Map[Node, Int]() + (node -> 0),
      1,
      Set[Node](node),
      Map[Node, Set[(Annotation, Node)]]())

  override def factory(ids: Map[Node, Int],
                       next: Int,
                       nodes: Set[Node],
                       edges: Map[Node, Set[(Annotation, Node)]]): Graph[Node, Annotation] =
    new HyperlinkedGraph[Node, Annotation](ids, next, nodes, edges)

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

  private def writeNodesDump(path: String): Unit = {
    /*
     * Htmlfile wile be saved in the same path as the .dot file, except with the .html extension.
     */
    val htmlOutputPath = path.split(".dot").head + ".html"
    val bw = openBufferedWriter(htmlOutputPath)
    val descriptor = implicitly[Descriptor[Node]]
    bw.write("<!DOCTYPE html>\n<html>\n<head><link rel=\"stylesheet\" type=\"text/css\" href=\"styles.css\"></head><body>\n" +
    "\t<script src=\"http://code.jquery.com/jquery-1.10.1.min.js\"></script>\n" +
    "\t<script type=\"text/javascript\">" +
        "$(function(){" +
          "$('ul.collapsable').find('SPAN').click(function(e){" +
                                                 "$(this).parent().children('ul').toggle();" +
                                                 "});" +
          "});" +
    "</script>\n")
    ids.foreach({ case (node, id) => bw.write(s"<a name=$id></a>\n${descriptor.describe(node)}\n") })
    bw.write("</body>\n</html>")
    bw.close()
  }

  override def toDotFile(path: String,
                         label: Node => List[scala.xml.Node],
                         color: Node => String,
                         annotLabel: Annotation => List[scala.xml.Node],
                         colorEdge: Option[((Node, Annotation, Node)) => String]): Unit = {
    writeNodesDump(path)
    val bw = openBufferedWriter(path)
    bw.write(toDot(label, color, annotLabel, colorEdge))
    bw.close()
  }

}
