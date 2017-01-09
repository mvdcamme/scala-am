trait Descriptor[+Component] {
  val collapsableClass = "collapsable"

  def describe[U >: Component](component: U): String

  /**
    * Generates a description, which can be collapsed, for each element that can be iterated over.
    * @param elements The elements to iterate over.
    * @param name The name (i.e., title) of this list.
    * @param describe The function with which to describe each element.
    * @param divClass The name of the div-class used to wrap the list. If None, no div-element is generated. Defaults
    *                 to None.
    * @tparam A The type of the elements to describe.
    * @return The description.
    */
  def describeCollapsableList[A](elements: Iterable[A],
                                 name: String,
                                 describe: A => String = (x: A) => convertHTML(x.toString),
                                 divClass: Option[String] = None):
  String =
    putIntoCollapsableList(elements.map(describe), name, divClass)

  def putIntoCollapsableList(iterable: Iterable[String], name: String, divClass: Option[String] = None): String =
    divClass.fold("")((name) => "<div class=\"" + convertHTML(name) + "\">") + s"<UL class=" +
    "\"" + collapsableClass + "\">\n" + s"\t<LI><SPAN>${convertHTML(name)}</SPAN>\n\t\t<UL>\n" +
    iterable.map( (el) => s"\t\t\t<LI><SPAN>$el</SPAN></LI>" ).mkString("\n") +
    "\t\t</UL>\n\t</LI></UL>" + divClass.fold("")((_) => "</div>")

  /**
    * Converts certain character sequences in the given HTML string that will not be accurately displayed on the webpage
    * (such as the '<' or '>' characters) by their proper HTML entities.
    * @param html The string in which to replace the characters.
    * @return A new string representing the HTML in which all necessary characters have been replaced by their proper
    *         entities.
    */
  def convertHTML(html: String): String = {
    val stringsToReplace = List(("<", "&#60;"), (">", "&#62;"))
    stringsToReplace.foldLeft(html)( (converted, toReplace) => converted.replace(toReplace._1, toReplace._2))
  }
}

class BasicDescriptor[+Component] extends Descriptor[Component] {
  def describe[U >: Component](component: U): String = convertHTML(component.toString)
}