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
                                 describe: A => String = (x: A) => x.toString,
                                 divClass: Option[String] = None):
  String =
    putIntoCollapsableList(elements.map(describe), name, divClass)

  def putIntoCollapsableList(iterable: Iterable[String], name: String, divClass: Option[String] = None): String =
    divClass.fold("")((name) => "<div class=\"" + name + "\">") + s"<UL class=" + "\"" + collapsableClass + "\">\n" +
    s"\t<LI><SPAN>$name</SPAN>\n\t\t<UL>\n" + iterable.map( (el) => s"\t\t\t<LI><SPAN>$el</SPAN></LI>" ).mkString("\n") +
    "\t\t</UL>\n\t</LI></UL>" + divClass.fold("")((_) => "</div>")
}

class BasicDescriptor[+Component] extends Descriptor[Component] {
  def describe[U >: Component](component: U): String = component.toString
}