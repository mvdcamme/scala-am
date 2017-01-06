trait Descriptor[+Component] {
  val collapsableClass = "collapsable"

  def describe: String

  /**
    * Generates a description, which can be collapsed, for each element that can be iterated over.
    * @param elements The elements to iterate over.
    * @param describe The function with which to describe each element.
    * @tparam A The type of the elements to describe.
    * @return The description.
    */
  def describeCollapsableList[A](elements: Iterable[A], describe: A => String = (x: A) => x.toString): String =
    s"<UL class=" + "\"" + collapsableClass + "\">" + elements.map((el) => s"<LI><span>${describe(el)}</span></LI>\n") +
     "</UL>"

  def putIntoCollapsableList(iterable: Iterable[String]): String =
    s"<UL class=" + "\"" + collapsableClass + "\">" + iterable.map( (el) => s"<LI><span>$el</span></LI>\n" ) + "</UL>"
}

class BasicDescriptor[+Component](val component: Component) extends Descriptor[Component] {
  override def describe: String = component.toString
}