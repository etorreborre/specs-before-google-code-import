package org.specs.runner
import scala.xml._
import org.specs.specification._
import org.specs.util.Classes._

trait LiterateDescriptionFormatter {
  
  /** 
   * return the literate description as a xml node which can be easily transformed to a string with 
   * the text method. 
   */
  def format(desc: Elem, examples: Iterable[Example]): Node
  def formatDesc(example: Example): Node = Text(example.exampleDescription.toString) 
}

class DescriptionFormatter extends LiterateDescriptionFormatter {
  def className(name: String) = "org.specs.runner." + name.toLowerCase.capitalize + "Formatter"
  def formatter(n: String) = createObject[LiterateDescriptionFormatter](className(n)) match {
    case Some(f) =>  f
    case None => new TextFormatter
  }
  
  def format(desc: Elem, examples: Iterable[Example]) = {
    formatter(desc.label).format(desc, examples)
  }
  override def formatDesc(ex: Example): Node = {
    ex.exampleDescription match {
      case desc: WikiExampleDescription =>  formatter("wiki").formatDesc(ex)
      case _ =>  new Text(ex.exampleDescription.format)
    }
  }
}
class TextFormatter extends LiterateDescriptionFormatter {
  def format(desc: Elem, examples: Iterable[Example]) = Group(desc.child)
}
class HtmlFormatter extends LiterateDescriptionFormatter {
  def format(desc: Elem, examples: Iterable[Example]) = Group(desc.child)
}
