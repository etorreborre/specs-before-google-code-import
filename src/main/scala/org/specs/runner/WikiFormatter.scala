package org.specs.runner
import org.specs.specification._
import scala.xml._
import net.java.textilej.parser._
import org.specs.util.ExtendedString._

class WikiFormatter extends LiterateDescriptionFormatter {
  def setStatus(desc: String, examples: Iterable[Example]) = {
    var result = desc
    examples foreach { example => 
      def onmouse(example: Example) = {
        var function = if (example.hasIssues) "showExampleMessage('" + example.status.capitalize + "','" else "showExampleDesc('" 
        "onmouseover=\"" + function + System.identityHashCode(example) +"',event)\" " +
        "onmouseout=\"hideToolTip();\""
      }
      def toReplace = "==<ex class=\"" + example.status + "\" " + onmouse(example) + ">" +
                      format(example.description) + "</ex>=="
      result = result.replace(example.description.toString, toReplace)
    }
    result
  }
  def format(desc: String): String = {
	val parsed = parseToHtml(desc)
	val p1 = parsed.substring(parsed.indexOf("<p>") + 3, parsed.size)
 	p1.substring(0, p1.indexOf("</p>"))
  }
  private def parseToHtml(s: String) = new TextileParser().parseToHtml(s).
    replace("&#8217;", "'").
    replaceGroups("(<code>(.+)</code>)", (s: String) => s.replace("<br/>", "\n"))
  
  def format(desc: Elem): Node = format(desc, Nil)
  def format(desc: Elem, examples: Iterable[Example]): Node = {
    val parsed = parseToHtml(setStatus(desc.child.text, examples))
    try {
      XML.loadString(parsed) 
    } catch {
      case e => Text(e.getMessage + "\\n" + parsed)
    }
  }
  override def formatDesc(ex: Example) = {
    val text =  XML.loadString("<t>" + format(ex.exampleDescription.toString) + "</t>")
    text
  }
}
case class WikiExampleDescription(override val desc: String) extends ExampleDescription(desc)
