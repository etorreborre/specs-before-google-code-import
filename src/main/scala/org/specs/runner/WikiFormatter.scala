package org.specs.runner
import org.specs.log.ConsoleLog
import org.specs.specification._
import scala.xml._
import org.eclipse.mylyn.wikitext.core.parser.MarkupParser
import org.eclipse.mylyn.wikitext.textile.core.TextileLanguage
import org.specs.util.ExtendedString._

class WikiFormatter extends LiterateDescriptionFormatter with ConsoleLog {
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
    if (parsed contains "<p>") {
      val p1 = parsed.substring(parsed.indexOf("<p>") + 3, parsed.size)
      p1.substring(0, p1.indexOf("</p>"))
    } else
      parsed
  }
  protected def parseToHtml(s: String) = {
    debug("before is \n" + s)
    val parser = new MarkupParser()
    parser.setMarkupLanguage(new TextileLanguage())
    val parsed = parser.parseToHtml(s)
    debug("parsed is \n" + parsed)
    val replaced = parsed.
    replace("&#8220;", "\"").
    replace("&#8221;", "\"").
    replace("&#8216;", "'").
    replace("&#8217;", "'").
    replaceGroups("(<code>((.)*)</code>)", (s: String) =>
      s.replace("<br/>", "<br></br>").
      replace("&amp;quot;", "\"")
    )
    debug("replaced is \n" + replaced)
    replaced
  }

  def format(desc: Elem): Node = format(desc, Nil)
  def format(desc: Elem, examples: Iterable[Example]): Node = {
    val parsed = parseToHtml(setStatus(desc.child.text, examples))
    try {
      XML.loadString(parsed)
    } catch {
      case e => {println(parsed);Text(e.getMessage + "\\n" + parsed)}
    }
  }
  override def formatDesc(ex: Example) = {
    val text =  XML.loadString("<t>" + format(ex.exampleDescription.toString) + "</t>")
    text
  }
}
class TextileFormatter extends WikiFormatter
case class WikiExampleDescription(override val desc: String) extends ExampleDescription(desc)
