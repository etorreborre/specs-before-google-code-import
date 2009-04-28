/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS INTHE SOFTWARE.
 */
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
        var function = if (example.hasIssues) "showExampleMessage('" + example.statusClass.capitalize + "','" else "showExampleDesc('"
        "onmouseover=\"" + function + System.identityHashCode(example) +"',event)\" " +
        "onmouseout=\"hideToolTip();\""
      }
      def toReplace = escapeHtml("<ex class=\"" + example.statusClass + "\" " + onmouse(example) + ">" +
                      format(example.description) + "</ex>")
      result = result.replace(example.description.toString, toReplace)
    }
    result
  }
  def escapeHtml(s: String) = s
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
    replace("<br/>", "").
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
class TextileFormatter extends WikiFormatter {
  override def escapeHtml(s: String) = "=="+s+"=="
}
case class WikiExampleDescription(override val desc: String) extends ExampleDescription(desc)
case class TextileExampleDescription(override val desc: String) extends ExampleDescription(desc)
case class MarkdownExampleDescription(override val desc: String) extends ExampleDescription(desc)
