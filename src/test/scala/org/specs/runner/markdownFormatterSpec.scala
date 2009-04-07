package org.specs.runner
import org.specs.specification.Example
import scala.xml._

class markdownFormatterSpec extends spex.Specification {
  def formatString(s: String): String = new MarkdownFormatter{}.format(s)
  def formatElem(e: Elem): Node = new MarkdownFormatter{}.format(e)

  "A markdown formatter" should {
    "return a string as it is if isn't some html text" in {
      formatString("a description") must_== "a description"
    }
    "format the description of example as some xml text" in {
      val example = new Example("", null)
      example.exampleDescription = new WikiExampleDescription("a description")
      new MarkdownFormatter().formatDesc(example) must_== <t>a description</t>
    }
    "format single quotes as single quotes" in {
      formatString("don't") must include("don't")
    }
    "format html which is well formed for further parsing" in {
      formatElem(<ex>this is some *text* to format</ex>) must ==/(<div><p>this is some <em>text</em> to format</p></div>)
    }
  }
}
