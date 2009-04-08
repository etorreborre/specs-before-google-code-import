package org.specs.runner
import scala.xml._
import org.specs.Sugar._
import org.specs.specification._
import org.specs.runner._

class descriptionFormatterSpec extends Specification with JUnit {

  "A description formatter" should {
    "format a description as text if it has the text tag" in {
      format(<text>Hello world</text>).text must_==
        "Hello world"
    }
    detailedDiffs()
    "format a description as wiki markup if it has the wiki tag" in {
      format(<wiki>h1. Hello world</wiki>) must \\("h1")
    }
    "format a description as html if it has the html tag" in {
      format(<html>This is some <i>html</i> text</html>) must \\("i")
    }
  }
  "A wiki description formatter" should {
    val example = new Example("example desc", this).in { 1 must_== 1 }
    "set the status of the example descriptions depending on the example status" in {
      wikiFormatter.setStatus("this is the " + example.description + " to be highlighted", List(example)) must (
        include("""this is the ==<ex class="success" """) and
        include("</ex>== to be highlighted")
      )
    }
  }
  def formatter = new DescriptionFormatter()
  def format(node: Elem) = formatter.format(node, Nil)
  def wikiFormatter = new WikiFormatter
}
