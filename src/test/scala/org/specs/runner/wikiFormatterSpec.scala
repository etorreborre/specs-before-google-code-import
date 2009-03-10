package org.specs.runner
import org.specs._
import org.specs.specification._
import scala.xml._
import org.specs.Sugar._

class wikiFormatterSpec extends Specification with JUnit {
  "A wiki formatter" should {
    def formatString(s: String): String = new WikiFormatter().format(s)
    def formatElem(e: Elem): Node = new WikiFormatter().format(e)

    "return a string as it is if isn't some html text" in {
      formatString("a description") must_== "a description"
    }
    "format the description of example as some xml text" in {
      val example = new Example("", null)
      example.exampleDescription = new WikiExampleDescription("a description")
      new WikiFormatter().formatDesc(example) must_== <t>a description</t>
    }
    "format single quotes as single quotes inside brackets when using html escape convention ==" in {
      formatElem(<wiki>==['a description']==</wiki>) must \\(<p>['a description']</p>)
    }
    "format single quotes as single quotes" in {
      formatString("don't") must include("don't")
    }
  }
  "A wiki formatter setStatus function" should {
    val exampleDesc = "a description"
    val example = new Example("a description", this)
    val descWithStatus = new WikiFormatter().setStatus(exampleDesc, List(example))

    "set the example status as an xml attribute" in {
      descWithStatus must include("<ex class=\"success\"")
    }
    "set a mouseover function to open a tooltip for the example as an xml attribute" in {
      descWithStatus must include("onmouseover=\"showExampleDesc")
    }
    "set a mouseout function to close the tooltip for the example" in {
      descWithStatus must include("onmouseout=\"hideToolTip();\"")
      descWithStatus must include("a description")
      descWithStatus must beMatching("==\\<ex.*\\>.*\\<\\/ex\\>==")
    }
    "leave the example description" in {
      descWithStatus must include("a description")
    }
    "enclose the description with ex tags protected by wiki markup" in {
      descWithStatus must beMatching("==\\<ex.*\\>a description\\<\\/ex\\>==")
    }
  }
}
