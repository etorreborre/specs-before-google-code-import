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
package org.specs.literate
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
        include("""this is the <ex class="success" """) and
        include("</ex> to be highlighted")
      )
    }
  }
  def formatter = new DescriptionFormatter()
  def format(node: Elem) = formatter.format(node, Nil)
  def wikiFormatter = new WikiFormatter {}
}
