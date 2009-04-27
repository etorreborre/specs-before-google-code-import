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
      case desc: TextileExampleDescription =>  formatter("textile").formatDesc(ex)
      case desc: MarkdownExampleDescription =>  formatter("markdown").formatDesc(ex)
      case _ =>  new Text(ex.exampleDescription.format)
    }
  }
  /** create an example description which may be processed differently depending on the markup language */
  def makeExampleDescription(content: Elem, node: NodeSeq) = {
   if (content.exists(_.label == "wiki")) 
     WikiExampleDescription(node.first.text) 
   else if (content.exists(_.label == "textile")) 
     TextileExampleDescription(node.first.text) 
   else if (content.exists(_.label == "markdown")) 
     MarkdownExampleDescription(node.first.text) 
   else ExampleDescription(node.first.text)
  }
  
  
}
class TextFormatter extends LiterateDescriptionFormatter {
  def format(desc: Elem, examples: Iterable[Example]) = Group(desc.child)
}
class HtmlFormatter extends LiterateDescriptionFormatter {
  def format(desc: Elem, examples: Iterable[Example]) = Group(desc.child)
}
