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
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.literate
import org.specs.util.Classes._
import scala.xml._
import org.specs.specification._

class DescriptionFormatter extends WikiFormatter {
  def className(name: String) = "org.specs.literate." + name.toLowerCase.capitalize + "Formatter"
  def formatter(n: String) = createObject[LiterateDescriptionFormatter](className(n)) match {
    case Some(f) =>  f
    case None => new TextFormatter
  }
  
  override def format(desc: Elem, examples: Iterable[Example]) = {
    formatter(desc.label).format(desc, examples)
  }
  override def formatDesc(ex: Example): Node = {
    ex.exampleDescription match {
      case desc: WikiExampleDescription =>  formatter("wiki").formatDesc(ex)
      case desc: TextileExampleDescription =>  formatter("textile").formatDesc(ex)
      case desc: MarkdownExampleDescription =>  formatter("markdown").formatDesc(ex)
      case _ =>  super.formatDesc(ex)
    }
  }
}
