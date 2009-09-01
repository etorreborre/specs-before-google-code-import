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
package org.specs.specification
import org.specs.util._
import org.specs.literate._

trait Snippets extends ScalaInterpreter {
  def formatCode(code: String): String = code
  implicit def asSnippet(s: java.lang.String) = new SnippetAdder(Snippet(s))
  class SnippetAdder(snippet: Snippet) {
	def add(prop: Property[Snippet]): String = addTo(prop)
	def addTo(prop: Property[Snippet]): String = {
	  prop.forceUpdate(prop.get ++ snippet)
	  formatCode(snippet.snippet)
	}
	def prelude(prop: Property[Snippet]): String = {
	  prop.get.prelude(snippet.snippet)
	  formatCode(snippet.snippet)
	}
	def snip(prop: Property[Snippet]): String = {
	  val newSnippet = Snippet(snippet.snippet)
	  newSnippet.prelude(prop.get.prelude)
	  prop(newSnippet) 
	  formatCode(snippet.snippet)
	}
  }

  def execute(it: Property[Snippet]) = interpret(it().code)

}
object Snippet {
  def apply(s: String) = new Snippet(Property(s))
}
case class Snippet(snippetCode: Property[String]) {
  private val preludeCode: Property[String] = Property("")
  def ++(other: Snippet): Snippet = {
    val newSnippet = Snippet(append(this.snippet, other.snippet))
    newSnippet.prelude(append(prelude, other.prelude))
    newSnippet
  }
  def prelude(p: String) = { preludeCode.forceUpdate(append(preludeCode(), p)); this }
  def prelude = preludeCode()
  def snippet = snippetCode()
  def code = append(prelude, snippet)
  private def append(a: String, b: String) = {
    if (a.isEmpty)
      b
    else if (b.isEmpty)
      a
    else if (a.endsWith("\n"))
      a + b
    else 
      a + "\n" + b
  }
  def reset = snippetCode("")
}
object Snippets extends Snippets
