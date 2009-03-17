package org.specs.specification
import org.specs.util._

trait Snippets extends ScalaInterpreter {
  def format(code: String): String = code
  implicit def asSnippet(s: java.lang.String) = new SnippetAdder(Snippet(s))
  class SnippetAdder(snippet: Snippet) {
	def add(prop: Property[Snippet]): String = addTo(prop)
	def addTo(prop: Property[Snippet]): String = {
	  prop(prop() ++ snippet)
	  format(snippet.snippet)
	}
	def prelude(prop: Property[Snippet]): String = {
	  prop().prelude(snippet.snippet)
	  format(snippet.snippet)
	}
	def snip(prop: Property[Snippet]): String = {
	  val newSnippet = Snippet(snippet.snippet)
	  newSnippet.prelude(prop().prelude)
	  prop(newSnippet) 
	  format(snippet.snippet)
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
  def prelude(p: String) = { preludeCode(append(preludeCode(), p)); this }
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
trait SnipIt extends Snippets with Wiki {
  val it: Property[Snippet] = new Property[Snippet](Snippet(""))
  override def format(code: String) = code >@
  override def execute(it: Property[Snippet]) = super[Snippets].execute(it) >@
}
