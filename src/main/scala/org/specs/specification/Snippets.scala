package org.specs.specification
import org.specs.util._

trait Snippets extends ScalaInterpreter {
  def format(code: String): String = code

  case class Snippet(snippet: String) {
    val prelude: Property[String] = Property("")
    def ++(other: Snippet): Snippet = {
      val newSnippet = Snippet(append(this.snippet, other.snippet))
      newSnippet.prelude(append(this.prelude(), other.prelude()))
      newSnippet
    }
    def addTo(prop: Property[Snippet]): String = {
      prop(prop() ++ this)
      format(this.snippet)
    }
    def prelude(prop: Property[Snippet]): String = {
      prop().prelude(this.snippet)
      format(this.snippet)
    }
    def snip(prop: Property[Snippet]): String = {
      val newSnippet = Snippet(this.snippet)
      newSnippet.prelude(prop().prelude())
      prop(newSnippet) 
      format(this.snippet)
    }
    def code = append(prelude(), snippet)
    private def append(a: String, b: String) = {
      if (a.isEmpty)
        b
      else if (b.isEmpty)
        a
      else
        a + "\n" + b
    }
  }
  implicit def asSnippet(s: java.lang.String) = new Snippet(s)

  def execute(it: Property[Snippet]) = interpret(it().code)

}
object Snippets extends Snippets
trait SnipIt extends Snippets with Wiki {
  val it: Property[Snippet] = new Property[Snippet](Snippet(""))
  override def format(code: String) = code >@
  override def execute(it: Property[Snippet]) = super[Snippets].execute(it) >@
}
