package org.specs.specification
import org.specs.util._

trait Snippets extends ScalaInterpreter with Wiki {

  case class Snippet(snippet: String) {
    def ++(other: Snippet) = Snippet(this.snippet + "\n" + other.snippet)
    def addTo(prop: Property[Snippet]): String = {
      prop(prop() ++ this)
      this.snippet >@
    }
    def snip(prop: Property[Snippet]): String = {
      prop(prop(Snippet("")) ++ this)
      this.snippet >@
    }
  }
  implicit def asSnippet(s: java.lang.String) = new Snippet(s)

  def execute(it: Property[Snippet]) = interpret(it().snippet)

  val it: Property[Snippet] = new Property[Snippet](Snippet(""))
}
object Snippets extends Snippets
