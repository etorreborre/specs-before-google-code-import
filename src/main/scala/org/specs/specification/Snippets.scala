package org.specs.specification
import org.specs.util._

trait Snippets extends ScalaInterpreter with Wiki {

  case class Snippet(snippet: String) {
    val prelude: Property[String] = Property("")
    def ++(other: Snippet): Snippet = {
      val newSnippet = Snippet(this.snippet + "\n" + other.snippet)
      newSnippet.prelude(this.prelude() + "\n" + other.prelude())
      newSnippet
    }
    def addTo(prop: Property[Snippet]): String = {
      prop(prop() ++ this)
      this.snippet >@
    }
    def prelude(prop: Property[Snippet]): String = {
      prop().prelude(this.snippet)
      this.snippet >@
    }
    def snip(prop: Property[Snippet]): String = {
      val newSnippet = Snippet(this.snippet)
      newSnippet.prelude(prop().prelude())
      prop(newSnippet) 
      this.snippet >@
    }
    def code = prelude() + "\n" + snippet
  }
  implicit def asSnippet(s: java.lang.String) = new Snippet(s)

  def execute(it: Property[Snippet]) = interpret(it().code).replace("<console>:", "res")

  val it: Property[Snippet] = new Property[Snippet](Snippet(""))
}
object Snippets extends Snippets
