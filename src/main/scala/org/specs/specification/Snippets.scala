package org.specs.specification
import org.specs.util._

trait Snippets extends ScalaInterpreter {

  case class Snippet(snippet: String) {
    val prelude: Property[String] = Property("")
    def ++(other: Snippet): Snippet = {
      val newSnippet = Snippet(this.snippet + "\n" + other.snippet)
      newSnippet.prelude(this.prelude() + "\n" + other.prelude())
      newSnippet
    }
    def format(code: String) = code
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
    def code = prelude() + "\n" + snippet
  }
  implicit def asSnippet(s: java.lang.String) = new Snippet(s)

  def execute(it: Property[Snippet]) = interpret(it().code).replace("<console>:", "res")

}
object Snippets extends Snippets
trait SnipIt extends Snippets with Wiki {
  val it: Property[Snippet] = new Property[Snippet](Snippet(""))
  override def format(code: String) = code >@
}
