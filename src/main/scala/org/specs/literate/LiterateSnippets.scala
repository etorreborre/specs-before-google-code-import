package org.specs.literate
import org.specs.specification._
import org.specs.matcher._

/**
 * This trait adds the possibility to define and execute fragments of code in Literate specifications
 */
trait LiterateSnippets extends SnipIt with ExpectableFactory with Matchers { 
  def executeIs(s: String) = { execute(it) must include(s) }
  def executeIsNot(s: String) = execute(it) mustNot include(s)
  implicit def toOutputSnippet(s: String) = OutputSnippet(s)
  case class OutputSnippet(s: String) {
    def add_> = {
      s add it
      "> " + execute(it)
    } 
  }
  def >(s: String) = outputIs(s)
  def outputIs(s: String) = {
    val result = execute(it)
    var out = "> " + result
    try  { result must include(s) }
    catch { case e => out = "> " + e.getMessage }
    out
  }
}