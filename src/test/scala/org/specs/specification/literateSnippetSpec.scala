package org.specs.specification
import org.specs._
import org.specs.util._
import org.specs.runner._

class literateSnippetSpec extends LiterateSpecification("Literate snippet") with JUnit with Html with Examples { <wiki>

h3. Declaration

In Literate specifications it often desirable to show the behaviour of some code:{ """
  val a = 1
  val b = 1
  a + b
""" snip (it) }

<ex>The code above should be executable and return the value "2"</ex>: { executeIt }

</wiki> isSus
}
trait Examples extends Snippets with Expectations {
  def executeIt = {
    val result = execute(it)
    result must_== "2"
    result
  }
}
