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

<ex>The code above should be executable and return the value "2"</ex>

> { executeAndExpect("2") }

h3. Execution

<ex>Snipping a new piece of code should remove the previous one</ex>: { """
  var a = 2
  var b = 3
  a + b
""" snip (it) }

> { executeAndExpect("5") }

But <ex>it is possible to add a piece of code to a previous snippet with @addTo@</ex>:
{ """
  b = 6
  a + b
""" addTo (it) }

> { executeAndExpect("8") }


h3. Prelude

<ex>Imports can be added to a snippet variable with @prelude@ and be persistent even if the code is snipped again</ex>:
{ "import java.util.ArrayList" prelude (it) }
{ """val l = new ArrayList[Int]()
     l.add(3)
     l.get(0)""" snip (it) }

> { executeAndExpect("3") }


</wiki> isSus
}
trait Examples extends SnipIt with Expectations {
  def executeAndExpect(expected: String) = {
    val result = execute(it)
    result must include(expected)
    result
  }
}
