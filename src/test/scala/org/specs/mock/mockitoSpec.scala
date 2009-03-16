package org.specs.mock
import org.specs._
import org.specs.specification._
import org.specs.runner._

class mockitoSpec extends mockitoRules {
  <wiki>
Mockito is a Java library for mocking.

  The following samples are taken from the main documentation which can be found "here":http://mockito.googlecode.com/svn/branches/1.7/javadoc/org/mockito/Mockito.html

h3. Let's verify some behaviour

First of all, we need to import some classes and traits for our examples: {"""
import org.specs.Specification
import org.specs.mock.Mockito
import java.util.List""" prelude it }

A mock is created with the @mock@ method{"""
object s extends Specification with Mockito {

    // mock creation
    val mockedList = mock[List[String]]

    // using mock object
    mockedList.add("one")
    mockedList.clear()""" snip it }

   // <ex>It is possible to check that some methods have been called on the mock with the @called@ matcher</ex>:
{"""  // verification
    theMethod(mockedList).add("one") was called
    theMethod(mockedList).clear() was called
} """ addTo it } { executeIsNot("error") }

h4. Failures

If one method has not been called on a mock, <ex>the @was called@ matcher must throw a FailureException</ex>:
{"""object s extends Specification with Mockito {
   val m = mock[List[String]]
   theMethod(m).clear() was called
 }
 s.failures.first.getMessage
  """ snip it } { outputIs("The method was not called: list.clear()") }

  </wiki> isSus
}
class mockitoRules extends LiterateSpecification("Mockito Specification") with Mockito with SnipIt with Wiki with Html with JUnit {
  override def htmlDir = "target"
  def executeIs(s: String) = { execute(it) must include(s) }
  def outputIs(s: String) = {
    val result = execute(it)
    var out = "> " + result
    try  { result must include(s) }
    catch { case e => out = "> " + e.getMessage }
    out >@
  }
  def executeIsNot(s: String) = execute(it) mustNot include(s)
}

